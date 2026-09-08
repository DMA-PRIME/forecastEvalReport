(function () {
  "use strict";

  const payload = JSON.parse(document.getElementById("dashboard-data").textContent);
  const models = payload.models || [];
  const parameters = payload.parameters || [];
  const forecasts = payload.forecasts || [];
  const truth = payload.truth || [];
  const performance = payload.performance || [];
  const peakDetails = payload.peak_details || [];
  const seasonStartMonth = Number(payload.season_start_month) || 8;
  const descriptions = payload.metric_descriptions || [];
  const trendHorizon = Number(payload.trend_horizon) || 2;
  const selectedModels = new Set(models.map(x => x.model));
  const hiddenPerformanceModels = new Set();
  const palette = ["#087f86", "#e76f51", "#3a6ea5", "#8b5fbf", "#27815b", "#c18a14", "#c94f72", "#50667a"];
  const colors = Object.fromEntries(models.map((m, i) => [m.model, m.color || palette[i % palette.length]]));

  const $ = id => document.getElementById(id);
  const uniq = values => [...new Set(values.filter(v => v !== null && v !== undefined && String(v) !== ""))].sort((a, b) => String(a).localeCompare(String(b), undefined, { numeric: true }));
  const numeric = value => value !== null && value !== "" && Number.isFinite(Number(value));
  const trendStableThreshold = numeric(payload.trend_stable_threshold) ? Number(payload.trend_stable_threshold) : null;
  const fmt = value => numeric(value) ? Number(value).toLocaleString(undefined, { maximumFractionDigits: 4 }) : (value ?? "—");
  const esc = value => String(value ?? "").replace(/[&<>"']/g, ch => ({"&":"&amp;","<":"&lt;",">":"&gt;",'"':"&quot;","'":"&#39;"}[ch]));

  function setOptions(select, values, allLabel, preserve = true) {
    const previous = preserve ? select.value : "";
    const items = allLabel ? [{ value: "__all__", label: allLabel }] : [];
    values.forEach(value => items.push({ value: String(value), label: String(value) }));
    select.innerHTML = items.map(x => `<option value="${esc(x.value)}">${esc(x.label)}</option>`).join("");
    if (items.some(x => x.value === previous)) select.value = previous;
  }

  const splitKeys = value => String(value || "").split("|||").filter(Boolean);

  function filteredModels() {
    const disease = $("diseaseFilter").value;
    const location = $("locationFilter").value;
    const reason = $("reasonFilter").value;
    return models.filter(m =>
      (disease === "__all__" || splitKeys(m.disease_keys).includes(disease)) &&
      (location === "__all__" || splitKeys(m.location_keys).includes(location)) &&
      (reason === "__all__" || m.reason === reason));
  }

  function modelInGlobalFilters(model) {
    return filteredModels().some(m => m.model === model);
  }

  function selected(row) {
    return selectedModels.has(row.model) && modelInGlobalFilters(row.model);
  }

  function setupGlobalFilters() {
    const diseases = uniq(models.flatMap(m => splitKeys(m.disease_keys)));
    setOptions($("diseaseFilter"), diseases, "All diseases", false);
    updateLocationFilter();
    setOptions($("reasonFilter"), uniq(models.map(m => m.reason)), "All forecasting reasons", false);
    $("diseaseFilter").addEventListener("change", () => {
      updateLocationFilter();
      buildModelFilters();
      buildOverview();
      refreshControlDomains();
    });
    $("locationFilter").addEventListener("change", () => {
      buildModelFilters();
      buildOverview();
      refreshControlDomains();
    });
    $("reasonFilter").addEventListener("change", () => {
      buildModelFilters();
      buildOverview();
      refreshControlDomains();
    });
  }

  function updateLocationFilter() {
    const disease = $("diseaseFilter").value;
    const eligible = models.filter(m => disease === "__all__" ||
      splitKeys(m.disease_keys).includes(disease));
    setOptions($("locationFilter"),
      uniq(eligible.flatMap(m => splitKeys(m.location_keys))),
      "All locations");
  }

  function buildModelFilters() {
    const shell = $("modelChecks");
    shell.innerHTML = filteredModels().map((m, i) => `
      <label class="model-check">
        <input type="checkbox" value="${esc(m.model)}" ${selectedModels.has(m.model) ? "checked" : ""}>
        <span style="color:${colors[m.model]}">●</span>${esc(m.model)}
      </label>`).join("");
    shell.querySelectorAll("input").forEach(input => input.addEventListener("change", () => {
      input.checked ? selectedModels.add(input.value) : selectedModels.delete(input.value);
      updatePerformanceDiseases(false);
      refreshAll();
    }));
  }

  function buildOverview() {
    const visibleModels = filteredModels();
    $("summaryModels").textContent = visibleModels.length;
    $("summaryLocations").textContent = uniq(visibleModels.flatMap(m => splitKeys(m.location_keys))).length;
    $("summaryDiseases").textContent = uniq(visibleModels.flatMap(m => splitKeys(m.disease_keys))).length;
    const dates = visibleModels.flatMap(x => [x.training_start, x.training_end, x.validation_start, x.validation_end, x.testing_start, x.testing_end, x.realtime_start, x.realtime_end]).filter(Boolean).sort();
    $("summaryWindow").textContent = dates.length ? `${dates[0]} → ${dates[dates.length - 1]}` : "Forecast only";
    renderTrendIndicators();

    const period = (start, end) => start && end ? `${esc(start)} through ${esc(end)}` : "Not available";

    $("modelGrid").innerHTML = visibleModels.map(m => `
      <article class="model-card" style="--model-color:${colors[m.model]}">
        <h3>${esc(m.model)}</h3>
        <p>${esc(m.description || "No model description supplied.")}</p>
        <dl>
          <dt>Forecasting context</dt><dd>${esc(m.reason)}</dd>
          <dt>Disease</dt><dd>${esc(m.disease)}</dd>
          <dt>Outcome</dt><dd>${esc(m.outcome)}</dd>
          <dt>Spatial scale</dt><dd>${esc(m.spatial_scale)}</dd>
          <dt>Evaluation included</dt><dd>${esc(m.evaluation)}</dd>
          <dt>Locations</dt><dd>${esc(m.locations || "—")}</dd>
          <dt>Data sets used</dt><dd>${esc(m.data_sets)}</dd>
          <dt>Variables used</dt><dd>${esc(m.variables)}</dd>
          <dt>Training period</dt><dd>${period(m.training_start, m.training_end)}</dd>
          <dt>Validation period</dt><dd>${period(m.validation_start, m.validation_end)}</dd>
          <dt>Testing period</dt><dd>${period(m.testing_start, m.testing_end)}</dd>
          <dt>Real-time period</dt><dd>${period(m.realtime_start, m.realtime_end)}</dd>
        </dl>
      </article>`).join("");

    $("coverageGrid").innerHTML = visibleModels.map(m => `
      <article class="coverage-card" style="--model-color:${colors[m.model]}">
        <h3>${esc(m.model)}</h3>
        <p><strong>${esc(m.evaluation)}</strong></p>
        <p class="method-note">${esc(m.locations || "No locations")} · ${esc(m.spatial_scale)}</p>
      </article>`).join("");
  }

  function median(values) {
    const clean = values.filter(Number.isFinite).sort((a, b) => a - b);
    if (!clean.length) return null;
    const middle = Math.floor(clean.length / 2);
    return clean.length % 2 ? clean[middle] : (clean[middle - 1] + clean[middle]) / 2;
  }

  function percentile(values, probability) {
    const clean = values.filter(Number.isFinite).sort((a, b) => a - b);
    if (!clean.length) return null;
    const index = (clean.length - 1) * probability;
    const lower = Math.floor(index), upper = Math.ceil(index);
    return lower === upper ? clean[lower] : clean[lower] + (clean[upper] - clean[lower]) * (index - lower);
  }

  function observedTrendThresholds(disease, location) {
    const byDate = new Map();
    truth.filter(row => selected(row) && row.disease === disease && row.location === location &&
      numeric(row.value) && Number.isFinite(Date.parse(row.date))).forEach(row => {
      if (!byDate.has(row.date)) byDate.set(row.date, []);
      byDate.get(row.date).push(Number(row.value));
    });
    const points = [...byDate.entries()].map(([date, values]) => ({ date, value: median(values) }))
      .filter(point => point.value !== null).sort((a, b) => Date.parse(a.date) - Date.parse(b.date));
    const changes = [];
    for (let i = 1; i < points.length; i++) {
      const days = (Date.parse(points[i].date) - Date.parse(points[i - 1].date)) / 86400000;
      if (Math.abs(days - 7) < .01) changes.push(points[i].value - points[i - 1].value);
    }
    return changes.length >= 4 ? {
      p05: percentile(changes, .05), p25: percentile(changes, .25),
      p75: percentile(changes, .75), p95: percentile(changes, .95)
    } : null;
  }

  function classifyTrend(change, thresholds) {
    if (!Number.isFinite(change)) return null;
    if (trendStableThreshold !== null && Math.abs(change) < trendStableThreshold) return "stable";
    if (!thresholds) return change > 0 ? "increase" : change < 0 ? "decrease" : "stable";
    if (change <= thresholds.p05) return "large decrease";
    if (change <= thresholds.p25) return "decrease";
    if (change < thresholds.p75) return "stable";
    if (change < thresholds.p95) return "increase";
    return "large increase";
  }

  function latestModelTrend(model, disease, location, thresholds) {
    const central = row => !numeric(row.quantile) || Math.abs(Number(row.quantile) - .5) < 1e-8;
    const sourceOrder = ["Current forecast", "Real-time archived forecasts", "Testing median forecasts"];
    const modelRows = forecasts.filter(row => selected(row) && row.model === model &&
      row.disease === disease && row.location === location && central(row) && numeric(row.value) &&
      Number.isFinite(Date.parse(row.target_end_date)));
    const source = sourceOrder.find(name => modelRows.some(row => row.series_type === name));
    if (!source) return null;
    let rows = modelRows.filter(row => row.series_type === source);
    const references = uniq(rows.map(row => row.reference_date)).filter(Boolean).sort();
    const reference = references.length ? references[references.length - 1] : null;
    if (reference) rows = rows.filter(row => row.reference_date === reference);

    const horizonRows = rows.filter(row => numeric(row.horizon) && Number(row.horizon) >= 0 && Number(row.horizon) <= trendHorizon);
    if (horizonRows.length >= 2) rows = horizonRows;
    const byDate = new Map();
    rows.forEach(row => {
      if (!byDate.has(row.target_end_date)) byDate.set(row.target_end_date, []);
      byDate.get(row.target_end_date).push(Number(row.value));
    });
    const points = [...byDate.entries()].map(([date, values]) => ({ date, value: median(values) }))
      .filter(point => point.value !== null).sort((a, b) => Date.parse(a.date) - Date.parse(b.date))
      .slice(0, trendHorizon + 1);
    if (points.length < 2) return null;
    const steps = points.length - 1;
    const totalChange = points[points.length - 1].value - points[0].value;
    const averageStepChange = totalChange / steps;
    const percentChange = Math.abs(points[0].value) > 1e-8 ? totalChange / Math.abs(points[0].value) * 100 : null;
    return {
      model, call: classifyTrend(averageStepChange, thresholds), totalChange,
      percentChange, steps, source, reference,
      through: points[points.length - 1].date
    };
  }

  function renderTrendIndicators() {
    const shell = $("trendIndicators");
    const groups = uniq(forecasts.filter(row => selected(row) && row.disease && row.location)
      .map(row => `${row.disease}\r${row.location}`));
    const labelMap = {
      "large increase": { label: "Sharp increase", icon: "↗", css: "large-increase" },
      "increase": { label: "Increasing", icon: "↗", css: "increase" },
      "stable": { label: "Stable", icon: "→", css: "stable" },
      "decrease": { label: "Decreasing", icon: "↘", css: "decrease" },
      "large decrease": { label: "Sharp decrease", icon: "↘", css: "large-decrease" },
      "mixed": { label: "Mixed outlook", icon: "↔", css: "mixed" }
    };
    const sourceLabels = {
      "Current forecast": "Latest/current forecasts",
      "Real-time archived forecasts": "Latest archived forecasts",
      "Testing median forecasts": "Latest testing-period forecasts"
    };

    const cards = groups.map(key => {
      const [disease, location] = key.split("\r");
      const thresholds = observedTrendThresholds(disease, location);
      const modelNames = uniq(forecasts.filter(row => selected(row) && row.disease === disease && row.location === location).map(row => row.model));
      const calls = modelNames.map(model => latestModelTrend(model, disease, location, thresholds)).filter(result => result && result.call);
      if (!calls.length) return null;
      const counts = Object.fromEntries(uniq(calls.map(result => result.call)).map(call => [call, calls.filter(result => result.call === call).length]));
      const largest = Math.max(...Object.values(counts));
      const winners = Object.keys(counts).filter(call => counts[call] === largest);
      const consensusCall = winners.length === 1 ? winners[0] : "mixed";
      const agreement = largest / calls.length;
      const strength = calls.length === 1 ? "Single-model signal" : winners.length > 1 ? "Split signal" :
        agreement >= .8 ? "Strong consensus" : agreement >= .6 ? "Moderate consensus" : "Limited consensus";
      const display = labelMap[consensusCall];
      const percentChange = median(calls.map(result => result.percentChange).filter(Number.isFinite));
      const rawChange = median(calls.map(result => result.totalChange));
      const changeText = percentChange !== null ? `${percentChange >= 0 ? "+" : ""}${percentChange.toFixed(1)}%` :
        `${rawChange >= 0 ? "+" : ""}${fmt(rawChange)}`;
      const steps = Math.max(...calls.map(result => result.steps));
      const through = calls.map(result => result.through).filter(Boolean).sort().slice(-1)[0];
      const sources = uniq(calls.map(result => result.source));
      const sourceText = sources.length === 1 ? sourceLabels[sources[0]] : "Latest available forecasts";
      const distribution = Object.entries(counts).sort((a, b) => b[1] - a[1])
        .map(([call, count]) => `${count} ${labelMap[call].label.toLowerCase()}`).join(" · ");
      return `<article class="trend-indicator trend-${display.css}">
        <div class="trend-card-head"><div><span class="trend-disease">${esc(disease)}</span><h3>${esc(location)}</h3></div><span class="trend-icon" aria-hidden="true">${display.icon}</span></div>
        <div class="trend-call">${display.label}</div><div class="trend-strength">${strength}</div>
        <div class="trend-meter" aria-label="${Math.round(agreement * 100)} percent model agreement"><span style="width:${Math.round(agreement * 100)}%"></span></div>
        <div class="trend-stats"><div><strong>${largest} of ${calls.length}</strong><span>models agree</span></div><div><strong>${changeText}</strong><span>median projected change</span></div><div><strong>${steps}</strong><span>forecast step${steps === 1 ? "" : "s"}</span></div></div>
        <p class="trend-distribution">${esc(distribution)}</p>
        <p class="trend-source">${esc(sourceText)}${through ? ` · through ${esc(through)}` : ""}</p>
      </article>`;
    }).filter(Boolean);

    shell.innerHTML = cards.length ? cards.join("") :
      '<div class="callout trend-unavailable">Near-term trend indicators require at least two median forecast points for the selected models.</div>';
  }

  function activateTabs() {
    document.querySelectorAll(".tab").forEach(button => button.addEventListener("click", () => {
      document.querySelectorAll(".tab").forEach(x => x.classList.toggle("active", x === button));
      document.querySelectorAll(".panel").forEach(x => x.classList.toggle("active", x.id === button.dataset.panel));
      window.setTimeout(refreshAll, 20);
    }));
  }

  function setupForecastControls() {
    setOptions($("forecastLocation"), uniq(forecasts.filter(selected).map(x => x.location)), null, false);
    setOptions($("forecastReason"), uniq(filteredModels().map(m => m.reason)), "All forecasting purposes", false);
    setForecastViewOptions(false);
    updateForecastReferences(false);
    ["forecastLocation", "forecastReason", "forecastView", "forecastReference", "forecastHorizon"].forEach(id => $(id).addEventListener("change", () => {
      if (["forecastLocation", "forecastView", "forecastReason"].includes(id)) updateForecastReferences(false);
      if (["forecastView", "forecastReason", "forecastReference"].includes(id)) updateForecastHorizons();
      drawForecast();
    }));
    $("downloadForecast").addEventListener("click", () => downloadCsv(filteredForecastRows(), "forecast-comparison.csv"));
    ["showForecast80", "showForecast95", "showForecastObserved"].forEach(id =>
      $(id).addEventListener("change", drawForecast));
    $("printDashboard").addEventListener("click", () => window.print());
    updateForecastHorizons();
  }

  function setForecastViewOptions(preserve = true) {
    const select = $("forecastView");
    const previous = preserve ? select.value : "";
    const labels = {
      "Current forecast": "Latest/current forecast",
      "Testing median forecasts": "Testing-period forecasts",
      "Real-time archived forecasts": "Historical real-time forecasts"
    };
    const items = uniq(forecasts.filter(selected).map(x => x.series_type))
      .map(value => ({ value: String(value), label: labels[value] || String(value) }));
    select.innerHTML = items.map(x => `<option value="${esc(x.value)}">${esc(x.label)}</option>`).join("");
    if (items.some(x => x.value === previous)) select.value = previous;
  }

  function attachForecastHover(canvas, tooltip) {
    canvas.addEventListener("mousemove", event => {
      const rect = canvas.getBoundingClientRect();
      const x = event.clientX - rect.left, y = event.clientY - rect.top;
      let nearest = null, best = 14 * 14;
      (canvas._forecastHitPoints || []).forEach(point => {
        const distance = (point.x-x)*(point.x-x) + (point.y-y)*(point.y-y);
        if (distance < best) { best = distance; nearest = point; }
      });
      if (!nearest) { tooltip.classList.remove("show"); return; }
      tooltip.textContent = nearest.text;
      tooltip.style.left = `${Math.min(rect.width - 235, Math.max(8, x + 14))}px`;
      tooltip.style.top = `${Math.max(8, y - 58)}px`;
      tooltip.classList.add("show");
    });
    canvas.addEventListener("mouseleave", () => tooltip.classList.remove("show"));
  }

  function attachPerformanceHover(canvas, tooltip) {
    canvas.addEventListener("mousemove", event => {
      const rect = canvas.getBoundingClientRect();
      const x = event.clientX - rect.left, y = event.clientY - rect.top;
      let nearest = null, best = 16 * 16;
      (canvas._performanceHitPoints || []).forEach(point => {
        const distance = (point.x - x) * (point.x - x) + (point.y - y) * (point.y - y);
        if (distance < best) { best = distance; nearest = point; }
      });
      if (!nearest) { tooltip.classList.remove("show"); return; }
      tooltip.textContent = nearest.text;
      tooltip.style.left = `${Math.min(rect.width - 245, Math.max(8, x + 14))}px`;
      tooltip.style.top = `${Math.max(8, y - 72)}px`;
      tooltip.classList.add("show");
    });
    canvas.addEventListener("mouseleave", () => tooltip.classList.remove("show"));
  }

  function forecastRowsBeforeIssuance() {
    const location = $("forecastLocation").value;
    const view = $("forecastView").value;
    const reason = $("forecastReason").value;
    const allowedModels = new Set(models.filter(m => reason === "__all__" || m.reason === reason).map(m => m.model));
    return forecasts.filter(x => selected(x) && allowedModels.has(x.model) &&
      x.location === location && x.series_type === view);
  }

  function updateForecastReferences(preserve = true) {
    const select = $("forecastReference");
    const previous = preserve ? select.value : "__all__";
    const view = $("forecastView").value;
    const dates = uniq(forecastRowsBeforeIssuance().map(x => x.reference_date));
    const isCurrent = view === "Current forecast";
    const allLabel = view.startsWith("Testing") ? "All testing forecast dates" :
      view.startsWith("Real-time") ? "All archived forecast dates" : "Current available forecast";
    const items = isCurrent ? [{ value: "__all__", label: allLabel }] : [
      { value: "__all__", label: allLabel },
      { value: "__latest__", label: "Latest forecast date for each model" },
      ...dates.map(date => ({ value: String(date), label: `Issued ${date}` }))
    ];
    select.innerHTML = items.map(x => `<option value="${esc(x.value)}">${esc(x.label)}</option>`).join("");
    select.value = items.some(x => x.value === previous) ? previous : "__all__";
    select.disabled = isCurrent;
  }

  function updateForecastHorizons() {
    const view = $("forecastView").value;
    const location = $("forecastLocation").value;
    const values = uniq(forecasts.filter(x => selected(x) && x.series_type === view &&
      x.location === location && numeric(x.horizon)).map(x => Number(x.horizon)));
    const select = $("forecastHorizon");
    const previous = select.value;
    const items = [{ value: "__all__", label: "All available horizons (0–X)" },
      ...values.map(x => ({ value: String(x), label: `Up to ${x} step${Number(x) === 1 ? "" : "s"} ahead (0–${x})` }))];
    select.innerHTML = items.map(x => `<option value="${esc(x.value)}">${esc(x.label)}</option>`).join("");
    if (items.some(x => x.value === previous)) select.value = previous;
  }

  function filteredForecastRows() {
    const reference = $("forecastReference").value;
    const horizon = $("forecastHorizon").value;
    let rows = forecastRowsBeforeIssuance();
    if (reference === "__latest__") {
      const latest = Object.fromEntries(uniq(rows.map(x => x.model)).map(model => [model,
        rows.filter(x => x.model === model).map(x => x.reference_date || "").sort().slice(-1)[0]]));
      rows = rows.filter(x => (x.reference_date || "") === latest[x.model]);
    } else if (reference !== "__all__") {
      rows = rows.filter(x => String(x.reference_date || "") === reference);
    }
    return rows.filter(x => horizon === "__all__" ||
      (numeric(x.horizon) && Number(x.horizon) <= Number(horizon)));
  }

  function canvasContext(canvas) {
    const rect = canvas.getBoundingClientRect();
    const ratio = window.devicePixelRatio || 1;
    canvas.width = Math.max(1, Math.round(rect.width * ratio));
    canvas.height = Math.max(1, Math.round(rect.height * ratio));
    const ctx = canvas.getContext("2d");
    ctx.setTransform(ratio, 0, 0, ratio, 0, 0);
    return { ctx, width: rect.width, height: rect.height };
  }

  function drawAxes(ctx, width, height, xMin, xMax, yMin, yMax, xDate = true) {
    const p = { l: 68, r: 24, t: 24, b: 54 };
    const pw = width - p.l - p.r, ph = height - p.t - p.b;
    ctx.clearRect(0, 0, width, height);
    ctx.font = "12px Segoe UI";
    ctx.fillStyle = "#627789";
    ctx.strokeStyle = "#dfe6ea";
    ctx.lineWidth = 1;
    for (let i = 0; i <= 5; i++) {
      const y = p.t + ph * i / 5;
      const val = yMax - (yMax - yMin) * i / 5;
      ctx.beginPath(); ctx.moveTo(p.l, y); ctx.lineTo(width - p.r, y); ctx.stroke();
      ctx.textAlign = "right"; ctx.fillText(fmt(val), p.l - 9, y + 4);
    }
    for (let i = 0; i <= 5; i++) {
      const x = p.l + pw * i / 5;
      const val = xMin + (xMax - xMin) * i / 5;
      const label = xDate ? new Date(val).toLocaleDateString(undefined, { month: "short", day: "numeric", year: i === 0 || i === 5 ? "2-digit" : undefined }) : fmt(val);
      ctx.textAlign = i === 0 ? "left" : i === 5 ? "right" : "center";
      ctx.fillText(label, x, height - 22);
    }
    ctx.strokeStyle = "#8da0ad"; ctx.lineWidth = 1.5;
    ctx.beginPath(); ctx.moveTo(p.l, p.t); ctx.lineTo(p.l, height - p.b); ctx.lineTo(width - p.r, height - p.b); ctx.stroke();
    return {
      x: value => p.l + (Number(value) - xMin) / (xMax - xMin || 1) * pw,
      y: value => p.t + (yMax - Number(value)) / (yMax - yMin || 1) * ph,
      p, pw, ph
    };
  }

  function drawForecastLegacy() {
    const rows = filteredForecastRows();
    const location = $("forecastLocation").value;
    const view = $("forecastView").value;
    const observed = truth.filter(x => x.location === location && selectedModels.has(x.model));
    const canvas = $("forecastCanvas");
    const { ctx, width, height } = canvasContext(canvas);
    const empty = $("forecastEmpty");
    const valid = rows.filter(x => numeric(x.value) && !Number.isNaN(Date.parse(x.target_end_date)));
    if (!valid.length) {
      ctx.clearRect(0, 0, width, height); empty.classList.add("show");
      $("forecastLegend").innerHTML = ""; renderForecastTable([]); return;
    }
    empty.classList.remove("show");
    const dateVals = [...valid.map(x => Date.parse(x.target_end_date)), ...observed.map(x => Date.parse(x.date)).filter(Number.isFinite)];
    const yVals = [...valid.map(x => Number(x.value)), ...observed.map(x => Number(x.value)).filter(Number.isFinite)];
    let yMin = Math.min(...yVals), yMax = Math.max(...yVals);
    const yPad = Math.max((yMax - yMin) * .1, Math.abs(yMax) * .03, 1);
    yMin = Math.min(0, yMin - yPad); yMax += yPad;
    const axes = drawAxes(ctx, width, height, Math.min(...dateVals), Math.max(...dateVals), yMin, yMax, true);
    const allHorizons = $("forecastHorizon").value === "__all__";
    const groups = new Map();
    valid.forEach(row => {
      const horizonLabel = allHorizons && row.horizon !== null && row.horizon !== "" && view !== "Current forecast" ? ` · H${row.horizon}` : "";
      const key = `${row.model}${horizonLabel}`;
      if (!groups.has(key)) groups.set(key, []);
      groups.get(key).push(row);
    });
    const legend = [];
    groups.forEach((points, label) => {
      points.sort((a, b) => Date.parse(a.target_end_date) - Date.parse(b.target_end_date));
      const model = points[0].model;
      const color = colors[model];
      ctx.strokeStyle = color; ctx.fillStyle = color; ctx.lineWidth = label.includes(" · H") ? 2 : 3;
      ctx.beginPath();
      points.forEach((point, i) => {
        const x = axes.x(Date.parse(point.target_end_date)), y = axes.y(point.value);
        i ? ctx.lineTo(x, y) : ctx.moveTo(x, y);
      });
      ctx.stroke();
      if (points.length <= 32) points.forEach(point => { ctx.beginPath(); ctx.arc(axes.x(Date.parse(point.target_end_date)), axes.y(point.value), 2.7, 0, Math.PI * 2); ctx.fill(); });
      legend.push({ label, color });
    });
    if (observed.length) {
      const byLabel = new Map();
      observed.filter(x => numeric(x.value) && !Number.isNaN(Date.parse(x.date))).forEach(row => {
        const key = row.label || "Observed"; if (!byLabel.has(key)) byLabel.set(key, []); byLabel.get(key).push(row);
      });
      byLabel.forEach((points, label) => {
        points.sort((a, b) => Date.parse(a.date) - Date.parse(b.date));
        ctx.strokeStyle = "#172f45"; ctx.lineWidth = 2.5; ctx.setLineDash([7,5]); ctx.beginPath();
        points.forEach((point, i) => { const x=axes.x(Date.parse(point.date)), y=axes.y(point.value); i ? ctx.lineTo(x,y) : ctx.moveTo(x,y); });
        ctx.stroke(); ctx.setLineDash([]); legend.push({ label, color: "#172f45", dashed: true });
      });
    }
    $("forecastLegend").innerHTML = legend.map(x => `<span class="legend-item"><span class="legend-line" style="background:${x.color};${x.dashed ? 'border-top:2px dashed #fff' : ''}"></span>${esc(x.label)}</span>`).join("");
    renderForecastTable(rows);
  }

  function drawForecastForDisease(disease, canvas, legendElement, empty) {
    const rows = filteredForecastRows().filter(x => x.disease === disease);
    const location = $("forecastLocation").value;
    const view = $("forecastView").value;
    const allowedModels = new Set(rows.map(x => x.model));
    const { ctx, width, height } = canvasContext(canvas);
    const hitPoints = [];
    canvas._forecastHitPoints = hitPoints;
    const show80 = $("showForecast80").checked;
    const show95 = $("showForecast95").checked;
    const showObserved = $("showForecastObserved").checked;
    const valid = rows.filter(x => numeric(x.value) && Number.isFinite(Date.parse(x.target_end_date)));
    const central = valid.filter(x => !numeric(x.quantile) || Math.abs(Number(x.quantile) - .5) < 1e-8);
    if (!central.length) {
      ctx.clearRect(0, 0, width, height); empty.classList.add("show");
      legendElement.innerHTML = ""; return;
    }
    empty.classList.remove("show");

    const forecastDates = central.map(x => Date.parse(x.target_end_date));
    const hasRealtimeTruth = truth.some(x => x.location === location && selected(x) &&
      allowedModels.has(x.model) && String(x.label || "").includes("(Real-time)"));
    const periodWord = view.startsWith("Testing") ? "Testing" :
      view.startsWith("Real-time") ? "Real-time" :
      hasRealtimeTruth ? "Real-time" : "Testing";
    const observedRaw = truth.filter(x => x.disease === disease && x.location === location && selected(x) && allowedModels.has(x.model) &&
      String(x.label || "").includes(`(${periodWord})`) && numeric(x.value) &&
      Number.isFinite(Date.parse(x.date)));
    const observed = showObserved ? observedRaw.filter((row, index, source) => index === source.findIndex(x =>
      x.date === row.date && x.label === row.label)) : [];

    const dateVals = [...forecastDates, ...observed.map(x => Date.parse(x.date))];
    const xMin = Math.min(...dateVals), xMax = Math.max(...dateVals);
    const yVals = [...valid.map(x => Number(x.value)), ...observed.map(x => Number(x.value))];
    const yMaxRaw = Math.max(0, ...yVals);
    const axes = drawAxes(ctx, width, height, xMin, xMax, 0,
      yMaxRaw + Math.max(yMaxRaw * .08, 1), true);
    const rgba = (hex, alpha) => {
      const value = hex.replace("#", "");
      const n = parseInt(value.length === 3 ? value.split("").map(x => x + x).join("") : value, 16);
      return `rgba(${(n >> 16) & 255},${(n >> 8) & 255},${n & 255},${alpha})`;
    };

    const groups = new Map();
    valid.forEach(row => {
      const key = `${row.model}\r${row.reference_date || "Current"}\r${row.target || "Target"}`;
      if (!groups.has(key)) groups.set(key, []);
      groups.get(key).push(row);
    });
    const models80 = new Set(), models95 = new Set();
    const drawBand = (points, lowerQ, upperQ, alpha) => {
      const lower = new Map(points.filter(x => numeric(x.quantile) && Math.abs(Number(x.quantile) - lowerQ) < 1e-8).map(x => [x.target_end_date, x]));
      const upper = new Map(points.filter(x => numeric(x.quantile) && Math.abs(Number(x.quantile) - upperQ) < 1e-8).map(x => [x.target_end_date, x]));
      const dates = [...lower.keys()].filter(x => upper.has(x)).sort((a,b) => Date.parse(a)-Date.parse(b));
      if (dates.length < 2) return false;
      ctx.fillStyle = rgba(colors[points[0].model], alpha); ctx.beginPath();
      dates.forEach((date, i) => { const p=lower.get(date); i ? ctx.lineTo(axes.x(Date.parse(date)),axes.y(p.value)) : ctx.moveTo(axes.x(Date.parse(date)),axes.y(p.value)); });
      dates.slice().reverse().forEach(date => { const p=upper.get(date); ctx.lineTo(axes.x(Date.parse(date)),axes.y(p.value)); });
      ctx.closePath(); ctx.fill(); return true;
    };
    groups.forEach(points => {
      const model = points[0].model;
      if (show95 && drawBand(points, .025, .975, .08)) models95.add(model);
      if (show80 && drawBand(points, .1, .9, .15)) models80.add(model);
    });

    const latestReference = Object.fromEntries(uniq(central.map(x => x.model)).map(model => [model,
      central.filter(x => x.model === model).map(x => x.reference_date || "").sort().slice(-1)[0]]));
    groups.forEach(points => {
      const medians = points.filter(x => !numeric(x.quantile) || Math.abs(Number(x.quantile) - .5) < 1e-8)
        .sort((a,b) => Number(a.horizon)-Number(b.horizon) || Date.parse(a.target_end_date)-Date.parse(b.target_end_date));
      if (!medians.length) return;
      const model = medians[0].model, color = colors[model];
      const latest = (medians[0].reference_date || "") === latestReference[model];
      ctx.strokeStyle=color; ctx.fillStyle=color; ctx.lineWidth=latest ? 3 : 1.35; ctx.globalAlpha=latest ? 1 : .32; ctx.beginPath();
      medians.forEach((point,i) => { const x=axes.x(Date.parse(point.target_end_date)), y=axes.y(point.value); i ? ctx.lineTo(x,y) : ctx.moveTo(x,y); });
      ctx.stroke();
      medians.forEach(point => {
        const x=axes.x(Date.parse(point.target_end_date)), y=axes.y(point.value);
        if (medians.length <= 20) { ctx.beginPath(); ctx.arc(x,y,latest ? 2.7 : 1.8,0,Math.PI*2); ctx.fill(); }
        hitPoints.push({x, y, text: `${model}\nIssued: ${point.reference_date || "Current"}\nHorizon: ${point.horizon ?? "—"}\nTarget: ${point.target_end_date}\nMedian: ${fmt(point.value)}`});
      });
      ctx.globalAlpha=1;
    });

    const byLabel = new Map();
    observed.forEach(row => { const key=row.label || "Observed"; if(!byLabel.has(key)) byLabel.set(key,[]); byLabel.get(key).push(row); });
    byLabel.forEach(points => {
      points.sort((a,b) => Date.parse(a.date)-Date.parse(b.date));
      ctx.strokeStyle="#172f45"; ctx.lineWidth=2.5; ctx.setLineDash([7,5]); ctx.beginPath();
      points.forEach((point,i) => {
        const x=axes.x(Date.parse(point.date)), y=axes.y(point.value);
        i ? ctx.lineTo(x,y) : ctx.moveTo(x,y);
        hitPoints.push({x, y, text: `Observed data\nDate: ${point.date}\nValue: ${fmt(point.value)}`});
      });
      ctx.stroke(); ctx.setLineDash([]);
    });

    const modelLegend = uniq(central.map(x => x.model)).map(model => {
      const marks = `<span class="legend-line" style="background:${colors[model]}"></span>` +
        (models80.has(model) ? `<span class="legend-band" title="80% PI" style="background:${rgba(colors[model],.15)}"></span>` : "") +
        (models95.has(model) ? `<span class="legend-band" title="95% PI" style="background:${rgba(colors[model],.08)}"></span>` : "");
      return `<button class="legend-item legend-toggle" data-model="${esc(model)}">${marks}${esc(model)}</button>`;
    }).join("");
    const intervalLegend = (models80.size || models95.size) ? '<span class="legend-note">Model-colored shading: 80% and 95% prediction intervals</span>' : '';
    const observedLegend = observed.length ? '<span class="legend-item"><span class="legend-line observed-line"></span>Observed data</span>' : '';
    legendElement.innerHTML = modelLegend + intervalLegend + observedLegend;
    legendElement.querySelectorAll("[data-model]").forEach(button => button.addEventListener("click", () => {
      const model=button.dataset.model; selectedModels.has(model) ? selectedModels.delete(model) : selectedModels.add(model);
      buildModelFilters(); refreshControlDomains();
    }));
  }

  function drawForecast() {
    const rows = filteredForecastRows();
    const diseases = uniq(rows.map(x => x.disease).filter(Boolean));
    const shell = $("forecastCharts");
    shell.innerHTML = "";

    diseases.forEach(disease => {
      const article = document.createElement("article");
      article.className = "chart-card disease-chart";
      article.innerHTML = `
        <div class="disease-chart-head">
          <h3>${esc(disease)}</h3>
          <p>Forecasts and observed outcome data for this disease.</p>
        </div>
        <div class="canvas-wrap">
          <canvas aria-label="Forecast comparison for ${esc(disease)}" role="img"></canvas>
          <div class="plot-tooltip" role="status"></div>
          <div class="empty">No forecast values are available for this disease and filter combination.</div>
        </div>
        <div class="legend" aria-label="Forecast legend"></div>`;
      shell.appendChild(article);
      const canvas = article.querySelector("canvas");
      const tooltip = article.querySelector(".plot-tooltip");
      const empty = article.querySelector(".empty");
      const legend = article.querySelector(".legend");
      attachForecastHover(canvas, tooltip);
      drawForecastForDisease(disease, canvas, legend, empty);
    });

    if (!diseases.length) {
      shell.innerHTML = '<article class="chart-card"><div class="empty show static-empty">No forecast values are available for this filter combination.</div></article>';
    }
    renderForecastTable(rows);
  }

  function renderForecastTable(rows) {
    const fields = ["model", "disease", "series_type", "location", "reference_date", "target_end_date", "horizon", "target", "quantile", "value"];
    renderTable($("forecastTable"), rows, fields, $("forecastTableNote"));
  }

  const evaluationLabel = value => ({
    "Testing": "Testing-period evaluation",
    "Real-time": "Real-time evaluation"
  })[value] || String(value || "Not available");

  const scopeLabel = value => ({
    "overall": "Overall summary",
    "horizon": "Summary by forecast horizon",
    "row": "Over time by forecast horizon"
  })[value] || String(value || "Not available");

  const familyLabel = value => ({
    "percentAgreement": "Percent Agreement",
    "forecastBias": "Forecast Bias",
    "peakPhase": "Peak Timing & Magnitude",
    "traditional": "Traditional Metrics"
  })[value] || String(value || "Not available");

  const preferredOrder = (values, order) => [
    ...order.filter(value => values.includes(value)),
    ...values.filter(value => !order.includes(value))
  ];

  function metricLabel(value) {
    const raw = String(value || "");
    const exact = {
      per_agreement: "Percent agreement",
      raw_error: "Raw forecast error",
      pct_error: "Percentage forecast error",
      WIS: "Weighted interval score (WIS)",
      MAE: "Mean absolute error (MAE)",
      ae_median: "Absolute error of the median forecast",
      interval_score: "Prediction interval score",
      predictedPeakTimingOff: "Peak timing difference (early or late)",
      predictedPeakTimingLabel: "Peak timing result (early, on time, or late)",
      predictedPeakMagnitudeOff: "Peak-to-peak magnitude difference",
      predictedPeakAccuracy: "Peak-to-peak magnitude accuracy",
      sameDayMagnitudeOff: "Magnitude at predicted peak — difference",
      sameDayAccuracy: "Magnitude at predicted peak — accuracy",
      peakWeekMagnitudeOff: "Magnitude at observed peak — difference",
      peakWeekAccuracy: "Magnitude at observed peak — accuracy"
    };
    if (exact[raw]) return exact[raw];
    const agreementSummary = raw.match(/^(median|min|max)(?:Horizon|Overall)$/i);
    if (agreementSummary) {
      const statistic = { median: "Median", min: "Minimum", max: "Maximum" }[agreementSummary[1].toLowerCase()];
      return `${statistic} Percent Agreement`;
    }

    let label = raw.replace(/_+/g, " ").replace(/([a-z0-9])([A-Z])/g, "$1 $2")
      .replace(/\bHorizon\b/gi, "").replace(/\bOverall\b/gi, "")
      .replace(/\bPct All\b/gi, "percentage bias — all observations")
      .replace(/\bPct Stable\b/gi, "percentage bias — stable observations")
      .replace(/\bCov\s*50\b/gi, "50% prediction interval coverage")
      .replace(/\bCov\s*80\b/gi, "80% prediction interval coverage")
      .replace(/\bCov\s*95\b/gi, "95% prediction interval coverage")
      .replace(/\bWIS\b/g, "weighted interval score (WIS)")
      .replace(/\bMAE\b/g, "mean absolute error (MAE)")
      .replace(/\bRaw\b/g, "raw-count bias")
      .replace(/^Under\b/i, "Under-prediction component")
      .replace(/^Over\b/i, "Over-prediction component")
      .replace(/^mean\b/i, "Mean").replace(/^median\b/i, "Median")
      .replace(/^min\b/i, "Minimum").replace(/^max\b/i, "Maximum")
      .replace(/\boverpredict(?:ion)?\b/gi, "over-prediction")
      .replace(/\bunderpredict(?:ion)?\b/gi, "under-prediction")
      .replace(/\s+/g, " ").trim();
    return label ? label.charAt(0).toUpperCase() + label.slice(1) : "Measure not available";
  }

  function setDisplayOptions(select, values, formatter, allLabel = null, preserve = true) {
    const previous = preserve ? select.value : "";
    const items = allLabel ? [{ value: "__all__", label: allLabel }] : [];
    values.forEach(value => items.push({ value: String(value), label: formatter(value) }));
    select.innerHTML = items.map(x => `<option value="${esc(x.value)}">${esc(x.label)}</option>`).join("");
    if (items.some(x => x.value === previous)) select.value = previous;
  }

  function renderEvaluationBreakdown() {
    const shell = $("evaluationBreakdown");
    const active = $("perfPeriod").value;
    const available = uniq(performance.filter(selected).map(x => x.evaluation_period));
    const ordered = ["Testing", "Real-time"].filter(x => available.includes(x));
    const descriptionsByPeriod = {
      "Testing": "Measures performance on the held-out testing period defined before report generation.",
      "Real-time": "Measures archived forecasts against observations as those outcomes became available."
    };
    const visibleModels = models.filter(m => selectedModels.has(m.model) && modelInGlobalFilters(m.model));
    shell.innerHTML = ordered.map(period => {
      const rows = performance.filter(x => selected(x) && x.evaluation_period === period && reportMetricAllowed(x));
      const modelCount = uniq(rows.map(x => x.model)).length;
      const metricCount = uniq(rows.map(x => `${x.family}\r${x.metric}`)).length;
      const locationCount = uniq(rows.map(x => x.location)).length;
      const starts = visibleModels.map(m => period === "Testing" ? m.testing_start : m.realtime_start).filter(Boolean).sort();
      const ends = visibleModels.map(m => period === "Testing" ? m.testing_end : m.realtime_end).filter(Boolean).sort();
      const dateRange = starts.length && ends.length ? `${starts[0]} through ${ends[ends.length - 1]}` : "Dates not available";
      return `<button type="button" class="evaluation-card ${active === period ? "active" : ""}" data-period="${esc(period)}">
        <h3>${esc(evaluationLabel(period))}</h3><p>${esc(descriptionsByPeriod[period])}</p>
        <span class="evaluation-card-stats"><span><strong>${modelCount}</strong> model${modelCount === 1 ? "" : "s"}</span><span><strong>${metricCount}</strong> measure${metricCount === 1 ? "" : "s"}</span><span><strong>${locationCount}</strong> location${locationCount === 1 ? "" : "s"}</span><span>${esc(dateRange)}</span></span>
      </button>`;
    }).join("");
    shell.querySelectorAll("[data-period]").forEach(button => button.addEventListener("click", () => {
      $("perfPeriod").value = button.dataset.period;
      updatePerformanceDiseases(false);
      updatePerformanceFamilies();
    }));
  }

  function setupPerformanceControls() {
    setDisplayOptions($("perfPeriod"), preferredOrder(
      uniq(performance.filter(selected).map(x => x.evaluation_period)), ["Testing", "Real-time"]),
      evaluationLabel, null, false);
    updatePerformanceDiseases(false);
    $("perfPeriod").addEventListener("change", () => { updatePerformanceDiseases(false); updatePerformanceFamilies(); });
    $("perfDisease").addEventListener("change", () => { updatePerformanceLocations(false); updatePerformanceFamilies(); });
    $("perfLocation").addEventListener("change", () => { updatePerformanceSeasons(false); updatePerformanceFamilies(); });
    $("perfSeason").addEventListener("change", updatePerformanceFamilies);
    $("perfFamily").addEventListener("change", updatePerformanceScopes);
    $("perfScope").addEventListener("change", updatePerformanceMetrics);
    $("perfMetric").addEventListener("change", () => { updateMetricHelp(); updatePerformanceHorizons(); drawPerformance(); });
    $("perfHorizon").addEventListener("change", drawPerformance);
    $("downloadPerformance").addEventListener("click", () => downloadCsv(filteredPerformanceRows(), "performance-comparison.csv"));
    attachPerformanceHover($("performanceCanvas"), $("performanceTooltip"));
    updatePerformanceFamilies();
  }

  function reportMetricAllowed(row) {
    const metric = String(row.metric || "");
    if (row.family === "percentAgreement") return /per_agreement|^(min|max|median)/i.test(metric);
    if (row.family === "forecastBias") return /raw_error|pct_error|^(min|max|median)(Raw|Pct)/i.test(metric);
    if (row.family === "traditional") return /^(WIS|MAE|Under|Over|Cov50|Cov80|Cov95)(_|$)/i.test(metric);
    if (row.family === "peakPhase") return /^(predictedPeak|sameDay|peakWeek)/i.test(metric);
    return false;
  }

  function updatePerformanceDiseases(preserve = true) {
    const period = $("perfPeriod").value;
    const rows = performance.filter(row => selected(row) && row.evaluation_period === period && reportMetricAllowed(row));
    setOptions($("perfDisease"), uniq(rows.map(row => row.disease)), null, preserve);
    updatePerformanceLocations(false);
  }

  function updatePerformanceLocations(preserve = true) {
    const period = $("perfPeriod").value;
    const disease = $("perfDisease").value;
    const rows = performance.filter(row => selected(row) && row.evaluation_period === period &&
      row.disease === disease && reportMetricAllowed(row));
    setOptions($("perfLocation"), uniq(rows.map(row => row.location)), null, preserve);
    updatePerformanceSeasons(false);
  }

  function updatePerformanceSeasons(preserve = true) {
    const period = $("perfPeriod").value;
    const disease = $("perfDisease").value;
    const location = $("perfLocation").value;
    const rows = performance.filter(row => selected(row) && row.evaluation_period === period &&
      row.disease === disease && row.location === location && reportMetricAllowed(row));
    const seasons = uniq(rows.map(row => row.season));
    if (seasons.length) setDisplayOptions($("perfSeason"), seasons, value => `Season ${value}`, null, preserve);
    else $("perfSeason").innerHTML = '<option value="__all__">All available dates</option>';
  }

  function performanceSliceRows() {
    const period = $("perfPeriod").value;
    const disease = $("perfDisease").value;
    const location = $("perfLocation").value;
    const season = $("perfSeason").value;
    return performance.filter(row => selected(row) && row.evaluation_period === period &&
      row.disease === disease && row.location === location &&
      (season === "__all__" || row.season === season || row.season === null) && reportMetricAllowed(row));
  }

  function basePerformanceRows() {
    const scope = $("perfScope").value;
    return performanceSliceRows().filter(row => row.scope === scope);
  }

  function updatePerformanceFamilies() {
    setDisplayOptions($("perfFamily"), preferredOrder(
      uniq(performanceSliceRows().map(x => x.family)),
      ["percentAgreement", "forecastBias", "peakPhase", "traditional"]), familyLabel);
    renderEvaluationBreakdown();
    updatePerformanceScopes();
  }
  function updatePerformanceScopes() {
    const family = $("perfFamily").value;
    const preferred = ["percentAgreement", "forecastBias"].includes(family) ?
      ["row", "horizon", "overall"] : family === "traditional" ? ["overall", "horizon"] : ["row", "overall", "horizon"];
    setDisplayOptions($("perfScope"), preferredOrder(
      uniq(performanceSliceRows().filter(row => row.family === family).map(row => row.scope)), preferred), scopeLabel);
    updatePerformanceMetrics();
  }
  function updatePerformanceMetrics() {
    const family = $("perfFamily").value;
    const metrics = uniq(basePerformanceRows().filter(x => x.family === family).map(x => x.metric));
    setDisplayOptions($("perfMetric"), metrics, metricLabel);
    updateMetricHelp(); updatePerformanceHorizons(); drawPerformance();
  }
  function updatePerformanceHorizons() {
    const rows = basePerformanceRows().filter(x => x.family === $("perfFamily").value && x.metric === $("perfMetric").value);
    const horizons = uniq(rows.map(x => x.horizon));
    const peakFamily = $("perfFamily").value === "peakPhase";
    setDisplayOptions($("perfHorizon"), horizons,
      value => `Horizon ${value}`, peakFamily ? null : "All forecast horizons");
  }
  function updateMetricHelp() {
    const metric = $("perfMetric").value || "";
    const family = $("perfFamily").value || "";
    const hit = descriptions.find(x => {
      if (x.family !== family) return false;
      try { return new RegExp(x.pattern, "i").test(metric); } catch (_) { return false; }
    });
    const description = hit ? hit.description : "Select a measure to compare its values across models.";
    $("metricHelp").innerHTML = `<strong>${esc(metricLabel(metric))}</strong><span>${esc(description)}</span>`;
  }
  function filteredPerformanceRows() {
    const family = $("perfFamily").value, metric = $("perfMetric").value, horizon = $("perfHorizon").value;
    return basePerformanceRows().filter(x => x.family === family && x.metric === metric && (horizon === "__all__" || String(x.horizon ?? "") === horizon));
  }

  function drawPerformance() {
    const rows = filteredPerformanceRows();
    const family = $("perfFamily").value;
    const scope = $("perfScope").value;
    const timeSeries = scope === "row" && ["percentAgreement", "forecastBias"].includes(family);
    const peakFigure = scope === "row" && family === "peakPhase";
    const card = $("performanceChartCard");
    card.style.display = timeSeries || peakFigure ? "" : "none";
    if (timeSeries) drawReportTimeSeries(rows, family);
    else if (peakFigure) drawPeakComparison(rows);
    renderPerformanceTable(rows);
  }

  function peakPanel(metric) {
    if (/^sameDay/i.test(metric)) return "predicted-week";
    if (/^peakWeek/i.test(metric)) return "observed-week";
    return "peak-to-peak";
  }

  function seasonForDate(date) {
    const parsed = new Date(`${date}T00:00:00`);
    if (!Number.isFinite(parsed.getTime())) return null;
    const year = parsed.getFullYear(), month = parsed.getMonth() + 1;
    const start = month < seasonStartMonth ? year - 1 : year;
    return `${start}-${start + 1}`;
  }

  function drawPeakComparison(rows) {
    const canvas = $("performanceCanvas");
    const { ctx, width, height } = canvasContext(canvas);
    const empty = $("performanceEmpty");
    const disease = $("perfDisease").value, location = $("perfLocation").value;
    const season = $("perfSeason").value, horizon = $("perfHorizon").value;
    const metric = $("perfMetric").value, panel = peakPanel(metric);
    const details = peakDetails.filter(row => selected(row) && !hiddenPerformanceModels.has(row.model) &&
      row.disease === disease && row.location === location &&
      (season === "__all__" || row.season === season) && String(row.horizon ?? "") === horizon);
    canvas._performanceHitPoints = [];

    if (!details.length) {
      ctx.clearRect(0, 0, width, height); empty.classList.add("show");
      empty.textContent = "The report-style peak figure is not available for these choices.";
      renderPerformanceLegend(rows, "peakPhase"); return;
    }
    empty.classList.remove("show");

    const allowed = new Set(details.map(row => row.model));
    const curveRows = forecasts.filter(row => allowed.has(row.model) && row.disease === disease &&
      row.location === location && row.series_type === "Testing median forecasts" &&
      String(row.horizon ?? "") === horizon && (!numeric(row.quantile) || Math.abs(Number(row.quantile) - .5) < 1e-8) &&
      (season === "__all__" || seasonForDate(row.reference_date) === season) &&
      numeric(row.value) && Number.isFinite(Date.parse(row.target_end_date)));
    const dateNumbers = curveRows.map(row => Date.parse(row.target_end_date));
    details.forEach(row => {
      [row.observed_peak_date, row.predicted_peak_date].forEach(date => {
        const number = Date.parse(date); if (Number.isFinite(number)) dateNumbers.push(number);
      });
    });
    if (!dateNumbers.length) {
      ctx.clearRect(0, 0, width, height); empty.classList.add("show");
      empty.textContent = "Peak results exist, but the matching testing-period curves are unavailable.";
      renderPerformanceLegend(rows, "peakPhase"); return;
    }

    const xMin = Math.min(...dateNumbers), xMax = Math.max(...dateNumbers);
    const truthRows = truth.filter(row => row.disease === disease && row.location === location &&
      String(row.label || "").includes("(Testing)") && numeric(row.value) &&
      Number.isFinite(Date.parse(row.date)) && Date.parse(row.date) >= xMin && Date.parse(row.date) <= xMax);
    const yValues = [...curveRows.map(row => Number(row.value)), ...truthRows.map(row => Number(row.value))];
    details.forEach(row => yValues.push(row.observed_peak_value, row.predicted_peak_value,
      row.observed_at_predicted_peak, row.peak_week_forecast_value));
    const cleanY = yValues.map(Number).filter(Number.isFinite);
    const yMax = Math.max(1, ...cleanY) * 1.12;
    const axes = drawAxes(ctx, width, height, xMin, xMax, 0, yMax, true);

    const observedByDate = new Map();
    truthRows.forEach(row => {
      if (!observedByDate.has(row.date)) observedByDate.set(row.date, []);
      observedByDate.get(row.date).push(Number(row.value));
    });
    const observed = [...observedByDate.entries()].map(([date, values]) => ({ date, value: median(values) }))
      .sort((a, b) => Date.parse(a.date) - Date.parse(b.date));
    ctx.strokeStyle = "#172f45"; ctx.lineWidth = 3; ctx.setLineDash([]); ctx.beginPath();
    observed.forEach((point, i) => i ? ctx.lineTo(axes.x(Date.parse(point.date)), axes.y(point.value)) :
      ctx.moveTo(axes.x(Date.parse(point.date)), axes.y(point.value)));
    ctx.stroke();

    const curveGroups = new Map();
    curveRows.forEach(row => {
      if (!curveGroups.has(row.model)) curveGroups.set(row.model, new Map());
      const dates = curveGroups.get(row.model);
      if (!dates.has(row.target_end_date)) dates.set(row.target_end_date, []);
      dates.get(row.target_end_date).push(Number(row.value));
    });
    curveGroups.forEach((dateMap, model) => {
      const points = [...dateMap.entries()].map(([date, values]) => ({ date, value: median(values) }))
        .sort((a, b) => Date.parse(a.date) - Date.parse(b.date));
      ctx.strokeStyle = colors[model]; ctx.lineWidth = 2.4; ctx.globalAlpha = .86; ctx.beginPath();
      points.forEach((point, i) => i ? ctx.lineTo(axes.x(Date.parse(point.date)), axes.y(point.value)) :
        ctx.moveTo(axes.x(Date.parse(point.date)), axes.y(point.value)));
      ctx.stroke(); ctx.globalAlpha = 1;
    });

    const observedPeaks = new Map();
    details.forEach(detail => {
      if (detail.observed_peak_date && numeric(detail.observed_peak_value))
        observedPeaks.set(`${detail.observed_peak_date}\r${detail.observed_peak_value}`, detail);
    });
    observedPeaks.forEach(detail => {
      const x = axes.x(Date.parse(detail.observed_peak_date)), y = axes.y(detail.observed_peak_value);
      ctx.fillStyle = "#172f45"; ctx.beginPath(); ctx.arc(x, y, 5.5, 0, Math.PI * 2); ctx.fill();
      canvas._performanceHitPoints.push({ x, y, text: `Observed peak\nDate: ${detail.observed_peak_date}\nValue: ${fmt(detail.observed_peak_value)}` });
    });

    const drawGap = (detail, dateA, valueA, dateB, valueB, label) => {
      const ax = axes.x(Date.parse(dateA)), ay = axes.y(valueA);
      const bx = axes.x(Date.parse(dateB)), by = axes.y(valueB);
      if (![ax, ay, bx, by].every(Number.isFinite)) return;
      ctx.strokeStyle = colors[detail.model]; ctx.lineWidth = 2; ctx.setLineDash([5, 4]);
      ctx.beginPath(); ctx.moveTo(ax, ay); ctx.lineTo(bx, ay); ctx.lineTo(bx, by); ctx.stroke(); ctx.setLineDash([]);
      ctx.fillStyle = colors[detail.model]; ctx.beginPath(); ctx.arc(ax, ay, 5, 0, Math.PI * 2); ctx.fill();
      ctx.fillStyle = "#fff"; ctx.strokeStyle = "#172f45"; ctx.lineWidth = 2;
      ctx.beginPath(); ctx.arc(bx, by, 4.2, 0, Math.PI * 2); ctx.fill(); ctx.stroke();
      const result = rows.find(row => row.model === detail.model);
      const resultValue = result && result.value_chr ? result.value_chr : result?.value;
      canvas._performanceHitPoints.push({ x: ax, y: ay, text: `${detail.model}\n${label}\nForecast peak: ${detail.predicted_peak_date || "—"}\nObserved peak: ${detail.observed_peak_date || "—"}\n${metricLabel(metric)}: ${fmt(resultValue)}` });
    };
    details.forEach(detail => {
      if (panel === "predicted-week") drawGap(detail, detail.predicted_peak_date, detail.predicted_peak_value,
        detail.predicted_peak_date, detail.observed_at_predicted_peak, "Forecasted peak compared with what happened that week");
      else if (panel === "observed-week") drawGap(detail, detail.observed_peak_date, detail.peak_week_forecast_value,
        detail.observed_peak_date, detail.observed_peak_value, "Forecast compared with the actual peak week");
      else drawGap(detail, detail.predicted_peak_date, detail.predicted_peak_value,
        detail.observed_peak_date, detail.observed_peak_value, "Forecasted peak compared with the actual peak");
    });

    const copy = panel === "predicted-week" ? {
      title: "How high was the forecast on its predicted peak week?",
      guide: "The colored dot is each model’s forecasted peak. The connector compares it with what was actually observed during that same week."
    } : panel === "observed-week" ? {
      title: "How high was the forecast on the actual peak week?",
      guide: "The connector compares each model’s forecast for the true peak week with the observed peak. This asks whether the model captured the real peak when it happened."
    } : {
      title: "Did each model predict the right peak week and height?",
      guide: "The connector shows both parts of the miss: sideways means the predicted peak was early or late, and up or down means the predicted peak was too high or too low."
    };
    $("performanceChartTitle").textContent = copy.title;
    $("performanceChartSubtitle").textContent = `Testing-period evaluation · ${disease} · ${location} · Season ${season} · Horizon ${horizon}`;
    $("performanceChartGuide").innerHTML = `<strong>How to read it:</strong> ${copy.guide}`;
    renderPerformanceLegend(rows, "peakPhase");
  }

  function drawReportTimeSeries(rows, family) {
    const canvas = $("performanceCanvas");
    const { ctx, width, height } = canvasContext(canvas);
    const empty = $("performanceEmpty");
    const numericRows = rows.filter(row => !hiddenPerformanceModels.has(row.model) && numeric(row.value) &&
      Number.isFinite(Date.parse(row.target_end_date || row.reference_date)));
    canvas._performanceHitPoints = [];
    if (!numericRows.length) {
      ctx.clearRect(0, 0, width, height); empty.classList.add("show");
      empty.textContent = hiddenPerformanceModels.size ? "All models are hidden. Select a model below to show it again." :
        "No numeric performance results match these choices.";
      renderPerformanceLegend(rows, family); return;
    }
    empty.classList.remove("show");
    const dateOf = row => Date.parse(row.target_end_date || row.reference_date);
    const dates = numericRows.map(dateOf);
    const values = numericRows.map(row => Number(row.value));
    let yMin, yMax;
    if (family === "percentAgreement") { yMin = 0; yMax = 100; }
    else {
      const extent = Math.max(1, ...values.map(value => Math.abs(value))) * 1.12;
      yMin = -extent; yMax = extent;
    }
    const axes = drawAxes(ctx, width, height, Math.min(...dates), Math.max(...dates), yMin, yMax, true);
    if (family === "percentAgreement") {
      ctx.fillStyle = "rgba(39,129,91,.06)";
      ctx.fillRect(axes.p.l, axes.y(100), axes.pw, axes.y(75) - axes.y(100));
      ctx.fillStyle = "#27815b"; ctx.font = "700 12px Segoe UI"; ctx.textAlign = "right";
      ctx.fillText("Higher is better", width - axes.p.r - 4, axes.p.t + 16);
    }
    if (family === "forecastBias") {
      ctx.fillStyle = "rgba(231,111,81,.045)";
      ctx.fillRect(axes.p.l, axes.p.t, axes.pw, axes.y(0) - axes.p.t);
      ctx.fillStyle = "rgba(58,110,165,.045)";
      ctx.fillRect(axes.p.l, axes.y(0), axes.pw, axes.p.t + axes.ph - axes.y(0));
      ctx.strokeStyle = "#7e8c96"; ctx.lineWidth = 1.5; ctx.setLineDash([4, 4]);
      ctx.beginPath(); ctx.moveTo(axes.p.l, axes.y(0)); ctx.lineTo(width - axes.p.r, axes.y(0)); ctx.stroke(); ctx.setLineDash([]);
      ctx.font = "700 12px Segoe UI"; ctx.textAlign = "right";
      ctx.fillStyle = "#9f4f3c"; ctx.fillText("Forecasts were too high", width - axes.p.r - 4, axes.p.t + 16);
      ctx.fillStyle = "#3a6ea5"; ctx.fillText("Forecasts were too low", width - axes.p.r - 4, height - axes.p.b - 10);
    }

    const groups = new Map();
    numericRows.forEach(row => {
      const key = row.model;
      if (!groups.has(key)) groups.set(key, []);
      groups.get(key).push(row);
    });
    [...groups.entries()].forEach(([model, source]) => {
      const byDate = new Map();
      source.forEach(row => {
        const date = row.target_end_date || row.reference_date;
        if (!byDate.has(date)) byDate.set(date, []);
        byDate.get(date).push(Number(row.value));
      });
      const points = [...byDate.entries()].map(([date, vals]) => ({ date, value: median(vals) }))
        .sort((a, b) => Date.parse(a.date) - Date.parse(b.date));
      ctx.strokeStyle = colors[model]; ctx.fillStyle = colors[model]; ctx.lineWidth = 3;
      ctx.setLineDash([]); ctx.beginPath();
      points.forEach((point, i) => { const x = axes.x(Date.parse(point.date)), y = axes.y(point.value); i ? ctx.lineTo(x, y) : ctx.moveTo(x, y); });
      ctx.stroke();
      points.forEach(point => {
        const x = axes.x(Date.parse(point.date)), y = axes.y(point.value);
        ctx.beginPath(); ctx.arc(x, y, 3.4, 0, Math.PI * 2); ctx.fill();
        const direction = family === "percentAgreement" ? "Higher is better" :
          point.value > 0 ? "Forecasts were too high" : point.value < 0 ? "Forecasts were too low" : "Forecasts were on target";
        canvas._performanceHitPoints.push({ x, y,
          text: `${model}\nDate: ${point.date}\n${metricLabel($("perfMetric").value)}: ${fmt(point.value)}${family === "percentAgreement" ? "%" : ""}\n${direction}` });
      });
    });
    $("performanceChartTitle").textContent = family === "percentAgreement" ?
      "How often did the forecast get the trend right?" : "Were forecasts too high or too low?";
    const horizon = $("perfHorizon").value === "__all__" ? "all forecast horizons combined" :
      `forecast horizon ${$("perfHorizon").value}`;
    $("performanceChartSubtitle").textContent = `${evaluationLabel($("perfPeriod").value)} · ${$("perfDisease").value} · ${$("perfLocation").value} · ${horizon}`;
    $("performanceChartGuide").innerHTML = family === "percentAgreement" ?
      "<strong>How to read it:</strong> Higher lines are better. 100% means the forecast trend matched the observed trend every time. Each dot summarizes the selected horizons for that date." :
      "<strong>How to read it:</strong> The middle line (0) is the target. Above 0 means forecasts were too high; below 0 means they were too low. Closer to 0 is better.";
    renderPerformanceLegend(rows, family);
  }

  function renderPerformanceLegend(rows, family) {
    const modelNames = uniq(rows.filter(row => numeric(row.value) || (family === "peakPhase" && row.value_chr)).map(row => row.model));
    $("performanceLegend").innerHTML = modelNames.map(model => {
      const modelRows = rows.filter(row => row.model === model && numeric(row.value));
      const typical = median(modelRows.map(row => Number(row.value)));
      const textResult = rows.find(row => row.model === model && row.value_chr)?.value_chr;
      const hidden = hiddenPerformanceModels.has(model);
      const result = typical === null ? textResult : `${fmt(typical)}${family === "percentAgreement" ? "%" : ""}`;
      const suffix = result === null || result === undefined ? "" : ` <small>${family === "peakPhase" ? "Result" : "Typical"}: ${esc(result)}</small>`;
      return `<button type="button" class="legend-item legend-toggle performance-legend-toggle${hidden ? " muted" : ""}" data-performance-model="${esc(model)}" aria-pressed="${hidden ? "false" : "true"}"><span class="legend-line" style="background:${colors[model]}"></span><span>${esc(model)}${suffix}</span></button>`;
    }).join("") + (family === "peakPhase" ? '<span class="legend-item"><span class="legend-line observed-line"></span>Observed outcome</span>' : '') +
      '<span class="legend-note">Select a model name to show or hide its line.</span>';
    $("performanceLegend").querySelectorAll("[data-performance-model]").forEach(button => button.addEventListener("click", () => {
      const model = button.dataset.performanceModel;
      hiddenPerformanceModels.has(model) ? hiddenPerformanceModels.delete(model) : hiddenPerformanceModels.add(model);
      drawPerformance();
    }));
  }

  function renderPerformanceTable(rows) {
    const displayRows = rows.map(row => ({
      "Model": row.model,
      "Disease": row.disease,
      "Evaluation type": evaluationLabel(row.evaluation_period),
      "Location": row.location,
      "Season": row.season,
      "Forecast horizon": numeric(row.horizon) ? `Horizon ${row.horizon}` : row.horizon,
      "Level of detail": scopeLabel(row.scope),
      "Performance question": familyLabel(row.family),
      "Measure": metricLabel(row.metric),
      "Numeric result": row.value,
      "Text result": row.value_chr,
      "Forecast issued": row.reference_date,
      "Target date": row.target_end_date
    }));
    renderTable($("performanceTable"), displayRows, Object.keys(displayRows[0] || {
      "Model": "", "Disease": "", "Evaluation type": "", "Location": "", "Season": "",
      "Forecast horizon": "", "Level of detail": "", "Performance question": "",
      "Measure": "", "Numeric result": "", "Text result": "",
      "Forecast issued": "", "Target date": ""
    }), $("performanceTableNote"));
  }

  function setupConfigurationControls() {
    const visible = parameters.filter(selected);
    setOptions($("configModel"), uniq(visible.map(x => x.model)), "All models", false);
    setOptions($("configLocation"), uniq(visible.map(x => x.location).filter(x => x !== "All locations")), "All locations", false);
    setOptions($("configGroup"), uniq(visible.map(x => x.group)), "All parameter groups", false);
    ["configModel", "configLocation", "configGroup"].forEach(id => $(id).addEventListener("change", renderConfigurationTable));
    $("configSearch").addEventListener("input", renderConfigurationTable);
    $("downloadConfiguration").addEventListener("click", () => downloadCsv(filteredConfigurationRows(), "model-configuration-comparison.csv"));
    renderConfigurationTable();
  }

  function filteredConfigurationRows() {
    const model = $("configModel").value;
    const location = $("configLocation").value;
    const group = $("configGroup").value;
    const search = $("configSearch").value.trim().toLowerCase();
    return parameters.filter(row => selected(row) &&
      (model === "__all__" || row.model === model) &&
      (location === "__all__" || row.location === "All locations" || row.location === location) &&
      (group === "__all__" || row.group === group) &&
      (!search || [row.model, row.disease, row.location, row.group, row.parameter, row.value, row.definition]
        .some(value => String(value ?? "").toLowerCase().includes(search))));
  }

  function renderConfigurationTable() {
    const rows = filteredConfigurationRows();
    const fields = ["model", "disease", "location", "group", "parameter", "value", "definition"];
    renderTable($("configurationTable"), rows, fields, $("configurationTableNote"));
  }

  function renderTable(table, rows, fields, note) {
    const limit = 1000, shown = rows.slice(0, limit);
    table.innerHTML = `<thead><tr>${fields.map(x => `<th>${esc(x)}</th>`).join("")}</tr></thead><tbody>` +
      shown.map(row => `<tr>${fields.map(field => `<td>${esc(fmt(row[field]))}</td>`).join("")}</tr>`).join("") + "</tbody>";
    note.textContent = rows.length > limit ? `Showing the first ${limit.toLocaleString()} of ${rows.length.toLocaleString()} filtered rows.` : `${rows.length.toLocaleString()} filtered row${rows.length === 1 ? "" : "s"}.`;
  }

  function downloadCsv(rows, filename) {
    if (!rows.length) return;
    const fields = uniq(rows.flatMap(x => Object.keys(x)));
    const quote = value => `"${String(value ?? "").replace(/"/g, '""')}"`;
    const csv = [fields.map(quote).join(","), ...rows.map(row => fields.map(f => quote(row[f])).join(","))].join("\r\n");
    const blob = new Blob([csv], { type: "text/csv;charset=utf-8" });
    const url = URL.createObjectURL(blob); const a = document.createElement("a");
    a.href = url; a.download = filename; a.click(); URL.revokeObjectURL(url);
  }

  function refreshAll() {
    buildOverview(); drawForecast(); updatePerformanceFamilies(); renderConfigurationTable();
  }

  function refreshControlDomains() {
    const visibleForecasts = forecasts.filter(selected);
    const visiblePerformance = performance.filter(selected);
    setOptions($("forecastLocation"), uniq(visibleForecasts.map(x => x.location)), null, false);
    setOptions($("forecastReason"), uniq(filteredModels().map(m => m.reason)), "All forecasting purposes");
    setForecastViewOptions(false);
    updateForecastReferences(false);
    updateForecastHorizons();
    setDisplayOptions($("perfPeriod"), preferredOrder(
      uniq(visiblePerformance.map(x => x.evaluation_period)), ["Testing", "Real-time"]),
      evaluationLabel, null, false);
    updatePerformanceDiseases(false);
    const visibleParameters = parameters.filter(selected);
    setOptions($("configModel"), uniq(visibleParameters.map(x => x.model)), "All models", false);
    setOptions($("configLocation"), uniq(visibleParameters.map(x => x.location).filter(x => x !== "All locations")), "All locations", false);
    setOptions($("configGroup"), uniq(visibleParameters.map(x => x.group)), "All parameter groups", false);
    refreshAll();
  }

  setupGlobalFilters();
  buildModelFilters();
  buildOverview();
  activateTabs();
  setupForecastControls();
  setupPerformanceControls();
  setupConfigurationControls();
  window.addEventListener("resize", () => window.requestAnimationFrame(() => { drawForecast(); drawPerformance(); }), { passive: true });
})();
