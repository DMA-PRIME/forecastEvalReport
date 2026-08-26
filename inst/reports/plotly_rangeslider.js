function(el, x) {
  var gd = document.getElementById(el.id);
  if (!gd) return;

  var maxPhase = x.maxPhase || 1000;
  var SHADOW_TAG = '__rangeslider_shadow__';
  var PHASE_GROUPS = ['Training Period', 'Validation Period', 'Testing Period'];

  function paddedRange(min, max) {
    if (min === Infinity || max === -Infinity) return [0, 1];
    if (min === max) return [min - 1, max + 1];
    return [0, max + ((max - min) * 0.10)];
  }

  //--------------------------------------------------------------------------//
  // Computing the axis maximum for visible non-ribbon traces only ------------//
  //--------------------------------------------------------------------------//
  // About: Phase ribbon traces are excluded from the max calculation since    //
  // they are set to a fixed large height (999999) and would otherwise inflate //
  // the y-axis range far beyond the actual data.                              //
  //--------------------------------------------------------------------------//
  function computeAxisMax(side) {
    var yMax = -Infinity;

    gd._fullData.forEach(function(trace) {
      if (trace[SHADOW_TAG]) return;
      if (trace.visible === 'legendonly' || trace.visible === false) return;
      if (!trace.y) return;
      if (PHASE_GROUPS.indexOf(trace.legendgroup) !== -1) return;

      var isRight = (trace.yaxis === 'y2');
      if (side === 'right' && !isRight) return;
      if (side === 'left'  &&  isRight) return;

      var m = Math.max.apply(null, trace.y.filter(function(v) { return v != null && !isNaN(v); }));
      if (m > yMax) yMax = m;
    });

    return yMax === -Infinity ? null : yMax;
  }

  function updateRangesliderY() {
    var leftMax  = computeAxisMax('left');
    var rightMax = computeAxisMax('right');

    var leftEffective  = (leftMax  !== null && leftMax  > 0) ? leftMax  : maxPhase;
    var rightEffective = (rightMax !== null && rightMax > 0) ? rightMax : null;

    var leftRange  = paddedRange(0, leftEffective);
    var update = {
      'xaxis.rangeslider.yaxis.range':     [leftRange[0],  leftRange[1]],
      'xaxis.rangeslider.yaxis.autorange':  false
    };

    if (rightEffective !== null) {
      var rightRange = paddedRange(0, rightEffective);
      update['xaxis.rangeslider.yaxis2.range']    = [rightRange[0], rightRange[1]];
      update['xaxis.rangeslider.yaxis2.autorange'] = false;
    }

    return Plotly.relayout(gd, update).then(removeGreyBars);
  }

  //--------------------------------------------------------------------------//
  // Main axis scaling on zoom/pan and trace toggle ---------------------------//
  //--------------------------------------------------------------------------//
  // About: Recalculates the y-axis range based on visible non-ribbon traces  //
  // within the current x-axis window. Phase ribbon traces are excluded from  //
  // the calculation since their y values are set to 999999.                  //
  //--------------------------------------------------------------------------//
  function updateMainY() {
    var xaxis = gd._fullLayout.xaxis;
    if (!xaxis || !xaxis.range) return;

    var xMin = new Date(xaxis.range[0]).getTime();
    var xMax = new Date(xaxis.range[1]).getTime();
    var localMaxLeft  = -Infinity;
    var localMaxRight = -Infinity;

    gd._fullData.forEach(function(trace) {
      if (trace[SHADOW_TAG]) return;
      if (trace.visible === 'legendonly' || trace.visible === false) return;
      if (!trace.x || !trace.y) return;
      if (PHASE_GROUPS.indexOf(trace.legendgroup) !== -1) return;

      var isRight = (trace.yaxis === 'y2');

      for (var i = 0; i < trace.x.length; i++) {
        var t = new Date(trace.x[i]).getTime();
        if (t >= xMin && t <= xMax) {
          var val = trace.y[i];
          if (val != null && !isNaN(val)) {
            if (isRight) { if (val > localMaxRight) localMaxRight = val; }
            else         { if (val > localMaxLeft)  localMaxLeft  = val; }
          }
        }
      }
    });

    var u = {};

    if (localMaxLeft === -Infinity) {
      u['yaxis.autorange'] = true;
    } else {
      u['yaxis.autorange'] = false;
      var rl = paddedRange(0, localMaxLeft);
      u['yaxis.range[0]'] = rl[0];
      u['yaxis.range[1]'] = rl[1];
    }

    if (gd._fullLayout.yaxis2) {
      if (localMaxRight === -Infinity) {
        u['yaxis2.autorange'] = true;
      } else {
        u['yaxis2.autorange'] = false;
        var rr = paddedRange(0, localMaxRight);
        u['yaxis2.range[0]'] = rr[0];
        u['yaxis2.range[1]'] = rr[1];
      }
    }

    if (Object.keys(u).length > 0) Plotly.relayout(gd, u);
  }

  gd._updateMainY = updateMainY;

  function syncRightAxisVisibility() {
    if (!gd._fullLayout || !gd._fullLayout.yaxis2) return;
    var anyRightVisible = gd.data.some(function(trace) {
      return trace.yaxis === 'y2' && !trace[SHADOW_TAG] &&
             trace.visible !== false && trace.visible !== 'legendonly';
    });
    var currentlyVisible = gd._fullLayout.yaxis2.visible !== false;
    if (anyRightVisible === currentlyVisible) return;
    Plotly.relayout(gd, { 'yaxis2.visible': anyRightVisible });
  }

  function removeGreyBars() {
    el.querySelectorAll('.rangeslider-mask-min-opp-axis, .rangeslider-mask-max-opp-axis')
      .forEach(function(r) { r.style.opacity = '0'; });
  }

  var panel = el.closest('.plot-panel');
  if (panel) {
    var allPanels = Array.from(panel.parentNode.querySelectorAll('.plot-panel'));
    var panelIndex = allPanels.indexOf(panel);
    var wrap = el.closest('.fs-wrap');
    var plotId = wrap ? wrap.getAttribute('data-fs-id') : 'unknown';
    if (!window.plotMaxPhaseData) window.plotMaxPhaseData = {};
    if (!window.plotMaxPhaseData[plotId]) window.plotMaxPhaseData[plotId] = {};
    window.plotMaxPhaseData[plotId][panelIndex] = maxPhase;
  }

  gd.once('plotly_afterplot', function() {
    updateRangesliderY().then(function() {
      updateMainY();
      syncRightAxisVisibility();
    });
    var observer = new MutationObserver(removeGreyBars);
    observer.observe(el, { childList: true, subtree: true });
  });

  gd.on('plotly_relayout', function(ev) {
    if (ev['xaxis.range[0]'] || ev['xaxis.range'] || ev['xaxis.autorange']) {
      window.requestAnimationFrame(updateMainY);
    }
  });

  gd.on('plotly_restyle', function() {
    window.requestAnimationFrame(function() {
      syncRightAxisVisibility();
      updateRangesliderY().then(updateMainY);
    });
  });

}
