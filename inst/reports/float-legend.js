(function () {
  'use strict';
  
  /* ============================================================
    ENHANCED FLOATING LEGEND CONTROLLER WITH DROPDOWN SUPPORT
  ============================================================ */
    
    // ===================================================================
      // CONFIGURATION
    // ===================================================================
      
      const DEFAULT_PERSIST_KEY = null;
      const DEFAULT_KEEP_VISIBLE_IN_FS = false;
      const FULLSCREEN_OVERLAY_CLASS = 'fs-overlay';
      const FORCE_INITIAL_POS = { left: 110, top: 65 };
      
      // ===================================================================
        // UTILITY FUNCTIONS
      // ===================================================================
        
        const q  = (sel, root = document) => (root || document).querySelector(sel);
        const qa = (sel, root = document) => Array.from((root || document).querySelectorAll(sel));
        
        function getWrapForLegend(legend) {
          return legend.closest('.fs-wrap');
        }
        
        function getWrapId(wrap) {
          return (wrap && wrap.getAttribute('data-fs-id')) || '';
        }
        
        function getPersistKey(wrap) {
          const k = wrap && wrap.getAttribute('data-legend-persist-key');
          return (k && k.trim().length) ? k.trim() : DEFAULT_PERSIST_KEY;
        }
        
        function getKeepVisibleInFullscreen(wrap) {
          const v = wrap && wrap.getAttribute('data-legend-keep-visible-fs');
          if (!v) return DEFAULT_KEEP_VISIBLE_IN_FS;
          return String(v).toLowerCase() === 'true';
        }
        
        // ===================================================================
          // PLOTLY INTEGRATION
        // ===================================================================
          
          function findPlotlyGd(wrap) {
            if (!wrap) return null;
            
            const activeSelector = '.plot-panel.active-plot-panel .js-plotly-plot, .plot-panel.active-plot-panel .plotly';
            const activeGd = q(activeSelector, wrap);
            if (activeGd) return activeGd;
            
            const candidates = qa('.js-plotly-plot, .plotly', wrap);
            if (!candidates.length) return null;
            if (candidates.length === 1) return candidates[0];
            
            const checkboxEls = qa('.legend-checkbox', wrap);
            const wanted = checkboxEls.reduce((acc, el) => {
              if (el && el.dataset && el.dataset.trace) acc.push(String(el.dataset.trace));
              return acc;
            }, []);
            
            if (wanted.length) {
              for (const cand of candidates) {
                if (!cand || !cand.data) continue;
                const match = cand.data.some(t => {
                  const lg = (t.legendgroup || t.name || '').toString();
                  const uid = (t.uid || t._uid || '').toString();
                  return (lg && wanted.includes(lg)) || (uid && wanted.includes(uid));
                });
                if (match) return cand;
              }
            }
            
            return candidates[0];
          }
        
        function findTraceIndicesByGroup(gd, group) {
          if (!gd || !gd.data) return [];
          const indices = [];
          for (let i = 0; i < gd.data.length; i++) {
            if ((gd.data[i].legendgroup || '') === group) indices.push(i);
          }
          return indices;
        }
        
        function setTraceVisibility(gd, indices, visible) {
          if (!gd || !indices.length) return;
          Plotly.restyle(gd, { visible: visible }, indices);
        }
        
        function isTraceIndexVisible(gd, idx) {
          const v = gd.data && gd.data[idx] && gd.data[idx].visible;
          return !(v === false || v === 'legendonly');
        }

        // Map checkbox data-group to trace legendgroup
        function mapCheckboxGroupToLegendGroup(checkboxGroup) {
          const groupMappings = {
            'Combined': 'Health System Variables (Prisma/MUSC)',
            'CDC Parameters (State-Level)': 'CDC Variables (State-Level)', 
            'CDC(NHSN)': 'CDC Variables (State-Level)',
            'Prisma': 'Prisma Health System Variables',
            'MUSC': 'MUSC Health System Variables'
          };
          return groupMappings[checkboxGroup] || checkboxGroup;
        }
        
        // ===================================================================
          // STYLE UTILITIES
        // ===================================================================
          
          function convertPlotlyDashToSVG(plotlyDash) {
            const dashMap = {
              'solid': '0',
              'dot': '2,3',
              'dash': '6,4',
              'longdash': '10,4',
              'dashdot': '6,3,2,3',
              'longdashdot': '10,4,2,4'
            };
            return dashMap[plotlyDash] || '2,3';
          }
        
        // ===================================================================
          // MAIN LEGEND CONTROLLER
        // ===================================================================
          
          function initLegendController(legend) {
            if (!legend || legend.__legendControllerInitialized) return;
            legend.__legendControllerInitialized = true;
            
            const wrap = getWrapForLegend(legend);
            if (!wrap) return;
            
            const wrapId = getWrapId(wrap);
            const persistKey = getPersistKey(wrap);
            const keepVisibleInFullscreen = getKeepVisibleInFullscreen(wrap);
            const handle = q('.legend-drag', legend) || legend;
            
            // -------------------------------------------------------------------
              // CHECKBOX SYNC
            // -------------------------------------------------------------------
              
            function syncCheckboxesWithPlot() {
              const gd = findPlotlyGd(wrap);
              const checkboxes = qa('.legend-checkbox', legend);
              if (!gd) return;

              checkboxes.forEach(cb => {
                const traceNameAttr = cb.getAttribute('data-trace-name');
                const checkboxGroupAttr = cb.getAttribute('data-group');
                const traceGroupAttr = cb.getAttribute('data-trace');

                let indices = [];

                if (traceNameAttr && checkboxGroupAttr) {
                  // Parameter checkbox: match by name AND mapped legendgroup
                  const expectedLegendGroup = mapCheckboxGroupToLegendGroup(checkboxGroupAttr);
                  indices = gd.data.reduce((acc, trace, idx) => {
                    const nameMatch = trace && trace.name && trace.name.toString() === traceNameAttr.toString();
                    const groupMatch = trace && trace.legendgroup && trace.legendgroup.toString() === expectedLegendGroup;
                    if (nameMatch && groupMatch) acc.push(idx);
                    return acc;
                  }, []);
                } else if (traceNameAttr) {
                  // Non-parameter checkbox: match by name only
                  indices = gd.data.reduce((acc, trace, idx) => {
                    if (trace.name === traceNameAttr || trace.legendgroup === traceNameAttr) {
                      acc.push(idx);
                    }
                    return acc;
                  }, []);
                } else if (traceGroupAttr) {
                  // Match by legendgroup
                  indices = findTraceIndicesByGroup(gd, traceGroupAttr);
                }

                if (indices.length > 0) {
                  const anyVisible = indices.some(i => isTraceIndexVisible(gd, i));
                  cb.checked = !!anyVisible;
                } else {
                  cb.checked = false;
                }
              });
            }
            
            // -------------------------------------------------------------------
              // SWATCH STYLING
            // -------------------------------------------------------------------
              
              function syncSwatchStyles() {
                const gd = findPlotlyGd(wrap);
                if (!gd || !gd.data) return;
                
                // Handle evaluation horizon line swatches
                const lineSwatches = qa('.legend-line-swatch', legend);
                lineSwatches.forEach(swatch => {
                  const horizonName = swatch.dataset.horizon;
                  const lineType = swatch.dataset.lineType || 'dot';
                  
                  if (!horizonName) return;
                  
                  const trace = gd.data.find(t => t.name === horizonName);
                  if (!trace) return;
                  
                  let color = '#648FFF';
                  if (gd._fullData) {
                    const fullTrace = gd._fullData.find(t => t.name === horizonName);
                    if (fullTrace?.line?.color) {
                      color = fullTrace.line.color;
                    } else if (fullTrace?.marker?.color) {
                      color = fullTrace.marker.color;
                    }
                  }
                  
                  if (color === '#648FFF') {
                    if (trace.line?.color) {
                      color = trace.line.color;
                    } else if (trace.marker?.color) {
                      color = typeof trace.marker.color === 'string' 
                      ? trace.marker.color 
                      : (Array.isArray(trace.marker.color) ? trace.marker.color[0] : color);
                    }
                  }
                  
                  const dashArray = convertPlotlyDashToSVG(lineType);
                  
                  const svg = `
                  <svg width="24" height="12" style="display: block;">
                    <line 
                  x1="0" y1="6" 
                  x2="24" y2="6" 
                  stroke="${color}" 
                  stroke-width="2"
                  stroke-dasharray="${dashArray}"
                  stroke-linecap="round" />
                    </svg>
                    `;
                  
                  swatch.innerHTML = svg;
                });
                
                // Handle parameter (auxiliary variable) line swatches.
                // The legend item's data-trace is the trace's legendgroup
                // ("<source>|<param>"), set by build_auxiliary_variables_legend()
                // to mirror the legendgroup in add_parameter_traces_by_disease().
                // Match on that (or the trace name) and paint with the trace's
                // resolved line color + dash. (The previous code gated on a
                // data-group -> legendgroup map that does not fit "source|param"
                // groups, so it never matched and the swatch stayed blank.)
                const paramLineSwatches = qa('.legend-swatch.param-line', legend);
                paramLineSwatches.forEach(swatch => {
                  const item = swatch.closest('.legend-item');
                  const checkbox = item && q('.legend-checkbox', item);
                  if (!checkbox) return;

                  const key = checkbox.getAttribute('data-trace');
                  if (!key) return;

                  const matchTrace = t =>
                    (t.legendgroup && t.legendgroup.toString() === key) ||
                    (t.name && t.name.toString() === key);

                  const src = (gd._fullData || []).find(matchTrace) ||
                              gd.data.find(matchTrace);
                  if (!src) return;

                  let color = (src.line && src.line.color) || null;
                  if (!color && src.marker && src.marker.color) {
                    color = typeof src.marker.color === 'string'
                      ? src.marker.color
                      : (Array.isArray(src.marker.color) ? src.marker.color[0] : null);
                  }
                  if (!color) color = '#999';

                  const dashArray =
                    convertPlotlyDashToSVG((src.line && src.line.dash) || 'solid');

                  swatch.innerHTML =
                    '<svg width="24" height="12" style="display:block;">' +
                    '<line x1="0" y1="6" x2="24" y2="6" stroke="' + color +
                    '" stroke-width="2" stroke-dasharray="' + dashArray +
                    '" stroke-linecap="round" /></svg>';
                });
                
                //------------------------------------------------------------------//
                // Handle prediction interval swatches ----------------------------//
                //------------------------------------------------------------------//
                // About: PI swatch colors are read from data-fill-color and        //
                // data-hover-color attributes set by build_pi_legend_items() from  //
                // PLOT_STYLES. This means adding a new PI level to PLOT_STYLES     //
                // automatically gets the correct swatch color without any JS       //
                // changes needed.                                                  //
                //------------------------------------------------------------------//
                const piSwatches = qa('.legend-swatch[data-fill-color]', legend);
                piSwatches.forEach(swatch => {
                  const fillColor  = swatch.dataset.fillColor;
                  const hoverColor = swatch.dataset.hoverColor;
                  if (fillColor) {
                    swatch.style.backgroundColor = fillColor;
                    swatch.style.borderColor     = hoverColor || fillColor;
                  }
                });
                
                // Handle line+marker swatches
                const lineMarkerSwatches = qa('.legend-swatch.target-data, .legend-swatch.current-projections, .legend-swatch.historical-estimates', legend);
                lineMarkerSwatches.forEach(swatch => {
                  const item = swatch.closest('.legend-item');
                  const checkbox = q('.legend-checkbox', item);
                  if (!checkbox) return;
                  
                  const traceName = checkbox.dataset.traceName;
                  const trace = gd.data.find(t => t.name === traceName || t.legendgroup === traceName);
                  if (!trace) return;
                  
                  let color = trace.line?.color || trace.marker?.color || '#1f77b4';
                  
                  const svg = `
                  <svg width="24" height="12" style="display: block;">
                    <line 
                  x1="2" y1="6" 
                  x2="22" y2="6" 
                  stroke="${color}" 
                  stroke-width="2"
                  stroke-linecap="round" />
                    <circle cx="6" cy="6" r="2" fill="${color}" />
                    <circle cx="12" cy="6" r="2" fill="${color}" />
                    <circle cx="18" cy="6" r="2" fill="${color}" />
                    </svg>
                    `;
                  
                  swatch.innerHTML = svg;
                });
              }
            
            // -------------------------------------------------------------------
              // RANGE MONITORING (for evaluation model visibility)
            // -------------------------------------------------------------------
              
              function getEvaluationDateRange() {
                const gd = findPlotlyGd(wrap);
                if (!gd || !gd.data) return null;
                
                const evalTraces = gd.data.filter(trace => 
                                                    trace.legendgrouptitle &&
                                                    trace.legendgrouptitle.text === 'Evaluation Model'
                );
                
                if (!evalTraces.length) return null;
                
                let minDate = Infinity;
                let maxDate = -Infinity;
                
                evalTraces.forEach(trace => {
                  if (trace.x && trace.x.length) {
                    trace.x.forEach(dateStr => {
                      const timestamp = new Date(dateStr).getTime();
                      if (timestamp < minDate) minDate = timestamp;
                      if (timestamp > maxDate) maxDate = timestamp;
                    });
                  }
                });
                
                return (minDate !== Infinity && maxDate !== -Infinity) 
                ? { start: minDate, end: maxDate } 
                : null;
              }
            
            function monitorXAxisRange() {
              const gd = findPlotlyGd(wrap);
              if (!gd || !gd._fullLayout || !gd._fullLayout.xaxis) return;
              
              const xaxis = gd._fullLayout.xaxis;
              const xRange = xaxis.range || [xaxis._min, xaxis._max];
              
              const evalRange = getEvaluationDateRange();
              
              legend.style.display = 'block';
              legend.style.pointerEvents = 'auto';
              
              if (evalRange) {
                const rangeStart = new Date(xRange[0]).getTime();
                const rangeEnd = new Date(xRange[1]).getTime();
                const isInRange = (rangeEnd >= evalRange.start && rangeStart <= evalRange.end);
                
                const evalSections = qa('.legend-section', legend).filter(section => {
                  const title = q('.legend-section-title', section);
                  return title && (title.textContent.includes('Evaluation') || title.textContent.includes('Modeling Periods'));
                });
                
                evalSections.forEach(section => {
                  section.style.display = isInRange ? 'block' : 'none';
                });
              }
            }
            
            // -------------------------------------------------------------------
              // PERSISTENCE
            // -------------------------------------------------------------------
              
              function maybePersistState() {
                if (!persistKey) return;
                const rect = legend.getBoundingClientRect();
                const cs = getComputedStyle(legend);
                const left = parseFloat(cs.left) || rect.left;
                const top  = parseFloat(cs.top)  || rect.top;
                const collapsed = legend.classList.contains('collapsed');
                
                const payload = {
                  left: Math.round(left),
                  top:  Math.round(top),
                  collapsed: !!collapsed,
                  wrapId: wrapId
                };
                
                try { localStorage.setItem(persistKey, JSON.stringify(payload)); } catch (err) {}
              }
            
            function maybeRestoreState() {
              if (!persistKey) return;
              try {
                const raw = localStorage.getItem(persistKey);
                if (!raw) return;
                const obj = JSON.parse(raw);
                
                if (obj.wrapId && wrapId && obj.wrapId !== wrapId) return;
                
                if (typeof obj.left === 'number') legend.style.left = obj.left + 'px';
                if (typeof obj.top  === 'number') legend.style.top  = obj.top  + 'px';
                
                if (typeof obj.collapsed === 'boolean') {
                  if (obj.collapsed) legend.classList.add('collapsed');
                  else legend.classList.remove('collapsed');
                  updateToggleButton();
                }
              } catch (err) {}
            }
            
            // -------------------------------------------------------------------
              // INTERACTION HANDLERS
            // -------------------------------------------------------------------
              
              function updateToggleButton() {
                const btn = q('.legend-toggle', legend);
                if (!btn) return;
                const collapsed = legend.classList.contains('collapsed');
                
                const arrowSVG = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><polyline points="6 9 12 15 18 9"></polyline></svg>';
                btn.innerHTML = arrowSVG;
              }
            
            function onLegendCheckboxChange(e) {
              const cb = e.currentTarget;
              const gd = findPlotlyGd(wrap);
              if (!gd) return;
              
              const traceNameAttr = cb.getAttribute('data-trace-name');
              const checkboxGroupAttr = cb.getAttribute('data-group');
              const traceGroupAttr = cb.getAttribute('data-trace');

              let indices = [];

              if (traceNameAttr && checkboxGroupAttr) {
                // Parameter checkbox: match by name AND mapped legendgroup
                const expectedLegendGroup = mapCheckboxGroupToLegendGroup(checkboxGroupAttr);
                indices = gd.data.reduce((acc, trace, idx) => {
                  const nameMatch = trace && trace.name && trace.name.toString() === traceNameAttr.toString();
                  const groupMatch = trace && trace.legendgroup && trace.legendgroup.toString() === expectedLegendGroup;
                  if (nameMatch && groupMatch) acc.push(idx);
                  return acc;
                }, []);
              } else if (traceNameAttr) {
                // Non-parameter checkbox: match by name only
                indices = gd.data.reduce((acc, trace, idx) => {
                  if (trace.name === traceNameAttr || trace.legendgroup === traceNameAttr) {
                    acc.push(idx);
                  }
                  return acc;
                }, []);
              } else if (traceGroupAttr) {
                // Match by legendgroup
                indices = findTraceIndicesByGroup(gd, traceGroupAttr);
              }

              if (indices.length > 0) {
                  setTraceVisibility(gd, indices, !!cb.checked);
                
                  // If toggling a left-axis parameter, relayout yaxis to autorange
                  const isLeftAxisParam = indices.some(idx => {
                    const t = gd.data[idx];
                    return Array.isArray(t.customdata) &&
                           t.customdata[0] === 'param' &&
                           (!t.yaxis || t.yaxis === 'y');
                  });
                
                  if (isLeftAxisParam) {
                    const anyLeftParamOn = gd.data.some(t =>
                      Array.isArray(t.customdata) &&
                      t.customdata[0] === 'param' &&
                      (!t.yaxis || t.yaxis === 'y') &&
                      t.visible === true
                    );
                    Plotly.relayout(gd, {
                      'yaxis.autorange': anyLeftParamOn,
                      'yaxis.rangemode': 'tozero'
                    });
                  }
                }
                
                maybePersistState();
              
            }
            
            function installSectionCollapseHandlers() {
              const clickableTitles = qa('.clickable-title', legend);
              
              clickableTitles.forEach(title => {
                title.addEventListener('click', function(e) {
                  e.preventDefault();
                  e.stopPropagation();
                  
                  const section = title.getAttribute('data-section');
                  const content = q(`[data-section-content="${section}"]`, legend);
                  const toggle = q('.section-toggle', title);
                  
                  if (!content || !toggle) return;
                  
                  if (content.classList.contains('collapsed')) {
                    content.classList.remove('collapsed');
                    toggle.textContent = '▾';
                  } else {
                    content.classList.add('collapsed');
                    toggle.textContent = '▸';
                  }
                });
              });
            }
            
            // -------------------------------------------------------------------
              // DRAG HANDLERS (WITH PLOTLY BOUNDS RESTRICTION)
            // -------------------------------------------------------------------
              
              function installDragHandlers() {
                let isPointerDown = false;
                let hasMoved = false;
                let startX = 0, startY = 0;
                let origLeft = 0, origTop = 0;
                const MOVE_THRESHOLD = 10;
                
                function isInteractiveTarget(el) {
                  return !!el.closest('.select-button, .select-menu, a, button, input, select, textarea, label, .legend-checkbox, .clickable-title');
                }
                
                function ensurePositionInitialized() {
                  const rect = legend.getBoundingClientRect();
                  const cs = getComputedStyle(legend);
                  if (!cs.left || cs.left === 'auto') legend.style.left = rect.left + 'px';
                  if (!cs.top  || cs.top  === 'auto') legend.style.top  = rect.top  + 'px';
                }
                
                function onPointerDown(e) {
                  if (e.button !== undefined && e.button !== 0) return;
                  if (isInteractiveTarget(e.target)) return;
                  
                  isPointerDown = true;
                  hasMoved = false;
                  startX = e.clientX;
                  startY = e.clientY;
      
                  ensurePositionInitialized();
                  const cs = getComputedStyle(legend);
                  origLeft = parseFloat(cs.left) || legend.getBoundingClientRect().left;
                  origTop  = parseFloat(cs.top)  || legend.getBoundingClientRect().top;
                }
      
                function onPointerMove(e) {
                  if (!isPointerDown) return;
      
                  const dx = e.clientX - startX;
                  const dy = e.clientY - startY;
                  const distance = Math.sqrt(dx * dx + dy * dy);
      
                  if (distance > MOVE_THRESHOLD) {
                    if (!hasMoved) {
                      hasMoved = true;
                      handle.style.cursor = 'grabbing';
                    }
      
                    e.preventDefault();
      
                    let newLeft = origLeft + dx;
                    let newTop  = origTop  + dy;
      
                    const gd = findPlotlyGd(wrap);
                    const plotContainer = gd || wrap;
                    const containerRect = plotContainer.getBoundingClientRect();
                    const legendRect = legend.getBoundingClientRect();
                    const wrapRect = wrap.getBoundingClientRect();
                    
                    const minLeft = containerRect.left - wrapRect.left;
                    const minTop = containerRect.top - wrapRect.top;
                    const maxLeft = containerRect.right - wrapRect.left - legendRect.width;
                    const maxTop = containerRect.bottom - wrapRect.top - legendRect.height;
      
                    newLeft = Math.min(Math.max(newLeft, minLeft), maxLeft);
                    newTop = Math.min(Math.max(newTop, minTop), maxTop);
      
                    legend.style.left = Math.round(newLeft) + 'px';
                    legend.style.top  = Math.round(newTop)  + 'px';
                  }
                }
      
                function onPointerUp(e) {
                  if (!isPointerDown) return;
                  
                  handle.style.cursor = 'pointer';
                  
                  if (!hasMoved) {
                    legend.classList.toggle('collapsed');
                    updateToggleButton();
                    maybePersistState();
                  } else {
                    maybePersistState();
                  }
                  
                  isPointerDown = false;
                  hasMoved = false;
                }
      
                handle.addEventListener('pointerdown', onPointerDown, { passive: false });
                window.addEventListener('pointermove', onPointerMove, { passive: false });
                window.addEventListener('pointerup', onPointerUp, { passive: false });
      
                qa('.select-button, .select-menu, .legend-checkbox, .clickable-title', legend)
                  .forEach(el => el.addEventListener('pointerdown', ev => ev.stopPropagation(), { passive: false }));
              }
      
    function installCollapseToggle() {
      const btn = q('.legend-toggle', legend);
      if (!btn) return;
    
      btn.addEventListener('click', function (ev) {
        ev.preventDefault();
        ev.stopPropagation();
        legend.classList.toggle('collapsed');
        updateToggleButton();
        maybePersistState();
      });
    
      updateToggleButton();
    }
      
    function installCheckboxHandlers() {
      const checkboxes = qa('.legend-checkbox', legend);
      checkboxes.forEach(cb => cb.addEventListener('change', onLegendCheckboxChange));
      syncCheckboxesWithPlot();
    }

    function attachPlotListeners() {
      const gd = findPlotlyGd(wrap);
      if (!gd || !gd.on) return;
      
      gd.on('plotly_restyle', function() {
        syncCheckboxesWithPlot();
        syncSwatchStyles();
      });
      gd.on('plotly_relayout', function(eventData) {
        syncCheckboxesWithPlot();
        syncSwatchStyles();
        monitorXAxisRange();
      });
      gd.on('plotly_afterplot', function() {
        syncCheckboxesWithPlot();
        syncSwatchStyles();
        monitorXAxisRange();
      });
    }

    // -------------------------------------------------------------------
    // GEOGRAPHY DROPDOWN HANDLER
    // -------------------------------------------------------------------

    function installDropdownHandler() {
      const plotId = wrapId;
      const geoSelect = document.querySelector(`select[data-plot-id="${plotId}"]`);
      
      if (!geoSelect) {
        console.log('No geography dropdown found for plot:', plotId);
        return;
      }
      
      console.log('Installing dropdown handler for:', plotId);
      
      geoSelect.addEventListener('change', function() {
        const selectedIndex = parseInt(this.value);
        const selectedOption = this.options[selectedIndex];
        const hasParams = selectedOption.getAttribute('data-has-params') === 'true';
        
        const hoverText = selectedOption.getAttribute('data-hover-text');
        const targetLabel = document.getElementById('target-data-label-' + plotId);
        if (targetLabel && hoverText) targetLabel.textContent = hoverText;
        
        console.log('Dropdown changed to index:', selectedIndex, 'Has params:', hasParams);

        //------------------------------------------------------------------//
        // Capturing current checkbox states before switching panels --------//
        //------------------------------------------------------------------//
        // About: Before hiding the current panel we read the checked state  //
        // of every legend checkbox. This snapshot is used after the new     //
        // panel becomes active to restore the same visibility state on the  //
        // incoming plot's traces so the user sees a consistent legend       //
        // experience when switching between geographies.                    //
        //------------------------------------------------------------------//
        const checkboxStates = [];
        qa('.legend-checkbox', legend).forEach(function(cb) {
          checkboxStates.push({
            traceNameAttr  : cb.getAttribute('data-trace-name'),
            checkboxGroup  : cb.getAttribute('data-group'),
            traceGroupAttr : cb.getAttribute('data-trace'),
            checked        : cb.checked
          });
        });
        
        // Hide all plot panels in this wrap and show the selected one. The
        // selected panel's plot is already rendered in the DOM (just hidden),
        // so showing it is instant -- no loading gap. Saved checkbox states are
        // applied immediately below.
        const panels = qa('.plot-panel', wrap);
        panels.forEach(function(panel, idx) {
          if (idx === selectedIndex) {
            panel.style.display = 'block';
            panel.classList.add('active-plot-panel');
          } else {
            panel.style.display = 'none';
            panel.classList.remove('active-plot-panel');
          }
        });
        
        // Show/hide parameter sections based on location
        const paramSections = qa('[data-param-section="true"]', legend);
        paramSections.forEach(function(section) {
          section.style.display = hasParams ? 'block' : 'none';
        });
        
        // Give plotly time to render before applying states and syncing
        setTimeout(function() {
          const newGd = findPlotlyGd(wrap);
          
          if (newGd) {

            //--------------------------------------------------------------//
            // Applying captured checkbox states to the new plot panel ------//
            //--------------------------------------------------------------//
            // About: For each checkbox state captured before the switch, we  //
            // find the matching traces in the new plot and apply the same    //
            // visibility. This means if the user had unchecked "Target Data" //
            // on location 1, it will also be unchecked on location 2. The   //
            // same matching logic used in syncCheckboxesWithPlot() and       //
            // onLegendCheckboxChange() is reused here for consistency. Trace //
            // indices are resolved fresh against the new plot's data so      //
            // there is no risk of applying states to the wrong traces.       //
            //--------------------------------------------------------------//
            checkboxStates.forEach(function(state) {
              const visible = state.checked ? true : 'legendonly';
              let indices = [];

              if (state.traceNameAttr && state.checkboxGroup) {
                //----------------------------------------------------------//
                // Parameter checkbox: match by name AND mapped legendgroup --//
                //----------------------------------------------------------//
                const expectedLegendGroup = mapCheckboxGroupToLegendGroup(state.checkboxGroup);
                indices = newGd.data.reduce(function(acc, trace, idx) {
                  const nameMatch  = trace && trace.name &&
                                     trace.name.toString() === state.traceNameAttr.toString();
                  const groupMatch = trace && trace.legendgroup &&
                                     trace.legendgroup.toString() === expectedLegendGroup;
                  if (nameMatch && groupMatch) acc.push(idx);
                  return acc;
                }, []);

              } else if (state.traceNameAttr) {
                //----------------------------------------------------------//
                // Non-parameter checkbox: match by name or legendgroup -----//
                //----------------------------------------------------------//
                indices = newGd.data.reduce(function(acc, trace, idx) {
                  if (trace.name === state.traceNameAttr ||
                      trace.legendgroup === state.traceNameAttr) {
                    acc.push(idx);
                  }
                  return acc;
                }, []);

              } else if (state.traceGroupAttr) {
                //----------------------------------------------------------//
                // Phase ribbon checkbox: match by legendgroup -------------//
                //----------------------------------------------------------//
                indices = findTraceIndicesByGroup(newGd, state.traceGroupAttr);
              }

              if (indices.length) {
                try {
                  Plotly.restyle(newGd, { visible: visible }, indices);
                } catch(e) {
                  console.warn('Legend state persistence: Plotly.restyle failed', e);
                }
              }
            });

            // Get maxPhase from the global lookup
            let newMaxPhase = 1000;
            
            if (window.plotMaxPhaseData && window.plotMaxPhaseData[plotId]) {
              newMaxPhase = window.plotMaxPhaseData[plotId][selectedIndex] || newMaxPhase;
              console.log('Found maxPhase from lookup:', newMaxPhase, 'for plot:', plotId, 'index:', selectedIndex);
            } else {
              console.warn('Could not find maxPhase in lookup for plot:', plotId);
            }
            
            console.log('Using maxPhase:', newMaxPhase);
            
            // Update rangeslider
            if (newGd._fullLayout && newGd._fullLayout.xaxis && newGd._fullLayout.xaxis.rangeslider) {
              newGd._fullLayout.xaxis.rangeslider.yaxis.range = [0, newMaxPhase];
              newGd._fullLayout.xaxis.rangeslider.yaxis.autorange = false;
              
              if (newGd._fullLayout.xaxis.rangeslider._input) {
                newGd._fullLayout.xaxis.rangeslider._input.yaxis = newGd._fullLayout.xaxis.rangeslider._input.yaxis || {};
                newGd._fullLayout.xaxis.rangeslider._input.yaxis.range = [0, newMaxPhase];
                newGd._fullLayout.xaxis.rangeslider._input.yaxis.autorange = false;
              }
              
              console.log('Updated rangeslider internal structures to maxPhase:', newMaxPhase);
              
              setTimeout(function() {
                Plotly.Plots.resize(newGd).then(function() {
                  console.log('Rangeslider resized');
                });
              }, 50);
            }
            
            // Trigger updateMainY for the newly visible plot
            if (newGd._updateMainY) {
              console.log('Calling updateMainY for newly visible plot');
              newGd._updateMainY();
            }
            
            // Re-sync everything for the new plot
            syncCheckboxesWithPlot();
            syncSwatchStyles();
            monitorXAxisRange();
            
            // Attach listeners to new plot
            if (newGd.on && !newGd._hasFloatLegendListeners) {
              console.log('Attaching listeners to new plot');
              newGd.on('plotly_restyle', function() {
                syncCheckboxesWithPlot();
                syncSwatchStyles();
              });
              newGd.on('plotly_relayout', function() {
                syncCheckboxesWithPlot();
                syncSwatchStyles();
                monitorXAxisRange();
              });
              newGd.on('plotly_afterplot', function() {
                syncCheckboxesWithPlot();
                syncSwatchStyles();
                monitorXAxisRange();
              });
              newGd._hasFloatLegendListeners = true;
            }
          }
        }, 40);
      });
      
      // Initialize parameter visibility on page load
      const initialOption = geoSelect.options[geoSelect.selectedIndex];
      const initialHasParams = initialOption.getAttribute('data-has-params') === 'true';
      const paramSections = qa('[data-param-section="true"]', legend);
      paramSections.forEach(function(section) {
        section.style.display = initialHasParams ? 'block' : 'none';
      });
    }

    // -------------------------------------------------------------------
    // FULLSCREEN HANDLING
    // -------------------------------------------------------------------

    function installFullscreenObserver() {
      if (!keepVisibleInFullscreen) return;

      let originalParent = legend.parentNode;
      let originalNext = legend.nextSibling;
      let originalZIndex = legend.style.zIndex || '';
      let movedToOverlay = false;

      function getActiveOverlayForWrap() {
        const ov = wrap.closest('.' + FULLSCREEN_OVERLAY_CLASS) || q('.' + FULLSCREEN_OVERLAY_CLASS);
        return ov || document.body;
      }

      function moveToOverlay() {
        const overlay = getActiveOverlayForWrap();
        if (!overlay || movedToOverlay) return;

        const rect = legend.getBoundingClientRect();
        legend.style.left = Math.round(rect.left) + 'px';
        legend.style.top  = Math.round(rect.top)  + 'px';

        try { legend.style.setProperty('display', 'block', 'important'); }
        catch (err) { legend.style.display = 'block'; }

        legend.style.zIndex = String(300010 + 30);
        overlay.appendChild(legend);
        movedToOverlay = true;
      }

      function restoreFromOverlay() {
        if (!movedToOverlay) return;

        if (originalParent) originalParent.insertBefore(legend, originalNext || null);
        else document.body.appendChild(legend);

        legend.style.zIndex = originalZIndex;
        legend.style.removeProperty('display');
        movedToOverlay = false;
      }

      const body = document.body;
      const mo = new MutationObserver(mutations => {
        for (const m of mutations) {
          if (m.type === 'attributes' && m.attributeName === 'class') {
            const fs = body.classList.contains('fs-lock');
            if (fs) moveToOverlay();
            else restoreFromOverlay();
          }
        }
      });
      mo.observe(body, { attributes: true, attributeFilter: ['class'] });
    }

    // -------------------------------------------------------------------
    // INITIALIZATION
    // -------------------------------------------------------------------

    function initializeNow() {
      legend.style.left = FORCE_INITIAL_POS.left + 'px';
      legend.style.top  = FORCE_INITIAL_POS.top  + 'px';

      maybeRestoreState();
      installDragHandlers();
      installCollapseToggle();
      installCheckboxHandlers();
      installSectionCollapseHandlers();
      installDropdownHandler();
      attachPlotListeners();
      installFullscreenObserver();

      syncCheckboxesWithPlot();
      syncSwatchStyles();
      
      setTimeout(monitorXAxisRange, 250);
    }

    initializeNow();

    legend.__floatLegendAPI = {
      sync: syncCheckboxesWithPlot,
      syncStyles: syncSwatchStyles,
      persist: maybePersistState,
      restore: maybeRestoreState,
      monitorRange: monitorXAxisRange,
      findGd: () => findPlotlyGd(wrap)
    };
  }

  // ===================================================================
  // INITIALIZATION
  // ===================================================================

  function tryInitializeWithRetries(maxAttempts = 12, interval = 250) {
    let attempts = 0;

    const tryOnce = () => {
      attempts++;

      const wraps = qa('.fs-wrap');
      let anyInitialized = false;

      wraps.forEach(wrap => {
        const legend = q('.float-legend', wrap);
        if (!legend) return;
        initLegendController(legend);
        anyInitialized = true;
      });

      if (!anyInitialized && attempts < maxAttempts) {
        setTimeout(tryOnce, interval);
        return;
      }

      const needsMore = wraps.some(w => {
        const legend = q('.float-legend', w);
        if (!legend) return false;
        const gd = findPlotlyGd(w);
        return !gd;
      });

      if (needsMore && attempts < maxAttempts) {
        setTimeout(tryOnce, interval);
      } else {
        wraps.forEach(w => {
          const legend = q('.float-legend', w);
          if (legend && legend.__floatLegendAPI) {
            legend.__floatLegendAPI.sync();
            legend.__floatLegendAPI.syncStyles();
            legend.__floatLegendAPI.monitorRange();
          }
        });
      }
    };

    tryOnce();
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', () => tryInitializeWithRetries());
  } else {
    tryInitializeWithRetries();
  }

  window.__floatLegendMulti = {
    resyncAll: () => {
      qa('.fs-wrap .float-legend').forEach(l => {
        if (l.__floatLegendAPI) {
          l.__floatLegendAPI.sync();
          l.__floatLegendAPI.monitorRange();
        }
      });
    }
  };

})();
