(function () {
  'use strict';
  
  // Define global function
  window.syncLegendCheckboxesOnRender = function (el, x) {
    try {
      // Ensure jQuery is present since original logic uses $
      if (typeof window.$ === 'undefined') {
        console.warn('syncLegendCheckboxesOnRender: jQuery ($) not found — aborting binding.');
        return;
      }
      
      var $el = window.$(el);
      var $wrap = $el.closest('.fs-wrap');
      var init = false;
      
      function getGd() {
        // prefer active plot-panel (multi-geo), else fallback to any plotly in this wrap
        var active = $wrap.find('.plot-panel:visible .plotly').get(0);
        if (!active) {
          active = $el.hasClass('plotly') ? el : $el.find('.plotly').get(0);
        }
        return active || el;
      }
      
      function syncCB($cb) {
        var id = $cb.attr('data-trace');
        var gd = getGd();
        if (!gd || !gd.data) return;
        var anyVisible = false;
        for (var t = 0; t < gd.data.length; t++) {
          if (gd.data[t].legendgroup === id) {
            var v = gd.data[t].visible;
            if (v === true || v === undefined) { anyVisible = true; break; }
          }
        }
        $cb.prop('checked', anyVisible);
      }
      
      function bind() {
        if (init) return;
        init = true;
        $wrap.find('.legend-checkbox[data-trace]').each(function () {
          var $cb = window.$(this);
          // sync initial state
          syncCB($cb);
          
          // change handler toggles all traces with legendgroup === data-trace
          $cb.off('change').on('change', function () {
            var id = $cb.attr('data-trace');
            var checked = $cb.prop('checked');
            var gd = getGd();
            if (!gd || !gd.data) return;
            var inds = [];
            for (var t = 0; t < gd.data.length; t++) {
              if (gd.data[t].legendgroup === id) inds.push(t);
            }
            var vis = checked ? true : 'legendonly';
            if (inds.length) {
              try {
                Plotly.restyle(gd, { visible: vis }, inds);
              } catch (e) {
                // If Plotly not ready, swallow error
                console.warn('syncLegendCheckboxesOnRender: Plotly.restyle failed', e);
              }
            }
          });
        });
      }
      
      // Bind once plotly is ready (handle cases where plotly_afterplot already fired)
      $el.on('plotly_afterplot', function () { bind(); });
      
      // If plot is already present when this runs, try binding after short delay
      setTimeout(function () {
        var gd = getGd();
        if (gd && gd.data && gd.data.length > 0) bind();
      }, 500);
      
      // Re-sync on DOM changes (panels switching, etc.)
      var obs = new MutationObserver(function () {
        $wrap.find('.legend-checkbox[data-trace]').each(function () { syncCB(window.$(this)); });
      });
      var target = $wrap.get(0);
      if (target) obs.observe(target, { childList: true, subtree: true, attributes: false });
    } catch (err) {
      // defensive: log but don't throw in page context
      console.error('syncLegendCheckboxesOnRender error:', err);
    }
  }; // end syncLegendCheckboxesOnRender

})();