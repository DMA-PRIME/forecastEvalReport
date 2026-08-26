function(el, x) {

  //--------------------------------------------------------------------------//
  // PER-PLOT SETUP (runs for every plot, including hidden non-first panels)    //
  //--------------------------------------------------------------------------//
  // About: Plots 2..n are created inside display:none panels, so Plotly cannot
  // measure them at creation and they render at the wrong size. We capture each
  // plot's intended normal size once, expose a forceNormalSize() helper on the
  // element, and watch the containing panel so the plot snaps to its normal
  // size whenever it becomes visible (e.g. via the geography dropdown). The
  // exit-fullscreen path reuses the exact same helper so shrink-back is
  // deterministic for every plot, not just the first.
  //--------------------------------------------------------------------------//
  (function() {
    var gd = document.getElementById(el.id) || el;

    // Intended normal-view size, taken from the plot's own layout at creation
    gd.__normalW = (gd.layout && gd.layout.width)  ||
                   (gd._fullLayout && gd._fullLayout.width)  || 900;
    gd.__normalH = (gd.layout && gd.layout.height) ||
                   (gd._fullLayout && gd._fullLayout.height) || 800;

    // Force this plot back to its normal size. Always updates the layout (so
    // even hidden plots are correct when later shown); only repaints via
    // Plots.resize when the plot is actually visible.
    gd.__forceNormalSize = function() {
      if (!gd._fullLayout) return;
      gd.style.width  = gd.__normalW + 'px';
      gd.style.height = gd.__normalH + 'px';
      Plotly.relayout(gd, {
        autosize: false,
        width:    gd.__normalW,
        height:   gd.__normalH
      }).then(function() {
        if (gd.offsetParent !== null) Plotly.Plots.resize(gd);
      });
    };

    // When the panel becomes visible and we are NOT in fullscreen, snap to size
    function snapWhenShown() {
      if (document.querySelector('.fs-overlay.active')) return;  // fullscreen owns sizing
      if (!gd._fullLayout) return;
      if (gd.offsetParent === null) return;                      // still hidden
      if (gd._fullLayout.width === gd.__normalW &&
          gd._fullLayout.height === gd.__normalH) return;        // already correct
      gd.__forceNormalSize();
    }

    var panel = gd.closest ? gd.closest('.plot-panel') : null;
    if (panel) {
      var obs = new MutationObserver(snapWhenShown);
      obs.observe(panel, { attributes: true, attributeFilter: ['style', 'class'] });
    }

    // One attempt shortly after render in case it is already visible
    setTimeout(snapWhenShown, 300);
  })();

  //--------------------------------------------------------------------------//
  // GLOBAL SETUP (runs once per page)                                         //
  //--------------------------------------------------------------------------//
  if (window.__fsInstalled) return;
  window.__fsInstalled = true;

  function ensureOverlay() {
    var overlay = document.querySelector('.fs-overlay');
    if (!overlay) {
      overlay = document.createElement('div');
      overlay.className = 'fs-overlay';
      document.body.appendChild(overlay);
    }
    return overlay;
  }

  function findWrap(node) {
    var n = node;
    while (n && n !== document.body) {
      if (n.classList && n.classList.contains('fs-wrap')) return n;
      n = n.parentNode;
    }
    return null;
  }

  // Size every plot in the wrap to an exact pixel size (used for fullscreen)
  function sizeAllPlots(wrap, w, h) {
    if (!w || !h) return;
    wrap.querySelectorAll('.plotly').forEach(function(p) {
      if (!p._fullLayout) return;
      p.style.width  = w + 'px';
      p.style.height = h + 'px';
      Plotly.relayout(p, { autosize: false, width: w, height: h }).then(function() {
        Plotly.Plots.resize(p);
      });
    });
  }

  // Poll until the overlay width is stable, then size the plots to fill it
  function sizeWhenStable(wrap, overlay) {
    var last = -1;
    var tries = 0;
    (function attempt() {
      var w = overlay.clientWidth;
      var h = overlay.clientHeight;
      tries++;
      if (w && h && w === last) { sizeAllPlots(wrap, w, h); return; }
      last = w;
      if (tries < 30) requestAnimationFrame(attempt);
      else if (w && h) sizeAllPlots(wrap, w, h);
    })();
  }

  //--------------------------------------------------------------------------//
  // Main toggle -- called by the modebar fullscreen button                    //
  //--------------------------------------------------------------------------//
  window.toggleFullscreenFromGd = function(gd) {
    if (!gd) return;

    var wrap = findWrap(gd);
    if (!wrap) return;

    var overlay  = ensureOverlay();
    var entering = !overlay.classList.contains('active');

    if (entering) {

      //----------------------------- ENTER --------------------------------//

      // Save wrap position + inline style so we can restore it on exit
      wrap.__fsParent    = wrap.parentNode;
      wrap.__fsNext      = wrap.nextSibling;
      wrap.__fsStyleAttr = wrap.getAttribute('style') || '';

      document.body.style.overflow = 'hidden';

      overlay.appendChild(wrap);
      overlay.classList.add('active');

      wrap.setAttribute('style',
        'width:100%;height:100%;margin:0;padding:0;position:relative;box-sizing:border-box;');

      sizeWhenStable(wrap, overlay);

      gd.__fsResize = function() {
        sizeAllPlots(wrap, overlay.clientWidth, overlay.clientHeight);
      };
      window.addEventListener('resize', gd.__fsResize);

    } else {

      //----------------------------- EXIT ---------------------------------//

      if (gd.__fsResize) {
        window.removeEventListener('resize', gd.__fsResize);
        gd.__fsResize = null;
      }

      // Move the wrap back to its original location
      if (wrap.__fsParent) {
        wrap.__fsParent.insertBefore(wrap, wrap.__fsNext || null);
      }

      overlay.classList.remove('active');
      wrap.setAttribute('style', wrap.__fsStyleAttr);
      document.body.style.overflow = '';

      // Restore EVERY plot to its own normal size using the shared helper.
      // Deferring one frame lets the DOM settle back into the in-page layout
      // before we measure/repaint, so the shrink-back is reliable for all plots.
      requestAnimationFrame(function() {
        wrap.querySelectorAll('.plotly').forEach(function(p) {
          if (p.__forceNormalSize) p.__forceNormalSize();
        });
      });

    }
  };
}
