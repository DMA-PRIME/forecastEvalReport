//------------------------------------------------------------------------------//
// Syncing geography dropdowns across plot sections ----------------------------//
//------------------------------------------------------------------------------//
// About: This script links the geography dropdowns for plot1 and plot3 so      //
// that selecting a geography in one automatically selects the same geography   //
// in the other. It works by listening for change events on each dropdown and   //
// dispatching a matching change event on the other dropdown when a selection   //
// is made. The sync is bidirectional so it works regardless of which dropdown  //
// the user interacts with first. A guard flag prevents infinite event loops    //
// where each change event triggers the other which triggers the first again.   //
//------------------------------------------------------------------------------//
  
  (function() {
    
    function initGeoSync() {
      
      //------------------------------------------------------------------------//
        // Finding the two geography dropdowns ------------------------------------//
        //------------------------------------------------------------------------//
        var select1 = document.getElementById('geoSelect-plot1');
        var select3 = document.getElementById('geoSelect-plot3');
        
        //------------------------------------------------------------------------//
          // Skipping if either dropdown is not present on the page ----------------//
          //------------------------------------------------------------------------//
          // About: Not all reports have both plot sections. If either dropdown is   //
          // missing the sync is skipped gracefully without throwing an error.       //
          //------------------------------------------------------------------------//
          if (!select1 || !select3) return;
        
        //------------------------------------------------------------------------//
          // Skipping if already initialized ----------------------------------------//
          //------------------------------------------------------------------------//
          if (select1.__geoSyncInitialized) return;
        select1.__geoSyncInitialized = true;
        
        //------------------------------------------------------------------------//
          // Guard flag to prevent infinite event loops -----------------------------//
          //------------------------------------------------------------------------//
          // About: When select1 changes it triggers a change on select3. Without   //
          // a guard, select3's change listener would then trigger select1 again,   //
    // creating an infinite loop. The flag is set before dispatching and      //
    // cleared after so normal user interactions still trigger the sync.      //
    //------------------------------------------------------------------------//
    var syncing = false;

    //------------------------------------------------------------------------//
    // Syncing select1 -> select3 ---------------------------------------------//
    //------------------------------------------------------------------------//
    // About: When the user changes the geography in plot1, this listener      //
    // finds the matching option in plot3 by geography name and selects it,   //
    // then dispatches a change event so the plot3 panel display updates.     //
    //------------------------------------------------------------------------//
    select1.addEventListener('change', function() {
      if (syncing) return;
      syncing = true;

      var selectedText = select1.options[select1.selectedIndex].text;

      //----------------------------------------------------------------------//
      // Finding matching option in select3 by name --------------------------//
      //----------------------------------------------------------------------//
      var match = Array.from(select3.options).find(function(opt) {
        return opt.text === selectedText;
      });

      if (match) {
        select3.value = match.value;
        select3.dispatchEvent(new Event('change', { bubbles: true }));
      }

      syncing = false;
    });

    //------------------------------------------------------------------------//
    // Syncing select3 -> select1 ---------------------------------------------//
    //------------------------------------------------------------------------//
    // About: When the user changes the geography in plot3, this listener      //
    // finds the matching option in plot1 by geography name and selects it,   //
    // then dispatches a change event so the plot1 panel display updates.     //
    //------------------------------------------------------------------------//
    select3.addEventListener('change', function() {
      if (syncing) return;
      syncing = true;

      var selectedText = select3.options[select3.selectedIndex].text;

      //----------------------------------------------------------------------//
      // Finding matching option in select1 by name --------------------------//
      //----------------------------------------------------------------------//
      var match = Array.from(select1.options).find(function(opt) {
        return opt.text === selectedText;
      });

      if (match) {
        select1.value = match.value;
        select1.dispatchEvent(new Event('change', { bubbles: true }));
      }

      syncing = false;
    });

    //------------------------------------------------------------------------//
    // Syncing the search inputs when dropdown selection changes --------------//
    //------------------------------------------------------------------------//
    // About: If the geo-search input is present above either dropdown, clear  //
    // it when the other dropdown changes so the options list is fully visible  //
    // and the selected option is always visible in the list.                  //
    //------------------------------------------------------------------------//
    function clearSearchInput(select) {
      var searchInput = select.previousElementSibling;
      if (searchInput && searchInput.classList.contains('geo-search-input')) {
        searchInput.value = '';
        Array.from(select.options).forEach(function(opt) {
          opt.style.display = '';
        });
      }
    }

    select1.addEventListener('change', function() {
      clearSearchInput(select3);
    });

    select3.addEventListener('change', function() {
      clearSearchInput(select1);
    });
  }

  //--------------------------------------------------------------------------//
  // Initializing on DOM ready and observing for dynamic content --------------//
  //--------------------------------------------------------------------------//
  // About: The dropdowns may not be present immediately on page load if they  //
  // are rendered dynamically. A MutationObserver watches for new content and  //
  // re-runs initGeoSync() so the sync is always attached even if the          //
  // dropdowns appear after the initial page load.                             //
  //--------------------------------------------------------------------------//
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', initGeoSync);
  } else {
    initGeoSync();
  }

  var observer = new MutationObserver(function() {
    initGeoSync();
  });
  observer.observe(document.body, { childList: true, subtree: true });

})();