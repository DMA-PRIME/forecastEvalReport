#' HTML loading overlay for report templates
#'
#' Builds the HTML, CSS, and JavaScript for a full-page loading spinner shown
#' while a rendered HTML report is loading. Fades out automatically on the
#' browser `window` `load` event. Intended for use inside the package's
#' RMarkdown report templates.
#'
#' @return An [htmltools::tagList()] containing `<style>`, `<div>`, and
#'   `<script>` tags ready to be embedded in an HTML document.
#'
#' @keywords internal
#' @noRd
loading_overlay_html <- function() {
  htmltools::tagList(
    htmltools::tags$style(htmltools::HTML("
      #loading-overlay {
        position: fixed;
        top: 0;
        left: 0;
        width: 100vw;
        height: 100vh;
        background: rgba(255, 255, 255, 0.92);
        z-index: 999999;
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        gap: 16px;
        transition: opacity 0.4s ease;
      }
      #loading-overlay.hidden {
        opacity: 0;
        pointer-events: none;
      }
      .loading-spinner {
        width: 52px;
        height: 52px;
        border: 5px solid #E4D9F2;   /* pastel purple track */
        border-top-color: #C9B6E4;   /* pastel purple accent */
        border-radius: 50%;
        animation: spin 0.85s linear infinite;
      }
      .loading-text {
        font-size: 15px;
        color: #6b7280;
        font-family: sans-serif;
        letter-spacing: 0.02em;
      }
      @keyframes spin {
        to { transform: rotate(360deg); }
      }
    ")),
    htmltools::tags$div(
      id = "loading-overlay",
      htmltools::tags$div(class = "loading-spinner"),
      htmltools::tags$div(class = "loading-text", "Loading report...")
    ),
    htmltools::tags$script(htmltools::HTML("
      window.addEventListener('load', function() {
        var overlay = document.getElementById('loading-overlay');
        if (!overlay) return;
        overlay.classList.add('hidden');
        setTimeout(function() {
          overlay.style.display = 'none';
        }, 450);
      });
    "))
  )
}
