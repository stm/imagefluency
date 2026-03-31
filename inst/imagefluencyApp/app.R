#' @include utils.R complexity.R contrast.R self-similarity.R simplicity.R symmetry.R typicality.R
NULL

# Configuration
MAX_IMAGES <- 100

# Default (demo) image
defaultimage <- system.file("imagefluencyApp", "www", "rails.jpg", package = "imagefluency")

# Load required packages
if (!requireNamespace("bslib", quietly = TRUE)) {
  message("Note: Install 'bslib' package for enhanced UI in the dashboard: install.packages('bslib')")
}

use_bslib <- requireNamespace("bslib", quietly = TRUE)

if (use_bslib) {
  library(bslib)
}

# Metric descriptions
metric_info <- list(
  contrast = list(
    name = "Contrast",
    description = "Measures overall luminance contrast using RMS (root mean square) contrast. Higher values indicate greater contrast between light and dark areas. This metric captures the overall luminance variation in the image.",
    reference = "RMS contrast of luminance"
  ),
  selfsim = list(
    name = "Self-Similarity",
    description = "Quantifies fractal-like patterns and repetitive structures across different scales. Higher values indicate more self-similar, fractal-like patterns. Uses quadtree decomposition to analyze similarity at different levels.",
    reference = "Quadtree decomposition"
  ),
  simpl = list(
    name = "Simplicity",
    description = "Inverse measure of visual complexity based on compressed file size. Higher values indicate simpler images. Computed as 1 minus the ratio of compressed to maximum possible size. Simple images compress better than complex ones.",
    reference = "1 - (compressed / maximum)"
  ),
  sym = list(
    name = "Symmetry",
    description = "Measures vertical mirror symmetry by correlating the image with its horizontally flipped version. Values range from 0 (no symmetry) to 1 (perfect symmetry). Captures bilateral symmetry around the vertical axis.",
    reference = "Pixel correlation after mirroring"
  ),
  typ = list(
    name = "Typicality",
    description = "Measures how typical each image is relative to the set of all uploaded images. Computed as the correlation of each image with the mean of all images. Values range from -1 (inversely typical) through 0 (not typical) to 1 (perfectly typical). Requires at least 2 images.",
    reference = "Correlation with mean image"
  )
)

documentation_intro <- list(
  shiny::h4("Processing Fluency Theory"),
  shiny::p("This app computes image statistics based on processing fluency theory,
           a framework from cognitive psychology that explains how easily information
           is processed affects aesthetic judgments and preferences. The metrics quantify
           aesthetic principles that facilitate fluent (easy, fast) cognitive processing
           of visual stimuli."),
  shiny::p("Visual processing fluency is influenced by multiple factors including contrast,
           complexity, symmetry, and typicality. Images that are easier to process (more fluent)
           are generally preferred and judged as more aesthetically pleasing, though context matters.")
)

documentation_references <- shiny::tags$ul(
  shiny::tags$li(shiny::HTML(
    'Mayer, S. &amp; Landwehr, J. R. (2018). Quantifying Visual Aesthetics. <em>Psychology of Aesthetics, Creativity, and the Arts</em>, 12(4), 399-431. <a href="https://doi.org/10.1037/aca0000187" target="_blank">doi:10.1037/aca0000187</a>'
  )),
  shiny::tags$li(shiny::HTML(
    'Mayer, S. &amp; Landwehr, J. R. (2018). Objective measures of design typicality. <em>Design Studies</em>, 54, 146-161. <a href="https://doi.org/10.1016/j.destud.2017.09.004" target="_blank">doi:10.1016/j.destud.2017.09.004</a>'
  )),
  shiny::tags$li(
    "Package: ",
    shiny::tags$a(href = "https://imagefluency.com", "imagefluency.com", target = "_blank")
  ),
  shiny::tags$li(
    "GitHub: ",
    shiny::tags$a(href = "https://github.com/stm/imagefluency", "github.com/stm/imagefluency", target = "_blank")
  )
)

documentation_faq <- list(
  shiny::tags$div(
    class = "mb-3",
    shiny::tags$div(class = "fw-bold", "Q: How many images can I process?"),
    shiny::p(sprintf("A: Up to %d images at once. Note: Processing more than 10 images may take considerable time.", MAX_IMAGES))
  ),
  shiny::tags$div(
    class = "mb-3",
    shiny::tags$div(class = "fw-bold", "Q: Which formats are supported?"),
    shiny::p("A: PNG, JPEG, BMP, and TIFF.")
  ),
  shiny::tags$div(
    class = "mb-3",
    shiny::tags$div(class = "fw-bold", "Q: How long does it take?"),
    shiny::p("A: Typically 0.1-2 seconds per image per metric.")
  ),
  shiny::tags$div(
    class = "mb-3",
    shiny::tags$div(class = "fw-bold", "Q: Can I use this for research?"),
    shiny::p("A: Yes! Please cite the package and relevant papers above.")
  )
)

shiny::shinyApp(

  # UI
  ui = if (use_bslib) {
    # Modern UI with bslib
    bslib::page_fluid(
      theme = bslib::bs_theme(
        version = 5,
        primary = "#9D2235",
        secondary = "#6F6A68",
        success = "#9D2235",
        bg = "#FFFFFF",
        fg = "#2F2628"
      ),

        shiny::tags$head(
        shiny::tags$style(shiny::HTML("
          body { background: #ffffff; color: #2f2628; font-size: 14px; line-height: 1.4; }
          .container-fluid { --bs-gutter-x: 0.75rem; --bs-gutter-y: 0.75rem; }
          .card { border: 1px solid #d8d8d8; background: #ffffff; box-shadow: 0 8px 18px rgba(47, 38, 40, 0.04); }
          .card-header { background: #f7f7f7; border-bottom: 1px solid #d8d8d8; font-weight: 700; padding: 0.65rem 0.9rem; }
          .card-body { padding: 0.9rem; }
          .nav-tabs { border-bottom-color: #d8d8d8; }
          .nav-tabs .nav-link { color: #5e5053; padding: 0.7rem 0.95rem; font-size: 0.96rem; }
          .nav-tabs .nav-link:hover { color: #7f293a; border-color: transparent; }
          .nav-tabs .nav-link.active { color: #9d2235; background: #ffffff; border-color: #d8d8d8 #d8d8d8 #ffffff; font-weight: 600; }
          .thumbnail-gallery { display: flex; flex-wrap: wrap; gap: 8px; margin: 10px 0; }
          .thumbnail-item { position: relative; border: 2px solid #dee2e6; border-radius: 4px;
                           overflow: hidden; cursor: pointer; transition: all 0.2s; }
          .thumbnail-item:hover { border-color: #9d2235; box-shadow: 0 6px 18px rgba(47, 38, 40, 0.10); }
          .thumbnail-item.selected { border-color: #9d2235; border-width: 3px; }
          .thumbnail-img { width: 88px; height: 88px; object-fit: cover; display: block; }
          .thumbnail-label { position: absolute; bottom: 0; left: 0; right: 0;
                            background: rgba(0,0,0,0.7); color: white; padding: 2px 5px;
                            font-size: 10px; text-align: center; }
          .metric-card { border-left: 4px solid #9d2235; margin-bottom: 15px; }
          .info-banner { background: #ffffff; color: #2f2628; padding: 14px 18px; border-radius: 8px;
                        margin-bottom: 12px; border: 1px solid #d8d8d8; border-top: 4px solid #9d2235;
                        box-shadow: 0 8px 18px rgba(47, 38, 40, 0.04); }
          .info-banner h2 { color: #7d1d2c; letter-spacing: 0.01em; font-size: 1.6rem; line-height: 1.2; }
          .info-banner p { color: #5e5053; }
          .comparison-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
                            gap: 14px; margin-top: 14px; }
          .comparison-item { border: 1px solid #d8d8d8; border-radius: 8px; padding: 10px; background: #ffffff; }
          .comparison-item img { width: 100%; max-height: 300px; object-fit: contain;
                                 border-radius: 4px; }
          .results-table-wrapper .table {
            border-color: #d8d8d8;
            --bs-table-bg: #ffffff;
            --bs-table-striped-bg: #f3f3f3;
            --bs-table-striped-color: #2f2628;
            --bs-table-hover-bg: #f7f7f7;
            --bs-table-hover-color: #2f2628;
            --bs-table-color: #2f2628;
          }
          .results-table-wrapper .table > thead > tr > th { background: #f7f7f7; border-bottom: 1px solid #d8d8d8; }
          .results-table-wrapper table tbody tr.selected-row > td {
            background-color: #f3e3e6 !important;
            box-shadow: inset 0 0 0 9999px #f3e3e6 !important;
            color: #2f2628 !important;
          }
          .results-table-wrapper table tbody tr.selected-row:hover > td {
            background-color: #f3e3e6 !important;
            box-shadow: inset 0 0 0 9999px #f3e3e6 !important;
          }
          .results-table-wrapper table tbody tr.selected-row > td:first-child {
            box-shadow: inset 4px 0 0 #9d2235, inset 0 0 0 9999px #f3e3e6 !important;
          }
          .results-table-wrapper .thumbnail-col { width: 36px; text-align: center; }
          .results-table-wrapper .thumbnail-col img { width: 32px; height: 32px; object-fit: cover;
                                                       border-radius: 3px; border: 1px solid #d8d8d8; }
          .select-all-section {
            display: inline-block;
            width: auto;
            margin-left: -0.65rem;
            margin-bottom: -0.5rem;
            margin-top: -0.5rem;
            padding: 0.5rem 0.7rem 0rem 0.65rem;
            background: #faf7f8;
            box-shadow: inset 3px 0 0 #9d2235;
            border-radius: 4px;
          }
          .select-all-section .shiny-input-container { margin-bottom: 0 !important; }
          .select-all-section .form-check {
            margin: 0;
            padding: 0 0 0 1.5em;
            min-height: auto;
            line-height: 1;
          }
          .select-all-section .form-check-input {
            float: left;
            margin-left: -1.5em;
            margin-top: 0.15em;
          }
          .select-all-section .form-check-label { margin-bottom: 0; font-weight: 500; }
          .btn-primary { background-color: #9d2235; border-color: #9d2235; }
          .btn-primary:hover, .btn-primary:focus { background-color: #7d1d2c; border-color: #7d1d2c; }
          .btn-outline-secondary { color: #5f5a58; border-color: #b9b9b9; background: #ffffff; }
          .btn-outline-secondary:hover, .btn-outline-secondary:focus { background-color: #f3f3f3; color: #4f4043; border-color: #989898; }
          .btn { padding: 0.42rem 0.75rem; font-size: 0.95rem; }
          hr { margin: 0.9rem 0; }
          h4, .h4 { font-size: 1.2rem; margin-bottom: 0.65rem; font-weight: 700; }
          h5, .h5 { font-size: 1.05rem; margin-bottom: 0.55rem; font-weight: 700; }
          h6, .h6 { font-size: 0.95rem; margin-bottom: 0.45rem; font-weight: 700; }
          p { margin-bottom: 0.6rem; }
          .form-check, .checkbox { margin-bottom: 0.35rem; }
          .alert { border: 1px solid #d8d8d8; color: #4f4043; background: #ffffff; }
          .alert-info { background: #f7f7f7; border-left: 4px solid #b9b9b9; }
          .alert-success { background: #9d2235; border: 1px solid #9d2235; color: #ffffff; font-weight: 600; }
          .alert-warning { background: #f7f7f7; border-left: 4px solid #8d6a33; }
        "))
      ),

      shiny::div(
        class = "info-banner",
        shiny::h2("imagefluency", style = "margin: 0 0 6px 0;"),
        shiny::p("Quantify visual processing fluency through computational aesthetic metrics",
          style = "margin: 0;")
      ),

      bslib::navset_card_tab(
        id = "main_tabs",

        # Upload Tab
        bslib::nav_panel(
          "Upload & Calculate",
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              width = 280,
              shiny::h5("Upload images"),
              shiny::fileInput(
                "file",
                label = NULL,
                multiple = TRUE,
                accept = c("image/png", "image/jpeg", "image/jpg", "image/bmp", "image/tiff"),
                buttonLabel = "Browse...",
                placeholder = "Select images"
              ),
              shiny::tags$small(
                class = "text-muted",
                style = "margin-top: -1.5rem; margin-left:0.15rem;",
                "PNG, JPEG, BMP, TIFF supported"
              ),
              shiny::hr(),

              shiny::h5("Select metrics"),

              shiny::div(
                class = "select-all-section",
                shiny::checkboxInput(
                  "select_all_metrics",
                  "Select all metrics",
                  value = FALSE
                )
              ),

              shiny::checkboxGroupInput(
                "statistic",
                label = NULL,
                choices = c(
                  "Contrast" = "contr",
                  "Self-Similarity" = "selfsim",
                  "Simplicity" = "simpl",
                  "Symmetry" = "sym",
                  "Typicality (requires 2+ images)" = "typ"
                ),
                selected = character(0)
              ),

              shiny::hr(),

              shiny::actionButton(
                "calcfluency",
                "Calculate metrics",
                class = "btn-primary w-100",
                icon = shiny::icon("calculator")
              ),

              shiny::tags$small(
                class = "text-muted mt-0 d-block text-center",
                style = "margin-top: -0.75rem !important; line-height: 1.2;",
                "Processing may take a moment ..."
              )
            ),

            bslib::card(
              bslib::card_header("Image gallery"),
              bslib::card_body(
                shiny::uiOutput("thumbnail_gallery"),
                shiny::hr(),
                shiny::uiOutput("selected_image_display")
              )
            ),

            bslib::card(
              bslib::card_header(
                shiny::div(
                  class = "d-flex justify-content-between align-items-center gap-2",
                  shiny::span("Analysis results"),
                  shiny::downloadButton(
                    "download_results",
                    "Download csv",
                    class = "btn-sm btn-outline-secondary"
                  )
                )
              ),
              bslib::card_body(
                shiny::uiOutput("results_info"),
                shiny::div(
                  class = "results-table-wrapper",
                  shiny::uiOutput("results_table_ui")
                )
              )
            )
          )
        ),

        # Compare Tab
        bslib::nav_panel(
          "Compare",
          bslib::card(
            bslib::card_header("Side-by-side comparison"),
            bslib::card_body(
              shiny::p("Select images from the Upload tab to compare them here."),
              shiny::uiOutput("comparison_selector"),
              shiny::hr(),
              shiny::uiOutput("comparison_display")
            )
          )
        ),

        # Documentation Tab
        bslib::nav_panel(
          "About",
          bslib::card(
            bslib::card_header("About imagefluency"),
            bslib::card_body(
              documentation_intro,

              shiny::h5("Metrics explained", class = "mt-4"),

              lapply(c("contrast", "selfsim", "simpl", "sym", "typ"), function(key) {
                info <- metric_info[[key]]
                bslib::card(
                  class = "metric-card",
                  bslib::card_body(
                    shiny::h6(info$name),
                    shiny::p(info$description),
                    shiny::tags$small(class = "text-muted", paste("Method:", info$reference))
                  )
                )
              }),

              shiny::h5("References", class = "mt-4"),
              documentation_references,

              shiny::h5("FAQ", class = "mt-4"),
              documentation_faq,

              shiny::hr(),

              shiny::p(
                class = "text-center text-muted",
                shiny::tags$small(
                  "© 2019-2026 Stefan Mayer (University of Tübingen)"
                )
              )
            )
          )
        )
      )
    )
  } else {
    # Fallback: Enhanced UI without bslib
    shiny::fluidPage(
      title = "imagefluency",

      shiny::tags$head(
        shiny::tags$style(shiny::HTML("
          body { background: #ffffff; color: #2f2628; font-size: 20px; line-height: 1.5; }
          .container-fluid { padding-left: 6px; padding-right: 6px; }
          .well { background: #ffffff; border: 1px solid #d8d8d8; box-shadow: 0 8px 18px rgba(47, 38, 40, 0.04); padding: 14px; margin-bottom: 12px; }
          .nav-tabs { border-bottom-color: #d8d8d8; }
          .nav-tabs > li > a { color: #5e5053; padding: 12px 16px; font-size: 1.42rem !important; }
          .nav-tabs > li > a:hover { color: #7f293a; background: transparent; border-color: transparent; }
          .nav-tabs > li.active > a,
          .nav-tabs > li.active > a:hover,
          .nav-tabs > li.active > a:focus { color: #9d2235; background: #ffffff; border: 1px solid #d8d8d8; border-bottom-color: transparent; font-weight: 600; }
          .header-banner { background: #ffffff; color: #2f2628; padding: 14px 18px; border-radius: 8px;
                          margin-bottom: 12px; border: 1px solid #d8d8d8; border-top: 4px solid #9d2235;
                          box-shadow: 0 8px 18px rgba(47, 38, 40, 0.04); }
          .header-banner h2 { color: #7d1d2c; margin-top: 0; font-size: 1.6rem; line-height: 1.2; }
          .header-banner p { color: #5e5053; margin-bottom: 0; }
          .thumbnail-gallery { display: flex; flex-wrap: wrap; gap: 8px; margin: 10px 0; }
          .thumbnail-item { position: relative; border: 2px solid #ddd; border-radius: 4px;
                           overflow: hidden; cursor: pointer; }
          .thumbnail-item:hover { border-color: #9d2235; }
          .thumbnail-item.selected { border-color: #9d2235; border-width: 3px; }
          .thumbnail-img { width: 88px; height: 88px; object-fit: cover; }
          .thumbnail-label { position: absolute; bottom: 0; left: 0; right: 0;
                            background: rgba(0,0,0,0.7); color: white; padding: 2px;
                            font-size: 10px; text-align: center; }
          .results-table-wrapper .table { border-color: #d8d8d8; }
          .results-table-wrapper .table > thead > tr > th { background: #f7f7f7; border-bottom: 1px solid #d8d8d8; }
          .results-table-wrapper .table-striped > tbody > tr:nth-of-type(odd) > td,
          .results-table-wrapper .table-striped > tbody > tr:nth-of-type(odd) > th {
            background-color: #f3f3f3;
          }
          .results-table-wrapper .table-striped > tbody > tr:nth-of-type(even) > td,
          .results-table-wrapper .table-striped > tbody > tr:nth-of-type(even) > th {
            background-color: #ffffff;
          }
          .results-table-wrapper table tbody tr.selected-row td {
            background-color: #f3e3e6 !important;
            color: #2f2628 !important;
          }
          .results-table-wrapper table tbody tr.selected-row:hover td {
            background-color: #f3e3e6 !important;
          }
          .results-table-wrapper table tbody tr.selected-row td:first-child {
            box-shadow: inset 4px 0 0 #9d2235 !important;
          }
          .results-table-wrapper .thumbnail-col { width: 36px; text-align: center; }
          .results-table-wrapper .thumbnail-col img { width: 32px; height: 32px; object-fit: cover;
                                                       border-radius: 3px; border: 1px solid #d8d8d8; }
          .select-all-section {
            display: block;
            width: auto;
            margin-left: -1.5rem;
            margin-top: 1.5rem;
            margin-bottom: 2.25rem;
            padding: 0.5rem 0rem 0rem 0.65rem;
            background: transparent;
            box-shadow: none;
            border-radius: 0;
          }
          .select-all-section .shiny-input-container,
          .select-all-section .form-group { margin-bottom: 0 !important; }
          .select-all-section .checkbox {
            margin: 0;
            padding: 0;
            display: block;
          }
          .select-all-section .checkbox input {
            position: absolute;
            margin-left: -20px;
            margin-top: 0.15rem;
          }
          .select-all-section .checkbox label {
            display: inline-block;
            margin: 0;
            padding: 0.32rem 0.7rem 0.32rem 29px;
            background: #faf7f8;
            border-radius: 4px;
            font-weight: 500;
            line-height: 1;
          }
          .btn-primary { background-color: #9d2235; border-color: #9d2235; }
          .btn-primary:hover, .btn-primary:focus { background-color: #7d1d2c; border-color: #7d1d2c; }
          .btn-default:hover, .btn-default:focus { background-color: #f3f3f3; border-color: #b9b9b9; }
          .btn { padding: 7px 13px; font-size: 1.18rem !important; }
          hr { margin: 0.9rem 0; }
          h4, .h4 { font-size: 1.6rem; margin-bottom: 0.65rem; font-weight: 700; }
          h5, .h5 { font-size: 1.34rem; margin-bottom: 0.55rem; font-weight: 700; }
          h6, .h6 { font-size: 1.16rem; margin-bottom: 0.45rem; font-weight: 700; }
          p { margin-bottom: 0.6rem; }
          .checkbox { margin-bottom: 0.4rem; font-size: 1.18rem; }
          .form-control, .help-block, .text-muted, .btn-sm, .table, label, .control-label,
          .tab-content, .well, .nav, .nav-tabs, .checkbox label, .radio label {
            font-size: 1.18rem !important;
          }
          .shiny-input-container { font-size: 1.18rem !important; width: 100%; }
          .form-control, input, button, select, textarea { font-size: 1.18rem !important; }
          .checkbox input[type='checkbox'] { transform: scale(1.15); }
          .alert { border: 1px solid #d8d8d8; color: #4f4043; background: #ffffff; }
          .alert-info { background: #f7f7f7; border-left: 4px solid #b9b9b9; }
          .alert-success { background: #9d2235; border: 1px solid #9d2235; color: #ffffff; font-weight: 600; }
          .alert-warning { background: #f7f7f7; border-left: 4px solid #8d6a33; }
        "))
      ),

      shiny::div(
        class = "header-banner",
        shiny::h2("imagefluency", style = "margin-bottom: 6px;"),
        shiny::p("Quantify visual processing fluency through computational aesthetics")
      ),

      shiny::tabsetPanel(
        id = "main_tabs",

        shiny::tabPanel(
          "Upload & Calculate",
          shiny::fluidRow(
            shiny::column(
              width = 3,
              shiny::wellPanel(
                shiny::h4("Upload images"),
                shiny::fileInput(
                  "file",
                  label = NULL,
                  multiple = TRUE,
                  accept = c("image/png", "image/jpeg", "image/jpg", "image/bmp", "image/tiff"),
                  buttonLabel = "Browse...",
                  placeholder = "Select images"
                ),

                shiny::h4("Select metrics"),

                shiny::div(
                  class = "select-all-section",
                  shiny::checkboxInput(
                    "select_all_metrics",
                    "Select all metrics",
                    value = FALSE
                  )
                ),

                shiny::checkboxGroupInput(
                  "statistic",
                  label = NULL,
                  choices = c(
                    "Contrast" = "contr",
                    "Self-Similarity" = "selfsim",
                    "Simplicity" = "simpl",
                    "Symmetry" = "sym",
                    "Typicality (requires 2+ images)" = "typ"
                  ),
                  selected = character(0)
                ),

                shiny::hr(),

                shiny::actionButton(
                  "calcfluency",
                  "Calculate metrics",
                  class = "btn btn-primary btn-block",
                  icon = shiny::icon("calculator")
                ),
                shiny::tags$small(
                  class = "text-muted",
                  style = "display: block; text-align: center; margin-top: 0.5rem; line-height: 1.2;",
                  "Processing may take a moment ..."
                )
              )
            ),

            shiny::column(
              width = 9,
              shiny::wellPanel(
                shiny::h4("Images"),
                shiny::uiOutput("thumbnail_gallery"),
                shiny::hr(),
                shiny::uiOutput("selected_image_display")
              ),

              shiny::wellPanel(
                shiny::fluidRow(
                  shiny::column(
                    width = 10,
                    shiny::h4("Analysis results")
                  ),
                  shiny::column(
                    width = 2,
                    shiny::div(style = "margin-left: 8px;",
                      shiny::downloadButton("download_results", "Download csv", class = "btn-sm pull-right")
                    )
                  )
                ),
                shiny::uiOutput("results_info"),
                shiny::div(
                  class = "results-table-wrapper",
                  shiny::uiOutput("results_table_ui")
                )
              )
            )
          )
        ),

        shiny::tabPanel(
          "Compare",
          shiny::wellPanel(
            shiny::h4("Side-by-side comparison"),
            shiny::p("Select images to compare."),
            shiny::uiOutput("comparison_selector"),
            shiny::hr(),
            shiny::uiOutput("comparison_display")
          )
        ),

        shiny::tabPanel(
          "About",
          shiny::wellPanel(
            shiny::h4("About imagefluency"),
            documentation_intro,

            shiny::h5("Metrics explained"),
            lapply(c("contrast", "selfsim", "simpl", "sym", "typ"), function(key) {
              info <- metric_info[[key]]
              shiny::div(
                style = "margin-bottom: 12px; padding-left: 10px; border-left: 4px solid #9d2235;",
                shiny::h6(info$name),
                shiny::p(info$description),
                shiny::tags$small(class = "text-muted", paste("Method:", info$reference))
              )
            }),

            shiny::h5("References"),
            documentation_references,

            shiny::h5("FAQ"),
            documentation_faq,

            shiny::hr(),

            shiny::p(
              class = "text-muted text-center",
              "© 2019-2026 Stefan Mayer"
            )
          )
        )
      )
    )
  },

  # Server
  server = function(input, output, session) {

    # Reactive values
    rv <- shiny::reactiveValues(
      image_paths = NULL,
      image_names = NULL,
      results = NULL,
      selected_image = 1,
      processing = FALSE
    )

    # Handle Select All checkbox
    shiny::observeEvent(input$select_all_metrics, {
      if (input$select_all_metrics) {
        all_choices <- c("contr", "selfsim", "simpl", "sym", "typ")
        shiny::updateCheckboxGroupInput(session, "statistic", selected = all_choices)
      } else {
        shiny::updateCheckboxGroupInput(session, "statistic", selected = character(0))
      }
    })

    # Sync individual checkboxes with Select All
    shiny::observe({
      shiny::req(input$statistic)  # Wait for input to be available
      all_choices <- c("contr", "selfsim", "simpl", "sym", "typ")

      all_selected <- length(input$statistic) == length(all_choices) &&
                      all(all_choices %in% input$statistic)

      # Only update if there's a mismatch to avoid loops
      if (all_selected && !isTRUE(input$select_all_metrics)) {
        shiny::updateCheckboxInput(session, "select_all_metrics", value = TRUE)
      } else if (!all_selected && isTRUE(input$select_all_metrics)) {
        shiny::updateCheckboxInput(session, "select_all_metrics", value = FALSE)
      }
    })

    # Handle file upload
    shiny::observeEvent(input$file, {
      shiny::req(input$file)

      n_files <- min(nrow(input$file), MAX_IMAGES)

      if (nrow(input$file) > MAX_IMAGES) {
        shiny::showNotification(
          sprintf("Limited to %d images. Processing first %d.", MAX_IMAGES, MAX_IMAGES),
          type = "warning"
        )
      } else if (nrow(input$file) > 10) {
        shiny::showNotification(
          sprintf("Processing %d images may take considerable time.", nrow(input$file)),
          type = "message",
          duration = 5
        )
      }

      rv$image_paths <- input$file$datapath[1:n_files]
      rv$image_names <- input$file$name[1:n_files]
      rv$selected_image <- 1
      rv$results <- NULL

      shiny::showNotification(
        sprintf("Loaded %d image%s", n_files, if(n_files > 1) "s" else ""),
        type = "default"
      )
    })

    # Load demo image if none uploaded
    shiny::observe({
      if (is.null(input$file)) {
        rv$image_paths <- defaultimage
        rv$image_names <- "rails.jpg (demo)"
        rv$selected_image <- 1
      }
    })

    # Thumbnail gallery
    output$thumbnail_gallery <- shiny::renderUI({
      shiny::req(rv$image_paths)

      if (length(rv$image_paths) == 1) {
        return(shiny::p(class = "text-muted", "Upload multiple images to see gallery."))
      }

      thumbnails <- lapply(seq_along(rv$image_paths), function(i) {
        shiny::div(
          class = paste("thumbnail-item", if(i == rv$selected_image) "selected" else ""),
          onclick = sprintf("Shiny.setInputValue('select_image', %d, {priority: 'event'})", i),
          shiny::tags$img(
            src = session$fileUrl(basename(rv$image_paths[i]), rv$image_paths[i]),
            class = "thumbnail-img"
          ),
          shiny::div(class = "thumbnail-label", rv$image_names[i])
        )
      })

      shiny::div(class = "thumbnail-gallery", thumbnails)
    })

    # Handle thumbnail click
    shiny::observeEvent(input$select_image, {
      rv$selected_image <- input$select_image
    })

    # Display selected image with metrics
    output$selected_image_display <- shiny::renderUI({
      shiny::req(rv$image_paths, rv$selected_image)

      idx <- rv$selected_image

      # Get metrics for selected image if available
      metrics_display <- if (!is.null(rv$results) && idx <= nrow(rv$results)) {
        result_row <- rv$results[idx, ]
        metric_items <- lapply(names(result_row)[-1], function(col_name) {  # Skip 'Image' column
          value <- result_row[[col_name]]
          formatted_value <- if (is.numeric(value)) {
            if (is.na(value)) "Error" else sprintf("%.3f", value)
          } else {
            as.character(value)
          }
          shiny::tags$li(shiny::tags$strong(paste0(col_name, ": ")), formatted_value)
        })
        shiny::div(
          # shiny::h6("Metrics:"),
          shiny::tags$ul(class = "list-unstyled", metric_items)
        )
      } else {
        shiny::p(class = "text-muted", "Calculate metrics to see results here.")
      }

        shiny::div(
          style = "display: flex; gap: 20px; align-items: flex-start;",
          shiny::div(
            style = "flex-shrink: 0;",
          shiny::h6(paste("Selected:", rv$image_names[idx])),
            shiny::img(
              src = session$fileUrl(basename(rv$image_paths[idx]), rv$image_paths[idx]),
            style = "width: 180px; height: 180px; object-fit: cover;
                     border: 2px solid #9d2235; border-radius: 4px; padding: 5px;"
          )
        ),
        shiny::div(
          style = "flex-grow: 1; padding-top: 30px;",
          metrics_display
        )
      )
    })

    # Calculate metrics
    shiny::observeEvent(input$calcfluency, {
      shiny::req(rv$image_paths)

      if (is.null(input$statistic) || length(input$statistic) == 0) {
        shiny::showNotification("Please select at least one metric", type = "warning")
        return()
      }

      rv$processing <- TRUE

      # Progress
      progress <- shiny::Progress$new(session)
      on.exit(progress$close())
      progress$set(message = "Processing images...", value = 0)

      n_images <- length(rv$image_paths)
      n_metrics <- length(input$statistic)
      total_steps <- n_images * n_metrics
      current_step <- 0

      # Process images
      results_list <- list()

      for (i in seq_along(rv$image_paths)) {
        img_path <- rv$image_paths[i]
        img_name <- rv$image_names[i]

        progress$set(
          message = sprintf("Processing %s", img_name),
          detail = sprintf("Image %d of %d", i, n_images),
          value = current_step / total_steps
        )

        img <- tryCatch(
          img_read(img_path),
          error = function(e) {
            shiny::showNotification(
              sprintf("Error reading %s", img_name),
              type = "error"
            )
            return(NULL)
          }
        )

        if (is.null(img)) next

        result_row <- data.frame(Image = img_name, stringsAsFactors = FALSE)

        for (metric in input$statistic) {
          # Skip typicality for individual images (calculated separately)
          if (metric == "typ") next

          current_step <- current_step + 1
          progress$inc(1/total_steps)

          value <- tryCatch({
            switch(metric,
              "contr" = img_contrast(img),
              "selfsim" = as.numeric(img_self_similarity(img)),
              "simpl" = img_simplicity(img),
              "sym" = img_symmetry(img, horizontal = FALSE),
              NA
            )
          }, error = function(e) NA)

          col_name <- switch(metric,
            "contr" = "Contrast",
            "selfsim" = "Self_Similarity",
            "simpl" = "Simplicity",
            "sym" = "Symmetry"
          )

          result_row[[col_name]] <- value
        }

        results_list[[i]] <- result_row
      }

      # Calculate typicality if selected and we have 2+ images
      if ("typ" %in% input$statistic) {
        if (n_images >= 2) {
          progress$set(message = "Calculating typicality...", value = 0.95)

          # Read all images into list
          img_list <- lapply(rv$image_paths, function(path) {
            tryCatch(img_read(path), error = function(e) NULL)
          })

          # Remove any NULL images
          img_list <- img_list[!sapply(img_list, is.null)]

          if (length(img_list) >= 2) {
            typ_scores <- tryCatch({
              img_typicality(img_list)
            }, error = function(e) {
              shiny::showNotification(
                paste("Error calculating typicality:", e$message),
                type = "error"
              )
              return(NULL)
            })

            if (!is.null(typ_scores)) {
              # Add typicality scores to results
              for (i in seq_along(results_list)) {
                results_list[[i]]$Typicality <- as.numeric(typ_scores[i])
              }
            }
          }
        } else {
          shiny::showNotification(
            "Typicality requires at least 2 images. Skipping.",
            type = "warning"
          )
          # Add NA column for typicality
          for (i in seq_along(results_list)) {
            results_list[[i]]$Typicality <- NA
          }
        }
      }

      rv$results <- do.call(rbind, results_list)
      rv$processing <- FALSE

      progress$set(value = 1, message = "Complete!")

      shiny::showNotification(
        sprintf("Processed %d image%s", n_images, if(n_images > 1) "s" else ""),
        type = "default"
      )

      # Results are now displayed in the same tab, no need to switch
    })

    # Results info
    output$results_info <- shiny::renderUI({
      if (is.null(rv$results)) {
        return(shiny::div(
          class = "alert alert-info",
          "Upload images and calculate metrics to see results."
        ))
      }

      shiny::div(
        class = "alert alert-success",
        style = "padding: 10px 14px;",
        sprintf("Results for %d image%s",
                nrow(rv$results),
                if(nrow(rv$results) > 1) "s" else "")
      )
    })

    # Results table with thumbnails and row highlighting
    output$results_table_ui <- shiny::renderUI({
      shiny::req(rv$results, rv$image_paths)

      # Explicitly depend on selected_image to trigger re-render
      selected_idx <- rv$selected_image

      df <- rv$results

      # Build table HTML
      table_html <- shiny::tags$table(
        class = "table table-striped table-hover table-bordered",
        shiny::tags$thead(
          shiny::tags$tr(
            shiny::tags$th(class = "thumbnail-col", ""),  # Thumbnail column
            lapply(names(df), function(col) shiny::tags$th(col))
          )
        ),
        shiny::tags$tbody(
          lapply(seq_len(nrow(df)), function(i) {
            # Determine if this row is selected
            is_selected <- i == selected_idx
            row_class <- if (is_selected) "selected-row" else ""
            # Add inline style as fallback
            row_style <- if (is_selected) "border-left: 4px solid #9d2235 !important;" else ""

            # Create thumbnail
            thumbnail <- shiny::tags$td(
              class = "thumbnail-col",
              shiny::tags$img(
                src = session$fileUrl(basename(rv$image_paths[i]), rv$image_paths[i])
              )
            )

            # Create data cells
            data_cells <- lapply(seq_along(df[i, ]), function(j) {
              value <- df[i, j]
              formatted_value <- if (is.numeric(value)) {
                if (is.na(value)) "Error" else sprintf("%.3f", value)
              } else {
                as.character(value)
              }
              shiny::tags$td(formatted_value)
            })

            shiny::tags$tr(
              class = row_class,
              style = row_style,
              onclick = sprintf("Shiny.setInputValue('select_image', %d, {priority: 'event'})", i),
              thumbnail,
              data_cells
            )
          })
        )
      )

      table_html
    })

    # Download results
    output$download_results <- shiny::downloadHandler(
      filename = function() {
        paste0("imagefluency_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        shiny::req(rv$results)
        write.csv(rv$results, file, row.names = FALSE)
      }
    )

    # Comparison selector
    output$comparison_selector <- shiny::renderUI({
      shiny::req(rv$results)

      if (nrow(rv$results) < 2) {
        return(shiny::div(
          class = "alert alert-warning",
          "Need at least 2 processed images for comparison."
        ))
      }

      shiny::selectInput(
        "compare_images",
        "Select images to compare:",
        choices = rv$results$Image,
        selected = rv$results$Image[1:min(2, nrow(rv$results))],
        multiple = TRUE
      )
    })

    # Comparison display
    output$comparison_display <- shiny::renderUI({
      shiny::req(rv$results, input$compare_images)

      if (length(input$compare_images) < 2) {
        return(shiny::p("Select at least 2 images."))
      }

      compare_data <- rv$results[rv$results$Image %in% input$compare_images, ]
      img_indices <- which(rv$image_names %in% input$compare_images)

      comparison_items <- lapply(seq_len(nrow(compare_data)), function(i) {
        img_idx <- img_indices[i]

        metrics_html <- shiny::tags$ul(
          if ("Contrast" %in% names(compare_data))
            shiny::tags$li(shiny::tags$strong("Contrast: "),
                    sprintf("%.3f", compare_data$Contrast[i])),
          if ("Self_Similarity" %in% names(compare_data))
            shiny::tags$li(shiny::tags$strong("Self-Similarity: "),
                    sprintf("%.3f", compare_data$Self_Similarity[i])),
          if ("Simplicity" %in% names(compare_data))
            shiny::tags$li(shiny::tags$strong("Simplicity: "),
                    sprintf("%.3f", compare_data$Simplicity[i])),
          if ("Symmetry" %in% names(compare_data))
            shiny::tags$li(shiny::tags$strong("Symmetry: "),
                    sprintf("%.3f", compare_data$Symmetry[i]))
        )

        shiny::div(
          class = "comparison-item",
          style = "display: inline-block; width: 300px; margin: 10px;
                   border: 1px solid #d8d8d8; padding: 10px; border-radius: 4px;
                   background: #ffffff;",
          shiny::h6(compare_data$Image[i]),
          shiny::img(
            src = session$fileUrl(basename(rv$image_paths[img_idx]), rv$image_paths[img_idx]),
            style = "width: 100%; max-height: 250px; object-fit: contain;"
          ),
          shiny::hr(),
          metrics_html
        )
      })

      shiny::div(comparison_items)
    })
  }
)
