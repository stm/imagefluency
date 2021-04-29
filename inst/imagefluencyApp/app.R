#' @include utils.R complexity.R contrast.R self-similarity.R simplicity.R symmetry.R typicality.R
NULL

# default (demo) image
defaultimage <- system.file("imagefluencyApp", "www", "rails.jpg", package = "imagefluency")

shiny::shinyApp(

  # Define UI
  ui = shiny::fluidPage(title="Image fluency",

                  # Application title
                  shiny::h4("Image fluency"),

                  # Sidebar with a slider input for number of bins
                  shiny::sidebarLayout(
                    shiny::sidebarPanel(
                      # # see https://stackoverflow.com/a/22475216
                      # tags$head(tags$style(type="text/css", "
                      #                      #loadmessage {
                      #                      position: fixed;
                      #                      top: 0px;
                      #                      left: 0px;
                      #                      width: 100%;
                      #                      padding: 5px 0px 5px 0px;
                      #                      text-align: center;
                      #                      font-weight: bold;
                      #                      font-size: 100%;
                      #                      color: #000000;
                      #                      background-color: #CCFF66;
                      #                      z-index: 105;
                      #                      }
                      #                      ")),
                      #
                      # conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                      #                  tags$div("Calculating ...",id="loadmessage")),
                      #
                      # # see https://stackoverflow.com/a/17338810
                      # HTML('<script type="text/javascript">
                      #   $(document).ready(function() {
                      #      $("#calcfluency").click(function() {
                      #      $("#imgfluency").text("Calculating ...");
                      #      });
                      #      });
                      #      </script>
                      #      '),

                      # # see https://github.com/rstudio/shiny/issues/609#issuecomment-58205655
                      # tags$head(tags$style(HTML("
                      #   .progress-striped .bar {
                      #     background-color: #149bdf;
                      #     background-image: -webkit-gradient(linear, 0 100%, 100% 0, color-stop(0.25, rgba(255, 255, 255, 0.6)), color-stop(0.25, transparent), color-stop(0.5, transparent), color-stop(0.5, rgba(255, 255, 255, 0.6)), color-stop(0.75, rgba(255, 255, 255, 0.6)), color-stop(0.75, transparent), to(transparent));
                      #     background-image: -webkit-linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
                      #     background-image: -moz-linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
                      #     background-image: -o-linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
                      #     background-image: linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
                      #     -webkit-background-size: 40px 40px;
                      #        -moz-background-size: 40px 40px;
                      #          -o-background-size: 40px 40px;
                      #             background-size: 40px 40px;
                      #   }
                      # "))),

                      shiny::fileInput(inputId = "file",
                                label = "Choose own image",
                                multiple = FALSE,
                                accept = c("image/png", "image/jpeg", "image/jpg", "image/bmp", "image/tiff")

                      ),
                      shiny::checkboxGroupInput(inputId = "statistic",
                                         label = "Fluency statistic",
                                         choices = c("Contrast" = "contr",
                                                     "Self-similarity" = "selfsim",
                                                     "Simplicity" = "simpl",
                                                     "Symmetry" = "sym"
                                         )

                      ),
                      shiny::tags$small(shiny::span("(Typicality not implemented yet)", style="color:gray")),
                      shiny::hr(),
                      shiny::actionButton(inputId = "calcfluency", label = "Calculate"),
                      shiny::br(),
                      shiny::tags$small("(might take a while)")
                    ),

                    # Show a plot of the generated distribution
                    shiny::mainPanel(
                      # tags$style(type="text/css", "img{display: block; margin-left: auto; margin-right: auto;}"),
                      shiny::tags$style(type="text/css", "img{max-width: 100%;}"),
                      shiny::tableOutput(outputId = "imgfluency"),
                      shiny::tags$small(shiny::span(shiny::textOutput(outputId = "demo"), style="color:gray")),
                      imageOutput(outputId = "img")
                    )
                  ),
                  shiny::hr(),
                  shiny::div(style="text-align:center; color:gray", shiny::tags$small("(c) 2019 Stefan Mayer (University of Tübingen)"))
  ),

  server = function(input, output, session) {

    # get image
    imagepath <- shiny::reactive({
      ## require that something was uploaded
      # req(input$file)

      # use default image
      if(is.null(input$file)) return(defaultimage)

      # correct for differences between different OS
      gsub("\\\\", "/", input$file$datapath)
    })

    # displayimage
    output$img <- shiny::renderImage({
      list(src = imagepath(), width = paste0(input$width,"%"))
    },deleteFile = FALSE)

    # inform about demo image
    output$demo <- shiny::renderText({
      if(is.null(input$file)) "(Demo image)"
    })


    calc <- shiny::reactiveValues(contr = FALSE, selfsim = FALSE, simpl = FALSE, sym = FALSE)
    flu <- shiny::reactiveValues(contr = NA, selfsim = NA, simpl = NA, sym = NA)

    shiny::observeEvent(input$calcfluency,{
      # read image
      img <- img_read(imagepath())

      ## calculation needs image file to be uploaded
      # req(input$file)

      # calculate values only if checked and was previously not checked
      #
      # inspired by https://stackoverflow.com/a/41548353
      #
      # is contrast checked?
      is.contr <- "contr" %in% input$statistic
      # look whether checked status has changed, if so store in reactive value
      if(calc$contr != is.contr) {
        calc$contr <- is.contr
        # do calculation only if turned on
        if ("contr" %in% input$statistic) {
          # print("calculating contrast")
          shiny::showModal(shiny::modalDialog("Calculating contrast", footer=NULL))
          i_contr <- tryCatch(img_contrast(img), error = function(err) err)
          if (inherits(i_contr, "error")) {
            flu$contr <- "computation not possible"
          } else {
            flu$contr <- sprintf("%.3f", i_contr)
          }
          shiny::removeModal()
        } else {flu$contr <- NA}
      }

      is.selfsim <- "selfsim" %in% input$statistic
      if(calc$selfsim != is.selfsim) {
        calc$selfsim <- is.selfsim
        if ("selfsim" %in% input$statistic) {
          # print("calculating selfsim")
          shiny::showModal(shiny::modalDialog("Calculating self-similarity", footer=NULL))
          i_selfsim <- tryCatch(suppressWarnings(as.numeric(img_self_similarity(img))), error = function(err) err)
          if (inherits(i_selfsim, "error")) {
            flu$selfsim <- "computation not possible"
          } else {
            flu$selfsim <- sprintf("%.3f", i_selfsim)
          }
          shiny::removeModal()
        } else {flu$selfsim <- NA}
      }

      is.simpl <- "simpl" %in% input$statistic
      if(calc$simpl != is.simpl) {
        calc$simpl <- is.simpl
        if ("simpl" %in% input$statistic) {
          # print("calculating simplicity")
          shiny::showModal(shiny::modalDialog("Calculating simplicity", footer=NULL))
          i_simpl <- tryCatch(img_simplicity(img), error = function(err) err)
          if (inherits(i_simpl, "error")) {
            flu$simpl <- "computation not possible"
          } else {
            flu$simpl <- sprintf("%.3f", i_simpl)
          }
          shiny::removeModal()
        } else {flu$simpl <- NA}
      }

      is.sym <- "sym" %in% input$statistic
      if(calc$sym != is.sym) {
        calc$sym <- is.sym
        if ("sym" %in% input$statistic) {
          # print("calculating symmetry")
          shiny::showModal(shiny::modalDialog("Calculating symmetry", footer=NULL))
          i_sym <- tryCatch(img_symmetry(img, horizontal = FALSE), error = function(err) err)
          if (inherits(i_sym, "error")) {
            flu$sym <- "computation not possible"
          } else {
            flu$sym <- sprintf("%.3f", i_sym)
          }
          shiny::removeModal()
        } else {flu$sym <- NA}
      }
    })

    # reset everything if a new file is uploaded
    shiny::observeEvent(input$file, {
      shiny::updateCheckboxGroupInput(session, inputId = "statistic", selected = "")
      # checkbox states
      calc$contr <- FALSE
      calc$selfsim <- FALSE
      calc$simpl <- FALSE
      calc$sym <- FALSE
      # fluency scores
      flu$contr <- NA
      flu$selfsim <- NA
      flu$simpl <- NA
      flu$sym <- NA
    })


    output$imgfluency <- shiny::renderTable({
      # display nothing if nothing is calculated yet
      #
      # req(input$calcfluency)
      if(all(is.na(c(flu$contr, flu$selfsim, flu$simpl, flu$sym)))) return(NULL)

      stats::na.omit(data.frame(Dimension=c("Contrast", "Self-similarity", "Simplicity", "Symmetry"),
                         Score = c(flu$contr, flu$selfsim, flu$simpl, flu$sym)))
    })

  }
)

# Run the application
# shinyApp(ui = ui, server = server)

