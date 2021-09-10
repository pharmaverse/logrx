#' timberAddin
#' Code needed to create the miniUI for the addin
#' @return returns mimUI Addin to batch submit r files and create logs around them
#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel
#' @importFrom shiny p textInput checkboxInput actionButton uiOutput reactiveValues observeEvent renderText stopApp runGadget conditionalPanel fluidRow column tags HTML
#' @importFrom rstudioapi selectFile selectDirectory
#' @importFrom stringr str_locate_all str_replace
#' @export
timberAddin <- function() {
   ui <- miniUI::miniPage(
      #CSS sheet to color ui
      shiny::tags$head(
         shiny::tags$style(
            shiny::HTML(" .gadget-title {
                        color:rgb(255,255,255);font-weight: bold;
                        background-color:rgb(243, 102, 51);}

                        .btn {
                        color:rgb(0,0,0);
                        text-align: left;
                        #border-color:rgb(0, 0, 0);
                        background-color:rgb(255,255,255);
                        }

                        #run{
                        background-color:rgb(213, 209, 206);
                        color:rgb(0,0,0);
                        }
                        #run:focus{
                        color:rgb(0,0,0);
                        background-color:rgb(243, 102, 51);
                        }

                        .btn:hover{
                        #border-color:rgb(0, 0, 0);
                        background-color: rgb(243, 102, 51);
                        color:   rgb(255,255,255);font-weight: bold;
                        }
                        .btn:focus{
                        background-color:rgb(255, 179, 4);
                        }

                        "))),
      #Title for miniui
      miniUI::gadgetTitleBar("timber",
                             left = NULL,
                             right = miniUI::miniTitleBarButton("done", "Done", primary = TRUE)
      ),
      miniUI::miniContentPanel(
         #location to save logs to
         shiny::fluidRow(
            shiny::column(3,
                          shiny::actionButton("log_path", "Location to Save Log")),
            shiny::column(width=8,
                          shiny::textInput("log_pathTx", label=NULL, value = getwd(),
                                           width = '100%'))),
         # Added, not checked
         # Log name
         shiny::fluidRow(
            shiny::column(3,
                          shiny::actionButton("log_name", "Log Name")),
            shiny::column(width=8,
                          shiny::textInput("log_nameTx", label=NULL,
                                           value = str_replace(basename(rstudioapi::getActiveDocumentContext()[["path"]]),
                                                               ".R$", ".log"),
                                           width = '100%'))),
         # /Added, not checked
         #location of file to run
         shiny::fluidRow(
            shiny::column(3,
                          shiny::actionButton("file", "File to Run")),
            shiny::column(8,
                          shiny::textInput("fileTx", label=NULL,
                                           value = rstudioapi::getActiveDocumentContext()[["path"]],
                                           width = '100%')
            )),
         # #User name check box
         # shiny::fluidRow(
         #    shiny::column(
         #       3,
         #       shiny::checkboxInput("addUse", "Add Username into Log?", TRUE)
         #    ),
         #    shiny::column(3,
         #                  shiny::checkboxInput("addMsg", "Include messages?", TRUE)
         #    )
         # ),
         shiny::fluidRow(
            shiny::column(3, shiny::actionButton("run", "Run"))
         ),
         shiny::uiOutput("output")
      )
            )

   server <- function(input, output, session) {

      ### Save current file
      rstudioapi::documentSave(id = rstudioapi::getActiveDocumentContext()[["id"]])
      #Reactive value to see if it done running
      doneCheck <- shiny::reactiveValues(data = NULL, progress = NULL)
      #Reactive value that contains all the information about the log
      logInfo <- shiny::reactiveValues(
         location = getwd(),
         ### Add default file to select
         file = rstudioapi::getActiveDocumentContext()[["path"]],
         path = NULL
      )

      #Updates the log location when the button is clicked
      shiny::observeEvent(input$log_path, {
         logInfo$location <- rstudioapi::selectDirectory()
         shiny::updateTextInput(session, "log_pathTx", value= logInfo$location)
      })
      #Updates the log location when the log location is manually edited
      shiny::observeEvent(input$log_pathTx, {
         logInfo$location <- input$log_pathTx
      })
      # Added, not checked
      shiny::observeEvent(input$log_nameTx, {
         logInfo$name <- input$log_nameTx # Should a check be run on name validity?
         shiny::updateTextInput(session, "log_nameTx", value= logInfo$name)
      })
      #Updates the log location when the log location is manually edited
      shiny::observeEvent(input$log_pathTx, {
         logInfo$location <- input$log_pathTx
      })
      # /Added, not checked
      #Updates the file location when the button is clicked
      shiny::observeEvent(input$file, {
         if (is.null(logInfo$path)) {
            logInfo$file <- rstudioapi::selectFile()
         } else {
            logInfo$file <- rstudioapi::selectFile(path = logInfo$path)
         }
         shiny::updateTextInput(session, "fileTx", value= logInfo$file)
      })
      #Updates the file location when manually edited
      shiny::observeEvent(input$fileTx, {
         logInfo$file <- input$fileTx
      })

      shiny::observeEvent(input$run, {



            # if (input$addUse) {
         #    log_file <- basename(logInfo$file) %>%
         #       str_remove("(?<=\\.)\\w*$") %>%
         #       paste0("log")
         #    create_log(logInfo$file, paste0(logInfo$location, "/", log_file),
         #               user_id = Sys.info()["user"],
         #               inc_messages = input$addMsg)
         #    doneCheck$data <- "Select a new file, if you wish to run more"
         # } else {
            log_file <- basename(logInfo$file) %>%
               stringr::str_remove("(?<=\\.)\\w*$") %>%
               paste0("log")
            axecute(file = logInfo$file, log_name = logInfo$name)
            # create_log(logInfo$file, paste0(logInfo$location, "/", log_file),
            #            inc_messages = input$addMsg)
            doneCheck$data <- "Select a new file, if you wish to run more files"
         # }
      })

      output$output <- shiny::renderText({
         doneCheck$data
      })
      #Event what happens when you click the done button
      shiny::observeEvent(input$done, {
         stopApp()
      })
   }
   viewer <- shiny::dialogViewer("Run with timber", width = 800, height = 300)
   shiny::runGadget(ui, server, viewer = viewer)
   }


