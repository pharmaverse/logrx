#' timberAddin
#' Code needed to create the miniUI for the Addin
#' @return returns miniUI Addin to batch submit r files and create logs around them
#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel
#' @importFrom shiny textInput checkboxInput actionButton uiOutput reactiveValues observeEvent renderText stopApp runGadget conditionalPanel fluidRow column tags HTML
#' @importFrom rstudioapi selectFile selectDirectory
#' @importFrom stringr str_locate_all str_replace
#' @importFrom waiter useWaiter waiter_show waiter_hide spin_solar
#' @export
timberAddin <- function() {
   ui <- miniUI::miniPage(
      useWaiter(), # include dependencies
      # css
      shiny::tags$head(
         shiny::tags$style(
            shiny::HTML(
               ".gadget-title {
                  color:rgb(0,0,0);
                  font-weight: bold;
                  background-color:rgb(134,202,198);
                  border: 1px solid rgb(0,0,0);
               }

               #axecute{
               text-align: left;
               color:rgb(255,255,255);
               background-color: rgb(134,202,198);
               border: 1px solid rgb(0,0,0);
               font-weight: bold;
               }
               #axecute:hover{
               color:rgb(134,202,198);
               background-color: rgb(255,255,255);
               border: 1px solid rgb(0,0,0);
               }

               .btn {
                  text-align: left;
                  color:rgb(255,255,255);
                  background-color: rgb(243, 102, 51);
                  border: 1px solid rgb(0,0,0);
                  font-weight: bold;
               }

               .btn:hover{
                  background-color:rgb(255,255,255);
                  border: 1px solid rgb(243, 102, 51);
                  color:rgb(0,0,0);
               }"
            )
         )
      ),
      #Title for miniUI
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
         # Log name
         shiny::fluidRow(
            shiny::column(3,
                          shiny::actionButton("log_name", "Log Name")),
            shiny::column(width=8,
                          shiny::textInput("log_nameTx", label=NULL,
                                           value = str_replace(basename(rstudioapi::getActiveDocumentContext()[["path"]]),
                                                               ".R$", ".log"),
                                           width = '100%'))),
         #location of file to run
         shiny::fluidRow(
            shiny::column(3,
                          shiny::actionButton("file", "File to Run")),
            shiny::column(8,
                          shiny::textInput("fileTx", label=NULL,
                                           value = normalizePath(rstudioapi::getActiveDocumentContext()[["path"]]),
                                           width = '100%')
            )),
         #User name check box
         shiny::fluidRow(
            shiny::column(
               12,
               shiny::checkboxInput("rmLog", "Remove the log object after axecution?", TRUE)
            )
         ),
         shiny::fluidRow(
            shiny::column(
               12,
               shiny::checkboxGroupInput("toReport", "Optional elements to report:",
                                         c("Messages" = "messages",
                                           "Output" = "output",
                                           "Result" = "result"),
                                         inline = TRUE,
                                         selected = c("messages", "output", "result"))
            )
         ),
         shiny::fluidRow(
            shiny::column(3, shiny::actionButton("axecute", "Axecute"))
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
         logInfo$location <- normalizePath(rstudioapi::selectDirectory())
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
      #Updates the file location when the button is clicked
      shiny::observeEvent(input$file, {
         if (is.null(logInfo$path)) {
            logInfo$file <- normalizePath(rstudioapi::selectFile())
         } else {
            logInfo$file <- normalizePath(rstudioapi::selectFile(path = logInfo$path))
         }
         logInfo$name <- str_replace(basename(logInfo$file),".R$", ".log")
         shiny::updateTextInput(session, "fileTx", value= logInfo$file)
         shiny::updateTextInput(session, "log_nameTx", value= logInfo$name)
      })
      #Updates the file location when manually edited
      shiny::observeEvent(input$fileTx, {
         logInfo$file <- input$fileTx
         logInfo$name <- str_replace(basename(logInfo$file),".R$", ".log")
         shiny::updateTextInput(session, "log_nameTx", value= logInfo$name)
      })

      shiny::observeEvent(input$axecute, {
         waiter_show( # show the waiter
            html = spin_solar() # use a spinner
         )
         axecute(file = logInfo$file, log_name = logInfo$name,
                 log_path = logInfo$location, remove_log_object = input$rmLog,
                 to_report = input$toReport)
         doneCheck$data <- "Select a new file, if you wish to run more files"
         waiter_hide() # hide the waiter
      })

      output$output <- shiny::renderText({
         doneCheck$data
      })
      #Event what happens when you click the done button
      shiny::observeEvent(input$done, {
         stopApp()
      })
   }
   viewer <- shiny::dialogViewer("Run with timber", width = 800, height = 400)
   shiny::runGadget(ui, server, viewer = viewer)
   }


