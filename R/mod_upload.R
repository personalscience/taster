#' csv_upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_upload_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      includeMarkdown(app_sys("app/www/docs/upload_instructions.md")),
      hr(),
      textOutput(ns("user_message")),
      fluidRow(
        uiOutput(ns("ask_for_libreview_csv")),
        fileInput(ns("ask_filename_notes"), label = "Choose Notes File", accept = ".csv")),
      uiOutput(ns("ask_to_write_db")),
      hr(),
      fileInput(ns("ask_filename_nutrisense"), label = "Choose Nutrisense File", accept = ".csv"),
      plotOutput(ns("modChart")),
      hr(),
      wellPanel(dataTableOutput(ns("glucoseTable"))),
      wellPanel(dataTableOutput(ns("notesTable")))
    )

  )
}

#' csv_upload Server Functions
#'
#' @param con database connection
#' @noRd
mod_upload_server <- function(id, con, user){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Ask Filepath ----
    output$ask_for_libreview_csv<- renderUI({

      file_selection(item_id = ns("libreview_upload"), label = "Libreview CSV", accept = ".csv")

    }

    )

    # Current User ----
    current_user <- reactive({
      this_user <- user$f$get_signed_in()
      validate(need(!is.null(this_user), "Not signed in"))
      user <- user_find_id(con,
                           list(firebase_id = this_user$response$uid))
      message("Current User:", print_user(user))
      return(user)

    })



    # filepath_notes ----
    filepath_notes<- reactive({
      validate(
        need(input$ask_filename_notes,"Please select a Notes CSV file")
      )
      input$ask_filename_notes}
      )

    # filepath_nutrisense ----
    filepath_nutrisense<- reactive({
      validate(
        need(input$ask_filename_nutrisense,"Please select a Nutrisense CSV file")
      )
      input$ask_filename_nutrisense}
      )


    # glucose_df : Read Raw ----

    glucose_df_raw <- reactive({
      validate(
        need(input$libreview_upload, "Upload a file please")
      )
      new_df <- cgmr::libreview_csv_df(file=input$libreview_upload$datapath)
    }
    )
    libreview_name <- reactive({
      user_feedback(output=output, sprintf("You uploaded %s glucose values",nrow(glucose_df())))
      glucose_df_raw()[["name"]]
      })

    glucose_df <- reactive({
      g_df <- glucose_df_raw()[["glucose_raw"]] %>%
        transmute(
          `time` = `timestamp`,
          scan = glucose_scan,
          hist = glucose_historic,
          strip = strip_glucose,
          value = hist,
          food = notes,
          user_id = 0
        )
      return(g_df)
      }
    )

    # notes_df ----
    notes_df_csv<- reactive({
      notes <- cgmr::notes_df_from_csv(file = filepath_notes()$datapath)
      user_feedback(output, msg = sprintf("Uploaded %s new notes", nrow(notes)))
      return(notes)
    }
      )
    notes_df_libreview <- reactive(cgmr::notes_df_from_glucose_table(glucose_df()))


    # nutrisense_df ----
    glucose_df_nutrisense_raw <-
      reactive(cgmr::glucose_df_from_nutrisense(filepath = filepath_nutrisense()$datapath))


    # output$modChart ----
    output$modChart <- renderPlot({
      plot_glucose(glucose_df(),sprintf("Glucose Results for %s",libreview_name()))
    }
    )

    # output$ask_to_write_db ----
    output$ask_to_write_db <- renderUI({
      validate(
        need(!is.null(glucose_df()),sprintf("Please upload a CSV file"))
      )
      actionButton(NS(id,"write_db"),
                     label = "Write to Database")


    })


    # input$write_db ----
    observeEvent(input$write_db, {
      if(input$write_db) {
        message("write_db", print_user(user))
        ID = current_user()$user_id
      user_feedback(output,
                    msg = sprintf('Thinking about writing %s rows to %s database now\n
                                  User ID = %s',
                                  nrow(glucose_df_raw()[["glucose_raw"]]),
                                  class(con),
                                  ID))
      new_raw_table <- bind_cols(user_id = ID,
                                 glucose_df_raw()[["glucose_raw"]])




        response <- db_write_table(con=con,
                       table_name = "raw_glucose",
                       table_df = new_raw_table)
        user_feedback(output,
                      msg = sprintf("Wrote User %s with Response = \n %s",
                                    ID,
                                    response))
      }
    })

    # output$glucoseTable ----
    output$glucoseTable <- renderDataTable({

      glucose_df_raw()[["glucose_raw"]]
    },
    options = list(pageLength = 5)
    )

    # output$notesTable ----
    output$notesTable <- renderDataTable(
      notes_df_csv(),
      options = list(pageLenth = 5))


    cgm_data <- list(con = reactive(con),
                     glucose_records = glucose_df,
                     notes_records = notes_df_csv)

    return(cgm_data
           ) # This whole module returns a reactive glucose_df

  })
}

#' @title Feedback to User
#' @description A generic way to send feedback to the user.  Assumes a `user_message` tag in the UI.
#' @param output output
#' @param msg character message to display
#' @return feedback object
user_feedback <- function(output, msg = "Thank You For Listening"){

  output$user_message <- renderText(msg)
}

#' @title Make a File Selection Object
#' @param item_id character string NS id for UI
#' @param label character string name on label
#' @return file selection object
file_selection <- function(item_id, label = "Which File?", ...){

  selected <- fileInput(inputId = item_id, label = label, ...)
  return(selected)
}

## To be copied in the UI
# mod_upload_ui("csv_upload_ui_1")

## To be copied in the server
# mod_upload_server("csv_upload_ui_1")

#' @description Demo for mod_filter
#' @noRd
#'
demo_upload <- function() {
  ui <- fluidPage(firebase::useFirebase(),
                  firebase::firebaseUIContainer(),
    mod_upload_ui("csv_upload_ui_1"))
  #sample_glucose <- cgmr::glucose_df_from_libreview_csv()
  server <- function(input, output, session) {

    con <- db_connection()
    f <- firebase_setup()
    user <- UserObject(con, firebase_obj = f)
  message("demo:", print_user(user))
    g <- mod_upload_server("csv_upload_ui_1", con = con, user = user)
   # message(sprintf("g = %s", str(g$con())))
    onStop(function(){message("gracefully exiting from mod_upload...")
    DBI::dbDisconnect(con)})

  }
  shinyApp(ui, server)
}

