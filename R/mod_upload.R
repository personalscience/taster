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
      uiOutput(ns("ask_to_write_db")),uiOutput(ns("ask_to_write_final")),
      hr(),
      fileInput(ns("ask_filename_nutrisense"), label = "Choose Nutrisense File", accept = ".csv"),
      plotOutput(ns("modChart")),
      hr(),
      h1("Experiments"),
      wellPanel(dataTableOutput(ns("experimentsTable"))),
      hr(),
      wellPanel(dataTableOutput(ns("glucoseTable"))),
      hr(),
      h1("Notes Table"),
      wellPanel(dataTableOutput(ns("notesTable")))
    )

  )
}

#' csv_upload Server Functions
#'
#' @param con database connection
#' @importFrom cgmrdb classify_notes_to_experiment
#' @noRd
mod_upload_server <- function(id, con, user){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    experiments_mapping <- DBI::dbReadTable(con, "experiments_mapping")

    # Current User ----
    current_user <- reactive({
      this_user <- user$f$get_signed_in()
      validate(need(!is.null(this_user), "Not signed in"))
      user <- user_find_id(con,
                           list(firebase_id = this_user$response$uid))
      message("Current User:", print_user(user))
      return(user)

    })


    # Ask Filepath ----
    output$ask_for_libreview_csv<- renderUI({

      file_selection(item_id = ns("libreview_upload"), label = "Libreview CSV", accept = ".csv")

    }

    )



    ## filepath_notes ----
    filepath_notes<- reactive({
      validate(
        need(input$ask_filename_notes,"Please select a Notes CSV file")
      )
      input$ask_filename_notes}
      )

    ## filepath_nutrisense ----
    filepath_nutrisense<- reactive({
      validate(
        need(input$ask_filename_nutrisense,"Please select a Nutrisense CSV file")
      )
      input$ask_filename_nutrisense}
      )


    # glucose_df_raw : Read Raw ----

    glucose_df_raw <- reactive({
      validate(
        need(input$libreview_upload, "Upload a file please")
      )
      new_df <- cgmr::libreview_csv_df(file=input$libreview_upload$datapath)
    }
    )

    ## libreview_name ----
    libreview_name <- reactive({
      user_feedback(output=output, sprintf("You uploaded %s glucose values",nrow(glucose_df())))
      glucose_df_raw()[["name"]]
      })

    ## glucose_df ----
    glucose_df <- reactive({
      ID = current_user()$user_id
      g_df <- glucose_df_raw()[["glucose_raw"]] %>%
        transmute(
          `time` = `timestamp`,
          scan = glucose_scan,
          hist = glucose_historic,
          strip = strip_glucose,
          value = hist,
          food = notes,
          user_id = ID
        )
      return(g_df)
      }
    )

    ## nutrisense_df ----
    glucose_df_nutrisense_raw <-
      reactive(cgmr::glucose_df_from_nutrisense(filepath = filepath_nutrisense()$datapath))



    # notes_df_csv ----
    notes_df_csv<- reactive({
      ID = current_user()$user_id

      notes <- NULL
      try(notes <- cgmr::notes_df_from_csv(file = filepath_notes()$datapath,
                                       user_id = ID))

      validate(
        need(!is.null(notes), "Bad Notes Format; please upload a properly-formatted Notes CSV")
      )
    user_feedback(output, msg = sprintf("Uploaded %s new notes for User %s", nrow(notes), ID))

      return(notes)
    }
      )

    ## notes_df_libreview ----
    notes_df_libreview <- reactive(cgmr::notes_df_from_glucose_table(glucose_df()))


    ## notes_df ----
    notes_df <- reactive({

      return(bind_rows(notes_df_csv(),notes_df_libreview()))
    }
    )

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

    # output$ask_to_write_final ----
    output$ask_to_write_final <- renderUI({
      validate(
        need(!is.null(glucose_df()),sprintf("Please confirm you're ready to write to the database"))
      )
      actionButton(NS(id,"write_final"),
                   label = "Write to Final")



    })

    # input$write_final ----
    observeEvent(input$write_final, {
      if(input$write_final) {
        user_feedback(output, "Please log in")
        ID = current_user()$user_id

        message("ready to do final write")

        write_notes <- experiments_times()
        write_glucose <- glucose_df()

        notes_result <- db_replace_records(con, ID, "notes_records", write_notes)
        glucose_result <- db_replace_records(con, ID, "glucose_records", write_glucose)

        message(sprintf("wrote %s glucose records; %s notes_records\n", glucose_result, notes_result))
      }
    })


    # input$write_db ----
    observeEvent(input$write_db, {
      if(input$write_db) {
        user_feedback(output, "Please log in")
        ID = current_user()$user_id
        message("write_db")

      user_feedback(output,
                    msg = sprintf('Thinking about writing %s rows to %s database now\n
                                  User ID = %s',
                                  nrow(glucose_df_raw()[["glucose_raw"]]),
                                  class(con),
                                  ID))
      new_raw_table <- bind_cols(user_id = ID,
                                 glucose_df_raw()[["glucose_raw"]])

      # todo pull this code to cgmr:  convert the raw table to a glucose record
      notes_from_raw_glucose <- cgmr::notes_df_from_glucose_table(glucose_df(), user_id = ID)


      message(sprintf("Found notes in glucose file %s\n", notes_from_raw_glucose[,"Comment"]))




      response <-
        db_write_table(con=con,
                       table_name = "raw_glucose",
                       table_df = new_raw_table)
      user_feedback(output,
                    msg = sprintf("Wrote User %s with Response = \n %s",
                                  ID,
                                  response))
      validate(
        need(!is.null(notes_df_csv()), "Upload a Notes CSV to write Notes file")
      )
      new_raw_notes_table <- notes_df_csv()

      combined_raw_notes_table <- bind_rows(new_raw_notes_table, notes_from_raw_glucose)
      print(combined_raw_notes_table)

      notes_response <-
      db_write_notes_table(con = con,
                                             table_name = "raw_notes",
                                             table_df = combined_raw_notes_table,
                                             user_id = ID)
      user_feedback(output,
                    msg = sprintf("Glucose=%s\nNotes=%s",
                                  response,
                                  notes_response))

      }
    })

    # experiments ----
    experiments_times <- reactive({
      notes_table <- notes_df() %>% select(-TZ)

      notes_table$Comment <- cgmrdb::classify_notes_to_experiment(notes_table$Comment, experiments_mapping)
      message(sprintf("notes_table: %s\n", head(notes_table$Comment,5)))
      return(notes_table %>% filter(Comment != "other"))
    })

    # output$glucoseTable ----
    output$glucoseTable <- renderDataTable({

      glucose_df_raw()[["glucose_raw"]]
    },
    options = list(pageLength = 5)
    )

    # output$notesTable ----
    output$notesTable <- renderDataTable(
      notes_df(),
      options = list(pageLength = 5)
      )

    # output$experiments ----
    output$experimentsTable <- renderDataTable(
      experiments_times(),
      options = list(pageLength = 5)
    )



    cgm_data <- list(con = reactive(con),
                     glucose_records = glucose_df,
                     notes_records = notes_df_csv)

    return(cgm_data
           ) # This whole module returns a reactive glucose_df

  })
}

#' @title Feedback to User
#' @description A generic way to send feedback to the user.  Assumes a `user_message` tag in the UI.
#' If the message is NULL, return 'No Value Found'.
#' @param output output
#' @param msg character message to display
#' @return renderText object
user_feedback <- function(output, msg = "Thank You For Listening") {
  message("user feedback")
  message(msg)
  output$user_message <-
    renderText(if (rlang::is_empty(msg)) "No Value Found"
      else msg)
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

