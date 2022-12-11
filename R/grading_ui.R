#' @name grading_ui
#' @title Grade tests.
#' @author Nicolas Mangin
#' @description Module allowing the user to grade and calibrate tests.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return Save the test results in the relevant test sub-folder in the folder "5_tests".
#' @import shiny
#' @export


grading_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    shiny::fluidRow(
      shiny::column(
        4,
        editR::selection_ui(ns("select_question"))
      ),
      shiny::column(
        4,
        editR::selection_ui(ns("select_version"))
      ),
      shiny::column(
        4,
        editR::selection_ui(ns("select_student"))
      )
    ),
    shinydashboard::tabBox(
      side = "left", width = "100%",
      shiny::tabPanel(
        title = shiny::tagList(
          shiny::icon("tasks"),
          shiny::span(
            "Answer",
            title = "Check answers provided by each student to each question."
          )
        ),
        shiny::fluidRow(
          shiny::column(
            3,
            shiny::uiOutput(ns("viewversion"))
          ),
          shiny::column(
            4,
            shiny::uiOutput(ns("displaywordcount")),
            shiny::uiOutput(ns("display_selected_answer")),
            shiny::tags$hr(),
            shiny::uiOutput(ns("keywords_selection"))
          ),
          shiny::column(
            5,
            shiny::uiOutput(ns("displayearned")),
            shiny::uiOutput(ns("check_answers")),
            shiny::actionButton(
              ns("save_checks"), "Save", icon = shiny::icon("save"),
              style = "background-color:#006600;color:#FFF;width:100%;"
            )
          )
        )
      ),
      shiny::tabPanel(
        title = shiny::tagList(
          shiny::icon("comment"), shiny::span(
            "Solution",
            title = "Complement and correct the criteria, solutions, and feedback associated with each question."
          )
        ),
        shiny::fluidRow(shiny::uiOutput(ns("question_infoboxes"))),
        shiny::fluidRow(
          shiny::column(
            9,
            shiny::uiOutput(ns("edit_question_parameters"))
          ),
          shiny::column(
            3,
            shiny::actionButton(
              ns("update_solutions"), "Save solutions", icon = shiny::icon("save"),
              style = "background-color:#006600;color:#FFF;width:200px;"
            )
          )
        ),
        shiny::fluidRow(
          rhandsontable::rHandsontableOutput(ns("edit_solutions"))
        ), 
        shiny::fluidRow(
          shiny::column(
            6#,
            #shiny::uiOutput(ns("question_stats"))
          ),
          shiny::column(
            6#,
            #shiny::plotOutput(ns("question_diagram"))
          )
        )
      ),
      shiny::tabPanel(
        title = shiny::tagList(
          shiny::icon("magnifying-glass-chart"), shiny::span(
            "Diagnostics",
            title = "Check grading consistency across students."
          )
        ),
        shiny::fluidRow(
          shiny::column(
            4
            
          ),
          shiny::column(
            4
          ),
          shiny::column(
            4
          )
        )
      )
    )
  )
}
