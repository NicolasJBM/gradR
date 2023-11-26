#' @name grading_ui
#' @title Grade tests.
#' @author Nicolas Mangin
#' @description Module allowing the user to grade, check, and calibrate tests.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return Save the test results in the relevant test sub-folder.
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom shiny NS
#' @importFrom shiny actionButton
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny icon
#' @importFrom shiny numericInput
#' @importFrom shiny plotOutput
#' @importFrom shiny span
#' @importFrom shiny tabPanel
#' @importFrom shiny tagList
#' @importFrom shiny uiOutput
#' @importFrom shinyWidgets radioGroupButtons
#' @importFrom shinydashboard tabBox
#' @export


grading_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    shiny::fluidRow(
      shiny::column(
        4,
        shinyWidgets::radioGroupButtons(
          inputId = ns("focus"), label = NULL, 
          choices = c(
            `<i class='fa fa-question-circle'> Focus on questions </i>` = "question",
            `<i class='fa fa-user'> Focus on students </i>` = "student"
          ), selected = base::character(0),
          status = "primary", justified = TRUE,
          size = "sm", direction = "horizontal",
          checkIcon = base::list(yes = shiny::icon("check"))
        )
      ),
      shiny::column(
        8,
        shiny::uiOutput(ns("slctlanguage"))
      )
    ),
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::uiOutput(ns("filters"))
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
        shiny::fluidRow(
          shiny::column(
            12,
            shiny::uiOutput(ns("edit_question_parameters")),
            rhandsontable::rHandsontableOutput(ns("edit_solutions"))
          )
        )
      ),
      shiny::tabPanel(
        title = shiny::tagList(
          shiny::icon("magnifying-glass-chart"), shiny::span(
            "Diagnostics",
            title = "Check grading consistency across questions and students."
          )
        ),
        shiny::uiOutput(ns("diagnostics"))
      ),
      shiny::tabPanel(
        title = shiny::tagList(
          shiny::icon("chart-column"), shiny::span(
            "Results",
            title = "Test statistics and distribution of results."
          )
        ),
        shiny::fluidRow(
          shiny::column(
            2,
            shiny::numericInput(ns("defpass"), "Pass:", value = 0),
          ),
          shiny::column(
            10,
            shiny::uiOutput(ns("testmetrics"))
          )
        ),
        shiny::fluidRow(
          shiny::column(
            2,
            shiny::numericInput(ns("defhistbreaks"), "Breaks:", value = 1)
          ),
          shiny::column(
            10,
            shiny::plotOutput(ns("testdistribution"))
          )
        )
      )
    )
  )
}
