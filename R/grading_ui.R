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
        6,
        shiny::uiOutput(ns("question_selection"))
      ),
      shiny::column(
        6,
        shiny::uiOutput(ns("student_id_selection"))
      )
    ),
    
    shinydashboard::tabBox(
      side = "left", width = "100%",
      shiny::tabPanel(
        title = shiny::tagList(shiny::icon("tasks"),"Answer"),
        shiny::uiOutput(ns("gradingprogress")),
        shiny::fluidRow(
          shiny::column(
            1,
            shiny::actionButton(
              ns("previousstudent"), "",
              icon = shiny::icon("chevron-circle-left"),
              style = "background-color:#660000;color:#FFF;width:80px;
            height:75px;font-size:40px;text-align:center;"
            )
          ),
          shiny::column(
            10,
            shiny::uiOutput(ns("student_rank_selection"))
          ),
          shiny::column(
            1,
            shiny::actionButton(
              ns("nextstudent"), "",
              icon = shiny::icon("chevron-circle-right"),
              style = "background-color:#000066;color:#FFF;width:80px;
            height:75px;font-size:40px;text-align:center;"
            )
          )
        ),
        shiny::tags$hr(),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::actionButton(
              ns("save_checks"), "Save", icon = shiny::icon("save"),
              style = "background-color:#006600;color:#FFF;width:100%;"
            ),
            shiny::tags$hr(),
            shiny::uiOutput(ns("check_answers"))
          ),
          shiny::column(
            6,
            shiny::fluidRow(
              shiny::column(6, shiny::uiOutput(ns("displayearned"))),
              shiny::column(6, shiny::uiOutput(ns("displaywordcount")))
            ),
            shiny::tags$br(),
            shiny::uiOutput(ns("display_selected_answer")),
            shiny::tags$br(),
            shiny::uiOutput(ns("keywords_selection"))
          )
        )
      ),
      shiny::tabPanel(
        title = shiny::tagList(shiny::icon("comment"),"Feedback"),
        shiny::actionButton(
          ns("update_feedback"), "Save", icon = shiny::icon("save"),
          style = "background-color:#006600;color:#FFF;width:100%;
          margin-top:10px;margin-bottom:10px;"
        ),
        rhandsontable::rHandsontableOutput(ns("edit_feedback"))
      ),
      shiny::tabPanel(
        title = shiny::tagList(shiny::icon("question-circle"),"Question"),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::uiOutput(ns("edit_question_parameters")),
            shiny::uiOutput(ns("viewversion"))
          ),
          shiny::column(
            6,
            shiny::uiOutput(ns("question_metrics")),
            shiny::uiOutput(ns("question_stats")),
            shiny::plotOutput(ns("question_diagram"))
          )
        )
      ),
      shiny::tabPanel(
        title = shiny::tagList(shiny::icon("stethoscope"),"Diagnostics"),
        
      ),
      shiny::tabPanel(
        title = shiny::tagList(shiny::icon("chart-bar"),"Results"),
        shiny::fluidRow(
          shiny::column(
            9,
            shiny::uiOutput(ns("test_metrics")),
            shiny::plotOutput(ns("test_distribution"))
          ),
          shiny::column(
            3,
            shiny::textInput(
              ns("textbook_website"), "Root of the textbook website:",
              value = "", width = "100%"
            ),
            shiny::actionButton(
              ns("export_reports"), "Export for reports",
              icon = shiny::icon("sort-numeric-up-alt"),
              style = "background-color:#660000;color:#FFF;width:100%;
            margin-top:10px;margin-bottom:10px;"
            ),
            shiny::actionButton(
              ns("export_statistics"), "Export for statistics",
              icon = shiny::icon("comment"),
              style = "background-color:#000066;color:#FFF;width:100%;
            margin-top:10px;margin-bottom:10px;"
            )
          )
        )
      )
    )
    
  )
}
