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
    shiny::uiOutput(ns("slctlanguage")),
    shiny::fluidRow(
      shiny::column(
        2,
        editR::selection_ui(ns("select_intake"), "Intake:")
      ),
      shiny::column(
        2,
        editR::selection_ui(ns("select_test"), "Test:")
      ),
      shiny::column(
        2,
        editR::selection_ui(ns("select_question"), "Question:")
      ),
      shiny::column(
        2,
        editR::selection_ui(ns("select_version"), "Version:")
      ),
      shiny::column(
        2,
        editR::selection_ui(ns("select_student"), "Student:")
      ),
      shiny::column(
        2,
        editR::selection_ui(ns("select_attempt"), "Attempt:")
      )
    ),
    shinydashboard::tabBox(
      side = "left", width = "100%",
      shiny::tabPanel(
        title = shiny::tagList(
          shiny::icon("tasks"),
          shiny::span(
            "Answers",
            title = "Check answers provided by each student to each question."
          )
        ),
        shiny::uiOutput(ns("check_answers")),
        shiny::tags$hr(),
        shiny::fluidRow(
          shiny::column(
            4,
            shiny::uiOutput(ns("viewversion"))
          ),
          shiny::column(
            4,
            shiny::uiOutput(ns("display_selected_answer")),
            shiny::actionButton(
              ns("open_selected_answer"), "Open answer file",
              icon = shiny::icon("file-import"),
              style = "background-color:#000066;color:#FFF;width:100%;"
            ),
            shiny::uiOutput(ns("customcomment")),
            shiny::actionButton(
              ns("save_comment"), "Save the comment",
              icon = shiny::icon("save"),
              style = "background-color:#006600;color:#FFF;width:100%;"
            )
          ),
          shiny::column(
            4,
            shiny::uiOutput(ns("displayscore")),
            shiny::uiOutput(ns("displaywordcount")),
            shinyWidgets::materialSwitch(
              inputId = ns("dispallkw"),
              label = "Highlight all keywords", 
              status = "primary",
              value = FALSE,
              right = TRUE
            ),
            shiny::uiOutput(ns("keywords_selection"))
          )
        )
      ),
      shiny::tabPanel(
        title = shiny::tagList(
          shiny::icon("comment"), shiny::span(
            "Solutions",
            title = "Edit and correct the criteria, solutions, and feedback associated with each question."
          )
        ),
        shiny::fluidRow(
          shiny::column(
            12,
            shiny::actionButton(
              ns("save_sol"), "Save", icon = shiny::icon("save"),
              style = "background-color:#006600;color:#FFF;width:100%;"
            ),
            rhandsontable::rHandsontableOutput(ns("edit_solutions"))
          )
        )
      ),
      shiny::tabPanel(
        title = shiny::tagList(
          shiny::icon("sliders"), shiny::span(
            "Parameters",
            title = "Change test parameters, like the distribution of points of penalties."
          )
        ),
        shiny::fluidRow(
          shiny::column(
            12,
            shiny::actionButton(
              ns("save_param"), "Save", icon = shiny::icon("save"),
              style = "background-color:#006600;color:#FFF;width:100%;"
            ),
            rhandsontable::rHandsontableOutput(ns("edit_parameters"))
          )
        )
      ),
      shiny::tabPanel(
        title = shiny::tagList(
          shiny::icon("magnifying-glass-chart"), shiny::span(
            "Diagnostics",
            title = "Check grading consistency across questions and students."
          )
        )
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
            shiny::selectInput(
              ns("defmetric"), "Metric:",
              choices = c("earned","score","grade"),
              selected = "earned", width = "100%"
            ),
            shiny::numericInput(ns("defpass"), "Pass:", value = 0, width = "100%")
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
      ),
      
      
      
      
      shiny::tabPanel(
        title = shiny::tagList(
          shiny::icon("envelope-open-text"), shiny::span(
            "Feedback",
            title = "Write a template for individual reporting and send e-mails."
          )
        ),
        base::list(
          shiny::fluidRow(
            shiny::column(
              4,
              shinydashboardPlus::box(
                width = 12, title = "Edition", solidHeader = TRUE,
                status = "navy", collapsible = FALSE, collapsed = FALSE,
                shiny::fluidRow(
                  shiny::column(
                    4,
                    shiny::actionButton(
                      ns("feedbackinrstudio"), "RStudio",
                      icon = shiny::icon("r-project"),
                      style = "background-color:#003366;color:#FFF;
                width:100%;margin-bottom:10px;"
                    )
                  ),
                  shiny::column(
                    4,
                    shiny::actionButton(
                      ns("refreshfeedback"), "Refresh",
                      icon = shiny::icon("rotate"),
                      style = "background-color:#006699;color:#FFF;
                width:100%;margin-bottom:10px;"
                    )
                  ),
                  shiny::column(
                    4,
                    shiny::actionButton(
                      ns("savefeedback"), "Save",
                      icon = shiny::icon("floppy-disk"),
                      style = "background-color:#006633;color:#FFF;
                width:100%;margin-bottom:10px;"
                    )
                  )
                ),
                shiny::fluidRow(
                  shiny::column(
                    12,
                    shiny::uiOutput(ns("edit_feedback"))
                  )
                )
              )
            ),
            shiny::column(
              4,
              shiny::uiOutput(ns("preview_feedback"))
            ),
            shiny::column(
              4,
              shinydashboardPlus::box(
                width = 12, title = "Mailing", solidHeader = TRUE,
                status = "maroon", collapsible = FALSE, collapsed = FALSE,
                shiny::fluidRow(
                  shiny::column(
                    12,
                    shiny::uiOutput(ns("slctcredentials")),
                    shiny::tags$hr(),
                    shiny::textAreaInput(
                      ns("mailsubject"), "Subject of the message:",
                      "Feedback on your test.",
                      width = "100%"
                    ),
                    shiny::textInput(ns("emailtest"), "Test e-mail", width = "100%"),
                    shiny::actionButton(
                      ns("sendtestemail"), "Send test", icon = shiny::icon("envelope"),
                      style = "background-color:#FF4500;color:#FFF;width:100%;margin-top:25px;"
                    ),
                    shiny::tags$hr(),
                    shiny::actionButton(
                      ns("sendfeedback"), "Send to all", icon = shiny::icon("paper-plane"),
                      style = "background-color:#330066;color:#FFF;width:100%;margin-bottom:10px;"
                    ),
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}
