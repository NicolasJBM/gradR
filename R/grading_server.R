#' @name grading_server
#' @title Grade tests.
#' @author Nicolas Mangin
#' @description Module allowing the user to grade, check, and calibrate tests.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param course_data Reactive. Function containing all the course data loaded with the course.
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @return Save the test results in the relevant test sub-folder.
#' @importFrom chartR display_curve
#' @importFrom chartR draw_composition_scatterplot
#' @importFrom chartR draw_correlogram
#' @importFrom chartR draw_density_plot
#' @importFrom chartR draw_grade_distribution
#' @importFrom chartR draw_score_differences
#' @importFrom dplyr anti_join
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_all
#' @importFrom dplyr mutate_if
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr starts_with
#' @importFrom dplyr summarise
#' @importFrom editR make_infobox
#' @importFrom editR selection_server
#' @importFrom editR selection_ui
#' @importFrom knitr knit2html
#' @importFrom psych fa
#' @importFrom purrr map
#' @importFrom readr locale
#' @importFrom readr read_csv
#' @importFrom readr guess_encoding
#' @importFrom rhandsontable hot_col
#' @importFrom rhandsontable hot_cols
#' @importFrom rhandsontable hot_context_menu
#' @importFrom rhandsontable hot_to_r
#' @importFrom rhandsontable renderRHandsontable
#' @importFrom rhandsontable rhandsontable
#' @importFrom shiny HTML
#' @importFrom shiny NS
#' @importFrom shiny actionButton
#' @importFrom shiny brushedPoints
#' @importFrom shiny checkboxGroupInput
#' @importFrom shiny column
#' @importFrom DT dataTableOutput
#' @importFrom shiny fluidRow
#' @importFrom shiny icon
#' @importFrom shiny isolate
#' @importFrom shiny moduleServer
#' @importFrom shiny numericInput
#' @importFrom shiny observe
#' @importFrom shiny observeEvent
#' @importFrom shiny plotOutput
#' @importFrom shiny reactive
#' @importFrom shiny reactiveValues
#' @importFrom DT renderDataTable
#' @importFrom shiny renderPlot
#' @importFrom shiny renderUI
#' @importFrom shiny req
#' @importFrom shiny selectInput
#' @importFrom shiny sliderInput
#' @importFrom shiny tagList
#' @importFrom shiny uiOutput
#' @importFrom shiny updateNumericInput
#' @importFrom shiny withMathJax
#' @importFrom shinyWidgets radioGroupButtons
#' @importFrom shinyWidgets switchInput
#' @importFrom shinyWidgets updateSwitchInput
#' @importFrom shinyalert shinyalert
#' @importFrom shinybusy remove_modal_spinner
#' @importFrom shinybusy show_modal_spinner
#' @importFrom shinydashboard valueBox
#' @importFrom stringr str_count
#' @importFrom stringr str_detect
#' @importFrom stringr str_remove
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_split
#' @importFrom tibble column_to_rownames
#' @importFrom tibble rownames_to_column
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr replace_na
#' @importFrom tidyr separate
#' @importFrom tidyr unite
#' @importFrom tidyr unnest
#' @export



grading_server <- function(id, course_data, course_paths){
  ns <- shiny::NS(id)
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      file_name <- NULL
      file_path <- NULL
      path <- NULL
      record <- NULL
      text <- NULL
      attempt <- NULL
      data <- NULL
      question <- NULL
      student <- NULL
      test_parameters <- NULL
      checked <- NULL
      correct <- NULL
      course_path <- NULL
      interrogation <- NULL
      keywords <- NULL
      letter <- NULL
      number <- NULL
      proposition <- NULL
      checked_chr <- NULL
      document <- NULL
      explanation <- NULL
      item <- NULL
      language <- NULL
      modifications <- NULL
      type <- NULL
      value <- NULL
      weight <- NULL
      observation <- NULL
      partial_credits <- NULL
      penalty <- NULL
      discrimination <- NULL
      success <- NULL
      given <- NULL
      keep <- NULL
      guess <- NULL
      grade <- NULL
      flag <- NULL
      langiso <- NULL
      observation <- NULL
      score <- NULL
      difficulty <- NULL
      code <- NULL
      confidence <- NULL
      encoding <- NULL
      
      ##########################################################################
      # Loading
      
      modrval <- shiny::reactiveValues()
      
      shiny::observe({
        modrval$tests <- course_data()$tests
        modrval$solutions <- course_data()$solutions
        modrval$answers <- course_data()$answers
        modrval$students <- course_data()$students
        
      })
      
      ##########################################################################
      # Selection
      
      output$slctset <- shiny::renderUI({
        shiny::req(!base::is.null(modrval$test_parameters))
        shiny::req(base::nrow(modrval$test_parameters) > 0)
        
        test_languages <- modrval$test_parameters$test_languages[1] |>
          stringr::str_split(pattern = ";", simplify = TRUE)
        
        if (base::is.character(course_paths)){
          langfile <- base::paste0(course_paths,"/languages.csv")
          exam_languages <- utils::read.csv(langfile)
        } else {
          exam_languages <- course_data()$languages
        }
        exam_languages <- exam_languages |>
          dplyr::filter(langiso %in% test_languages)
        
        shinyWidgets::radioGroupButtons(
          inputId = ns("slctexamlang"), label = NULL,
          choiceNames = base::lapply(
            base::seq_along(exam_languages$langiso), 
            function(i) shiny::tagList(
              shiny::tags$img(src = exam_languages$flag[i], width = 20, height = 15),
              exam_languages$language[i]
            )
          ),
          choiceValues = exam_languages$langiso,
          status = "primary", justified = FALSE, size = "sm",
          direction = "horizontal",
          checkIcon = base::list(yes = shiny::icon("check"))
        )
      })
      
      output$filters <- shiny::renderUI({
        shiny::req(base::length(input$focus) > 0)
        if (input$focus == "student") {
          shiny::fluidRow(
            shiny::column(
              3,
              editR::selection_ui(ns("select_student"), "Student:")
            ),
            shiny::column(
              3,
              editR::selection_ui(ns("select_attempt"), "Attempt:")
            ),
            shiny::column(
              3,
              editR::selection_ui(ns("select_version"), "Version:")
            ),
            shiny::column(
              3,
              editR::selection_ui(ns("select_question"), "Question:")
            )
          )
        } else {
          shiny::fluidRow(
            shiny::column(
              3,
              editR::selection_ui(ns("select_question"), "Question:")
            ),
            shiny::column(
              3,
              editR::selection_ui(ns("select_version"), "Version:")
            ),
            shiny::column(
              3,
              editR::selection_ui(ns("select_student"), "Student:")
            ),
            shiny::column(
              3,
              editR::selection_ui(ns("select_attempt"), "Attempt:")
            )
          )
        }
        
      })
      
      questions <- shiny::reactive({
        shiny::req(!base::is.null(modrval$selection_basis))
        shiny::req(base::nrow(modrval$selection_basis) > 0)
        #shiny::req(base::length(test()) > 1)
        shiny::req(base::length(input$focus) > 0)
        if (input$focus == "student") {
          shiny::req(!base::is.null(selected_version()))
          questions <- modrval$selection_basis |>
            dplyr::filter(version == selected_version()) |>
            dplyr::select(question) |> base::unique() |>
            base::unlist() |> base::as.character()
        } else {
          questions <- modrval$selection_basis |>
            dplyr::select(question) |> base::unique() |>
            base::unlist() |> base::as.character()
        }
        questions
      })
      selected_question <- editR::selection_server("select_question", questions)
      
      versions <- shiny::reactive({
        shiny::req(base::length(input$focus) > 0)
        shiny::req(!base::is.null(input$slctexamlang))
        if (input$focus == "student") {
          shiny::req(!base::is.null(selected_student()))
          shiny::req(!base::is.null(selected_attempt()))
          shiny::req(base::nrow(modrval$selection_basis) > 0)
          versions <- modrval$selection_basis |>
            dplyr::filter(stringr::str_detect(version, input$slctexamlang)) |>
            dplyr::filter(student == selected_student()) |>
            dplyr::filter(attempt == base::as.numeric(selected_attempt())) |>
            dplyr::select(version) |> base::unique() |>
            base::unlist() |> base::as.character()
        } else {
          shiny::req(!base::is.null(selected_question()))
          shiny::req(base::nrow(modrval$selection_basis) > 0)
          versions <- modrval$selection_basis |>
            dplyr::filter(stringr::str_detect(version, input$slctexamlang)) |>
            dplyr::filter(question == selected_question()) |>
            dplyr::select(version) |> base::unique() |>
            base::unlist() |> base::as.character()
        }
        versions
      })
      selected_version <- editR::selection_server("select_version", versions)
      
      students <- shiny::reactive({
        shiny::req(!base::is.null(modrval$selection_basis))
        shiny::req(base::nrow(modrval$selection_basis) > 0)
        shiny::req(base::length(input$focus) > 0)
        #shiny::req(base::length(test()) > 1)
        if (input$focus == "student") {
          shiny::req(!base::is.null(input$slctexamlang))
          students <- modrval$selection_basis |>
            dplyr::filter(stringr::str_detect(question, input$slctexamlang)) |>
            dplyr::select(student) |> base::unique() |>
            base::unlist() |> base::as.character()
          
        } else {
          shiny::req(!base::is.null(selected_version()))
          students <- modrval$selection_basis |>
            dplyr::filter(version == selected_version()) |>
            dplyr::select(student) |> base::unique() |>
            base::unlist() |> base::as.character()
        }
        students
      })
      selected_student <- editR::selection_server("select_student", students)
      
      attempts <- shiny::reactive({
        shiny::req(!base::is.null(modrval$selection_basis))
        shiny::req(base::nrow(modrval$selection_basis) > 0)
        shiny::req(base::length(input$focus) > 0)
        #shiny::req(base::length(test()) > 1)
        shiny::req(!base::is.null(selected_student()))
        if (input$focus == "student") {
          attempts <- modrval$selection_basis |>
            dplyr::filter(student == selected_student()) |>
            dplyr::select(attempt) |> base::unique() |>
            base::unlist() |> base::as.character()
        } else {
          attempts <- modrval$selection_basis |>
            dplyr::filter(version == selected_version()) |>
            dplyr::filter(student == selected_student()) |>
            dplyr::select(attempt) |> base::unique() |>
            base::unlist() |> base::as.character()
        }
        attempts
      })
      selected_attempt <- editR::selection_server("select_attempt", attempts)
      
      
      
      ##########################################################################
      # Answer tab
      output$viewversion <- shiny::renderUI({
        shiny::req(!base::is.null(selected_question()))
        shiny::req(!base::is.null(selected_version()))
        shiny::req(!base::is.null(selected_student()))
        shiny::req(!base::is.null(selected_attempt()))
        selected <- modrval$answers |>
          dplyr::filter(
            question == selected_question(),
            version == selected_version(),
            student == selected_student(),
            attempt == base::as.numeric(selected_attempt())
          )
        filepath <- base::paste0(modrval$test_path, "/5_examination/mdfiles/", selected_version())
        filepath <- stringr::str_replace(filepath, "Rmd$", "md")
        shiny::req(base::file.exists(filepath))
        base::suppressWarnings(
          shiny::withMathJax(shiny::HTML(knitr::knit2html(
            text = base::readLines(filepath), quiet = TRUE, template = FALSE
          )))
        )
      })
      
      output$check_answers <- shiny::renderUI({
        shiny::req(!base::is.null(selected_version()))
        shiny::req(!base::is.null(selected_student()))
        shiny::req(!base::is.null(selected_attempt()))
        shiny::req(!base::is.null(modrval$answers))
        
        typequest <- modrval$solutions |>
          dplyr::filter(version == selected_version()) |>
          dplyr::select(type) |>
          base::unlist() |>
          base::as.character() |>
          base::unique()
        
        graded_items <- modrval$answers |>
          dplyr::filter(
            version == selected_version(),
            student == selected_student(),
            attempt == selected_attempt()
          ) |>
          dplyr::left_join(base::unique(dplyr::select(
            modrval$solutions, version, number, letter, scale,
            proposition, interrogation, keywords, correct
          )), by = c("version","letter"))
        
        if (typequest %in% c("Essay","Problem")){
          graded_items <- graded_items |>
            dplyr::arrange(proposition) |>
            dplyr::select(letter, proposition, scale, checked, correct) |>
            base::unique()
        } else {
          graded_items <- graded_items |>
            dplyr::arrange(letter) |>
            dplyr::select(letter, proposition, scale, checked, correct) |>
            base::unique()
        }
        
        itemnbr <- base::nrow(graded_items)
        
        if (itemnbr < 7){
          
          ui <- base::list()
          for (i in 1:itemnbr){
            ui[[i]] <- gradR::make_scale(
              letter = graded_items$letter[i],
              proposition = graded_items$proposition[i],
              scale = graded_items$scale[i],
              checked = graded_items$checked[i],
              correct = graded_items$correct[i],
              ns
            )
          }
          
        } else{
          
          percolumn <- base::ceiling(itemnbr/3)
          
          col1 <- base::list()
          for (i in 1:percolumn){
            col1[[i]] <- gradR::make_scale(
              letter = graded_items$letter[i],
              proposition = graded_items$proposition[i],
              scale = graded_items$scale[i],
              checked = graded_items$checked[i],
              correct = graded_items$correct[i],
              ns
            )
          }
          
          beg <- percolumn+1
          end <- percolumn*2
          
          col2 <- base::list()
          for (i in beg:end){
            col2[[(i-percolumn)]] <- gradR::make_scale(
              letter = graded_items$letter[i],
              proposition = graded_items$proposition[i],
              scale = graded_items$scale[i],
              checked = graded_items$checked[i],
              correct = graded_items$correct[i],
              ns
            )
          }
          
          beg <- end+1
          
          col3 <- base::list()
          for (i in beg:itemnbr){
            col3[[(i-2*percolumn)]] <- gradR::make_scale(
              letter = graded_items$letter[i],
              proposition = graded_items$proposition[i],
              scale = graded_items$scale[i],
              checked = graded_items$checked[i],
              correct = graded_items$correct[i],
              ns
            )
          }
          
          col3[[(itemnbr+1)]] <- shiny::actionButton(
            ns("save_checks"), "Save", icon = shiny::icon("save"),
            style = "background-color:#006600;color:#FFF;width:100%;"
          )
          
          ui <- shiny::fluidRow(
            shiny::column(4, col1),
            shiny::column(4, col2),
            shiny::column(4, col3)
          )
        }
        
         ui
      })
      
      output$displayearned <- shiny::renderUI({
        shiny::req(!base::is.null(selected_question()))
        shiny::req(!base::is.null(selected_student()))
        shiny::req(!base::is.null(selected_attempt()))
        shiny::req(!base::is.null(modrval$question_grades))
        student_question_grade <- modrval$question_grades |>
          dplyr::filter(
            student == selected_student(),
            attempt == base::as.numeric(selected_attempt()),
            question == selected_question()
          ) |>
          dplyr::select(points, earned)
        shiny::req(base::nrow(student_question_grade) == 1)
        points <- student_question_grade$points[1]
        earned <- student_question_grade$earned[1]
        earned_color <- dplyr::case_when(
          earned/points >= 0.8 ~ "green",
          earned/points >= 0.6 ~ "lime",
          earned/points >= 0.5 ~ "yellow",
          earned/points >= 0.4 ~ "orange",
          earned/points >= 0.2 ~ "red",
          TRUE ~"black"
        )
        shinydashboard::valueBox(
          earned, "Points earned", icon = shiny::icon("sort-numeric-up-alt"),
          color = earned_color, width = 12
        )
      })
      
      open_answer_txt <- shiny::reactive({
        shiny::req(!base::is.null(modrval$open_answers_txt))
        shiny::req(!base::is.null(selected_version()))
        shiny::req(!base::is.null(selected_student()))
        shiny::req(!base::is.null(selected_attempt()))
        modrval$open_answers_txt |>
          dplyr::filter(
            version == selected_version(),
            student == selected_student(),
            attempt == base::as.numeric(selected_attempt())
          ) |>
          dplyr::select(text) |>
          base::unlist() |> base::as.character()
      })
      
      output$displaywordcount <- shiny::renderUI({
        shiny::req(!base::is.null(open_answer_txt()))
        shiny::req(base::length(open_answer_txt()) > 0)
        open_answer_txt() |>
          base::trimws() |>
          stringr::str_count("\\W+") |>
          base::sum() |>
          shinydashboard::valueBox(
          "Words", icon = shiny::icon("keyboard"),
          color = "blue", width = 12
        )
      })
      
      output$display_selected_answer <- shiny::renderUI({
        shiny::req(!base::is.null(open_answer_txt()))
        shiny::req(base::length(open_answer_txt()) > 0)
        shiny::req(!base::is.null(modrval$solutions))
        shiny::req(base::nrow(modrval$solutions) > 0)
        text <- base::as.character(base::unlist(open_answer_txt()))
        text <- text[(stringr::str_count(base::trimws(text), "\\W+") > 0)]
        answer_text <- base::paste0(text, collapse = "<br><br>")
        if (!base::is.null(input$slctkeywords)){
          keywords <- modrval$solutions |>
            dplyr::filter(proposition %in% input$slctkeywords) |>
            dplyr::select(keywords) |>
            base::unlist() |> base::as.character()
          keywords <- stringr::str_split(
            keywords, pattern = " ", simplify = FALSE
          ) |> base::unlist() |> base::unique()
          keywords <- keywords[!base::is.na(keywords)]
          keywords <- keywords[keywords != ""]
          for (word in keywords) {
            answer_text <- stringr::str_replace_all(
              answer_text,
              base::paste0("(?i)", word),
              base::paste0(
                '<font color="red"><b>',
                word,
                '</b></font>'
              )
            )
          }
        }
        shiny::HTML(answer_text)
      })
      
      output$keywords_selection <- shiny::renderUI({
        shiny::req(!base::is.null(modrval$solutions))
        shiny::req(base::nrow(modrval$solutions) > 0)
        shiny::req(!base::is.null(selected_version()))
        criteria <- modrval$solutions |>
          dplyr::filter(version == selected_version())
        shiny::req(criteria$type[1] %in% c("Essay","Problem"))
        criteria <- criteria |>
          dplyr::select(proposition) |>
          dplyr::arrange(proposition) |>
          base::unlist() |> base::as.character() |>
          base::unique()
        shiny::checkboxGroupInput(
          inputId = ns("slctkeywords"),
          label = "Highlight keywords for:", 
          choices = criteria, width = "100%"
        )
      })
      
      shiny::observeEvent(input$save_checks, {
        shiny::req(!base::is.null(selected_version()))
        
        if (base::is.character(course_paths)) {
          versiontype <- "Essay"
        } else {
          versiontype <- modrval$test_parameters |>
            dplyr::filter(version == selected_version()) |>
            dplyr::left_join(
              dplyr::select(course_data()$documents, question = file, type),
              by = "question"
            )
          versiontype <- versiontype$type[[1]]
        }
        
        if (!(versiontype %in% c("Essay","Problem"))){
          shinyalert::shinyalert("No change allowed!", "Answers to multiple-choice and numeric questions cannot be modified; this form is meant to be changed only to grade open-ended answers.", "error")
        } else {
          checkinput <- base::names(input)
          checkinput <- checkinput[stringr::str_detect(checkinput, "check_")]
          nbr <- base::length(checkinput)
          letters <- base::character(nbr)
          checks <- base::character(nbr)
          for (i in base::seq_len(nbr)){
            letters[i] <- stringr::str_remove(checkinput[i], "check_")
            checks[i] <- input[[checkinput[i]]]
          }
          update_answer <- tibble::tibble(
              student = selected_student(),
              attempt = selected_attempt(),
              question = selected_question(), 
              version = selected_version(),
              letter = base::as.character(letters),
              checked_chr = base::as.character(checks)
            ) |>
            dplyr::mutate(
              checked = dplyr::case_when(
                checked_chr == "1" ~ 1,
                checked_chr == "TRUE" ~ 1,
                checked_chr == "0.5" ~ 0.5,
                checked_chr == "0" ~ 0,
                checked_chr == "FALSE" ~ 0,
                checked_chr == "-0.5" ~ -0.5,
                TRUE ~ base::suppressWarnings(base::as.numeric(checked_chr))
              )
            ) |>
            dplyr::select(-checked_chr) |>
            dplyr::arrange(letter)
          
          answers <- modrval$answers |>
            dplyr::anti_join(update_answer, by = c("student","attempt","question","version")) |>
            dplyr::bind_rows(update_answer) |>
            dplyr::arrange(student, version, letter)
          
          open_answers <- answers |>
            dplyr::filter(version %in% modrval$open_answers$version) |>
            dplyr::select(student, attempt, version, letter, checked) |>
            dplyr::arrange(student, attempt, version, letter)
          
          compiled <- gradR::compile_grading(
            modrval$test_parameters,
            modrval$solutions,
            modrval$students,
            modrval$closed_answers,
            modrval$numeric_answers,
            open_answers,
            modrval$open_answers_txt,
            answers
          )
          modrval$open_answers <- compiled$open_answers
          modrval$answers <- compiled$answers
          modrval$scoring <- compiled$scoring
          modrval$results <- compiled$results
          modrval$question_grades <- compiled$question_grades
          modrval$student_grades <- compiled$student_grades
          gradR::write_compiled(compiled, modrval$test_path)
          shinyalert::shinyalert("Saved!", "Grading has been updated.", "success")
        }
      })
      
      
      
      ##########################################################################
      # Solution tab
      
      output$edit_question_parameters <- shiny::renderUI({
        shiny::req(!base::is.null(selected_question()))
        shiny::req(selected_question() %in% modrval$answers$question)
        selected <- modrval$test_parameters |>
          dplyr::filter(question == selected_question()) |>
          dplyr::select(question, points, partial_credits, penalty) |>
          base::unique()
        shiny::fluidRow(
          shiny::column(
            2,
            shiny::numericInput(
              ns("new_points"), "Points", value = selected$points[1],
              width = "100%"
            )
          ),
          shiny::column(
            1,
            shinyWidgets::switchInput(
              inputId = ns("new_partial_credits"),
              label = "Partial credits?",
              onLabel = "Yes", offLabel = "No",
              onStatus = "success", offStatus = "primary",
              value = selected$partial_credits[1]
            )
          ),
          shiny::column(
            1,
            shinyWidgets::switchInput(
              inputId = ns("new_penalty"),
              label = "Apply penalty?",
              onLabel = "Yes", offLabel = "No",
              onStatus = "danger", offStatus = "primary",
              value = selected$penalty[1]
            )
          ),
          shiny::column(
            2,
            shiny::actionButton(
              ns("update_quest_param"), "Save parameters", icon = shiny::icon("save"),
              style = "background-color:#006600;color:#FFF;width:100%;height:50px;margin-top:5px;"
            )
          ),
          shiny::column(2),
          shiny::column(
            2,
            shiny::actionButton(
              ns("update_solutions"), "Save solutions", icon = shiny::icon("save"),
              style = "background-color:#006600;color:#FFF;width:100%;height:50px;margin-top:5px;"
            )
          ),
          shiny::column(
            2,
            shiny::actionButton(
              ns("refresh_itemstats"), "Refresh item statistics", icon = shiny::icon("rotate"),
              style = "background-color:#006699;color:#FFF;width:100%;height:50px;margin-top:5px;"
            )
          )
        )
      })
      
      output$edit_solutions <- rhandsontable::renderRHandsontable({
        shiny::req(!base::is.null(selected_version()))
        shiny::req(!base::is.null(modrval$answers))
        shiny::req(!base::is.null(modrval$solutions))
        
        selected_solutions <- modrval$solutions |>
          dplyr::filter(version == selected_version())
        
        type <- base::unique(selected_solutions$type)
        
        item_metrics <- modrval$results |>
          dplyr::group_by(version, letter) |>
          dplyr::mutate(checked = base::as.numeric(checked >= 0.5)) |>
          dplyr::summarise(
            count = dplyr::n(),
            checked = base::sum(checked),
            weight = base::sum(weight),
            earned = base::sum(earned)
          ) |>
          dplyr::mutate(
            checked = base::round(checked/count, 2),
            earned = base::round(earned/weight, 2),
          ) |>
          dplyr::select(version, letter, count, checked, earned)
        
        selected_solutions <- selected_solutions |>
          dplyr::select(
            version, item, language, number, letter, document,
            modifications, proposition, value, scale, explanation,
            keywords, correct, weight
          ) |>
          dplyr::mutate(
            scale = base::factor(scale, levels = c(
              "logical","qualitative","percentage"
            ))
          ) |>
          dplyr::left_join(item_metrics, by = c("version","letter"))
        
        if (type %in% c("Essay","Problem")){
          if (base::length(stats::na.omit(selected_solutions$number)) > 0){
            lastitem <- base::max(selected_solutions$number, na.rm = TRUE) + 1
          } else {
            lastitem <- 1
          }
          new_row <- tibble::tibble(
            version = selected_version(),
            language = selected_solutions$language[1],
            number = lastitem, letter = letters[lastitem],
            modifications = 0, value = 0, correct = 0, weight = 0,
            count = 0, checked = 0, earned = 0, remove = TRUE
          )
          selected_solutions |>
            dplyr::mutate(remove = FALSE) |>
            dplyr::bind_rows(new_row) |>
            dplyr::arrange(proposition) |>
            rhandsontable::rhandsontable(
              width = "95%", rowHeaders = NULL, stretchH = "all"
            ) |>
            rhandsontable::hot_col(c(1,2,3,4,5,15,16,17), readOnly = TRUE) |>
            rhandsontable::hot_cols(
              colWidths = c(
                "5%","5%","3%","3%","2%","5%","3%","15%",
                "3%","7%","21%","10%","3%","3%",
                "3%","3%","3%","3%"
              ),
              manualColumnResize = TRUE
            ) |>
            rhandsontable::hot_context_menu(
              allowRowEdit = FALSE, allowColEdit = FALSE
            )
        } else {
          selected_solutions |>
            dplyr::arrange(letter) |>
            rhandsontable::rhandsontable(
              width = "95%", rowHeaders = NULL, stretchH = "all"
            ) |>
            rhandsontable::hot_col(c(1,2,3,4,5,7,8,10,15,16,17), readOnly = TRUE) |>
            rhandsontable::hot_cols(
              colWidths = c(
                "5%","5%","3%","3%","2%","5%","3%","15%",
                "3%","7%","21%","10%","3%","3%",
                "3%","3%","3%"
              ),
              manualColumnResize = TRUE
            ) |>
            rhandsontable::hot_context_menu(
              allowRowEdit = FALSE, allowColEdit = FALSE
            )
        }
      })
      
      shiny::observeEvent(input$update_quest_param, {
        shiny::req(!base::is.null(selected_question()))
        test_parameters <- modrval$test_parameters
        test_parameters$points[stringr::str_detect(
          test_parameters$question, selected_question())] <- input$new_points
        test_parameters$partial_credits[stringr::str_detect(
          test_parameters$question, selected_question())] <- input$new_partial_credits
        test_parameters$penalty[stringr::str_detect(
          test_parameters$question, selected_question())] <- input$new_penalty
        compiled <- gradR::compile_grading(
          test_parameters,
          modrval$solutions,
          modrval$students,
          modrval$closed_answers,
          modrval$numeric_answers,
          modrval$open_answers,
          modrval$open_answers_txt,
          modrval$answers,
          parameter_change = selected_question()
        )
        modrval$test_parameters <- compiled$test_parameters
        modrval$scoring <- compiled$scoring
        modrval$results <- compiled$results
        modrval$question_grades <- compiled$question_grades
        modrval$student_grades <- compiled$student_grades
        gradR::write_compiled(compiled, modrval$test_path, TRUE, FALSE)
        shinyalert::shinyalert("Saved!", "Question parameters have been updated.", "success")
      })
      
      shiny::observeEvent(input$update_solutions, {
        shiny::req(!base::is.null(selected_question()))
        shiny::req(!base::is.null(selected_version()))
        shiny::req(!base::is.null(modrval$solutions))
        shiny::req(!base::is.null(input$edit_solutions))
        unchanged_solutions <- modrval$solutions |>
          dplyr::filter(version != selected_version())
        edited_solutions <- modrval$solutions |>
          dplyr::filter(version == selected_version()) |>
          dplyr::select(path, test, type, interrogation) |>
          base::unique()
        edition <- rhandsontable::hot_to_r(input$edit_solutions) |>
          dplyr::mutate_if(base::is.factor, base::as.character)
        
        if (!("remove" %in% base::names(edition))) edition$remove <- FALSE
        
        edition <- edition |>
          dplyr::select(
            version, document, language,
            item, number, letter,
            modifications, proposition, value, scale,
            explanation, keywords, correct, weight, remove
          ) |>
          dplyr::mutate(
            path = edited_solutions$path[1],
            test = edited_solutions$test[1],
            type = edited_solutions$type[1],
            interrogation = edited_solutions$interrogation[1]
          ) |>
          dplyr::filter(remove == FALSE) |>
          dplyr::select(base::names(unchanged_solutions))
        solutions <- dplyr::bind_rows(
          unchanged_solutions,
          edition
        )
        compiled <- gradR::compile_grading(
          modrval$test_parameters,
          solutions,
          modrval$students,
          modrval$closed_answers,
          modrval$numeric_answers,
          modrval$open_answers,
          modrval$open_answers_txt,
          modrval$answers
        )
        modrval$solutions <- compiled$solutions
        modrval$scoring <- compiled$scoring
        modrval$results <- compiled$results
        modrval$question_grades <- compiled$question_grades
        modrval$student_grades <- compiled$student_grades
        gradR::write_compiled(compiled, modrval$test_path, FALSE, TRUE)
        shinyalert::shinyalert("Saved!", "Solutions have been updated.", "success")
      })
      
      
      
      ##########################################################################
      # Results tab
      
      shiny::observe({
        shiny::req(!base::is.null(modrval$student_grades))
        shiny::updateNumericInput(
          session,
          "defpass",
          value = (0.5*base::max(modrval$student_grades$points))
        )
      })
      
      output$testmetrics <- shiny::renderUI({
        shiny::req(!base::is.null(modrval$student_grades))
        shiny::req(!base::is.null(input$defpass))
        count <- base::length(base::unique(modrval$student_grades$student))
        minimum <- base::round(base::min(stats::na.omit(modrval$student_grades$grade)),2)
        ave <- base::round(base::mean(stats::na.omit(modrval$student_grades$grade)),2)
        med <- base::round(stats::median(stats::na.omit(modrval$student_grades$grade)),2)
        maximum <- base::round(base::max(stats::na.omit(modrval$student_grades$grade)),2)
        range <- minimum-minimum
        stddev <- base::round(stats::sd(stats::na.omit(modrval$student_grades$grade)),2)
        passrate <- base::round(100*base::mean(stats::na.omit(modrval$student_grades$grade) >= input$defpass),2)
        base::list(
          shinydashboard::valueBox(count, "Students", shiny::icon("users"), "maroon", width = 2),
          shinydashboard::valueBox(minimum, "Minimum", shiny::icon("sort-down"), "red", width = 1),
          shinydashboard::valueBox(ave, "Mean", shiny::icon("ruler-horizontal"), "orange", width = 2),
          shinydashboard::valueBox(med, "Median", shiny::icon("circle-half-stroke"), "yellow", width = 2),
          shinydashboard::valueBox(maximum, "Maximum", shiny::icon("sort-up"), "green", width = 1),
          shinydashboard::valueBox(stddev, "Standard deviation", shiny::icon("arrows-left-right"), "navy", width = 2),
          shinydashboard::valueBox(passrate, "Pass", shiny::icon("percent"), "purple", width = 2)
        )
      })
      
      output$testdistribution <- shiny::renderPlot({
        shiny::req(!base::is.null(modrval$student_grades))
        shiny::req(!base::is.null(input$defpass))
        shiny::req(!base::is.null(input$defhistbreaks))
        chartR::draw_grade_distribution(modrval$student_grades, input$defpass, input$defhistbreaks)
      })
      
    }
  )
}
