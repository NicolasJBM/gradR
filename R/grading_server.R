#' @name grading_server
#' @title Grade tests.
#' @author Nicolas Mangin
#' @description Module allowing the user to grade and calibrate tests.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param test Reactive.. Selected test.
#' @param tree Reactive. Function containing a list of documents as a classification tree compatible with jsTreeR.
#' @param course_data Reactive. Function containing all the course data loaded with the course.
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @return Save the test results in the relevant test sub-folder in the folder "5_tests".
#' @importFrom chartR display_curve
#' @importFrom dplyr anti_join
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_if
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom editR make_infobox
#' @importFrom editR selection_server
#' @importFrom knitr knit2html
#' @importFrom purrr map
#' @importFrom readr read_csv
#' @importFrom rhandsontable hot_col
#' @importFrom rhandsontable hot_cols
#' @importFrom rhandsontable hot_context_menu
#' @importFrom rhandsontable hot_to_r
#' @importFrom rhandsontable renderRHandsontable
#' @importFrom rhandsontable rhandsontable
#' @importFrom shiny actionButton
#' @importFrom shiny checkboxGroupInput
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny HTML
#' @importFrom shiny icon
#' @importFrom shiny isolate
#' @importFrom shiny moduleServer
#' @importFrom shiny NS
#' @importFrom shiny numericInput
#' @importFrom shiny observe
#' @importFrom shiny observeEvent
#' @importFrom shiny reactive
#' @importFrom shiny reactiveValues
#' @importFrom shiny renderPlot
#' @importFrom shiny renderUI
#' @importFrom shiny req
#' @importFrom shiny withMathJax
#' @importFrom shinyalert shinyalert
#' @importFrom shinybusy remove_modal_spinner
#' @importFrom shinybusy show_modal_spinner
#' @importFrom shinydashboard valueBox
#' @importFrom shinyWidgets switchInput
#' @importFrom stats as.formula
#' @importFrom stats na.omit
#' @importFrom stringr str_detect
#' @importFrom stringr str_remove
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_split
#' @importFrom teachR statistics_assign_colors
#' @importFrom teachR statistics_get_parameters
#' @importFrom tibble tibble
#' @importFrom tidyr separate
#' @importFrom tidyr unite
#' @importFrom tidyr unnest
#' @export



grading_server <- function(id, test, tree, course_data, course_paths){
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
      
      ##########################################################################
      # Loading
      
      modrval <- shiny::reactiveValues()
      
      shiny::observe({
        
        test_name <- test()$test[1]
        test_path <- base::paste0(course_paths()$subfolders$tests, "/", test_name)
        parameters_path <- base::paste0(test_path, "/test_parameters.RData")
        
        # Test parameters
        shiny::req(base::file.exists(parameters_path))
        base::load(parameters_path)
        
        shiny::req(base::nrow(stats::na.omit(test_parameters)) > 0)
        
        # Solutions
        solutions <- base::list.files(base::paste0(
          test_path, "/4_solutions"
        ), full.names = TRUE)
        if (base::length(solutions) > 0){
          solutions <- tibble::tibble(
            path = solutions
          ) |>
            dplyr::mutate(solutions = purrr::map(path, function(x){
              readr::read_csv(x, col_types = "ccncccccnccncccnn")
            })) |>
            tidyr::unnest(solutions)
        } else solutions <- NA
        
        # Students
        students <- base::paste0(
          test_path, "/6_students/student_list.csv"
        )
        if (base::file.exists(students)){
          students <- readr::read_csv(students, col_types = "ccccc") |>
            dplyr::mutate(enrolled = 1)
        } else {
          students <- tibble::tibble(
            student = base::character(0), team = base::character(0),
            firstname = base::character(0), lastname = base::character(0),
            email = base::character(0), enrolled = base::numeric(0)
          )
        }
        
        # Raw answers
        closed_answers <- base::paste0(
          test_path, "/7_answers/closed.csv"
        )
        if (base::file.exists(closed_answers)){
          closed_answers <- readr::read_csv(closed_answers, col_types = "ccncc")
        } else {
          closed_answers <- tibble::tibble(
            student = base::character(0),
            test = base::character(0),
            attempt = base::numeric(0),
            version = base::character(0),
            letter = base::character(0)
          )
        }
        
        numeric_answers <- base::paste0(
          test_path, "/7_answers/numeric.csv"
        )
        if (base::file.exists(numeric_answers)){
          numeric_answers <- readr::read_csv(numeric_answers, col_types = "ccncn")
        } else {
          numeric_answers <- tibble::tibble(
            student = base::character(0),
            test = base::character(0),
            attempt = base::numeric(0),
            version = base::character(0),
            proposition = base::numeric(0)
          )
        }
        
        open_answers <- base::list.files(base::paste0(
          test_path, "/7_answers/open"
        ), full.names = FALSE)
        if (base::length(open_answers) > 0){
          open_answers <- tibble::tibble(
            file_name = open_answers
          ) |>
            dplyr::mutate(file_path = base::paste0(
              test_path, "/7_answers/open/", file_name
            )) |>
            dplyr::mutate(
              test = test_name,
              text = purrr::map(
                file_path,
                readr::read_lines, locale = readr::locale(encoding = "Latin1")
              )
            ) |>
            tidyr::separate(file_name, into = c("record","version"), sep = "_") |>
            tidyr::separate(record, into = c("student","attempt"), sep = "-") |>
            dplyr::mutate(
              attempt = base::as.numeric(attempt),
              version = stringr::str_remove_all(version, ".txt")
            ) |>
            dplyr::select(student, test, attempt, version, text)
        } else {
          open_answers <- tibble::tibble(
            student = base::character(0),
            test = base::character(0),
            attempt = base::numeric(0),
            version = base::character(0),
            text = base::numeric(0)
          )
        }
        
        # Compiled answers
        answers <- base::paste0(
          test_path, "/8_results/answers.csv"
        )
        if (base::file.exists(answers)){
          answers <- readr::read_csv(answers, col_types = "cccccn")
        } else {
          answers <- tibble::tibble(
            student = base::character(0),
            test = base::character(0),
            question = base::character(0),
            version = base::character(0),
            letter = base::character(0),
            checked = base::numeric(0)
          )
        }
        
        # Results
        results <- base::paste0(
          test_path, "/8_results/results.csv"
        )
        if (base::file.exists(results)){
          results <- readr::read_csv(results, col_types = "ccnccnccccllnnnn")
        } else {
          results <- tibble::tibble(
            student = base::character(0),
            test = base::character(0),
            attempt = base::numeric(0),
            question = base::character(0),
            version = base::character(0),
            number = base::numeric(0),
            letter = base::character(0),
            item = base::character(0),
            language = base::character(0),
            scale = base::character(0),
            partial_credits = base::logical(0),
            penalty = base::logical(0),
            points = base::numeric(0),
            checked = base::numeric(0),
            weight = base::numeric(0),
            earned = base::numeric(0)
          )
        }
        
        shiny::req(
          (base::nrow(stats::na.omit(closed_answers)) +
            base::nrow(stats::na.omit(numeric_answers)) +
            base::nrow(stats::na.omit(open_answers))) > 0
        )
        
        compiled <- gradR::compile_grading(
          test_parameters,
          solutions,
          students,
          closed_answers,
          numeric_answers,
          open_answers,
          answers
        )
        
        modrval$test_name <- test_name
        modrval$test_path <- test_path
        modrval$test_parameters <- compiled$test_parameters
        modrval$solutions <- compiled$solutions
        modrval$students <- compiled$students
        modrval$closed_answers <- compiled$closed_answers
        modrval$numeric_answers <- compiled$numeric_answers
        modrval$open_answers <- compiled$open_answers
        modrval$answers <- compiled$answers
        modrval$scoring <- compiled$scoring
        modrval$results <- compiled$results
        modrval$question_grades <- compiled$question_grades
        modrval$student_grades <- compiled$student_grades
        
        modrval$selection_basis <- compiled$answers |>
          dplyr::select(student, question, version) |>
          base::unique()
        
        gradR::write_compiled(compiled, test_path)
        
        shinyWidgets::updateSwitchInput(
          session,
          "studentfocus",
          value = FALSE
        )
        
      })
      
      
      ##########################################################################
      # Selection
      
      output$filters <- shiny::renderUI({
        shiny::req(base::length(input$focus) > 0)
        if (input$focus == "student") {
          shiny::fluidRow(
            shiny::column(
              4,
              editR::selection_ui(ns("select_student"), "Student-attempt:")
            ),
            shiny::column(
              4,
              editR::selection_ui(ns("select_version"), "Version:")
            ),
            shiny::column(
              4,
              editR::selection_ui(ns("select_question"), "Question:")
            )
          )
        } else {
          shiny::fluidRow(
            shiny::column(
              4,
              editR::selection_ui(ns("select_question"), "Question:")
            ),
            shiny::column(
              4,
              editR::selection_ui(ns("select_version"), "Version:")
            ),
            shiny::column(
              4,
              editR::selection_ui(ns("select_student"), "Student-attempt:")
            )
          )
        }
        
      })
      
      questions <- shiny::reactive({
        shiny::req(!base::is.null(modrval$selection_basis))
        shiny::req(base::nrow(modrval$selection_basis) > 0)
        shiny::req(base::length(test()) > 1)
        shiny::req(base::length(input$focus) > 0)
        if (input$focus == "student") {
          shiny::req(!base::is.null(selected_version()))
          questions <- modrval$selection_basis |>
            dplyr::filter(version %in% selected_version()) |>
            dplyr::select(question) |> base::unique() |>
            base::unlist() |> base::as.character()
        } else {
          questions <- base::unique(modrval$selection_basis$question)
        }
        questions
      })
      
      selected_question <- editR::selection_server("select_question", questions)
      
      versions <- shiny::reactive({
        shiny::req(base::length(input$focus) > 0)
        if (input$focus == "student") {
          shiny::req(!base::is.null(selected_student()))
          shiny::req(base::nrow(modrval$selection_basis) > 0)
          versions <- modrval$selection_basis |>
            dplyr::filter(student == selected_student()) |>
            dplyr::select(version) |> base::unique() |>
            base::unlist() |> base::as.character()
        } else {
          shiny::req(!base::is.null(selected_question()))
          shiny::req(base::nrow(modrval$selection_basis) > 0)
          versions <- modrval$selection_basis |>
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
        shiny::req(base::length(test()) > 1)
        if (input$focus == "student") {
          students <- base::unique(modrval$selection_basis$student)
        } else {
          shiny::req(!base::is.null(selected_version()))
          students <- modrval$selection_basis |>
            dplyr::filter(version %in% selected_version()) |>
            dplyr::select(student) |> base::unique() |>
            base::unlist() |> base::as.character()
        }
        students
      })
      
      selected_student <- editR::selection_server("select_student", students)
      
      
      
      ##########################################################################
      # Answer tab
      output$viewversion <- shiny::renderUI({
        shiny::req(!base::is.null(selected_question()))
        shiny::req(!base::is.null(selected_version()))
        shiny::req(!base::is.null(selected_student()))
        selected <- modrval$answers |>
          dplyr::filter(
            question == selected_question(),
            version == selected_version(),
            student == selected_student()
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
        shiny::req(!base::is.null(modrval$answers))
        options <- modrval$answers |>
          dplyr::filter(
            version == selected_version(),
            student == selected_student()
          ) |>
          dplyr::left_join(base::unique(dplyr::select(
            modrval$solutions, version, number, letter, scale,
            proposition, interrogation, keywords, correct
          )), by = c("version","letter")) |>
          dplyr::arrange(letter) |>
          dplyr::select(letter, proposition, scale, checked, correct) |>
          base::unique()
        ui <- base::list()
        for (i in base::seq_len(base::nrow(options))){
          ui[[i]] <- gradR::make_scale(
            letter = options$letter[i],
            proposition = options$proposition[i],
            scale = options$scale[i],
            checked = options$checked[i],
            correct = options$correct[i],
            ns
          )
        }
        ui
      })
      
      output$displayearned <- shiny::renderUI({
        shiny::req(!base::is.null(selected_question()))
        shiny::req(!base::is.null(selected_student()))
        shiny::req(!base::is.null(modrval$question_grades))
        student_question_grade <- modrval$question_grades |>
          dplyr::filter(
            student == selected_student(),
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
      
      open_answer <- shiny::reactive({
        shiny::req(!base::is.null(modrval$open_answers))
        shiny::req(!base::is.null(selected_version()))
        shiny::req(!base::is.null(selected_student()))
        modrval$open_answers |>
          tidyr::unite(student, student, attempt, sep = "-") |>
          dplyr::filter(
            version == selected_version(),
            student == selected_student()
          ) |>
          dplyr::select(text) |>
          base::unlist() |> base::as.character()
      })
      
      output$displaywordcount <- shiny::renderUI({
        shiny::req(!base::is.null(open_answer()))
        shiny::req(base::length(open_answer()) > 0)
        open_answer() |>
          base::trimws() |>
          stringr::str_count("\\W+") |>
          base::sum() |>
          shinydashboard::valueBox(
          "Words", icon = shiny::icon("keyboard"),
          color = "blue", width = 12
        )
      })
      
      output$display_selected_answer <- shiny::renderUI({
        shiny::req(!base::is.null(open_answer()))
        shiny::req(base::length(open_answer()) > 0)
        shiny::req(!base::is.null(modrval$solutions))
        shiny::req(base::nrow(modrval$solutions) > 0)
        text <- base::as.character(base::unlist(open_answer()))
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
          versiontype <- modrval$test_parameters |>
            dplyr::filter(version == selected_version()) |>
            dplyr::left_join(
              dplyr::select(course_data()$documents, question = file, type),
              by = "question"
            )
          versiontype <- versiontype$type[[1]]
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
                test = modrval$test_name,
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
                  TRUE ~ 0
                )
              ) |>
              dplyr::select(-checked_chr) |>
              dplyr::arrange(letter)
            answers <- modrval$answers |>
              dplyr::anti_join(update_answer, by = c("student","test","question","version")) |>
              dplyr::bind_rows(update_answer) |>
              dplyr::arrange(student, version, letter)
            compiled <- gradR::compile_grading(
              modrval$test_parameters,
              modrval$solutions,
              modrval$students,
              modrval$closed_answers,
              modrval$numeric_answers,
              modrval$open_answers,
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
      # Computation of statistics
      
      basis_for_main_statistics <- shiny::reactive({
        shiny::req(!base::is.null(modrval$results))
        modrval$results |>
          tidyr::unite("observation", student, attempt, sep = "-") |>
          dplyr::select(observation, code = question, points, earned) |>
          dplyr::group_by(observation, code) |>
          dplyr::summarise(
            points = base::max(points),
            earned = base::sum(earned),
            .groups = "drop"
          ) |>
          dplyr::mutate(
            correct = base::as.numeric(earned > 0.5 * points)
          ) |>
          dplyr::select(observation, code, earned, correct) |>
          tidyr::replace_na(base::list(earned = 0, correct = 0))
      })
      
      question_statistics <- shiny::reactive({
        shiny::req(!base::is.null(basis_for_main_statistics()))
        shiny::req(base::length(base::unique(basis_for_main_statistics()$code) >= 3))
        shinybusy::show_modal_spinner(
          spin = "orbit",
          text = "Please wait while question statistics are computed..."
        )
        questats <- basis_for_main_statistics() |>
          dplyr::select(-earned) |>
          teachR::statistics_get_parameters(
            model_formula = "correct ~ success + proficiency",
            minobs = 10
          )
        shinybusy::remove_modal_spinner()
        questats
      })
      
      question_data <- shiny::reactive({
        shiny::req(!base::is.null(question_statistics()))
        document_parameters <- question_statistics()$parameters |>
          teachR::statistics_assign_colors(type = "questions") |>
          dplyr::rename(file = code)
        base::list(document_parameters = document_parameters)
      })
      
      item_statistics <- shiny::reactive({
        shiny::req(!base::is.null(input$refresh_itemstats))
        input$refresh_itemstats
        shiny::isolate({
          shiny::req(!base::is.null(modrval$results))
          prelimcheck <- modrval$results |>
            dplyr::select(student, question, item) |>
            stats::na.omit() |>
            base::unique()
          nbrstudents <- base::length(base::unique(prelimcheck$student))
          nbritems <- base::nrow(prelimcheck)
          shiny::req(nbrstudents >= 10)
          shiny::req(nbritems >= 3)
          shinybusy::show_modal_spinner(
            spin = "orbit",
            text = "Please wait while item statistics are computed..."
          )
          itemstats <- modrval$results |>
            tidyr::unite("observation", student, attempt, sep = "-") |>
            tidyr::unite("code", item, language, sep = "_") |>
            dplyr::select(observation, code, checked, weight, earned) |>
            dplyr::mutate(
              correct = dplyr::case_when(
                checked == 0 & weight <= 0 ~ 1,
                checked == 1 & weight > 0 ~ 1,
                TRUE ~ 0
              )
            ) |>
            dplyr::select(observation, code, correct) |>
            teachR::statistics_get_parameters(
              model = stats::as.formula("correct ~ success + proficiency"),
              minobs = 10
            )
        })
        shinybusy::remove_modal_spinner()
        itemstats
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
        
        # To get frequencies of answers
        shiny::isolate({
          shiny::req(!base::is.null(modrval$answers))
          shiny::req(!base::is.null(selected_student()))
          selected_version <- modrval$answers |>
            dplyr::filter(
              student == selected_student(),
              version == selected_version()
            )
          selected_version <- selected_version$version[1]
        })
        frequencies <- modrval$answers |>
          dplyr::group_by(version, letter) |>
          dplyr::summarise(frequency = base::sum(checked))
        
        # To get item statistics
        if (!base::is.null(item_statistics())) {
          show_item_statistics <- item_statistics()$parameters |>
            tidyr::separate(code, into = c("item","language"), sep = "_") |>
            dplyr::select(item, language, success, discrimination)
        } else {
          show_item_statistics <- tibble::tibble(
            item = base::character(0),
            language = base::character(0),
            success = base::numeric(0),
            discrimination = base::numeric(0)
          )
        }
        
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
          dplyr::left_join(frequencies, by = c("version","letter")) |>
          dplyr::left_join(show_item_statistics, by = c("item","language"))
        
        
        if (type %in% c("Essay","Problem")){
          if (base::length(stats::na.omit(selected_solutions$number)) > 0){
            lastitem <- base::max(selected_solutions$number, na.rm = TRUE) + 1
          } else {
            lastitem <- 1
          }
          new_row <- tibble::tibble(
            version = selected_version,
            language = selected_solutions$language[1],
            number = lastitem, letter = letters[lastitem],
            modifications = 0, value = 0, correct = 0, weight = 0,
            frequency = 0, success = 0, discrimination = 0, remove = TRUE
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
      # Diagnostics tab
      
      allscores <- shiny::reactive({
        shiny::req(!base::is.null(basis_for_main_statistics()))
        
        shinybusy::show_modal_spinner(
          spin = "orbit",
          text = "Please wait while additional metrics are computed..."
        )
        grades <- dplyr::select(modrval$student_grades, student, grade) |>
          dplyr::arrange(student)
        
        percentage <- basis_for_main_statistics() |>
          dplyr::select(student = observation, correct) |>
          dplyr::group_by(student) |>
          dplyr::summarise(percentage = base::mean(correct, na.rm = TRUE), .groups = "drop") |>
          dplyr::arrange(student)
        
        fa_basis <- basis_for_main_statistics() |>
          dplyr::select(-correct) |>
          dplyr::arrange(observation) |>
          stats::na.omit() |>
          tidyr::pivot_wider(names_from = "code", values_from = "earned", values_fill = 0) |>
          tibble::column_to_rownames("observation") |>
          stats::na.omit()
        
        fa_basis <- fa_basis[,(base::apply(fa_basis, 2, stats::sd) != 0)]
        
        factana <- psych::fa(fa_basis)
        fa_scores <- tibble::tibble(
          student = base::row.names(fa_basis),
          factor = factana$scores[,1]
        )
        
        allscores <- tibble::rownames_to_column(fa_basis, "student") |>
          dplyr::left_join(grades, by = "student") |>
          dplyr::left_join(percentage, by = "student") |>
          dplyr::left_join(fa_scores, by = "student") |>
          tibble::column_to_rownames("student") |>
          dplyr::mutate_all(function(x) base::replace(x, base::is.na(x), 0))
        
        shinybusy::remove_modal_spinner()
        
        allscores
      })
      
      output$diagnostics <- shiny::renderUI({
        shiny::req(base::length(input$focus) > 0)
        
        if (input$focus == "student"){
          base::list(
            shiny::fluidRow(
              shiny::column(
                2,
                shiny::uiOutput(ns("parameters"))
              ),
              shiny::column(
                4,
                shiny::plotOutput(ns("density"), brush = ns("density_brush"))
              ),
              shiny::column(
                4,
                shiny::plotOutput(ns("score_differences"))
              ),
              shiny::column(
                2,
                shiny::dataTableOutput(ns("selection"))
              )
            )
          )
        } else {
          base::list(
            shiny::fluidRow(
              shiny::column(
                2,
                shiny::tags$h4("Statistics for all questions")
              ),
              shiny::column(
                4,
                shiny::plotOutput(ns("question_map"))
              ),
              shiny::column(
                4,
                shiny::plotOutput(ns("question_correlations"))
              ),
              shiny::column(2)
            ),
            shiny::tags$hr(),
            shiny::fluidRow(
              shiny::column(
                2,
                shiny::tags$h4(base::paste0("Statistics for ", selected_question()))
              ),
              shiny::column(
                4,
                shiny::uiOutput(ns("question_infoboxes"))
              ),
              shiny::column(
                4,
                shiny::plotOutput(ns("question_curve"))
              ),
              shiny::column(2)
            )
          )
        }
      })
      
      # Question centric diagnostics
      
      output$question_map <- shiny::renderPlot({
        shiny::req(!base::is.null(question_statistics()))
        question_statistics()$parameters |>
          dplyr::select(question = code, success, discrimination, guess) |>
          chartR::draw_composition_scatterplot()
      })
      
      output$question_correlations <- shiny::renderPlot({
        shiny::req(!base::is.null(allscores()))
        allscores() |>
          dplyr::select(dplyr::starts_with("Q")) |>
          chartR::draw_correlogram()
      })
      
      output$question_infoboxes <- shiny::renderUI({
        shiny::req(!base::is.null(selected_question()))
        shiny::req(!base::is.null(question_data()))
        editR::make_infobox(question_data, selected_question(), "results")
      })
      
      output$question_curve <- shiny::renderPlot({
        shiny::req(!base::is.null(question_statistics()))
        shiny::req(!base::is.null(selected_question()))
        shiny::req(selected_question() %in% question_statistics()$models$code)
        selected_model <- question_statistics()$models |>
          dplyr::filter(code == selected_question())
        chartR::display_curve(selected_model$data[[1]])
      })
      
      # Student centric diagnostics
      
      output$parameters <- shiny::renderUI({
        shiny::req(!base::is.null(allscores()))
        base::list(
          shiny::selectInput(
            ns("slctx"), "Select the x axis:",
            choices = base::names(allscores()),
            selected = "factor"
          ),
          shiny::selectInput(
            ns("slcty"), "Select the y axis:",
            choices = base::names(allscores()),
            selected = "grade"
          ),
          shiny::sliderInput(
            ns("slctbkgalpha"), "Background opacity:",
            value = 0.4, min = 0, max = 1, step = 0.01
          ),
          shiny::sliderInput(
            ns("slctpntalpha"), "Points opacity:",
            value = 0.1, min = 0, max = 1, step = 0.01
          ),
          shiny::sliderInput(
            ns("slctpntsize"), "Points size:",
            value = 4, min = 0.5, max = 10, step = 0.5
          )
        )
      })
      
      output$density <- shiny::renderPlot({
        shiny::req(!base::is.null(input$slctx))
        shiny::req(!base::is.null(input$slcty))
        shiny::req(!base::is.null(input$slctbkgalpha))
        shiny::req(!base::is.null(input$slctpntalpha))
        shiny::req(!base::is.null(input$slctpntsize))
        allscores() |>
          chartR::draw_density_plot(
          input$slctx, input$slcty,
          input$slctbkgalpha, input$slctpntalpha, input$slctpntsize
        )
      })
      
      brushed_students <- shiny::reactive({
        shiny::req(!base::is.null(allscores()))
        shiny::req(!base::is.null(input$density_brush))
        scores <- allscores()[,c(input$slctx, input$slcty)]
        base::names(scores) <- c("x","y")
        brushed <- scores |>
          tibble::rownames_to_column("student") |>
          shiny::brushedPoints(input$density_brush)
        base::unique(brushed$student)
      })
      
      output$score_differences <- shiny::renderPlot({
        shiny::req(!base::is.null(allscores()))
        shiny::req(!base::is.null(brushed_students()))
        chartR::draw_score_differences(allscores(), brushed_students())
      })
      
      output$selection <- shiny::renderDataTable({
        shiny::req(!base::is.null(allscores()))
        shiny::req(!base::is.null(brushed_students()))
        allscores() |>
          tibble::rownames_to_column("student") |>
          dplyr::filter(student %in% brushed_students()) |>
          dplyr::select(student, grade)
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
