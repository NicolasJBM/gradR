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
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_if
#' @importFrom dplyr select
#' @importFrom dplyr slice_sample
#' @importFrom dplyr starts_with
#' @importFrom dplyr summarise
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_histogram
#' @importFrom ggplot2 ggplot
#' @importFrom knitr knit2html
#' @importFrom purrr map
#' @importFrom purrr map2
#' @importFrom readr read_csv
#' @importFrom rhandsontable hot_col
#' @importFrom rhandsontable hot_cols
#' @importFrom rhandsontable hot_context_menu
#' @importFrom rhandsontable hot_to_r
#' @importFrom rhandsontable renderRHandsontable
#' @importFrom rhandsontable rhandsontable
#' @importFrom shiny actionButton
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
#' @importFrom shiny selectInput
#' @importFrom shiny sliderInput
#' @importFrom shiny withMathJax
#' @importFrom shinyalert shinyalert
#' @importFrom shinydashboard valueBox
#' @importFrom shinyWidgets progressBar
#' @importFrom shinyWidgets switchInput
#' @importFrom stats as.formula
#' @importFrom stats na.omit
#' @importFrom stats sd
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_split
#' @importFrom teachR statistics_get_parameters
#' @importFrom tibble tibble
#' @importFrom tidyr nest
#' @importFrom tidyr separate
#' @importFrom tidyr unite
#' @importFrom tidyr unnest
#' @importFrom editR selection_server
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
      
      modrval <- shiny::reactiveValues()
      
      ##########################################################################
      # Load data from selected test folder
      
      shiny::observe({
        
        test_name <- test()$test[1]
        test_path <- base::paste0(course_paths()$subfolders$tests, "/", test_name)
        parameters_path <- base::paste0(test_path, "/test_parameters.RData")
        
        # Test parameters
        shiny::req(base::file.exists(parameters_path))
        base::load(parameters_path)
        
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
        
        # Raw answers
        closed_answers <- base::paste0(
          test_path, "/7_answers/closed.csv"
        )
        if (base::file.exists(closed_answers)){
          closed_answers <- readr::read_csv(closed_answers, col_types = "ccnccn")
        } else {
          closed_answers <- tibble::tibble(
            student = base::character(0),
            test = base::character(0),
            attempt = base::numeric(0),
            version = base::character(0),
            letter = base::character(0),
            success = base::numeric(0)
          )
        }
        
        numeric_answers <- base::paste0(
          test_path, "/7_answers/numeric.csv"
        )
        if (base::file.exists(numeric_answers)){
          numeric_answers <- readr::read_csv(numeric_answers, col_types = "ccncnn")
        } else {
          numeric_answers <- tibble::tibble(
            student = base::character(0),
            test = base::character(0),
            attempt = base::numeric(0),
            version = base::character(0),
            proposition = base::numeric(0),
            success = base::numeric(0)
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
              text = purrr::map(file_path, base::readLines)
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
          results <- readr::read_csv(results, col_types = "cccccn")
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
            partial_credits = base::numeric(0),
            penalty = base::numeric(0),
            points = base::numeric(0),
            checked = base::numeric(0),
            weight = base::numeric(0),
            earned = base::numeric(0)
          )
        }
        
        compiled <- compile_grading(
          test_parameters,
          solutions,
          students,
          closed_answers,
          numeric_answers,
          open_answers,
          answers
        )
        
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
        
        write_compiled(compiled, test_path)
        
      })
      
      questions <- shiny::reactive({
        shiny::req(base::length(test()) > 1)
        shiny::req(!base::is.null(modrval$answers))
        shiny::req(base::nrow(modrval$answers) > 0)
        base::unique(modrval$answers$question)
      })
      
      selected_question <- editR::selection_server("select_question", questions)
      
      versions <- shiny::reactive({
        shiny::req(!base::is.null(selected_question()))
        shiny::req(base::nrow(modrval$answers) > 0)
        modrval$answers |>
          dplyr::filter(question == selected_question()) |>
          dplyr::select(version) |> base::unique() |>
          base::unlist() |> base::as.character()
      })
      
      selected_version <- editR::selection_server("select_version", versions)
      
      students <- shiny::reactive({
        shiny::req(!base::is.null(selected_version()))
        shiny::req(base::nrow(modrval$answers) > 0)
        modrval$answers |>
          dplyr::filter(version == selected_version()) |>
          dplyr::select(student) |> base::unique() |>
          base::unlist() |> base::as.character()
      })
      
      selected_student <- editR::selection_server("select_student", students)
      
      output$viewversion <- shiny::renderUI({
        shiny::req(!base::is.null(selected_version()))
        shiny::req(!base::is.null(selected_student()))
        selected <- modrval$answers |>
          dplyr::filter(
            version == selected_version(),
            student == selected_student()
          )
        filepath <- base::paste0(modrval$test_path, "/2_versions/", selected_version())
        test_parameters <- modrval$test_parameters
        base::load(course_paths()$databases$propositions)
        base::load(course_paths()$databases$translations)
        as_latex <- FALSE
        record_solution <- FALSE
        base::suppressWarnings(
          shiny::withMathJax(shiny::HTML(knitr::knit2html(
            text = base::readLines(filepath),
            fragment.only = TRUE, quiet = TRUE
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
          dplyr::arrange(number) |>
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
        wordcount <- base::sum(base::lengths(
          base::gregexpr("\\W+", base::paste(open_answer()))
        )+1)
        shinydashboard::valueBox(
          wordcount, "Words", icon = shiny::icon("keyboard"),
          color = "blue", width = 12
        )
      })
      
      output$display_selected_answer <- shiny::renderUI({
        shiny::req(!base::is.null(open_answer()))
        shiny::req(base::length(open_answer()) > 0)
        shiny::req(!base::is.null(modrval$solutions))
        shiny::req(base::nrow(modrval$solutions) > 0)
        answer_text <- base::paste0(
          open_answer()[open_answer() != ""],
          collapse = "<br><br>"
        )
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
          dplyr::filter(version == selected_version()) |>
          dplyr::select(proposition) |>
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
        print(versiontype)
        if (!(versiontype %in% c("Essay","Problem"))){
          shinyalert::shinyalert("No change allowed!", "Answers to multiple-choice and numeric questions cannot be modified; this form is meant to be changed only to grade open-ended answers.", "error")
        } else {
          checkinput <- base::names(input)
          checkinput <- checkinput[stringr::str_detect(checkinput, "check_")]
          base::print(input[[checkinput]])
          
          #open_answers <- NA
          #compiled <- compile_grading(
          #  modrval$test_parameters,
          #  modrval$solutions,
          #  modrval$students,
          #  modrval$closed_answers,
          #  modrval$numeric_answers,
          #  open_answers,
          #answers
          #)
          #modrval$open_answers <- compiled$open_answers
          #modrval$answers <- compiled$answers
          #modrval$scoring <- compiled$scoring
          #modrval$results <- compiled$results
          #modrval$question_grades <- compiled$question_grades
          #modrval$student_grades <- compiled$student_grades
          #write_compiled(compiled, test_path)
        }
      })
      
      
      
      
      
    }
  )
}
