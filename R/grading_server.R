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
#' @import shiny
#' @importFrom teachR statistics_get_parameters
#' @export



grading_server <- function(id, test, tree, course_data, course_paths){
  ns <- shiny::NS(id)
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      test_parameters <- NULL
      question <- NULL
      student <- NULL
      proposition <- NULL
      interrogation <- NULL
      checked <- NULL
      test <- NULL
      letter <- NULL
      partial_credits <- NULL
      penalty <- NULL
      grade <- NULL
      weight <- NULL
      feedback_path <- NULL
      item <- NULL
      document <- NULL
      explanation <- NULL
      value <- NULL
      correct <- NULL
      number <- NULL
      modifications <- NULL
      attempt <- NULL
      data <- NULL
      language <- NULL
      success <- NULL
      discrimination <- NULL
      altnbr <- NULL
      bloc <- NULL
      folder <- NULL
      group <- NULL
      institution <- NULL
      program <- NULL
      program_level <- NULL
      section <- NULL
      seed <- NULL
      test_assessment <- NULL
      test_date <- NULL
      test_documentation <- NULL
      test_duration <- NULL
      test_format <- NULL
      test_points <- NULL
      test_unit <- NULL
      title <- NULL
      tree <- NULL
      type <- NULL
      website <- NULL
      observation <- NULL
      link <- NULL
      
      modrval <- shiny::reactiveValues()
      
      ##########################################################################
      # Load data from selected test folder
      
      shiny::observe({
        
        test_name <- test()
        test_path <- base::paste0(course_paths()$subfolders$tests, "/", test_name)
        
        
        
        test_name <- "rug_maib_2022_quiz1"
        test_path <- "D:/Dropbox/5-Education/Courses/management_accounting/materials/7_tests/rug_maib_2022_quiz1"
        
        
        # Test parameters
        base::load(base::paste0(
          test_path, "/test_parameters.RData"
        ))
        
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
          test_parameters, solutions, students,
          closed_answers, numeric_answers, open_answers,
          answers, results
        )
        
        modrval$test_parameters <- compiled$test_parameters
        modrval$solutions <- compiled$solutions
        modrval$students <- compiled$students
        modrval$closed_answers <- compiled$closed_answers
        modrval$numeric_answers <- compiled$numeric_answers
        modrval$open_answers <- compiled$open_answers
        modrval$answers <- compiled$answers
        modrval$scoring <- compiled$scoring
        modrval$results <- compiled$results
        
        compiled$solutions |>
          dplyr::group_by(path) |>
          tidyr::nest() |>
          dplyr::mutate(data = purrr::map2(path, data, function(x,y){
            utils::write.csv(y, x, row.names = FALSE)
          }))
        
        utils::write.csv(
          compiled$results,
          base::paste0(test_path, "/8_results/results.csv"),
          row.names = FALSE
        )
        
        utils::write.csv(
          compiled$answers,
          base::paste0(test_path, "/8_results/answers.csv"),
          row.names = FALSE
        )
        
      })
      
      
      
      
      
      
      
      
      
      
      
      output$question_selection <- shiny::renderUI({
        questions <- base::unique(modrval$test_parameters$question)
        shiny::selectInput(
          ns("slctgradedquestion"), "Question",
          choices = questions, selected = questions[1],
          width = "100%"
        )
      })
      
      
      
      
      
      
      
      
      question_statistics <- shiny::reactive({
        shiny::req(!base::is.null(modrval$results))
        modrval$results |>
          tidyr::unite("observation", test, student, sep = ".") |>
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
          dplyr::select(observation, code, correct) |>
          teachR::statistics_get_parameters(
            model_formula = "correct ~ success + proficiency",
            minobs = 10
          )
      })
      
      item_statistics <- shiny::reactive({
        shiny::req(!base::is.null(modrval$results))
        prelimcheck <- modrval$results |>
          dplyr::select(student, question) |>
          stats::na.omit() |>
          base::unique()
        nbrstudents <- base::length(base::unique(prelimcheck$student))
        nbrquestions <- base::length(base::unique(prelimcheck$question))
        shiny::req(nbrstudents >= 20)
        shiny::req(nbrquestions >= 5)
        modrval$results |>
          tidyr::unite("observation", test, student, sep = ".") |>
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
      
      
      
      
      
      
      
      
      
      # Student-attempt selection ##############################################
      
      shiny::observe({
        shiny::req(!base::is.null(input$slctgradedquestion))
        shiny::isolate({
          shiny::req(!base::is.null(modrval$answers))
          shiny::req(base::nrow(modrval$answers) > 0)
          selected <- modrval$answers |>
            dplyr::filter(question == input$slctgradedquestion)
        })
        modrval$students <- base::sort(base::unique(selected$student))
        modrval$student_rank <- 1
      })
      
      output$student_id_selection <- shiny::renderUI({
        shiny::req(!base::is.null(modrval$students))
        shiny::req(!base::is.null(modrval$student_rank))
        shiny::selectInput(
          ns("slctstudentid"),
          "Student-attempt:", choices = modrval$students,
          selected = modrval$students[modrval$student_rank],
          width = "100%"
        )
      })
      
      shiny::observeEvent(input$slctstudentid, {
        new_student_rank <- base::match(input$slctstudentid, modrval$students)
        modrval$student_rank <- new_student_rank
      })
      
      output$student_rank_selection <- shiny::renderUI({
        shiny::req(!base::is.null(modrval$students))
        shiny::req(!base::is.null(modrval$student_rank))
        shiny::req(modrval$student_rank <= base::length(modrval$students))
        shiny::sliderInput(
          ns("slctstudentrank"),
          "Progression within the question:",
          value = modrval$student_rank,
          min = 1, max = base::length(modrval$students),
          step = 1, width = "100%"
        )
      })
      shiny::observeEvent(input$previousstudent, {
        modrval$student_rank <- base::max(c(1,modrval$student_rank-1))
      })
      shiny::observeEvent(input$slctstudentrank, {
        modrval$student_rank <- input$slctstudentrank
      })
      shiny::observeEvent(input$nextstudent, {
        modrval$student_rank <- base::min(
          modrval$student_rank+1, base::length(modrval$students)
        )
      })
      
      output$gradingprogress <- shiny::renderUI({
        shiny::req(base::length(modrval$student_rank) > 0)
        shinyWidgets::progressBar(
          id = "progressbar", value = modrval$student_rank,
          total = base::length(modrval$students), status = "info"
        )
      })
      
      
      
      # Check answers ##########################################################
      
      output$check_answers <- shiny::renderUI({
        shiny::req(!base::is.null(modrval$propositions))
        shiny::req(!base::is.null(modrval$answers))
        shiny::req(!base::is.null(input$slctgradedquestion))
        shiny::req(!base::is.null(input$slctstudentid))
        selected_answer <- modrval$answers |>
          dplyr::filter(
            question == input$slctgradedquestion,
            student == input$slctstudentid
          ) |>
          dplyr::left_join(base::unique(dplyr::select(
            modrval$propositions, version, number, letter, scale,
            proposition, interrogation, keywords, correct
          )), by = c("version","letter"))
        shiny::req(base::nrow(selected_answer) > 0)
        modrval$selected_answer <- selected_answer
        ui <- base::list(
          shiny::HTML(base::paste0(
            "<b>", selected_answer$version[1], " - ",
            selected_answer$interrogation[1], "</b><br><br>"
          ))
        )
        options <- selected_answer |>
          dplyr::arrange(number) |>
          dplyr::select(letter, proposition, scale, checked, correct) |>
          base::unique()
        for (i in base::seq_len(base::nrow(options))){
          ui[[i+2]] <- gradR::make_scale(
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
      
      shiny::observeEvent(input$save_checks, {
        shiny::req(!base::is.null(input$slctgradedtest))
        shiny::req(!base::is.null(input$slctstudentid))
        shiny::req(!base::is.null(input$slctgradedquestion))
        shiny::req(!base::is.null(modrval$answers))
        answers <- gradR::update_answers(
          test_name = input$slctgradedtest,
          scoring = modrval$scoring,
          answers = modrval$answers,
          input = input
        )
        results <- gradR::update_results(
          input$slctgradedtest, modrval$scoring, answers
        )
        modrval$answers <- answers
        modrval$results <- results
        shinyalert::shinyalert(
          "Saved", "Checks have been saved.", type = "success"
        )
      })
      
      output$keywords_selection <- shiny::renderUI({
        shiny::req(!base::is.null(modrval$selected_answer))
        shiny::req(base::nrow(modrval$selected_answer) > 0)
        criteria <- base::unique(modrval$selected_answer$proposition)
        answer_file <- base::paste0(
          "5_tests/", input$slctgradedtest, "/f_answers/open/",
          modrval$selected_answer$student[1], "_",
          modrval$selected_answer$version[1],
          ".txt"
        )
        if (base::file.exists(answer_file)){
          shiny::selectInput(
            ns("slctkeywords"), "Highlight keywords for:",
            choices = criteria, multiple = TRUE
          )
        }
      })
      
      output$display_selected_answer <- shiny::renderUI({
        shiny::req(!base::is.null(modrval$selected_answer))
        shiny::req(base::nrow(modrval$selected_answer) > 0)
        answer_file <- base::paste0(
          "5_tests/", input$slctgradedtest, "/f_answers/open/",
          modrval$selected_answer$student[1], "_",
          modrval$selected_answer$version[1],
          ".txt"
        )
        if (base::file.exists(answer_file)) {
          answer_text <- base::readLines(answer_file)
          modrval$textual_answer <- base::paste0(
            answer_text[answer_text != ""],
            collapse = " "
          )
          answer_text <- base::paste0(
            answer_text[answer_text != ""],
            collapse = "<br><br>"
          )
          if (!base::is.null(input$slctkeywords)){
            keywords <- modrval$selected_answer |>
              dplyr::filter(proposition %in% input$slctkeywords) |>
              dplyr::select(keywords) |>
              base::unlist() |> base::as.character()
            keywords <- stringr::str_split(
              keywords, pattern = " ", simplify = FALSE
            ) |>
              base::unlist() |>
              base::unique()
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
        } else {
          modrval$textual_answer <- NULL
        }
      })
      
      output$displaywordcount <- shiny::renderUI({
        shiny::req(!base::is.null(modrval$textual_answer))
        wordcount <- base::sum(base::lengths(
          base::gregexpr("\\W+", modrval$textual_answer)
        )+1)
        shinydashboard::valueBox(
          wordcount, "Words", icon = shiny::icon("keyboard"),
          color = "blue", width = 12
        )
      })
      
      question_grades <- shiny::reactive({
        shiny::req(!base::is.null(modrval$results))
        modrval$results |>
          tidyr::unite("student", student, attempt, sep = "-") |>
          dplyr::group_by(student, test, question) |>
          dplyr::summarise(
            points = base::max(points, na.rm = TRUE),
            earned = base::sum(earned, na.rm = TRUE),
            .groups = "drop"
          ) |>
          dplyr::mutate(
            earned = dplyr::case_when(
              earned > points ~ points,
              TRUE ~ earned
            )
          ) |>
          dplyr::mutate(earned = base::round(earned, 2))
      })
      
      output$displayearned <- shiny::renderUI({
        shiny::req(!base::is.null(question_grades()))
        shiny::req(!base::is.null(input$slctgradedtest))
        shiny::req(!base::is.null(input$slctgradedquestion))
        shiny::req(!base::is.null(input$slctstudentid))
        student_question_grade <- question_grades() |>
          dplyr::filter(
            student == input$slctstudentid,
            question == input$slctgradedquestion
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
      
      
      
      # Edit propositions #########################################################
      
      output$edit_feedback <- rhandsontable::renderRHandsontable({
        
        shiny::req(!base::is.null(modrval$answers))
        shiny::req(!base::is.null(input$slctstudentid))
        shiny::req(!base::is.null(input$slctgradedquestion))
        shiny::req(!base::is.null(modrval$propositions))
        
        #if (!base::is.null(item_statistics())) {
        #  show_item_statistics <- item_statistics()$parameters |>
        #    dplyr::select(code, success, discrimination) |>
        #    tidyr::separate(code, into = c("item","language"), sep = "_")
        #} else {
        #  show_item_statistics <- tibble::tibble(
        #    item = base::character(0),
        #    langugage = base::character(0),
        #    success = base::numeric(0),
        #    discrimination = base::numeric(0)
        #  )
        #}
        
        selected_version <- modrval$answers |>
          dplyr::filter(
            student == input$slctstudentid,
            question == input$slctgradedquestion
          )
        selected_version <- selected_version$version[1]
        
        selected_propositions <- modrval$propositions |>
          dplyr::filter(version == selected_version)
        if (base::length(stats::na.omit(selected_propositions$number)) > 0){
          lastitem <- base::max(selected_propositions$number, na.rm = TRUE) + 1
        } else {
          lastitem <- 1
        }
        new_row <- tibble::tibble(
          version = selected_version,
          language = selected_propositions$language[1],
          number = lastitem, letter = letters[lastitem],
          modifications = 0, value = 0, correct = 0, weight = 0
        )
        
        selected_propositions |>
          dplyr::select(
            version, item, language, number, letter, document,
            modifications, proposition, value, scale, explanation,
            keywords, correct, weight
          ) |>
          dplyr::bind_rows(new_row) |>
          dplyr::mutate(
            scale = base::factor(scale, levels = c(
              "logical","qualitative","percentage"
            )),
            remove = FALSE
          ) |>
          dplyr::mutate(success = 0, discrimination = 0) |>
          dplyr::arrange(number) |>
          #dplyr::left_join(show_item_statistics, by = c("item","language")) |>
          rhandsontable::rhandsontable(
            width = "95%", rowHeaders = NULL, stretchH = "all"
          ) |>
          rhandsontable::hot_col(c(1,2,3,4,5), readOnly = TRUE) |>
          rhandsontable::hot_cols(
            colWidths = c(
              "6%","6%","3%","3%","2%","6%","3%","15%",
              "3%","7%","21%","10%","3%","3%",
              "3%","3%","3%"
            )
          ) |>
          rhandsontable::hot_context_menu(
            allowRowEdit = FALSE, allowColEdit = FALSE
          )
      })
      
      shiny::observeEvent(input$update_feedback, {
        edits <- rhandsontable::hot_to_r(input$edit_feedback) |>
          dplyr::mutate_if(base::is.factor, base::as.character)
        propositions <- gradR::update_propositions(
          test_name = input$slctgradedtest,
          test_parameters = modrval$test_parameters,
          propositions = modrval$propositions,
          edits = edits
        )
        scoring <- gradR::update_scoring(
          modrval$test_parameters, propositions
        )
        answers <- gradR::update_answers(
          input$slctgradedtest, scoring
        )
        results <- gradR::update_results(
          input$slctgradedtest, scoring, answers
        )
        modrval$propositions <- propositions
        modrval$scoring <- scoring
        modrval$answers <- answers
        modrval$results <- results
        shinyalert::shinyalert(
          "Saved", "Changes to the feedback file have been saved.", type = "success"
        )
      })
      
      
      
      # Edit question parameters ###############################################
      
      output$edit_question_parameters <- shiny::renderUI({
        shiny::req(!base::is.null(input$slctgradedtest))
        shiny::req(!base::is.null(input$slctgradedquestion))
        shiny::req(!base::is.null(input$slctstudentid))
        shiny::req(input$slctgradedquestion %in% modrval$answers$question)
        selected <- modrval$test_parameters |>
          dplyr::filter(
            question == input$slctgradedquestion
          ) |>
          dplyr::select(question, points, partial_credits, penalty) |>
          base::unique()
        shiny::fluidRow(
          shiny::column(
            3,
            shiny::numericInput(
              ns("new_points"), "Points", value = selected$points[1],
              width = "100%"
            )
          ),
          shiny::column(
            3,
            shinyWidgets::switchInput(
              inputId = ns("new_partial_credits"), label = "Partial credits?",
              onLabel = "Yes", offLabel = "No",
              onStatus = "success", offStatus = "primary",
              value = selected$partial_credits[1],
              labelWidth = "125px", handleWidth = "25px"
            )
          ),
          shiny::column(
            3,
            shinyWidgets::switchInput(
              inputId = ns("new_penalty"), label = "Apply penalty?",
              onLabel = "Yes", offLabel = "No",
              onStatus = "danger", offStatus = "primary",
              value = selected$penalty[1],
              labelWidth = "125px", handleWidth = "25px"
            )
          ),
          shiny::column(
            3,
            shiny::actionButton(
              ns("change_quest_param"), "Save", icon = shiny::icon("save"),
              style = "background-color:#006600;color:#FFF;
              width:100%;height:50px;"
            )
          )
        )
      })
      
      shiny::observeEvent(input$change_quest_param, {
        shinyalert::shinyalert(
          "Change question parameters?",
          "This action will reset all the weights previously defined for this question.",
          type = "warning", showCancelButton = TRUE,
          inputId = "confirm_change_quest_param"
        )
      })
      
      shiny::observeEvent(input$confirm_change_quest_param, {
        
        if (input$confirm_change_quest_param){
          
          test_parameters <- modrval$test_parameters
          changed <- test_parameters$question == input$slctgradedquestion
          test_parameters$points[changed] <- base::as.numeric(
            input$new_points
          )
          test_parameters$partial_credits[changed] <- base::as.numeric(
            input$new_partial_credits
          )
          test_parameters$penalty[changed] <- base::as.numeric(
            input$new_penalty
          )
          
          base::save(
            test_parameters,
            file = base::paste0(
              "5_tests/", input$slctgradedtest, "/test_parameters.RData"
            )
          )
          
          propositions <- gradR::reinitialize_weights(
            test_parameters, modrval$propositions, input$slctgradedquestion
          )
          
          propositions <- gradR::update_propositions(
            test_name = input$slctgradedtest,
            test_parameters = test_parameters,
            propositions = propositions
          )
          scoring <- gradR::update_scoring(
            test_parameters, propositions
          )
          results <- gradR::update_results(
            input$slctgradedtest, scoring, modrval$answers
          )
          
          modrval$test_parameters <- test_parameters
          modrval$propositions <- propositions
          modrval$scoring <- scoring
          modrval$results <- results
          
          shinyalert::shinyalert(
            "Question parameters changed!",
            "You should now check the reinitialized weights to make sure they are correct.",
            type = "success"
          )
        } else {
          
          shinyalert::shinyalert(
            "Action cancelled!",
            "No change was made to your question parameters.",
            type = "error"
          )
          
        }
        
        
      })
      
      
      output$viewversion <- shiny::renderUI({
        shiny::req(!base::is.null(input$slctgradedtest))
        shiny::req(!base::is.null(input$slctgradedquestion))
        shiny::req(!base::is.null(input$slctstudentid))
        shiny::req(input$slctgradedquestion %in% modrval$answers$question)
        shiny::req(input$slctstudentid %in% modrval$answers$student)
        if (input$slctstudentid %in% modrval$answers$student){
          selected <- modrval$answers |>
            dplyr::filter(
              question == input$slctgradedquestion,
              student == input$slctstudentid
            )
        } else {
          selected <- modrval$answers |>
            dplyr::filter(
              question == input$slctgradedquestion
            ) |>
            dplyr::slice_sample(n = 1)
        }
        filepath <- base::paste0(
          "5_tests/", input$slctgradedtest, "/b_versions/", selected$version[1]
        )
        test_parameters <- modrval$test_parameters
        base::load("2_documents/propositions.RData")
        as_latex <- FALSE
        record_version <- FALSE
        base::suppressWarnings(
          shiny::withMathJax(shiny::HTML(knitr::knit2html(
            text = base::readLines(filepath),
            fragment.only = TRUE, quiet = TRUE
          )))
        )
      })
      
      output$question_metrics <- shiny::renderUI({
        shiny::req(!base::is.null(input$slctgradedquestion))
        shiny::req(!base::is.null(question_grades()))
        question <- question_grades() |>
          dplyr::filter(
            question == input$slctgradedquestion
          )
        students <- base::nrow(question)
        average <-  base::round(base::mean(question$earned), 2)
        sdev <- base::round(stats::sd(question$earned), 2)
        shiny::fluidRow(
          shiny::column(
            4,
            shinydashboard::valueBox(
              students, "Students",
              icon = shiny::icon("users"),
              color = "blue", width = 12
            )
          ),
          shiny::column(
            4,
            shinydashboard::valueBox(
              average, "Mean",
              icon = shiny::icon("ruler"),
              color = "green", width = 12
            )
          ),
          shiny::column(
            4,
            shinydashboard::valueBox(
              sdev, "Standard deviation",
              icon = shiny::icon("arrows-alt-h"),
              color = "red", width = 12
            )
          )
        )
      })
      
      output$question_stats <- shiny::renderUI({
        shiny::req(!base::is.null(input$slctgradedquestion))
        shiny::req(!base::is.null(question_statistics()))
        selected_parameters <- question_statistics()$parameters |>
          dplyr::filter(code == input$slctgradedquestion)
        shiny::req(base::nrow(selected_parameters) == 1)
        
      })
      
      output$question_diagram <- shiny::renderPlot({
        shiny::req(!base::is.null(input$slctgradedquestion))
        shiny::req(!base::is.null(question_statistics()))
        selected_model <- question_statistics()$models |>
          dplyr::filter(code == input$slctgradedquestion) |>
          dplyr::select(code, data) |>
          tidyr::unnest(data)
        if (base::nrow(selected_model) > 0){
            
          } else shiny::tags$br()
      }, height = 600)
      
      
      
      # See test results #######################################################
      
      student_grades <- shiny::reactive({
        shiny::req(!base::is.null(question_grades()))
        question_grades() |>
          dplyr::group_by(student) |>
          dplyr::summarise(
            points = base::sum(points, na.rm = TRUE),
            grade = base::sum(earned, na.rm = TRUE),
            .groups = "drop"
          ) |>
          dplyr::mutate(grade = base::round(grade, 2))
      })
      
      output$test_metrics <- shiny::renderUI({
        shiny::req(!base::is.null(student_grades()))
        students <- base::nrow(student_grades())
        average <-  base::round(base::mean(student_grades()$grade), 2)
        sdev <- base::round(stats::sd(student_grades()$grade), 2)
        shiny::fluidRow(
          shiny::column(
            4,
            shinydashboard::valueBox(
              students, "Grades",
              icon = shiny::icon("users"),
              color = "blue", width = 12
            )
          ),
          shiny::column(
            4,
            shinydashboard::valueBox(
              average, "Mean",
              icon = shiny::icon("ruler"),
              color = "green", width = 12
            )
          ),
          shiny::column(
            4,
            shinydashboard::valueBox(
              sdev, "Standard deviation",
              icon = shiny::icon("arrows-alt-h"),
              color = "red", width = 12
            )
          )
        )
      })
      
      output$test_distribution <- shiny::renderPlot({
        shiny::req(!base::is.null(student_grades()))
        student_grades() |>
          ggplot2::ggplot(ggplot2::aes(x = grade)) +
          ggplot2::geom_histogram(
            breaks = base::seq_len(student_grades()$points[1])
          )
      })
      
      
      
      shiny::observeEvent(input$export_reports, {
        shiny::req(!base::is.null(modrval$test_parameters))
        shiny::req(!base::is.null(modrval$propositions))
        shiny::req(!base::is.null(modrval$results))
        
        test_parameters <- modrval$test_parameters |>
          dplyr::select(
            test, question, version, tree, test_points,
            points, partial_credits, penalty, altnbr
          )
        test_name <- test_parameters$test[1]
        
        grades <- student_grades() |>
          tidyr::separate(student, into = c("student","attempt"), sep = "-")
        test_results_dir <- base::paste0("6_reports/", test_name)
        if (!base::dir.exists(test_results_dir))
          base::dir.create(test_results_dir)
        utils::write.csv(
          grades,
          base::paste0(test_results_dir, "/grades.csv"),
          row.names = FALSE
        )
        
        propositions <- modrval$propositions |>
          dplyr::select(
            test, version, document, language, number, letter, item,
            type, interrogation, proposition, correct, explanation
          )
        
        results <- modrval$results |>
          dplyr::select(
            test, question, version, language, number, letter, item, 
            student, attempt, scale, checked, weight, earned
          )
        
        textbook <- course_data$tree$textbook |>
          dplyr::select(document, language, section, link)
        
        feedback <- test_parameters |>
          dplyr::left_join(propositions, by = c("test","version")) |>
          dplyr::left_join(
            results,
            by = c("test","question","version","language","number","letter","item")
          ) |>
          dplyr::left_join(textbook, by = c("document","language"))
        
        utils::write.csv(
          feedback,
          base::paste0(test_results_dir, "/feedback.csv"),
          row.names = FALSE
        )
        
        shinyalert::shinyalert(
          "Exported!", "Data for individual or team feedback have been exported.", type = "success"
        )
      })
      
      
      
      shiny::observeEvent(input$export_statistics, {
        shiny::req(!base::is.null(modrval$test_parameters))
        shiny::req(!base::is.null(modrval$results))
        
        test_parameters <- modrval$test_parameters |>
          dplyr::select(
            tree, institution, program, program_level, group,
            test, test_format, test_unit, test_assessment,
            test_documentation, test_date, test_duration,
            test_points, question, section, bloc, altnbr,
            points, partial_credits, penalty, version, seed
          )
        
        test_name <- test_parameters$test[1]
        
        results <- modrval$results |>
          dplyr::select(
            test, question, version, student, attempt, number, letter,
            item, language, scale, checked, weight, earned
          )
        
        base::load("2_documents/documents.Rdata")
        
        documents <- documents |>
          dplyr::select(
            question = file, type, document,
            dplyr::starts_with("tag_")
          )
        
        base_statistics <- test_parameters |>
          dplyr::left_join(results, by = c("test","question","version")) |>
          dplyr::left_join(documents, by = c("question"))
        
        utils::write.csv(
          base_statistics,
          base::paste0(
            "7_statistics/test_results/", test_name, ".csv"
          ),
          row.names = FALSE
        )
        
        shinyalert::shinyalert(
          "Exported!", "Data for statisticsl analyses have been exported.", type = "success"
        )
      })
      
    }
  )
}
