  #' @name grading_server
#' @title Grade tests.
#' @author Nicolas Mangin
#' @description Module allowing the user to grade, check, and calibrate tests.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param course_data Reactive. Function containing all the course data loaded with the course.
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @return Save the test results in the relevant test sub-folder.
#' @importFrom chartR draw_grade_distribution
#' @importFrom classR trees_structure_textbook
#' @importFrom dplyr anti_join
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr full_join
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr right_join
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom editR selection_server
#' @importFrom gradR make_scale
#' @importFrom knitr knit2html
#' @importFrom purrr map
#' @importFrom purrr map2_chr
#' @importFrom readr read_csv
#' @importFrom readr read_lines
#' @importFrom readr write_csv
#' @importFrom rhandsontable hot_col
#' @importFrom rhandsontable hot_cols
#' @importFrom rhandsontable hot_context_menu
#' @importFrom rhandsontable hot_to_r
#' @importFrom rhandsontable renderRHandsontable
#' @importFrom rhandsontable rhandsontable
#' @importFrom rstudioapi navigateToFile
#' @importFrom shiny HTML
#' @importFrom shiny NS
#' @importFrom shiny actionButton
#' @importFrom shiny checkboxGroupInput
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny icon
#' @importFrom shiny isolate
#' @importFrom shiny moduleServer
#' @importFrom shiny observe
#' @importFrom shiny observeEvent
#' @importFrom shiny reactive
#' @importFrom shiny reactiveValues
#' @importFrom shiny renderPlot
#' @importFrom shiny renderUI
#' @importFrom shiny req
#' @importFrom shiny tagList
#' @importFrom shiny textAreaInput
#' @importFrom shiny updateNumericInput
#' @importFrom shiny withMathJax
#' @importFrom shinyAce aceEditor
#' @importFrom shinyWidgets radioGroupButtons
#' @importFrom shinyalert shinyalert
#' @importFrom shinydashboard valueBox
#' @importFrom stringr str_count
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_remove
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_split
#' @importFrom tibble tibble
#' @importFrom tidyr replace_na
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
      selected_answer <- NULL
      selected_intake <- NULL
      start <- NULL
      test <- NULL
      test_languages <- NULL
      studentid <- NULL
      intake <- NULL
      altnbr <- NULL
      bloc <- NULL
      section <- NULL
      seed <- NULL
      show_points <- NULL
      show_version <- NULL
      test_assessment <- NULL
      test_date <- NULL
      test_documentation <- NULL
      test_duration <- NULL
      test_format <- NULL
      test_points <- NULL
      test_unit <- NULL
      email <- NULL
      firstname <- NULL
      lastname <- NULL
      team <- NULL
      earned <- NULL
      outl <- NULL
      
      
      
      ##########################################################################
      # Loading
      
      modrval <- shiny::reactiveValues()
      
      shiny::observe({
        shiny::req(!base::is.na(course_data()$answers))
        shiny::isolate({
          modrval$tests <- course_data()$tests
          modrval$solutions <- course_data()$solutions
          modrval$answers <- course_data()$answers
          modrval$students <- course_data()$students
          modrval$languages <- course_data()$languages
          modrval$preselected_answers <- modrval$answers
        })
      })
      
      ##########################################################################
      # Selection
      
      output$slctlanguage <- shiny::renderUI({
        shiny::req(base::nrow(modrval$answers) > 0)
        exam_languages <- course_data()$languages |>
          dplyr::filter(langiso %in% base::unique(modrval$answers$language))
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
      
      
      
      tests <- shiny::reactive({
        c("All", base::unique(modrval$preselected_answers$test))
      })
      selected_test <- editR::selection_server("select_test", tests)
      
      questions <- shiny::reactive({
        c("All", base::unique(modrval$preselected_answers$question))
      })
      selected_question <- editR::selection_server("select_question", questions)
      
      versions <- shiny::reactive({
        c("All", base::unique(modrval$preselected_answers$version))
      })
      selected_version <- editR::selection_server("select_version", versions)
      
      intakes <- shiny::reactive({
        c("All", base::unique(modrval$preselected_answers$intake))
      })
      selected_intake <- editR::selection_server("select_intake", intakes)
      
      students <- shiny::reactive({
        c("All", base::unique(modrval$preselected_answers$studentid))
      })
      selected_student <- editR::selection_server("select_student", students)
      
      attempts <- shiny::reactive({
        c("All", base::unique(base::as.character(modrval$preselected_answers$end)))
      })
      selected_attempt <- editR::selection_server("select_attempt", attempts)
      
      
      
      selected_answers <- shiny::reactive({
        shiny::req(!base::is.null(input$slctexamlang))
        shiny::req(!base::is.null(selected_test()))
        shiny::req(!base::is.null(selected_question()))
        shiny::req(!base::is.null(selected_version()))
        shiny::req(!base::is.null(selected_intake()))
        shiny::req(!base::is.null(selected_student()))
        shiny::req(!base::is.null(selected_attempt()))
        
        selected_answers <- modrval$preselected_answers
        
        selected_answers <- dplyr::filter(selected_answers, language == input$slctexamlang)
        
        if (selected_test() != "All" & selected_test() %in% selected_answers$test){
          selected_answers <- dplyr::filter(selected_answers, test == selected_test())
        }
        
        if (selected_question() != "All" & selected_question() %in% selected_answers$question){
          selected_answers <- dplyr::filter(selected_answers, question == selected_question())
        }
        
        if (selected_version() != "All" & selected_version() %in% selected_answers$version){
          selected_answers <- dplyr::filter(selected_answers, version == selected_version())
        }
        
        if (selected_intake() != "All" & selected_intake() %in% selected_answers$intake){
          selected_answers <- dplyr::filter(selected_answers, intake == selected_intake())
        }
        
        modrval$preselected_answers <- selected_answers
        
        if (selected_student() != "All" & selected_student() %in% selected_answers$studentid){
          selected_answers <- dplyr::filter(selected_answers, studentid == selected_student())
        }
        
        if (selected_attempt() != "All"){
          slctattempt <- selected_answers |>
            dplyr::filter(end == base::as.POSIXct(selected_attempt(), tz="UTC"))
        } else {
          slctattempt <- selected_answers
        }
        
        slctattempt <- base::unique(dplyr::select(slctattempt, -start, -letter, -checked))
        
        slctattempt
      })
      
      
      
      ##########################################################################
      # Answer tab
      
      output$check_answers <- shiny::renderUI({
        shiny::req(!base::is.null(selected_answers()))
        shiny::req(base::nrow(selected_answers()) == 1)
        
        solutions <- modrval$solutions |>
          dplyr::filter(
            test == selected_answers()$test[1],
            version == selected_answers()$version[1]
          )
        
        typequest <- solutions |>
          dplyr::select(type) |>
          base::unlist() |>
          base::as.character() |>
          base::unique()
        
        graded_items <- modrval$answers |>
          dplyr::filter(
            test == selected_answers()$test[1],
            question == selected_answers()$question[1],
            version == selected_answers()$version[1],
            intake == selected_answers()$intake[1],
            studentid == selected_answers()$studentid[1],
            end == selected_answers()$end[1]
          ) |>
          dplyr::right_join(base::unique(dplyr::select(
            solutions, test, version, number, letter, scale,
            proposition, interrogation, keywords, correct
          )), by = c("test","version","letter")) |>
          tidyr::replace_na(base::list(checked = 0))
        
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
          
          if (typequest %in% c("Essay","Problem")){
            col3[[(itemnbr+1)]] <- shiny::actionButton(
              ns("save_checks"), "Save", icon = shiny::icon("save"),
              style = "background-color:#006600;color:#FFF;width:100%;"
            )
          }
          
          
          ui <- shiny::fluidRow(
            shiny::column(4, col1),
            shiny::column(4, col2),
            shiny::column(4, col3)
          )
        }
        
        ui
      })
      
      
      
      shiny::observeEvent(input$save_checks, {
        checkinput <- base::names(input)
        checkinput <- checkinput[stringr::str_detect(checkinput, "check_")]
        nbr <- base::length(checkinput)
        letters <- base::character(nbr)
        checks <- base::character(nbr)
        
        for (i in base::seq_len(nbr)){
          letters[i] <- base::as.character(stringr::str_remove(checkinput[i], "check_"))
          checks[i] <- base::as.character(input[[checkinput[i]]])
        }
        
        update_answer <- tibble::tibble(
          test = selected_answers()$test[1],
          intake = selected_answers()$intake[1],
          studentid = selected_answers()$studentid[1],
          end = selected_answers()$end[1],
          question = selected_answers()$question[1], 
          version = selected_answers()$version[1],
          letter = letters,
          checked_chr = checks
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
        
        update_answer <- modrval$answers |>
          dplyr::select(-letter, -checked) |>
          base::unique() |>
          dplyr::right_join(update_answer, by = c("test","intake","studentid","end","question","version"))
        
        answers <- modrval$answers |>
          dplyr::anti_join(update_answer, by = c("test","intake","studentid","end","question","version")) |>
          dplyr::bind_rows(update_answer) |>
          dplyr::arrange(test, intake, studentid, question, version, letter)
        
        modrval$answers <- answers
        base::save(answers, file = course_paths()$databases$answers)
        
        answers_to_file <- answers |>
          dplyr::filter(
            test == selected_answers()$test[1],
            intake == selected_answers()$intake[1],
            language == selected_answers()$language[1]
          ) |>
          dplyr::select(-test, -intake, -language)
        
        filepath <- base::paste0(
          course_paths()$subfolders$answers, "/",
          selected_answers()$test[1], "-",
          selected_answers()$intake[1], "-",
          selected_answers()$language[1], ".csv"
        )
        readr::write_csv(answers_to_file, file = filepath)
        
        shinyalert::shinyalert("Saved!", "Grading has been updated.", "success")
      })
      
      
      
      output$viewversion <- shiny::renderUI({
        shiny::req(!base::is.null(selected_answers()))
        shiny::req(base::nrow(selected_answers()) == 1)
        
        filepath <- base::paste0(
          course_paths()$subfolders$tests, "/",
          selected_answers()$test[1], "/5_examination/mdfiles/",
          selected_answers()$version[1]
        )
        filepath <- stringr::str_replace(filepath, "Rmd$", "md")
        shiny::req(base::file.exists(filepath))
        base::suppressWarnings(
          shiny::withMathJax(shiny::HTML(knitr::knit2html(
            text = base::readLines(filepath), quiet = TRUE, template = FALSE
          )))
        )
      })
      
      
      
      open_answer_txt <- shiny::reactive({
        shiny::req(!base::is.null(selected_answers()))
        shiny::req(base::nrow(selected_answers()) == 1)
        
        filepath <- base::paste0(
          course_paths()$subfolders$tests, "/",
          selected_answers()$test[1], "/6_answers/",
          selected_answers()$version[1], "-",
          selected_answers()$intake[1], "-",
          selected_answers()$studentid[1], ".txt"
        )
        if (base::file.exists(filepath)){
          base::readLines(filepath)
        } else ""
      })
      
      
      
      shiny::observe({
        shiny::req(!base::is.null(selected_answers()))
        modrval$feedbackfolder <- base::paste0(
          course_paths()$subfolders$tests, "/",
          selected_answers()$test[1], "/7_feedback"
        )
        shiny::req(base::dir.exists(modrval$feedbackfolder))
        commentpath <- base::paste0(modrval$feedbackfolder, "/comments.csv")
        if (base::file.exists(commentpath)) {
          modrval$comments <- readr::read_csv(file = commentpath, col_types = "cccccTc")
        } else {
          modrval$comments <- selected_answers() |>
            dplyr::select(test, question, version, intake, studentid, end) |>
            dplyr::mutate(comment = "")
          readr::write_csv(modrval$comments, file = commentpath)
        }
      })
      
      
      
      output$customcomment <- shiny::renderUI({
        shiny::req(!base::is.null(selected_answers()))
        shiny::req(base::nrow(selected_answers()) == 1)
        shiny::req(!base::is.null(modrval$comments))
        
        slctcomment <- modrval$comments |>
          dplyr::filter(
            test == selected_answers()$test[1],
            question == selected_answers()$question[1],
            version == selected_answers()$version[1],
            intake == selected_answers()$intake[1],
            studentid == selected_answers()$studentid[1],
            end == selected_answers()$end[1]
          )
        
        shiny::textAreaInput(
          inputId = ns("writencomment"),
          label = "General comments:",
          value = slctcomment$comment[1],
          width = "100%",
          height = "300px"
        )
      })
      
      
      
      shiny::observeEvent(input$save_comment, {
        shiny::req(!base::is.null(selected_answers()))
        shiny::req(!base::is.null(input$writencomment))
        
        newcomment <- selected_answers() |>
          dplyr::select(test, question, version, intake, studentid, end) |>
          dplyr::mutate(comment = input$writencomment)
        
        oldcomments <- modrval$comments |>
          dplyr::anti_join(newcomment, by = c("test", "question", "version", "intake", "studentid", "end"))
        
        comments <- dplyr::bind_rows(oldcomments, newcomment)
        commentpath <- base::paste0(modrval$feedbackfolder, "/comments.csv")
        readr::write_csv(comments, file = commentpath)
        modrval$comments <- comments
        
        shinyalert::shinyalert("Saved!", "Your comment has been updated.", "success")
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
      
      
      
      output$keywords_selection <- shiny::renderUI({
        shiny::req(!base::is.null(selected_answers()))
        shiny::req(base::nrow(selected_answers()) == 1)
        
        criteria <- modrval$solutions |>
          dplyr::filter(version == selected_answers()$version[1])
        shiny::req(criteria$type[1] %in% c("Essay","Problem"))
        
        criteria <- criteria |>
          dplyr::select(proposition) |>
          dplyr::arrange(proposition) |>
          base::unlist() |>
          base::as.character() |>
          base::unique()
        
        if (input$dispallkw){
          preslctkw <- criteria
        } else preslctkw <- NA
        
        shiny::checkboxGroupInput(
          inputId = ns("slctkeywords"),
          label = "Highlight keywords for:", 
          choices = criteria, selected = preslctkw,
          width = "100%"
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
        
        kwcolors <- c(
          "#641e16","#4a235a","#154360","#0b5345","#186a3b",
          "#7d6608","#7e5109","#6e2c00","#424949","#1b2631",
          "#c0392b","#e74c3c","#9b59b6","#8e44ad","#2980b9",
          "#16a085","#27ae60","#e67e22","#d35400","#34495e"
        )
        kwcolors <- base::rep(kwcolors, 3)
        
        if (!base::is.null(input$slctkeywords)){
          keywords <- modrval$solutions |>
            dplyr::filter(proposition %in% input$slctkeywords) |>
            dplyr::select(keywords) |>
            base::unlist() |> base::as.character()
          
          for (i in base::seq_len(base::length(keywords))){
            km <- stringr::str_split(
              keywords[i], pattern = " ", simplify = TRUE
            ) |> base::unlist() |> base::unique()
            km <- km[!base::is.na(km)]
            km <- km[km != ""]
            for (word in km) {
              answer_text <- stringr::str_replace_all(
                answer_text,
                base::paste0("(?i)", word),
                base::paste0(
                  '<font color="',kwcolors[i],'"><b>',
                  word,
                  '</b></font>'
                )
              )
            }
          }
        }
        
        answer_text <- base::paste0(
          '<div style="background-color:#EEEEFF;font-size:1.2em;padding:20px;text-align:justify;">',
          '<h1>Answer</h1>',
          answer_text,
          '</div>'
        )
        shiny::HTML(answer_text)
      })
      
      
      
      shiny::observeEvent(input$open_selected_answer, {
        shiny::req(!base::is.null(selected_answers()))
        shiny::req(base::nrow(selected_answers()) == 1)
        
        filepath <- base::paste0(
          course_paths()$subfolders$tests, "/",
          selected_answers()$test[1], "/6_answers/",
          selected_answers()$version[1], "-",
          selected_answers()$intake[1], "-",
          selected_answers()$studentid[1], ".txt"
        )
        
        if (base::Sys.info()[1] == "Windows"){
          base::shell.exec(filepath)
        } else {
          base::system2(filepath)
        }
      })
      
      
      
      results <- shiny::reactive({
        
        shiny::req(!base::is.null(modrval$tests))
        shiny::req(!base::is.null(modrval$answers))
        shiny::req(!base::is.null(modrval$solutions))
        shiny::req(!base::is.null(modrval$students))
        
        tests <- modrval$tests |>
          dplyr::mutate(test_languages = purrr::map(test_languages, stringr::str_split, pattern = ";", simplify = TRUE)) |>
          dplyr::mutate(test_languages = purrr::map(test_languages, base::as.character)) |>
          tidyr::unnest(test_languages) |>
          dplyr::mutate(
            question = purrr::map2_chr(question, test_languages, function(x,y) stringr::str_replace(x,"US",y)),
            version = purrr::map2_chr(version, test_languages, function(x,y) stringr::str_replace(x,"US",y))
          )
        
        results <- modrval$answers |>
          dplyr::full_join(modrval$solutions, by = c("test","version","language","letter")) |>
          tidyr::replace_na(base::list(checked = 0)) |>
          dplyr::filter(!base::is.na(studentid)) |>
          dplyr::left_join(tests, by = c("test","question","version"))
        
        indiresults <- dplyr::filter(results, test_unit == "student") |>
          dplyr::left_join(modrval$students, by = c("intake","language","studentid"))
        
        teamresults <- dplyr::filter(results, test_unit == "team") |>
          dplyr::rename(team = studentid) |>
          dplyr::left_join(modrval$students, by = c("intake","language","team"), relationship = "many-to-many")
        
        results <- dplyr::bind_rows(indiresults, teamresults) |>
          dplyr::mutate(earned = checked * weight)
        
        results
      })
      
      
      
      scores <- shiny::reactive({
        shiny::req(!base::is.null(results()))
        results() |>
          dplyr::select(
            test, section, bloc, question, language,
            intake, studentid, team, firstname, lastname, email, end,
            checked, weight, test_points, points, partial_credits, penalty,
            earned
          ) |>
          dplyr::group_by(
            test, section, bloc, question, language,
            intake, studentid, team, firstname, lastname, email, end,
            test_points, points, partial_credits, penalty
          ) |>
          dplyr::summarise(score = base::sum(earned)) |>
          dplyr::ungroup() |>
          dplyr::mutate(score = dplyr::case_when(
            score < 0 & penalty == 0 ~ 0,
            score < points & partial_credits == 0 ~ 0,
            score > points ~ points,
            TRUE ~ score
          ))
      })
      
      
      
      grades <- shiny::reactive({
        shiny::req(!base::is.null(scores()))
        scores() |>
          dplyr::select(
            test, language,
            intake, studentid, team,
            firstname, lastname,
            email, end,
            test_points, score
          ) |>
          dplyr::group_by(
            test, language, intake,
            studentid, team, firstname, lastname, email, end,
            test_points
          ) |>
          dplyr::summarise(grade = base::sum(score)) |>
          dplyr::ungroup() |>
          dplyr::mutate(grade = dplyr::case_when(
            grade >= test_points ~ test_points,
            TRUE ~ grade
          ))
      })
      
      
      
      output$displayscore <- shiny::renderUI({
        shiny::req(!base::is.null(scores()))
        shiny::req(!base::is.null(selected_answers()))
        shiny::req(base::nrow(selected_answers()) == 1)
        
        score <- scores() |>
          dplyr::filter(
            studentid == selected_answers()$studentid[1] |
              team == selected_answers()$studentid[1],
            question == selected_answers()$question[1],
            end == selected_answers()$end[1],
          ) |>
          dplyr::select(points, score) |>
          base::unique()
        
        shiny::req(base::nrow(score) == 1)
        points <- score$points[1]
        score <- score$score[1]
        score_color <- dplyr::case_when(
          score/points >= 0.8 ~ "green",
          score/points >= 0.6 ~ "lime",
          score/points >= 0.5 ~ "yellow",
          score/points >= 0.4 ~ "orange",
          score/points >= 0.2 ~ "red",
          TRUE ~"black"
        )
        shinydashboard::valueBox(
          score, "Points earned", icon = shiny::icon("sort-numeric-up-alt"),
          color = score_color, width = 12
        )
      })
      
      
      
      
      
      ##########################################################################
      # Solution tab
      
      output$edit_solutions <- rhandsontable::renderRHandsontable({
        shiny::req(!base::is.null(selected_answers()))
        shiny::req(base::nrow(selected_answers()) == 1)
        shiny::req(!base::is.null(modrval$solutions))
        
        selected_solutions <- modrval$solutions |>
          dplyr::filter(
            test == selected_answers()$test[1],
            version == selected_answers()$version[1]
          )
        
        type <- base::unique(selected_solutions$type)
        
        selected_solutions <- selected_solutions |>
          dplyr::select(
            test, version, number, letter, item, type, document, language,
            modifications, interrogation, proposition, value, scale,
            explanation, keywords, correct, weight
          ) |>
          dplyr::mutate(remove = FALSE)
        
        if (type %in% c("Essay","Problem")){
          if (base::length(stats::na.omit(selected_solutions$number)) > 0){
            lastitem <- base::max(selected_solutions$number, na.rm = TRUE) + 1
          } else {
            lastitem <- 1
          }
          
          extraitems <- selected_solutions$item[stringr::str_detect(selected_solutions$item, "EITM")]
          if (base::length(extraitems) == 0){
            newitem <- 1
          } else {
            maxitem <- extraitems |>
              stringr::str_extract_all("[0-9]") |>
              base::lapply(paste0, collapse = "") |>
              base::unlist() |>
              base::as.numeric() |>
              base::max()
            newitem <- maxitem+1
          }
          newitem <- base::paste0(c("EITM", base::rep(0, 6-base::nchar(newitem)), newitem), collapse = "")
          
          new_row <- tibble::tibble(
            test = selected_answers()$test[1],
            version = selected_answers()$version[1],
            number = lastitem,
            letter = letters[lastitem],
            item = newitem,
            type = selected_solutions$type[1],
            document = "",
            language = selected_solutions$language[1],
            modifications = 1,
            interrogation = selected_solutions$interrogation[1],
            proposition = base::paste0(letters[lastitem],")"),
            value = 0,
            scale = "logical",
            explanation = "",
            keywords = "",
            correct = 0,
            weight = 0,
            remove = TRUE
          )
          
          selected_solutions |>
            dplyr::bind_rows(new_row) |>
            dplyr::mutate(
              scale = base::factor(scale, levels = c(
                "logical","qualitative","percentage"
              ))
            ) |>
            dplyr::arrange(proposition) |>
            rhandsontable::rhandsontable(
              width = "95%", rowHeaders = NULL, stretchH = "all"
            ) |>
            rhandsontable::hot_col(c(1,2,3,4,5,6,8), readOnly = TRUE) |>
            rhandsontable::hot_cols(
              colWidths = c(
                "5%","5%","3%","3%","5%","3%","5%","3%","3%",
                "15%","10%","3%","3%","15%","10%","3%","3%","3%"
              ),
              manualColumnResize = TRUE
            ) |>
            rhandsontable::hot_context_menu(
              allowRowEdit = FALSE, allowColEdit = FALSE
            )
        } else {
          selected_solutions |>
            dplyr::mutate(
              scale = base::factor(scale, levels = c(
                "logical","qualitative","percentage"
              ))
            ) |>
            dplyr::arrange(letter) |>
            rhandsontable::rhandsontable(
              width = "95%", rowHeaders = NULL, stretchH = "all"
            ) |>
            rhandsontable::hot_col(c(1,2,3,4,5,6,8,10,11,13,15), readOnly = TRUE) |>
            rhandsontable::hot_cols(
              colWidths = c(
                "5%","5%","3%","3%","2%","5%","3%","15%",
                "3%","7%","30%","10%","3%","3%","3%"
              ),
              manualColumnResize = TRUE
            ) |>
            rhandsontable::hot_context_menu(
              allowRowEdit = FALSE, allowColEdit = FALSE
            )
        }
      })
      
      
      
      shiny::observeEvent(input$save_sol, {
        shiny::req(!base::is.null(selected_answers()))
        shiny::req(base::nrow(selected_answers()) == 1)
        shiny::req(!base::is.null(modrval$solutions))
        
        updatesolutions <- rhandsontable::hot_to_r(input$edit_solutions) |>
          dplyr::filter(remove == FALSE) |>
          dplyr::select(-remove) |>
          dplyr::mutate(scale = base::as.character(scale))
        
        filtsolutions <- dplyr::anti_join(
          modrval$solutions,
          base::unique(dplyr::select(updatesolutions, test, version)),
          by = c("test","version")
        )
        
        solutions <- dplyr::bind_rows(filtsolutions, updatesolutions)
        
        modrval$solutions <- solutions
        base::save(solutions, file = course_paths()$databases$solutions)
        
        filepath <- base::paste0(
          course_paths()$subfolders$tests, "/",
          selected_answers()$test[1],
          "/4_solutions/",
          selected_answers()$version[1], ".csv"
        )
        readr::write_csv(updatesolutions, file = filepath)
        
        shinyalert::shinyalert("Saved!", "Grading has been updated.", "success")
      })
      
      
      
      
      ##########################################################################
      # Solution tab
      
      output$edit_parameters <- rhandsontable::renderRHandsontable({
        shiny::req(!base::is.null(selected_answers()))
        shiny::req(base::nrow(selected_answers()) == 1)
        shiny::req(!base::is.null(modrval$tests))
        
        selected_tests <- modrval$tests |>
          dplyr::filter(
            test == selected_answers()$test[1],
            question == stringr::str_replace(selected_answers()$question[1], "_...Rmd", "_US.Rmd")
          )
        
        selected_tests |>
          dplyr::select(
            test, test_format, test_unit,
            test_assessment, test_documentation, test_languages, test_date,
            test_duration, test_points, show_version, show_points,
            question, section, bloc,
            altnbr, points, partial_credits, penalty, version, seed
          ) |>
          dplyr::mutate(
            test_date = base::as.character(test_date),
            partial_credits = base::as.logical(partial_credits),
            penalty = base::as.logical(penalty)
          ) |>
          dplyr::arrange(letter) |>
          rhandsontable::rhandsontable(
            width = "95%", rowHeaders = NULL, stretchH = "all"
          ) |>
          rhandsontable::hot_col(c(1:15,19:20), readOnly = TRUE) |>
          rhandsontable::hot_cols(
            colWidths = c(
              "5%","5%","5%",
              "5%","5%","5%","5%",
              "5%","5%","5%","5%",
              "5%","5%","5%",
              "5%","5%","5%","5%","5%","5%"
            ),
            manualColumnResize = TRUE
          ) |>
          rhandsontable::hot_context_menu(
            allowRowEdit = FALSE, allowColEdit = FALSE
          )
      })
      
      
      
      shiny::observeEvent(input$save_param, {
        shiny::req(!base::is.null(selected_answers()))
        shiny::req(base::nrow(selected_answers()) == 1)
        shiny::req(!base::is.null(modrval$tests))
        
        test_parameters <- rhandsontable::hot_to_r(input$edit_parameters) |>
          dplyr::mutate(test_date = base::as.POSIXct(test_date, tz="UTC"))
        
        tests <- dplyr::anti_join(
          modrval$tests,
          base::unique(dplyr::select(test_parameters, test, question, version)),
          by  =c("test","question","version")
        )
        
        tests <- dplyr::bind_rows(tests, test_parameters) |>
          dplyr::arrange(test, section, bloc)
        
        modrval$tests <- tests
        base::save(tests, file = course_paths()$databases$tests)
        
        filepath <- base::paste0(
          course_paths()$subfolders$tests, "/",
          selected_answers()$test[1],
          "/test_parameters.RData"
        )
        base::save(test_parameters, file = filepath)
        
        shinyalert::shinyalert("Saved!", "Grading has been updated.", "success")
      })
      
      
      
      
      
      ##########################################################################
      # Diagnostics tab
      
      
      ##########################################################################
      # Results tab
      
      testmetric <- shiny::reactive({
        
        shiny::req(!base::is.null(results()))
        shiny::req(!base::is.null(scores()))
        shiny::req(!base::is.null(grades()))
        shiny::req(!base::is.null(input$defmetric))
        
        if (input$defmetric == "grade"){
          testmetric <- grades() |>
            dplyr::select(studentid, points = test_points, grade)
        } else if (input$defmetric == "score") {
          testmetric <- scores() |>
            dplyr::select(studentid, points, grade = score)
        } else {
          testmetric <- scores() |>
            dplyr::select(studentid, points, grade = earned)
        }
        
        testmetric  |>
          dplyr::group_by(studentid) |>
          dplyr::summarise(points = base::sum(points), grade = base::sum(grade))
      })
      
      
      shiny::observe({
        shiny::req(!base::is.null(testmetric()))
        shiny::updateNumericInput(
          session,
          "defpass",
          value = (base::ceiling(0.5*base::max(testmetric()$grade)))
        )
      })
      
      output$testmetrics <- shiny::renderUI({
        shiny::req(!base::is.null(testmetric()))
        shiny::req(!base::is.null(input$defpass))
        
        count <- base::length(base::unique(testmetric()$studentid))
        minimum <- base::round(base::min(stats::na.omit(testmetric()$grade)),2)
        ave <- base::round(base::mean(stats::na.omit(testmetric()$grade)),2)
        med <- base::round(stats::median(stats::na.omit(testmetric()$grade)),2)
        maximum <- base::round(base::max(stats::na.omit(testmetric()$grade)),2)
        range <- minimum-minimum
        stddev <- base::round(stats::sd(stats::na.omit(testmetric()$grade)),2)
        passrate <- base::round(100*base::mean(stats::na.omit(testmetric()$grade) >= input$defpass),2)
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
        shiny::req(!base::is.null(testmetric()))
        shiny::req(!base::is.null(input$defpass))
        shiny::req(!base::is.null(input$defhistbreaks))
        chartR::draw_grade_distribution(testmetric(), input$defpass, input$defhistbreaks)
      })
      
      
      
      ##########################################################################
      # Feedback
      
      # Editing
      feedback_template_path <- shiny::reactive({
        shiny::req(!base::is.null(selected_answers()))
        shiny::req(base::nrow(selected_answers()) == 1)
        shiny::req(!base::is.null(modrval$feedbackfolder))
        base::paste0(
          modrval$feedbackfolder, "/feedback_",
          selected_answers()$language[1],".Rmd"
        )
      })
      
      output$edit_feedback <- shiny::renderUI({
        shiny::req(!base::is.null(feedback_template_path()))
        shiny::req(base::file.exists(feedback_template_path()))
        lines <- readr::read_lines(feedback_template_path())
        input$refreshfeedback
        shinyAce::aceEditor(
          outputId = ns("editedfeedback"), value = lines,
          mode = "markdown", wordWrap = TRUE, debounce = 10,
          autoComplete = "live", height = "500"
        )
      })
      
      shiny::observeEvent(input$feedbackinrstudio, {
        shiny::req(!base::is.null(feedback_template_path()))
        shiny::req(base::file.exists(feedback_template_path()))
        rstudioapi::navigateToFile(feedback_template_path())
      })
      
      shiny::observeEvent(input$savefeedback, {
        shiny::req(!base::is.null(input$editedfeedback))
        base::writeLines(
          input$editedfeedback, feedback_template_path(), useBytes = TRUE
        )
        shinyalert::shinyalert(
          "Feedback saved!",
          "Refresh to preview with the changes you just made.",
          type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE,
          inputId = "refreshpreview"
        )
      })
      
      
      
      # Previewing
      
      feedback_data <- shiny::reactive({
        shiny::req(!base::is.null(course_data()))
        shiny::req(!base::is.null(selected_answers()))
        shiny::req(base::nrow(selected_answers()) == 1)
        shiny::req(!base::is.null(results()))
        shiny::req(!base::is.null(scores()))
        shiny::req(!base::is.null(grades()))
        shiny::req(!base::is.null(modrval$comments))
        
        selected_answers <- selected_answers()
        results <- results()
        scores <- scores()
        grades <- grades()
        documents <- course_data()$databases$documents
        intake <- dplyr::filter(course_data()$intakes, intake == selected_answers()$intake[1])
        tbltree <- course_data()$tbltrees[[base::paste0(intake$tree[[1]], ".RData")]]
        textbook <- classR::trees_structure_textbook(tbltree, intake$tree[1], intake$website[1])
        comments <- modrval$comments
        
        base::list(
          selected_answers = selected_answers,
          results = results,
          scores = scores,
          grades = grades,
          documents = documents,
          intake = intake,
          tbltree = tbltree,
          textbook = textbook,
          comments = comments
        )
      })
      
      output$preview_feedback <- shiny::renderUI({
        shiny::req(!base::is.null(feedback_data()))
        shiny::req(!base::is.null(selected_answers()))
        shiny::req(base::nrow(selected_answers()) == 1)
        input$refreshfeedback
        feedback_data <- feedback_data()
        lines <- readr::read_lines(feedback_template_path())
        base::suppressWarnings(
          shiny::withMathJax(shiny::HTML(knitr::knit2html(
            text = lines, quiet = TRUE, template = FALSE
          )))
        )
      })
      
      
      
      
      
      
      
      
      
      
      
      # Mailing
      
      credentials <- shiny::reactive({
        tibble::tibble(
          profile = base::list.files(course_paths()$subfolders$credentials, full.names = FALSE),
          file = base::list.files(course_paths()$subfolders$credentials, full.names = TRUE)
        )
      })
      
      output$slctcredentials <- shiny::renderUI({
        shiny::req(!base::is.null(credentials()))
        shiny::req(base::nrow(credentials()) > 0)
        credchoices <- base::as.vector(credentials()$file)
        base::names(credchoices) <- credentials()$profile
        shinyWidgets::radioGroupButtons(
          inputId = ns("slctcreds"),
          label = "Select credentials to send e-mails:",
          choices = credchoices,
          justified = TRUE,
          checkIcon = base::list(yes = shiny::icon("ok", lib = "glyphicon"))
        )
      })
      
      shiny::observeEvent(input$sendtestemail, {
        shiny::req(!base::is.null(feedback_data()))
        shiny::req(!base::is.null(input$slctcreds))
        shiny::req(!base::is.null(input$emailtest))
        
        if (base::grepl("^[[:alnum:]._-]+@[[:alnum:].-]+$", input$emailtest)){
          
          feedback_data <- feedback_data()
          
          shinybusy::show_modal_spinner(
            spin = "orbit",
            text = "Please wait while the e-mail is sent..."
          )
          
          message <- blastula::render_email(feedback_template_path())
          
          if (stringr::str_detect(input$slctcreds, "gmail")){
            sender <- base::readLines(input$slctcreds) |>
              stringr::str_extract(
                "([a-zA-Z0-9._-]+@[a-zA-Z0-9._-]+\\.[a-zA-Z0-9_-]+)"
              )
              blastula::smtp_send(
                message,
                from = sender,
                to = input$emailtest,
                subject = input$mailsubject,
                credentials = blastula::creds_file(input$slctcreds)
              )
          } else {
            base::load(input$slctcreds)
            em <- outl$create_email(
              message,
              subject = input$mailsubject,
              to = input$emailtest
            )
            em$send()
          }
          
          shinybusy::remove_modal_spinner()
          shinyalert::shinyalert(
            "Test e-mail sent!", "Check the test recipient's inbox.",
            type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE
          )
          
        } else {
          shinyalert::shinyalert(
            "Check test e-mail address!", "Please enter a valid e-mail address for the test.",
            type = "error", closeOnEsc = FALSE, closeOnClickOutside = TRUE
          )
        }
      })
      
      
      
      
      
      
      
      
      shiny::observeEvent(input$sendfeedback, {
        
        feedback_data <- feedback_data()
        
        valid_recipients <- feedback_data$grades |>
          dplyr::select(test, intake, studentid, team, email) |>
          dplyr::filter(
            test == feedback_data$selected_answers$test[1],
            intake == feedback_data$selected_answers$intake
          ) |>
          base::unique() |>
          stats::na.omit()
        
        valid_recipients[base::grepl("^[[:alnum:]._-]+@[[:alnum:].-]+$", valid_recipients$email),]
        
        shiny::req(base::nrow(valid_recipients) > 0)
        
        shinybusy::show_modal_spinner(
          spin = "orbit",
          text = "Please wait while the e-mail is sent..."
        )
        
        
        
        msgnbr <- base::nrow(valid_recipients)
        pgr <- 1/msgnbr
        shinybusy::show_modal_progress_line(
          value = pgr, text = "Reporting progress:"
        )
        
        for (i in base::seq_len(msgnbr)){
          pgr <- pgr + 1/msgnbr
          studentid <- valid_recipients$student[i]
          teamid <- valid_recipients$team[i]
          emailaddress <- valid_recipients$email[i]
          
          
          
          
          
          shinybusy::update_modal_progress(pgr)
        }
        shinybusy::remove_modal_progress()
        shinyalert::shinyalert(
          "Task complete!", "A report has been sent to each recipient individually.",
          type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE
        )
        
      })
      
      
      
      
      
      
      
      
      
    }
  )
}
