#' @name compile_grading
#' @title Compile all test data
#' @author Nicolas Mangin
#' @description Function Gathering and updating test data.
#' @param test_parameters Tibble.
#' @param solutions Tibble.
#' @param students Tibble.
#' @param closed_answers Tibble.
#' @param numeric_answers Tibble.
#' @param open_answers Tibble.
#' @param open_answers_txt Tibble.
#' @param answers Tibble.
#' @param parameter_change Character. If there is a change in a question parameters, give the question id.
#' @return List with all compiled and updated test data.
#' @importFrom dplyr anti_join
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom purrr map
#' @importFrom stats na.omit
#' @importFrom stringr str_detect
#' @importFrom stringr str_remove_all
#' @importFrom tibble tibble
#' @importFrom tidyr nest
#' @importFrom tidyr replace_na
#' @importFrom tidyr separate
#' @importFrom tidyr unite
#' @importFrom tidyr unnest
#' @export



compile_grading <- function(
    test_parameters, solutions, students,
    closed_answers, numeric_answers, open_answers, open_answers_txt,
    answers, parameter_change = NA
){
  
  proposition <- NULL
  path <- NULL
  type <- NULL
  document <- NULL
  language <- NULL
  interrogation <- NULL
  number <- NULL
  data <- NULL
  question <- NULL
  partial_credits <- NULL
  penalty <- NULL
  points <- NULL
  letter <- NULL
  item <- NULL
  weight <- NULL
  student <- NULL
  attempt <- NULL
  questype <- NULL
  raw_checked <- NULL
  checked <- NULL
  earned <- NULL
  grade <- NULL
  correct <- NULL
  count <- NULL
  duplication <- NULL
  nbrcorrect <- NULL
  nbrincorrect <- NULL
  standard_weight <- NULL
  test <- NULL
  
  # Add students missing in the student list
  if (test_parameters$test_unit[1] == "student"){
    not_enrolled <- base::setdiff(
      base::union(base::union(closed_answers$student, numeric_answers$student), open_answers$student),
      students$student
    )
    if (base::length(not_enrolled) > 0){
      not_enrolled <- tibble::tibble(
        student = not_enrolled, team = base::as.character(NA),
        firstname = base::as.character(NA), lastname = base::as.character(NA),
        email = base::as.character(NA), enrolled = 0
      )
      students <- dplyr::bind_rows(students, not_enrolled)
      base::rm(not_enrolled)
    }
  } else {
    not_enrolled <- base::setdiff(
      base::union(base::union(closed_answers$student, numeric_answers$student), open_answers$student),
      students$team
    )
    if (base::length(not_enrolled) > 0){
      not_enrolled <- tibble::tibble(
        student = base::as.character(NA), team = not_enrolled,
        firstname = base::as.character(NA), lastname = base::as.character(NA),
        email = base::as.character(NA), enrolled = 0
      )
      students <- dplyr::bind_rows(students, not_enrolled)
      base::rm(not_enrolled)
    }
  }
  
  
  
  # Add numeric answers missing in the solutions
  missing_numeric_propositions <- numeric_answers |>
    dplyr::select(version, proposition) |>
    base::unique() |>
    dplyr::mutate(proposition = base::as.character(proposition)) |>
    dplyr::anti_join(
      dplyr::select(solutions, version, proposition),
      by = c("version", "proposition")
    ) |>
    dplyr::arrange(version, proposition) |>
    base::unique()
  if (base::nrow(missing_numeric_propositions) > 0){
    missing_numeric_propositions <- missing_numeric_propositions |>
      dplyr::left_join(base::unique(
        dplyr::select(
          solutions, path, test, version, type,
          language, interrogation, scale
        )
      ),
      by = "version"
      ) |>
      dplyr::mutate(
        number = base::as.numeric(NA),
        letter = base::as.character(NA),
        item = base::as.character(NA),
        document = base::as.character(NA),
        modifications = 0,
        value = 0,
        explanation = base::as.character(NA),
        keywords = base::as.character(NA),
        correct = 0,
        weight = 0
      ) |>
      dplyr::select(base::names(solutions))
    solutions <- dplyr::bind_rows(solutions, missing_numeric_propositions) |>
      dplyr::arrange(version, number)
  }
  base::rm(missing_numeric_propositions)
  
  
  
  # create missing numbers, letters, and and items
  possible_letters <- c(
    letters, base::sapply(letters, function(x) base::paste0(x, letters))
  )
  solutions <- solutions |>
    dplyr::group_by(version) |>
    tidyr::nest() |>
    dplyr::mutate(
      data = purrr::map(data, function(x,y){
        x <- dplyr::arrange(x, number)
        for (i in base::seq_len(base::nrow(x))) {
          if (base::is.na(x$number[i])) x$number[i] <- i
        }
        x$letter <- y[x$number]
        x
      }, y = possible_letters)
    ) |>
    tidyr::unnest(data) |>
    dplyr::ungroup()
  max_extra <- solutions$item[stringr::str_detect(solutions$item, "EXTRAITEM")]
  max_extra <- base::as.numeric(stringr::str_remove_all(max_extra, "EXTRAITEM"))
  if (base::length(stats::na.omit(max_extra)) > 0) {
    max_extra <- base::max(stats::na.omit(max_extra))+1
  } else max_extra <- 1
  for (i in 1:nrow(solutions)){
    if (base::is.na(solutions$item[i])){
      solutions$item[i] <- base::paste0("EXTRAITEM", max_extra)
      max_extra <- max_extra+1
    }
  }
  base::rm(possible_letters)
  
  
  
  # Update standard scoring
  standard_scoring <- test_parameters |>
    dplyr::select(version, penalty, points) |>
    dplyr::left_join(
      dplyr::select(solutions, version, item, letter, correct, weight),
      by = c("version")
    ) |>
    dplyr::group_by(version, letter) |>
    dplyr::mutate(duplication = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::group_by(version) |>
    dplyr::mutate(
      count = dplyr::n(),
      nbrcorrect = base::sum(correct),
      nbrincorrect = count - nbrcorrect
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      standard_weight = points*duplication*(correct/nbrcorrect-penalty*(1-correct)/max(1,nbrincorrect)) #Change this so that when there is duplication, weight are correct weighted by points
    ) |>
    dplyr::mutate(
      standard_weight = dplyr::case_when(
        standard_weight > 0 & correct == 0 ~ 0,
        TRUE ~ standard_weight
      )
    ) |>
    dplyr::select(version, item, letter, standard_weight)
  
  
  
  # Update solutions weights with new standard scoring
  solutions <- solutions |>
    dplyr::left_join(standard_scoring, by = c("version","item","letter"))
  if (!base::is.na(parameter_change)){
    selected_versions <- test_parameters |>
      dplyr::filter(question == parameter_change)
    solutions <- solutions |>
      dplyr::mutate(
        weight = dplyr::case_when(
          version %in% selected_versions$version ~ standard_weight,
          TRUE ~ weight
        )
      )
  } else {
    solutions <- solutions |>
      dplyr::mutate(
        weight = dplyr::case_when(
          base::is.na(weight) ~ standard_weight,
          TRUE ~ weight
        )
      )
  }
  solutions <- dplyr::select(solutions, -standard_weight)
  
  
  
  # update compiled answers to integrate new students, attempts, or propositions
  raw_empty <- tibble::tibble(
    student = base::character(0),
    attempt = base::numeric(0),
    version = base::character(0),
    letter = base::character(0),
    questype = base::character(0),
    raw_checked = base::numeric(0)
  )
  
  if (base::nrow(closed_answers)>0){
    raw_closed <- closed_answers |>
      dplyr::select(student, attempt, version, letter) |>
      dplyr::group_by(student, attempt, version) |>
      tidyr::nest() |>
      dplyr::mutate(data = purrr::map(data, function(x){
        base::unique(base::as.character(x$letter))
      })) |>
      dplyr::left_join(
        base::unique(dplyr::select(solutions, version, letter)),
        by = "version"
      ) |>
      dplyr::mutate(
        raw_checked = purrr::map2_dbl(letter, data, function(x,y){
          base::as.numeric(x %in% y)
        }),
        questype = "closed") |>
      dplyr::select(student, attempt, version, letter, questype, raw_checked) |>
      dplyr::ungroup() |>
      base::unique()
  } else {
    raw_closed <- raw_empty
  }
  
  if (base::nrow(numeric_answers)>0){
    raw_numeric <- numeric_answers |>
      dplyr::select(student, attempt, version, proposition) |>
      dplyr::group_by(student, attempt, version) |>
      tidyr::nest() |>
      dplyr::mutate(data = purrr::map(data, function(x){
        base::unique(base::as.character(x$proposition))
      })) |>
      dplyr::left_join(
        base::unique(dplyr::select(solutions, version, letter, proposition)),
        by = "version"
      ) |>
      dplyr::mutate(
        raw_checked = purrr::map2_dbl(proposition, data, function(x,y){
          base::as.numeric(x %in% y)
        }),
        questype = "numeric") |>
      dplyr::select(student, attempt, version, letter, questype, raw_checked) |>
      dplyr::ungroup() |>
      base::unique()
  } else {
    raw_numeric <- raw_empty
  }
  
  if (base::nrow(open_answers_txt)>0){
    raw_open <- open_answers_txt |>
      dplyr::select(student, attempt, version) |>
      dplyr::left_join(
        base::unique(dplyr::select(solutions, version, letter)),
        by = "version"
      ) |>
      dplyr::mutate(raw_checked = 0, questype = "open") |>
      dplyr::select(student, attempt, version, letter, questype, raw_checked)
    if (base::nrow(open_answers) > 0) {
      raw_open <- raw_open |>
        dplyr::left_join(open_answers, by = c("student", "attempt", "version", "letter")) |>
        dplyr::mutate(raw_checked = dplyr::case_when(
          !base::is.na(checked) ~ checked,
          TRUE ~ raw_checked
        )) |>
        dplyr::select(-checked)
    }
  } else {
    raw_open <- raw_empty
  }
  
  answers <- dplyr::bind_rows(raw_closed, raw_numeric, raw_open) |>
    base::unique()|>
    dplyr::left_join(
      base::unique(dplyr::select(test_parameters, question, version)),
      by = "version"
    ) |>
    dplyr::left_join(
      dplyr::select(answers, student, attempt, version, letter, checked),
      by = c("student", "attempt", "version", "letter") 
    ) |>
    dplyr::right_join(
      base::unique(dplyr::select(solutions, version, letter)),
      by = c("version","letter")
    ) |>
    dplyr::mutate(
      checked = dplyr::case_when(
        questype == "closed" ~ raw_checked,
        questype == "numeric" ~ raw_checked,
        questype == "open" ~ raw_checked,
        base::is.na(checked) ~ 0,
        TRUE ~ checked
      )
    )
  
  open_answers <- answers |>
    dplyr::filter(questype == "open") |>
    dplyr::select(student, attempt, version, letter, checked) |>
    dplyr::arrange(student, attempt, version, letter)
  
  answers <- answers |>
    dplyr::select(student, attempt, question, version, letter, checked) |>
    dplyr::arrange(student, attempt, version, letter)
  
  # update results to account for new compiled answers and new scoring
  aggregated_weights <- solutions |>
    dplyr::group_by(version, number, letter, item, language, correct, scale) |>
    dplyr::summarise(weight = base::sum(weight), .groups = "drop")
  
  question_parameters <- test_parameters |>
    dplyr::select(question, partial_credits, penalty, points) |>
    base::unique()
  
  results <- answers |>
    dplyr::left_join(aggregated_weights, by = c("version","letter")) |>
    dplyr::left_join(question_parameters, by = c("question")) |>
    dplyr::mutate(earned = checked * weight) |>
    dplyr::select(
      student, attempt, question, version, number, letter, item,
      language, scale, partial_credits, penalty, points, checked, correct,
      weight, earned
    ) |>
    dplyr::group_by(
      student, attempt, question, version, partial_credits, penalty, points, checked
    ) |>
    tidyr::nest() |>
    dplyr::mutate(total_earned = purrr::map_dbl(data, function(x) base::sum(x$earned))) |>
    tidyr::unnest(data) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      earned = dplyr::case_when(
        partial_credits == 0 & total_earned < points ~ 0,
        penalty == 0 & total_earned < 0 ~ 0,
        TRUE ~ earned
      )
    ) |>
    dplyr::select(
      student, attempt, question, version, number, letter, item,
      language, scale, partial_credits, penalty, points, checked, correct,
      weight, earned
    )
  
  # Compute grades
  question_grades <- results |>
    dplyr::group_by(student, attempt, question, partial_credits, penalty) |>
    dplyr::summarise(
      points = base::max(points, na.rm = TRUE),
      earned = base::sum(earned, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate_if(base::is.numeric, base::as.double) |>
    dplyr::mutate(
      earned = dplyr::case_when(
        earned > points ~ points,
        partial_credits == 0 & earned > 0 & earned < points ~ 0,
        penalty == 0 & earned < 0 ~ 0,
        TRUE ~ earned
      )
    ) |>
    dplyr::select(-partial_credits, -penalty) |>
    dplyr::mutate(earned = base::round(earned, 2))
  
  student_grades <- question_grades |>
    dplyr::group_by(student, attempt) |>
    dplyr::summarise(
      points = base::sum(points, na.rm = TRUE),
      grade = base::sum(earned, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(grade = base::round(grade, 2))
  
  compilation <- base::list(
    test_parameters = test_parameters,
    solutions = solutions,
    students  = students,
    closed_answers = closed_answers,
    numeric_answers = numeric_answers,
    open_answers = open_answers,
    open_answers_txt = open_answers_txt,
    answers = answers,
    results = results,
    question_grades = question_grades,
    student_grades = student_grades
  )
  
  return(compilation)
}

