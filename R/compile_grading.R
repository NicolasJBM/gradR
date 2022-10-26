




compile_grading <- function(
    test_parameters, solutions, students,
    closed_answers, numeric_answers, open_answers,
    answers, results
){
  
  # Add students missing in the student list
  not_enrolled <- base::setdiff(
    base::union(base::union(closed_answers$student, numeric_answers$student), open_answers$student),
    students$student
  )
  if (base::length(not_enrolled) > 0){
    not_enrolled <- tibble::tibble(
      student = not_enrolled, team = base::as.character(),
      firstname = base::as.character(), lastname = base::as.character(),
      email = base::as.character(), enrolled = 0
    )
    students <- dplyr::bind_rows(students, not_enrolled)
    base::rm(not_enrolled)
  }
  
  # Add numeric answers missing in the solutions
  missing_numeric_propositions <- numeric_answers |>
    dplyr::select(test, version, proposition) |>
    base::unique() |>
    dplyr::mutate(proposition = base::as.character(proposition)) |>
    dplyr::anti_join(
      dplyr::select(solutions, test, version, proposition),
      by = c("test", "version", "proposition")
    )
  if (base::nrow(missing_numeric_propositions) > 0){
    missing_numeric_propositions <- missing_numeric_propositions |>
      dplyr::left_join(base::unique(
        dplyr::select(
          solutions, path, test, version, type,
          document, language, interrogation, scale
        )
      ),
      by = c("test", "version")
      ) |>
      dplyr::mutate(
        number = base::as.numeric(NA), letter = base::as.character(NA),
        item = base::as.character(NA), modifications = 0, value = 0,
        explanation = base::as.character(NA), keywords = base::as.character(NA),
        correct = 0, weight = 0
      ) |>
      dplyr::select(base::names(solutions))
    solutions <- dplyr::bind_rows(solutions, missing_numeric_propositions) |>
      dplyr::arrange(test, version, number)
  }
  base::rm(missing_numeric_propositions)
  
  # create missing numbers, letters, and and items
  possible_letters <- c(
    letters, base::sapply(letters, function(x) base::paste0(x, letters))
  )
  solutions <- solutions |>
    dplyr::group_by(test, version) |>
    tidyr::nest() |>
    dplyr::mutate(
      data = purrr::map(data, function(x,y){
        x <- dplyr::arrange(x, number)
        x$number <- 1:base::nrow(x)
        x$letter <- y[x$number]
        x
      }, y = possible_letters)
    ) |>
    tidyr::unnest(data) |>
    dplyr::ungroup()
  max_extra <- solutions$item[stringr::str_detect(solutions$item, "EXTRAITEM_")]
  max_extra <- base::as.numeric(stringr::str_remove_all(max_extra, "EXTRAITEM_"))
  max_extra <- base::max(stats::na.omit(max_extra))+1
  for (i in 1:nrow(solutions)){
    if (base::is.na(solutions$item[i])){
      solutions$item[i] <- base::paste0("EXTRAITEM_", max_extra)
      max_extra <- max_extra+1
    }
  }
  base::rm(possible_letters)
  
  # Update scoring
  scoring <- test_parameters |>
    dplyr::select(
      test, question, version, partial_credits, penalty, points
    ) |>
    dplyr::left_join(solutions, by = c("test","version")) |>
    dplyr::select(
      test, question, version, number, letter, item, language,
      proposition, scale, partial_credits, penalty, points, weight
    ) |>
    dplyr::group_by(
      test, question, version, number, letter, item, language,
      proposition, scale, partial_credits, penalty
    ) |>
    dplyr::summarise(
      points = base::mean(points),
      weight = base::mean(weight),
      .groups = "drop"
    )
  
  # update compiled answers to integrate new students, attempts, or propositions
  answers <- dplyr::bind_rows(
    dplyr::select(closed_answers, student, test, attempt, version),
    dplyr::select(numeric_answers, student, test, attempt, version),
    dplyr::select(open_answers, student, test, attempt, version)
  ) |>
    base::unique() |>
    tidyr::unite(student, student, attempt, sep = "-") |>
    dplyr::left_join(
      base::unique(dplyr::select(test_parameters, question, version)),
      by = "version"
    )  |>
    dplyr::left_join(
      base::unique(dplyr::select(solutions, test, version, letter)),
      by = c("test","version")
    ) |>
    dplyr::select(student, test, question, version, letter) |>
    dplyr::left_join(
      answers, by = c("student", "test", "question", "version", "letter")
    ) |>
    tidyr::replace_na(base::list(checked = 0)) |>
    dplyr::arrange(student, test, version, letter)
  
  # update results to account for new compiled answers and new scoring
  results <- answers |>
    tidyr::separate(student, into = c("student","attempt"), sep = "-") |>
    dplyr::left_join(
      scoring,
      by = base::intersect(base::names(scoring), base::names(answers))
    ) |>
    dplyr::select(
      student, test, attempt, question, version, number, letter, item,
      language, scale, partial_credits, penalty, points, checked, weight
    ) |>
    dplyr::mutate(earned = checked * weight) |>
    gradR::clean_points()
  
  compilation <- base::list(
    test_parameters = test_parameters,
    solutions = solutions,
    students  = students,
    closed_answers = closed_answers,
    numeric_answers = numeric_answers,
    open_answers = open_answers,
    answers = answers,
    results = results
  )
  
  return(compilation)
}

