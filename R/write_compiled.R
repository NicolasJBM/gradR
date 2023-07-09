#' @name write_compiled
#' @title Write test results on disk
#' @author Nicolas Mangin
#' @description Function writing formated answers, results, and grades in the exam results folder
#' @param compiled List. All test results.
#' @param test_path Character. Path to the test folder.
#' @param write_parameters Logical. Whether test parameters should be updated on disk.
#' @param write_solutions Logical. Whether solutions should be rewritten on disk.
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom purrr map2
#' @importFrom tidyr nest
#' @importFrom utils write.csv
#' @export



write_compiled <- function(
    compiled,
    test_path,
    write_parameters = FALSE,
    write_solutions = FALSE
){
  
  path <- NULL
  data <- NULL
  correct <- NULL
  document <- NULL
  explanation <- NULL
  interrogation <- NULL
  item <- NULL
  keywords <- NULL
  language <- NULL
  letter <- NULL
  modifications <- NULL
  number <- NULL
  proposition <- NULL
  test <- NULL
  type <- NULL
  value <- NULL
  weight <- NULL
  email <- NULL
  firstname <- NULL
  lastname <- NULL
  student <- NULL
  team <- NULL
  
  if (write_parameters){
    test_parameters <- compiled$test_parameters
    base::save(
      test_parameters,
      file = base::paste0(test_path, "/test_parameters.RData")
    )
  }
  
  if (write_solutions){
    compiled$solutions |>
      dplyr::select(
        path,
        test, version, number, letter, item, type, document, language,
        modifications, interrogation, proposition,
        value, scale, explanation, keywords, correct, weight
      ) |>
      dplyr::group_by(path) |>
      tidyr::nest() |>
      dplyr::mutate(
        export = purrr::map2(
          data, path, function(x,y){
            utils::write.csv(x, y, row.names = FALSE)
          }
        )
      )
  }
  
  student_list_path <- base::paste0(test_path, "/6_students/student_list.csv")
  if (base::file.exists(student_list_path)){
    students <- readr::read_csv(student_list_path, col_types = "ccccc")
  } else students <- NA
  if (base::length(students != 5) | base::nrow(students) < base::nrow(compiled$students)){
    compiled$students |>
      dplyr::select(student, team, firstname, lastname, email) |>
      utils::write.csv(student_list_path, row.names = FALSE)
  }
  
  if (base::nrow(compiled$open_answers) > 0){
    utils::write.csv(
      compiled$open_answers,
      base::paste0(test_path, "/7_answers/open.csv"),
      row.names = FALSE
    )
  }
  
  utils::write.csv(
    compiled$answers,
    base::paste0(test_path, "/8_results/answers.csv"),
    row.names = FALSE
  )
  
  utils::write.csv(
    compiled$results,
    base::paste0(test_path, "/8_results/results.csv"),
    row.names = FALSE
  )
  
  utils::write.csv(
    compiled$question_grades,
    base::paste0(test_path, "/8_results/question_grades.csv"),
    row.names = FALSE
  )
  
  utils::write.csv(
    compiled$student_grades,
    base::paste0(test_path, "/8_results/student_grades.csv"),
    row.names = FALSE
  )
}
