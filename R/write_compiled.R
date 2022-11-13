#' @name write_compiled
#' @title Write test results on disk
#' @author Nicolas Mangin
#' @description Function writing formated answers, results, and grades in the exam results folder
#' @param compiled List. All test results.
#' @param test_path Character. Path to the test folder.
#' @importFrom utils write.csv
#' @export



write_compiled <- function(compiled, test_path){
  
  path <- NULL
  data <- NULL
  
  compiled$solutions |>
    dplyr::group_by(path) |>
    tidyr::nest() |>
    dplyr::mutate(data = purrr::map2(path, data, function(x,y){
      utils::write.csv(y, x, row.names = FALSE)
    }))
  
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