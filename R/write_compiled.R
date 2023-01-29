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
  
  if (write_parameters){
    test_parameters <- compiled$test_parameters
    base::save(
      test_parameters,
      file = base::paste0(test_path, "/test_parameters.RData")
    )
  }
  
  if (write_solutions){
    compiled$solutions |>
      dplyr::group_by(path) |>
      tidyr::nest() |>
      dplyr::mutate(
        export = purrr::map2(
          data, path, function(x,y){
            utils::write.csv(x, y, row.names = FALSE)
            return("OK")
          }
        )
      )
  }
  
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
