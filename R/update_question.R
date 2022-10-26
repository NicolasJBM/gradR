#' @name update_question
#' @title Update answers
#' @author Nicolas Mangin
#' @description Function updating question parameters (penalty, partial credits, points) based on user input.
#' @param test_name Character. Name of the test.
#' @param test_parameters Tibble. Items checked in answers.
#' @param edited_question Character. Code of the question to edit.
#' @param new_points Numeric. Points assigned to the question.
#' @param new_penalty Logical. Whether negative points should be counted.
#' @param new_partial_credits Logical Whether partial credits should be earned.
#' @return Tibble. Updated test parameters.
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @export


update_question <- function(
  test_name = NULL, test_parameters = NULL, edited_question = NULL,
  new_points = NULL, new_penalty = NULL, new_partial_credits = NULL
){
  
  base::stopifnot(
    !base::is.null(test_name),
    !base::is.null(test_parameters),
    !base::is.null(edited_question)
  )
  
  if (!base::is.null(new_points)){
    test_parameters <- test_parameters |>
      dplyr::mutate(
        points = dplyr::case_when(
          question == edited_question ~ new_points,
          TRUE ~ points
        )
      )
  }
  
  if (!base::is.null(new_penalty)){
    test_parameters <- test_parameters |>
      dplyr::mutate(
        penalty = dplyr::case_when(
          question == edited_question ~ base::as.numeric(new_penalty),
          TRUE ~ penalty
        )
      )
  }
  
  if (!base::is.null(new_partial_credits)){
    test_parameters <- test_parameters |>
      dplyr::mutate(
        partial_credits = dplyr::case_when(
          question == edited_question ~ base::as.numeric(new_partial_credits),
          TRUE ~ partial_credits
        )
      )
  }
  
  base::save(test_parameters, file = base::paste0(
    "5_tests/", test_name, "/test_parameters.RData"
  ))
  
  return(test_parameters)
}


