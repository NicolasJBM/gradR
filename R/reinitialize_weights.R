#' @name reinitialize_weights
#' @title Re-initialize weights
#' @author Nicolas Mangin
#' @description Function computing new weights for items of each question based on test parameters.
#' @param test_parameters Tibble. Test parameters.
#' @param feedbacks Tibble. Local feedback files combined.
#' @param selected_question Character. Question file if one question in particular is targeted.
#' @return Feedback with updated weights
#' @importFrom dplyr case_when
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr n
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @export

reinitialize_weights <- function(
  test_parameters, feedbacks, selected_question = NULL
){
  
  correct <- NULL
  correctnbr <- NULL
  question <- NULL
  letter <- NULL
  partial_credits <- NULL
  penalty <- NULL
  points <- NULL
  propnbr <- NULL
  test <- NULL
  weight <- NULL
  item <- NULL
  letternbr <- NULL
  
  # Get or compute initial weights based on question definition
  weights <- test_parameters |>
    dplyr::select(
      test, question, version, points, partial_credits, penalty
    ) |>
    dplyr::left_join(feedbacks, by = c("test","version")) |>
    dplyr::select(
      test, question, version, points, partial_credits, penalty,
      letter, item, correct, weight
    ) |>
    base::unique() |>
    dplyr::mutate(weight = base::as.numeric(weight))
    
  # this is necessary because of two statements questions
  correct_incorrect_nbr <- weights |>
    dplyr::group_by(test, version, letter) |>
    dplyr::summarise(correct = base::min(correct), .groups = "drop") |>
    dplyr::group_by(test, version) |>
    dplyr::summarise(
      propnbr = dplyr::n(),
      correctnbr = base::sum(correct), .groups = "drop"
    ) |>
    dplyr::mutate(incorrectnbr = propnbr - correctnbr) |>
    base::unique()
  
  weights <- weights |>
    dplyr::left_join(correct_incorrect_nbr, by = c("test","version"))
  
  if (!base::is.null(selected_question)){
    weights <- weights |>
      dplyr::mutate(
        weight = dplyr::case_when(
          question == selected_question ~ base::as.numeric(NA),
          TRUE ~ weight
        )
      )
  }
  
  letter_nbr <- weights |>
    dplyr::group_by(test, version, letter) |>
    dplyr::summarise(letternbr = dplyr::n(), .groups = "drop")
  
  
  weights <- weights |>
    dplyr::left_join(letter_nbr, by = c("test","version","letter")) |>
    dplyr::mutate(correctnbr = correctnbr * letternbr) |>
    dplyr::mutate(weight = dplyr::case_when(
      !base::is.na(weight) ~ weight,
      base::is.na(weight) & correct == 1 ~ points / correctnbr,
      base::is.na(weight) & correct == 0.5 ~ 0.5 * points / correctnbr,
      base::is.na(weight) & correct == 0 & penalty == 1 ~ - points / incorrectnbr,
      TRUE ~ 0
    )) |>
    dplyr::group_by(test, version, letter) |>
    dplyr::summarise(weight = base::mean(weight))
  
  feedbacks <- feedbacks |>
    dplyr::select(-weight) |>
    base::unique() |>
    dplyr::left_join(weights, by = c("test","version","letter")) |>
    base::unique()
  
  return(feedbacks)
}


