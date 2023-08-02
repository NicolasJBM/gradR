#' @name clean_points
#' @title Clean points earned
#' @author Nicolas Mangin
#' @description Function applying penalties or partial credits based on question parameters.
#' @param results Tibble. Feedback or results used for reporting.
#' @return Tibble with clean earned points.
#' @importFrom dplyr case_when
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr ungroup
#' @export


clean_points <- function(results){
  
  earned <- NULL
  points <- NULL
  attempt <- NULL
  student <- NULL
  test <- NULL
  total_earned <- NULL
  
  results |> dplyr::mutate(
    points = base::as.double(points),
    earned = base::as.double(earned)
  ) |>
    dplyr::group_by(student, test, attempt, version) |>
    dplyr::mutate(total_earned = base::sum(earned)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      earned = dplyr::case_when(
        penalty == 0 & earned < 0 ~ 0,
        TRUE ~ earned
      )
    ) |>
    dplyr::mutate(
      earned = dplyr::case_when(
        partial_credits == 0 & total_earned >= 0 & total_earned < points ~ 0,
        TRUE ~ earned
      )
    ) |>
    dplyr::select(-total_earned)
}
