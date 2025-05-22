
retrieve_moodle_quiz <- function(sesskey = NA, quizid = NA){
  
  base::stopifnot(
    !base::is.na(sesskey),
    !base::is.na(quizid)
  )
  
  # To get both the sesskey and the quiz id go to
  # "edit activities" and check the url on a remove activity link.
  
  url <- base::paste0(
    "https://lms-bachelor.ehl.edu/mod/quiz/report.php?sesskey=",
    sesskey,
    "aH&download=csv&id=",
    quizid,
    "&mode=responses&attempts=enrolled_with&onlygraded=&qtext=&resp=1&right="
  )
  
  utils::browseURL(url)
}

