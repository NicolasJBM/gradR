#' @name make_scale
#' @title Create a grading scale
#' @author Nicolas Mangin
#' @description Function creating the user interface to grade an item.
#' @param letter Character. Letter in the feedbacks database.
#' @param proposition Character. Answer/criterion selected or given.
#' @param scale Character. Whether the initial scale of the letter is "logical", "numeric", a "percentage", or "qualitative".
#' @param checked Integer. 1 if the student provided this answer, 0 otherwise.
#' @param correct Numeric. Whether the answer is correct.
#' @param ns Function. Function assigning input IDs to the relevant module.
#' @return A shiny user interface for checking or grading answers.
#' @import shiny
#' @export


make_scale <- function(
  letter, proposition, scale, checked, correct, ns
){
  
  if (scale == "logical" | scale == "numeric" ){
    
    if(checked == 1) tmpvalue <- TRUE else tmpvalue <- FALSE
    if (correct > 0) tmpicon <- "check" else tmpicon <- "times"
    if (correct > 0) tmpstatus <- "success" else tmpstatus <- "danger"
    
    shinyWidgets::prettyToggle(
      inputId = ns(base::paste0("check_", letter)),
      value = tmpvalue,
      label_on = proposition,
      label_off = proposition,
      icon_on = shiny::icon(tmpicon),
      status_on = tmpstatus,
      status_off = NULL
    )
    
  } else if (scale == "percentage"){
    
    shiny::sliderInput(
      inputId = ns(base::paste0("check_", letter)),
      label = proposition,
      min = 0,
      max = 1,
      step = 0.01,
      value = checked,
      width = "100%"
    )
    
  } else {
    
    shinyWidgets::radioGroupButtons(
      inputId = ns(base::paste0("check_", letter)),
      label = proposition, 
      choices = c(
        `Incorrect` = -0.5,
        `Missing` = 0,
        `Imprecise` = 0.5,
        `Correct` = 1
      ),
      selected = checked, size = "sm",
      justified = TRUE, status = "primary",
      checkIcon = base::list(yes = shiny::icon("check"))
    )
    
  }
  
}
