#' Function to plot the deviance residuals of a fitted coxph object.
#'
#' This function assists the user in assessing model performance according to deviance residuals.
#' # The two plots produced are designed to help the analyst understand which observations are poorly preducted and how model performance fairs with respect to timing of events.
#' @param coxph_fit The output from a fitted "coxph" call.
#' @param time1 This is the start time column used in constructing the "Surv" object.
#' @param time2 This is the end time column used in constructing the "Surv" object.
#' @keywords summary deviance residuals performance fit plot
#' @return A ggplot2 object presenting the deviance residuals for the model.
#' @examples
#' library(survival)
#' library(survival)
#' data("heart")
#' # Coerce from factor
#' heart$transplant <- as.numeric(heart$transplant)
#' # Rescale age
#' heart$age <- heart$age+48
#' fit <- coxph(Surv(start, stop, event) ~
#'                age + transplant +surgery,
#'              data = heart,
#'              x = TRUE)
#'
#' plot_predicted_probs(fit,
#'                      var = "age",
#'                      time = mean(heart$stop-heart$start),
#'                      seed = 123,
#'                      xaxis_label = "Age-48 Years")
#'
#' @export

plot_martingale_residuals <- function(coxph_fit = NULL, time1 = NULL, time2 = NULL){



}
