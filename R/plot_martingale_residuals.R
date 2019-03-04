#' Function to plot the Martingale residuals of a fitted coxph object.
#'
#' This function assists the user in assessing model performance according to Martingale residuals.
#' Two plots for each covariate are presented.  One showing the Martingale residuals per mdoel with and without the covariate.
#' @param coxph_fit The output from a fitted "coxph" call.
#' @keywords summary martingale residuals performance fit plot
#' @return A ggplot2 object presenting the martingale residuals for the model.
#' @examples
#' library(survival)
#' data("heart")
#' # Coerce from factor
#' heart$transplant <- as.numeric(heart$transplant)
#' # Rescale age
#' heart$age <- heart$age+48
#' coxph_fit <- coxph(Surv(start, stop, event) ~
#'               age + transplant +surgery,
#'              data = heart,
#'              x = TRUE)
#'
#' plot_martingale_residuals(coxph_fit = coxph_fit)
#'
#' @export

plot_martingale_residuals <- function(coxph_fit = NULL){



}
