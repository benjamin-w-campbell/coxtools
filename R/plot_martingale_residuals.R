#' Function to plot the deviance residuals of a fitted coxph object.
#'
#' This function assists the user in assessing model performance according to deviance residuals.
#' The two plots produced are designed to help the analyst understand which observations are poorly predicted and how model performance fairs with respect to timing of events.
#' @param coxph_fit The output from a fitted "coxph" call.
#' @keywords summary deviance residuals performance fit plot
#' @return A ggplot2 object presenting the deviance residuals for the model.
#' @examples
#' library(survival)
#' data("heart")
#' # Coerce from factor
#' heart$transplant <- as.numeric(heart$transplant)
#' # Rescale age
#' heart$age <- heart$age+48
#' fit <- coxph(Surv(start, stop, event) ~
#'               age + transplant +surgery,
#'              data = heart,
#'              x = TRUE)
#'
#' plot_martingale_residuals(coxph_fit = fit)
#'
#' @export

plot_martingale_residuals <- function(coxph_fit = NULL){

  time1 <- coxph_fit$y[,1]
  time2 <- coxph_fit$y[,2]
  event <- coxph_fit$y[,3]

  dev_data <- data.frame(obs = seq_len(length(time1)),
                         time1 = time1,
                         time2 = time2,
                         Time = time2-time1,
                         sentences =event,
                         dev_res = residuals(coxph_fit, type = 'deviance'))

  p1 <- ggplot(dev_data, aes_string("obs", "dev_res")) + geom_point(aes_string(color = "dev_res"), alpha = 0.2) +
    geom_smooth(se = FALSE, size = 0.5) +
    geom_hline(yintercept = 0) +
    xlab('Observation Number') + ylab('Deviance Residuals') +
    theme_bw() +
    theme(legend.position = c(0.2, 0.8),
          axis.text=element_text(size=10),
          axis.title=element_text(size=12,face="bold")) +
    guides(color=FALSE)
  p2 <- ggplot(dev_data, aes_string("Time", "dev_res")) + geom_point(aes_string(color = "dev_res"), alpha = 0.2) +
    geom_smooth(se = FALSE, size = 0.5) +
    geom_hline(yintercept = 0) +
    xlab('Time Until Next Event') + ylab('Deviance Residuals') +
    theme_bw() +
    theme(legend.position = c(0.2, 0.8),
          axis.text=element_text(size=10),
          axis.title=element_text(size=12,face="bold"),
          axis.title.y =element_blank()) +
    guides(color=FALSE)

  p <- Rmisc::multiplot(p1, p2, cols = 2)
  return(p)

}
