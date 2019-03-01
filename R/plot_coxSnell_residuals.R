#' Function to plot the Cox-Snell residuals of a fitted coxph object.
#'
#' This function assists the user in assessing model performance according to Cox-Snell residuals.
#' The plot produced is designed to help the analyst understand how well the model fits.  A well-fitting model will hug the diagonal.
#' @param coxph_fit The output from a fitted "coxph" call.
#' @keywords summary Cox-Snell residuals performance fit plot
#' @return A ggplot2 object presenting the Cox-Snell residuals for the model.
#' @examples
#' library(survival)
#' data("heart")
#' # Coerce from factor
#' heart$transplant <- as.numeric(heart$transplant)
#' # Rescale age
#' heart$age <- heart$age+48
#' coxph_fit <- coxph(Surv(start, stop, event) ~
#'                     age + transplant +surgery,
#'                   data = heart,
#'                   x = TRUE)
#'
#' plot_coxSnell_residuals(coxph_fit)
#'
#'
#' coxph_fit <- coxph(Surv(start, stop, event) ~
#'                      age + transplant + strata(surgery),
#'                   data = heart,
#'                   x = TRUE)
#'
#' plot_coxSnell_residuals(coxph_fit)
#' @export

plot_coxSnell_residuals <- function(coxph_fit = NULL){
  out <- coxph_fit$y[,3]

  cs <- out - residuals(coxph_fit, type = "martingale")

  if(!is.null(coxph_fit$strata)){
    fitres <- survfit(coxph(Surv(cs, out) ~ 1 +
                              strata(coxph_fit$strata)), type = "aalen")

    strata <- unname(fitres$strata)

    strata_cum_sums <- cumsum(strata)

    strata_vec <- vector("numeric", length = sum(strata))

    for(i in 1:length(strata)){
      vals <- rep(i, strata[i])
      if(i == 1){
        strata_vec[i:strata[i]] <- vals
      } else {
        start_index <- as.integer(strata_cum_sums[i-1]+1)
        end_index <- strata_cum_sums[i]
        strata_vec[start_index:end_index] <- vals
      }
    }

    cd_df <- data.frame(res = fitres$time,
                        ch = -log(fitres$surv),
                        Strata = as.factor(strata_vec))


    p <- ggplot(cd_df, aes(res, ch, color = Strata, group = Strata)) +
      geom_line(size = 1) + geom_abline(intercept = 0, slope = 1, size = 1) +
      ylab('Est. Cumulative Hazard') +
      theme_bw() +
      theme(legend.position = c(0.2, 0.8),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold")) +
      xlab('Cox-Snell Residuals')

  } else {
    fitres <- survfit(coxph(Surv(cs, out) ~ 1), type = "aalen")
    cd_df <- data.frame(res = fitres$time,
                        ch = -log(fitres$surv))

    p <- ggplot(cd_df, aes(res, ch)) +
      geom_line(size = 1, colour = "red") + geom_abline(intercept = 0, slope = 1, size = 1) +
      ylab('Est. Cumulative Hazard') +
      theme_bw() +
      theme(legend.position = c(0.2, 0.8),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold")) +
      xlab('Cox-Snell Residuals')

  }

  return(p)
}
