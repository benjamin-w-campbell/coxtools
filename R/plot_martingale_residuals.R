#' Function to plot the Martingale residuals of a fitted coxph object.
#'
#' This function assists the user in assessing model performance according to Martingale residuals.
#' A list of plots.  There will be two plots for each covariate.  One showing the Martingale residuals per mdoel with and without the covariate.
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
#' plot_list <- plot_martingale_residuals(coxph_fit = coxph_fit)
#'
#' Rmisc::multiplot(plotlist = plot_list, cols = 3)
#'
#' @export

plot_martingale_residuals <- function(coxph_fit = NULL){

  covariates <- all.vars(coxph_fit$formula[[3]])

  dat <- as.data.frame(coxph_fit$x)
  outcomes <- coxph_fit$y
  colnames(outcomes) <- c("time1", "time2", "event")

  dat <- as.data.frame(cbind(dat, outcomes))

  martin_dt <- rbindlist(lapply(1:length(covariates), function(i) {
    f <- as.formula(paste("outcomes ~",
                          paste0(covariates[-i], collapse = "+")))
    res_without <- residuals(coxph(f, data = dat), type = "martingale")
    res_with <- residuals(coxph_fit, type = "martingale")
    d <- data.table(cov_name = covariates[i],
                    cov_value = as.numeric(dat[[covariates[i]]]),
                    res_with = res_with,
                    res_without = res_without)
    setnames(d, c("res_with", "res_without"),
             c("With Covariate", "Without Covariate"))
    melt(d, id.vars = c("cov_name", "cov_value"),
         variable.name = "res", value.name = "res_value")
  }))

  levs <- expand.grid(res = sort(unique(martin_dt$res)),
                      cov_name = sort(unique(martin_dt$cov_name)),
                      stringsAsFactors = FALSE)

  martin_plot_cf <- lapply(seq_len(nrow(levs)), function(i) {
    d <- martin_dt[cov_name == levs$cov_name[i] &
                     res == levs$res[i]]
    p <- ggplot(d, aes(cov_value, res_value)) +
      geom_point(alpha = 0.1) +
      theme_bw() +
      xlab(levs$cov_name[i]) +
      theme(axis.text.x = element_text(size = 7),
            axis.text.y = element_text(size = 7),
            axis.title.y = element_text(size = 9),
            axis.title.x = element_text(size = 9))

      p <- p + geom_smooth(method = 'loess', se = FALSE, size = 0.5)
    if (levs$res[i] == "With Covariate") {
      p <- p + ylab('Residual, With Cov.')
    } else {
      p <- p + ylab('Residual, Without Cov.')
    }
    p
  })

  return(martin_plot_cf)
}

