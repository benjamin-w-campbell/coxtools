#' Function to plot the DFBetas of a fitted coxph object.
#'
#' This function assists the user in assessing model performance according to DEFBetas.
#' # The two plots produced are designed to help the analyst understand which observations are poorly preducted and how model performance fairs with respect to timing of events.
#' @param coxph_fit The output from a fitted "coxph" call.
#' @param var_names A custom vector of variable names that correspond to the variables in the model.  Should be ordered as the covariates enter the model on the RHS of the formula.
#' @keywords summary DFBetas performance fit plot
#' @return A ggplot2 object presenting the DFBetas for the model.
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
#' plot_dfbetas(coxph_fit = fit, var_names = c("Age", "Transplant", "Surgery"))
#' @export


plot_dfbetas <- function(coxph_fit = NULL, var_names = NULL){
  dfb <- data.frame(residuals(coxph_fit, type = "dfbetas"))
  if(!is.null(var_names)){
    colnames(dfb) <- var_names
  } else {
    colnames(dfb) <- paste0("Var", 1:ncol(dfb))
  }
  dfb$Observation = seq_len(nrow(dfb))
  dfb <- data.table::melt(dfb,  id.vars = "Observation",
                          variable.name = "Covariate",
                          variable.factor = FALSE,
                          value.name = "dfbeta")
  p <- ggplot(dfb, aes(Observation, dfbeta)) +
    # geom_point(size = 0.1) +
    # geom_point(aes(size = abs(dfbeta)/10000, color = dfbeta), alpha = 0.5) +
    geom_point(aes(size = abs(dfbeta)/10000), alpha = 0.5) +
    # geom_point(aes(color = dfbeta), size =0.1, alpha = 0.5) +
    facet_wrap(~ Covariate, scales = "free") +
    theme_bw() +
    geom_hline(yintercept = 0) +
    xlab('Observation Number') +
    ylab('DFBETAs') +
    theme(strip.background = element_rect(fill = "white", color = "white"),
          legend.position = "none",
          axis.text.x = element_text(size = 7),
          axis.text.y = element_text(size = 9),
          panel.margin.x = unit(0.7, "lines"),
          panel.margin.y = unit(0.7, "lines"),
          axis.text=element_text(size=10),
          axis.title=element_text(size=12,face="bold"))

  return(p)
}


