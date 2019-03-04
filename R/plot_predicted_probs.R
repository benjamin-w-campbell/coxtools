#' Function to plot the predicted probability of an event using a coxph object.
#'
#' This function assists the user in interpreting effect sizes estimated using the coxph function.  It provides the predicted probability of not observing an event, the survival function, for a fixed time period over the full range of variable's values.
#' This routine is a plot-based wrapper for the "riskRegression" "predictCox" function, and as such, should be treated accordingly.
#' @param coxph_fit The output from a fitted "coxph" call.
#' @param var A character string specifying the variable from "coxph_fit" that predicted probabilities should be plotted for.
#' @param time This is the fixed time point that should be used to calculate the predicted probabilities.
#' @param seed This is the seed that should be set for replication
#' @param yaxis_label This is the label as it appears for the y-axis.  This is the probability of not observing an event.
#' @param xaxis_label This is the label ax it appears for the x-axis.  This is the range of variable values for the "var" variable.
#' @param title This is the plot title as it should appear.
#' @keywords summary interpretation plot transition probability
#' @return A ggplot2 object summarizing the corresponding predicted probability.
#' @examples
#' library(survival)
#' data("heart")
#' # Coerce from factor
#' heart$transplant <- as.numeric(heart$transplant)
#' # Rescale age
#' heart$age <- heart$age+48
#' coxph_fit <- coxph(Surv(start, stop, event) ~
#'                age + transplant +surgery,
#'              data = heart,
#'              x = TRUE)
#'
#' plot_predicted_probs(coxph_fit,
#'                      var = "age",
#'                      time = mean(heart$stop-heart$start),
#'                      seed = 123,
#'                      xaxis_label = "Age")
#'
#' @export

plot_predicted_probs <- function(coxph_fit = NULL, var = NULL, time = NULL, seed = NULL,
                                 yaxis_label = 'Survival Function: Probability of Not Observing Event',
                                 xaxis_label = 'Variable Values',
                                 title = NULL){

  if(!is.null(seed)){
    set.seed(seed)
  }

  dat <- coxph_fit$x
  var_mean = mean(dat[,var])
  var_sd = sd(dat[,var])
  # thin all values to avoid DOF problems, take first, second, and then every other after value after the second
  values_to_take = sort(unique(dat[,var]))[c(1,seq(2, length(unique(dat[,var])), 2))]

  # now create new data frame for predicted probability estimation
  mean_or_median <- function(x){
    l <- length(table(x))
    if(l == 2){
      out <- median(x, na.rm = TRUE)
    } else {
      out <- mean(x, na.rm = TRUE)
    }
    return(out)
  }

  controls <- apply(as.data.frame(dat[, (colnames(dat) != var)]), 2, mean_or_median)

  pred_data <- data.frame(
    id = c(as.character(1:length(values_to_take))),
    var = values_to_take
  )

  colnames(pred_data)[2] <- var

  # loop through and add columns to populate this prediction dataframe
  for(i in 1:length(controls)){
    pred_data <- cbind(pred_data, controls[i])
    colnames(pred_data)[i+2] <- names(controls)[i]
  }

  basehaz <- riskRegression::predictCox(coxph_fit, newdata = pred_data, times = time, band = TRUE, iid=FALSE, store.iid = "minimal")

  plot_df <- data.frame(
    X = pred_data[,var],
    Y = basehaz$survival,
    Y_Min = basehaz$survival.lowerBand,
    Y_Max = basehaz$survival.upperBand
  )

  p = ggplot(data = plot_df, aes_string(x = "X", y = "Y")) +
    geom_line(col = "firebrick4", size = 1.5) +
    geom_ribbon(aes_string(ymin="Y_Min", ymax="Y_Max"), col = "firebrick4", fill = "firebrick4", alpha = 0.25)  +
    theme_bw()  +
    theme(legend.position = c(0.2, 0.8),
          axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold")) +
    ylab(yaxis_label) +
    xlab(xaxis_label)

  if(!is.null(title)){
    p <- p+ggtitle(title)
  }

  if(nrow(plot_df) == 2){
    p <- p + scale_x_continuous(breaks=c(0,1))
  }

  return(p)

}
