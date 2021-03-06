% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_predicted_probs.R
\name{plot_predicted_probs}
\alias{plot_predicted_probs}
\title{Function to plot the predicted probability of an event using a coxph object.}
\usage{
plot_predicted_probs(coxph_fit = NULL, var = NULL, time = NULL,
  seed = NULL,
  yaxis_label = "Survival Function: Probability of Not Observing Event",
  xaxis_label = "Variable Values", title = NULL)
}
\arguments{
\item{coxph_fit}{The output from a fitted "coxph" call.}

\item{var}{A character string specifying the variable from "coxph_fit" that predicted probabilities should be plotted for.}

\item{time}{This is the fixed time point that should be used to calculate the predicted probabilities.}

\item{seed}{This is the seed that should be set for replication}

\item{yaxis_label}{This is the label as it appears for the y-axis.  This is the probability of not observing an event.}

\item{xaxis_label}{This is the label ax it appears for the x-axis.  This is the range of variable values for the "var" variable.}

\item{title}{This is the plot title as it should appear.}
}
\value{
A ggplot2 object summarizing the corresponding predicted probability.
}
\description{
This function assists the user in interpreting effect sizes estimated using the coxph function.  It provides the predicted probability of not observing an event, the survival function, for a fixed time period over the full range of variable's values.
This routine is a plot-based wrapper for the "riskRegression" "predictCox" function, and as such, should be treated accordingly.
}
\examples{
library(survival)
data("heart")
# Coerce from factor
heart$transplant <- as.numeric(heart$transplant)
# Rescale age
heart$age <- heart$age+48
coxph_fit <- coxph(Surv(start, stop, event) ~
               age + transplant +surgery,
             data = heart,
             x = TRUE)

plot_predicted_probs(coxph_fit,
                     var = "age",
                     time = mean(heart$stop-heart$start),
                     seed = 123,
                     xaxis_label = "Age")

}
\keyword{interpretation}
\keyword{plot}
\keyword{probability}
\keyword{summary}
\keyword{transition}
