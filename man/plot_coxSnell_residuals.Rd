% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_coxSnell_residuals.R
\name{plot_coxSnell_residuals}
\alias{plot_coxSnell_residuals}
\title{Function to plot the Cox-Snell residuals of a fitted coxph object.}
\usage{
plot_coxSnell_residuals(coxph_fit = NULL)
}
\arguments{
\item{coxph_fit}{The output from a fitted "coxph" call.}
}
\value{
A ggplot2 object presenting the Cox-Snell residuals for the model.
}
\description{
This function assists the user in assessing model performance according to Cox-Snell residuals.
The plot produced is designed to help the analyst understand how well the model fits.  A well-fitting model will hug the diagonal.
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

plot_coxSnell_residuals(coxph_fit)


coxph_fit <- coxph(Surv(start, stop, event) ~
                     age + surgery + strata(transplant),
                  data = heart,
                  x = TRUE)

plot_coxSnell_residuals(coxph_fit)
}
\keyword{Cox-Snell}
\keyword{fit}
\keyword{performance}
\keyword{plot}
\keyword{residuals}
\keyword{summary}
