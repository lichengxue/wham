% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/do_reference_points.R
\name{do_reference_points}
\alias{do_reference_points}
\title{Add reporting of biological reference points to WHAM model}
\usage{
do_reference_points(model, do.sdrep = FALSE, save.sdrep = TRUE)
}
\arguments{
\item{model}{a fitted WHAM model object returned by fit_wham or project_wham.}

\item{do.sdrep}{T/F, calculate standard deviations of model parameters? See \code{\link[TMB]{sdreport}}. Default = \code{FALSE}.}

\item{save.sdrep}{T/F, save the full \code{\link[TMB]{TMB::sdreport}} object? If \code{FALSE}, only save \code{\link[TMB:summary.sdreport]{summary.sdreport}} to reduce model object file size. Default = \code{TRUE}.}
}
\description{
Changes internal flags to do the extra calculations and reporting for reference points.
}
\seealso{
\code{\link{fit_wham}}, \code{\link{project_wham}}
}
