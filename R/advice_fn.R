#' Specify configuration for fully-selected fishing mortality
#'
#' @param input list containing data, parameters, map, and random elements (output from \code{\link{wham::prepare_wham_input}})
#' @param F_opts (optional) named list of initial values for annual fully-selected fishing mortality and configuration method for estimation.
#' 
#' \code{F_opts} specifies a few as well as the effect on the population. Environmental covariate data need not span
#' the same years as the fisheries data. It can be \code{NULL} if no environmental data are to be fit.
#' Otherwise, it must be a named list with the following components:
#'   \describe{
#'     \item{$F}{matrix (n_years x n_fleets) of (initial) values for fully-selected fishing morality.}
#'     \item{$F_config}{integer 1: (default) configure F parameters (on log scale) as an F in the initial year and then deviations from one year to the next,
#'        or 2: configure F parameters as (log) annual values.}
#'  }
#'
#' @export
# Function to calculate catch advice
advice_fn = function(em, pro.yr = assess.interval){
  #make 5 year projections using F40. Use average SSB/R and YPR inputs over most recent 5 years
  proj_opts = list(n.yrs=pro.yr, use.FXSPR=TRUE, avg.yrs=tail(em$years,5),percentFXSPR=75, percentSPR=40) 
  em_proj = project_wham(em, proj.opts = proj_opts, MakeADFun.silent=TRUE) #projected version of the em
  advice = em_proj$rep$pred_catch[length(em_proj$years) + 1:pro.yr,] 
  return(advice)
}
