#' Update the operating model and generate data
#' 
#' Function to update F in the operating model (see \code{\link{update_om_F}}) and generate data given the updated F.
#' 
#' @param om Operating model with years including burn-in + all feedback years
#' @param om2 Operating model with years including burn-in years + n feedback years (n = the number of assessments completed x assessment interval)
#' @param interval.info Catch advice for a number of years projected from the estimation model
#'   \itemize{
#'     \item \code{"$years"} projection years 
#'     \item \code{"$catch"} matrix (n_region x n_years) projected catch 
#'     \item \code{"=NULL"} generate data in the operating model
#'     }
#' @param seed Seed used to generate data
#' 
#' @return an operating model with simulated data and updated F time series
#'   
#' @export
#'
#' @seealso \code{\link{update_om_F}}
#' 
update_om_fn <- function(om, om2, interval.info, seed = 123) {
  proj_opts = list(n.yrs=length(interval.info$years), proj.catch = interval.info$catch)
  em_proj = project_wham(om2, proj.opts = proj_opts, MakeADFun.silent=TRUE)
  assess_interval = length(interval.info$years)
  updated_F = log(tail(em_proj$rep$Fbar,assess_interval))
  cat(paste0("\nFsolve: ", exp(updated_F),"\n"))
  year_ind = which(om$years %in% interval.info$years)
  om$input$par$F_pars[year_ind,] = updated_F
  
  obs_names = c("agg_indices","agg_catch","catch_paa","index_paa", "Ecov_obs", "obsvec")
  set.seed(seed)
  om_sim = om$simulate(complete=TRUE) #resimulate the population and observations
  om$input$data[obs_names] = om_sim[obs_names] #update any simulated data
  om$input$par[om$input$random] = om_sim[om$input$random]
  om <- fit_wham(om$input, do.fit = FALSE, do.retro = FALSE, do.osa = FALSE, MakeADFun.silent = TRUE)
  return(om)
}
