#' Update the operating model and generate data
#' 
#' Function to update F in the operating model (see \code{\link{update_om_F}}) and generate data given the updated F.
#' 
#' @param om Operating model with years including burn-in + feedback years
#' @param interval.info Catch advice for a number of years projected from the estimation model
#'   \itemize{
#'     \item \code{"$years"} projection years 
#'     \item \code{"$catch"} matrix (n_region x n_years) projected catch 
#'     }
#' @param seed Seed used to generate data
#' 
#' @return an operating model with simulated data and updated F time series
#'   
#' @export
#'
#' @seealso \code{\link{get_F_from_Catch_region}}
#' 
update_om_fn <- function(om, interval.info, seed = 123) {
  # Iterative update F in the OM using get_F_from_Catch_region function
  t = 0
  for (y in interval.info$years) {
    year = which(om$years == y)
    t = t+1
    rep = om$rep
    Fsolve = get_F_from_Catch_region(Catch = interval.info$catch[t,], 
                                     NAA   = rep$NAA[,,year,], 
                                     log_M = log(rep$MAA[,,year,]), 
                                     mu    = rep$mu[,,,year,,], 
                                     L     = rep$L[year,], 
                                     sel   = (rep$FAA/max(rep$FAA))[,year,], 
                                     fracyr_season = om$input$data$fracyr_seasons, 
                                     fleet_regions = om$input$data$fleet_regions, 
                                     fleet_seasons = om$input$data$fleet_seasons, 
                                     can_move = om$input$data$can_move, 
                                     mig_type = om$input$data$mig_type, 
                                     waacatch = om$input$data$waa[om$input$data$waa_pointer_totcatch, year,], 
                                     trace = TRUE, 
                                     F_init = rep(0.5,length(om$input$data$n_fleets)))
    cat(paste0("\nFsolve: ", round(Fsolve,3), " in Year ", y,"\n"))
    om$input$par$F_pars[year,] = log(Fsolve)
    obs_names = c("agg_indices","agg_catch","catch_paa","index_paa", "Ecov_obs", "obsvec")
    set.seed(seed)
    om_sim = om$simulate(complete=TRUE) #resimulate the population and observations
    om$input$data[obs_names] = om_sim[obs_names] #update any simulated data
    om$input$par[om$input$random] = om_sim[om$input$random]
    om <- fit_wham(om$input, do.fit = FALSE, do.retro = FALSE, do.osa = FALSE, do.brps = TRUE, MakeADFun.silent = TRUE)
  }
  
  return(om)
}
