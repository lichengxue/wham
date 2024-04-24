# Function to update F (given catch advice) in the OM and re-generate data given the updated F
update_om_fn = function(om, 
                        interval.info = NULL,
                        seed = 123) {
  obs_names = c("agg_indices","agg_catch","catch_paa","index_paa", "Ecov_obs", "obsvec")
  
  if(!is.null(interval.info)){ #iteratively update F over assessment interval for the given catch advice
    for(y in interval.info$years){
      set.seed(seed)
      if (!is.matrix(interval.info$catch)) interval.info$catch = matrix(interval.info$catch,1,length(interval.info$catch))
      om = update_om_F(om, year = y, catch = interval.info$catch[which(interval.info$years==y),]) #put in the right F values
      om_sim = om$simulate(complete=TRUE) #resimulate the population and observations
      # Option 1
      om$input$data[obs_names] = om_sim[obs_names] #update any simulated data

      # Option 2
      #om$input$data = om_sim
      
      om$input$par[om$input$random] = om_sim[om$input$random]
      
      # reset the om
      om <- fit_wham(om$input, do.fit = FALSE, do.retro = FALSE, do.osa = FALSE, MakeADFun.silent = TRUE)
    }
  } else { #otherwise just (re)generate the population
    set.seed(seed)
    om_sim = om$simulate(complete=TRUE) #resimulate the population and observations
    
    # Option 1
    om$input$data[obs_names] = om_sim[obs_names] #update any simulated data
    
    # Option 2
    #om$input$data = om_sim
    
    om$input$par[om$input$random] = om_sim[om$input$random]
    
    # reset the om
    om <- fit_wham(om$input, do.fit = FALSE, do.retro = FALSE, do.osa = FALSE, MakeADFun.silent = TRUE)
  }
  return(om)
}
