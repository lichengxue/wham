#' Function to perform management strategy evaluation 
#' 
#' a wrapper function to step through the feedback period and update the operating model, 
#' and refit the estimation model and generate the catch advice.
#' 
#' @param om Operating model including pseudo data
#' @param M_om Natural mortality configuration in the OM
#' @param sel_om Selectivity configuration in the OM
#' @param NAA_re_om Numbers-at-age (NAA) configuration in the OM
#' @param mean_rec_weights A vector of weights given to each stock (default = NULL) 
#' @param move_om Movement configuration in the OM
#' @param M_em Natural mortality configuration in the assessment model
#' @param sel_em Selectivity configuration in the assessment model
#' @param NAA_re_em Numbers-at-age (NAA) configuration in the assessment model
#' @param move_em Movement configuration in the assessment model
#' @param em.opt Type of assessment model
#'   \itemize{
#'     \item \code{$separate.em} TRUE = spatially implicit, FALSE = spatially disaggregated
#'     \item \code{$separate.em.type} when separate.em = TRUE \cr
#'     {=1} separate/independent assessment models (i.e. global SPR brps = FALSE) \cr
#'     {=2} fleets-as-areas (global SPR brps = TRUE) \cr
#'     {=3} panmictic (use spatially-aggregated data and projected total catch will be allocated based on stock-specific recruitment) \cr
#'     \item \code{$do.move} T/F movement is included (use when separate.em = FALSE)
#'     \item \code{$est.move} T/F movement rate is estimated (use when separate.em = FALSE)
#'     }
#' @param assess_years Year in which the assessment is conducted
#' @param assess_interval Assessment interval used in the MSE feedback loop
#' @param base_years Years used in the burn-in period
#' @param year.use Number of years used in the assessment model
#' @param seed Seed used for generate data
#' @param save.sdrep T/F save the full report (memory intensive) 
#' 
#' @return a list of model output
#'   \describe{
#'     \item{\code{$om}}{Operating model}
#'     \item{\code{$sdrep_list}}{Parameter estimates from each assessment model}
#'     \item{\code{$em_list}}{Short report of each assessment model}
#'     \item{\code{$opt_list}}{Output from \code{\link[stats:nlminb]{stats::nlminb}}}
#'     \item{\code{$converge_list}}{Convergence from each assessment model}
#'     \item{\code{$om_full}}{Full report of each assessment model}
#'   }
#'   
#' @export
#'
#' @seealso \code{\link{make_em_input}}, \code{\link{update_om_fn}}, \code{\link{advice_fn}}
#'

loop_through_fn = function(om, 
                           M_om,
                           sel_om, 
                           NAA_re_om, 
                           mean_rec_weights = NULL,
                           move_om,
                           M_em, 
                           sel_em, 
                           NAA_re_em,
                           move_em = NULL,
                           em.opt = NULL,
                           assess_years = NULL, 
                           assess_interval = NULL, 
                           base_years = NULL, 
                           year.use = 30,
                           seed = 123,
                           save.sdrep = FALSE) {
  
  if (!is.null(mean_rec_weights)) {
    SPR_weights = mean_rec_weights # mean_rec_weights is a vector (length = n_stocks)
  }
  
  if(all(move_om$stock_move)) {
    move.type = 2
  } else {
    if(any(move_om$stock_move)) {
      move.type = 1
    } else {
      move.type = 3
    }
  }
  
  if (is.null(em.opt)) warning("em.opt has to be specified!")
  if (em.opt$separate.em) move_em = NULL
  
  em_list <- list()
  sdrep_list <- list()
  opt_list <- list()
  converge_list <- list()
  
  if(save.sdrep) om_full <- list()
  
  if (em.opt$separate.em) {
    
    if (em.opt$separate.em.type == 1) {
      
      for(y in assess_years){
        
        i <- which(assess_years == y)
        
        em_list[[i]] <- list()
        sdrep_list[[i]] <- list()
        opt_list[[i]] <- list()
        converge_list[[i]] <- list()
        
        if(save.sdrep) om_full[[i]] <- list()
        
        em.years = base_years[1]:y
        
        em_input = make_em_input(om = om,
                                 M_em = M_em, 
                                 sel_em = sel_em, 
                                 NAA_re_em = NAA_re_em, 
                                 move_em,
                                 em.opt = em.opt,
                                 em_years = em.years,
                                 year.use = year.use)
        
        n_stocks = om$input$data$n_stocks
        advice = NULL
        em = list()
        
        conv = rep(0,n_stocks)
        pdHess = rep(0,n_stocks)

        for(s in 1:n_stocks){
          em[[s]] = fit_wham(em_input[[s]], do.retro = FALSE, do.osa = FALSE, do.brps = TRUE, MakeADFun.silent = TRUE) #no feedback period yet
          
          conv[s] = as.logical(1-em[[s]]$opt$convergence)
          pdHess[s] = as.logical(if(em[[s]]$na_sdrep==FALSE & !is.na(em[[s]]$na_sdrep)) 1 else 0)
          
          if (!conv[s] | !pdHess[s]) warning("Assessment model is not converged!")
          
          mod <- em[[s]]
          tmp = advice_fn(em = mod, pro.yr = assess_interval)
          
          advice = cbind(advice,tmp)
        }
        
        # set the catch for the next assess_interval years
        interval.info = list(catch = advice, years = y + 1:assess_interval)
        
        # update the operating model with the right Fs and resimulate the data given those Fs
        
        info = generate_basic_info(base.years = em.years)
        info = generate_NAA_where(basic_info = info, move.type = move.type)

        info$catch_info$agg_catch = om$input$data$agg_catch[1:length(em.years),,drop = F]
        info$index_info$agg_indices = om$input$data$agg_indices[1:length(em.years),,drop = F]
        info$catch_info$catch_paa = om$input$data$catch_paa[,1:length(em.years),,drop = F]
        info$index_info$index_paa = om$input$data$index_paa[,1:length(em.years),,drop = F]
        info$F_opts$F = exp(om$input$par$F_pars[1:length(em.years),])
        
        om_input = prepare_wham_input(basic_info = info, 
                                      selectivity = sel_om, 
                                      M = M_om, 
                                      NAA_re = NAA_re_om, 
                                      move = move_om)
        
        if(!is.null(mean_rec_weights)){
          om_input$data$SPR_weight_type = om_input$data$do_SPR_BRPs = 1
          om_input$data$SPR_weights     = SPR_weights
        }

        om2 <- fit_wham(om_input, do.fit = FALSE, do.retro = FALSE, do.osa = FALSE, MakeADFun.silent = TRUE)
        
        om = update_om_fn(om, om2, interval.info, seed = seed)
        
        for(s in 1:n_stocks){
          em_list[[i]][[s]] <- em[[s]]$rep
          sdrep_list[[i]][[s]] <- em[[s]]$sdrep$par.fixed
          opt_list[[i]][[s]] <- em[[s]]$opt
          converge_list[[i]][[s]] <- sum(conv,pdHess)
          if(save.sdrep) om_full[[i]][[s]] <- em[[s]]
        }
      }
    }
    
    if (em.opt$separate.em.type == 2) {

      for(y in assess_years){
        
        i <- which(assess_years == y)
        
        em.years = base_years[1]:y
        
        em_input = make_em_input(om = om,
                                 M_em = M_em, 
                                 sel_em = sel_em, 
                                 NAA_re_em = NAA_re_em, 
                                 move_em,
                                 em.opt = em.opt,
                                 em_years = em.years,
                                 year.use = year.use)
        
        # fit the estimation model
        em = fit_wham(em_input, do.retro = FALSE, do.osa = FALSE, do.brps = TRUE, MakeADFun.silent = TRUE) #no feedback period yet
        
        # check convergence
        conv = as.logical(1-em$opt$convergence)
        pdHess = as.logical(if(em$na_sdrep==FALSE & !is.na(em$na_sdrep)) 1 else 0)
        
        if (!conv | !pdHess) warning("Assessment model is not converged!")
        
        # make the catch advice
        advice = advice_fn(em, pro.yr = assess_interval)
        
        # set the catch for the next assess_interval years
        interval.info = list(catch = advice, years = y + 1:assess_interval)
        
        info = generate_basic_info(base.years = em.years)
        info = generate_NAA_where(basic_info = info, move.type = move.type)

        info$catch_info$agg_catch = om$input$data$agg_catch[1:length(em.years),,drop = F]
        info$index_info$agg_indices = om$input$data$agg_indices[1:length(em.years),,drop = F]
        info$catch_info$catch_paa = om$input$data$catch_paa[,1:length(em.years),,drop = F]
        info$index_info$index_paa = om$input$data$index_paa[,1:length(em.years),,drop = F]
        info$F_opts$F = exp(om$input$par$F_pars[1:length(em.years),])
        
        om_input = prepare_wham_input(basic_info = info, 
                                      selectivity = sel_om, 
                                      M = M_om, 
                                      NAA_re = NAA_re_om, 
                                      move = move_om)
        
        if(!is.null(mean_rec_weights)){
          om_input$data$SPR_weight_type = om_input$data$do_SPR_BRPs = 1
          om_input$data$SPR_weights     = SPR_weights
        }

        om2 <- fit_wham(om_input, do.fit = FALSE, do.retro = FALSE, do.osa = FALSE, MakeADFun.silent = TRUE)
        
        # update F in the operating model and re-simulate the data given the updated F
        
        om = update_om_fn(om, om2, interval.info, seed = seed)  
        
        em_list[[i]] <- em$rep
        sdrep_list[[i]] <- em$sdrep$par.fixed
        opt_list[[i]] <- em$opt
        converge_list[[i]] <- conv+pdHess
        if(save.sdrep) om_full[[i]] <- em
      }
    }
    
    if (em.opt$separate.em.type == 3) {
      # one area model with combined fleet and survey info
      for(y in assess_years){
        
        i <- which(assess_years == y)
        
        em.years = base_years[1]:y
        
        em_input = make_em_input(om = om,
                                 M_em = M_em, 
                                 sel_em = sel_em, 
                                 NAA_re_em = NAA_re_em, 
                                 move_em,
                                 em.opt = em.opt,
                                 em_years = em.years,
                                 year.use = year.use)
        
        n_stocks = om$input$data$n_stocks

        em = fit_wham(em_input, do.retro = FALSE, do.osa = FALSE, do.brps = TRUE, MakeADFun.silent = TRUE) #no feedback period yet
        
        # check convergence
        conv = as.logical(1-em$opt$convergence)
        pdHess = as.logical(if(em$na_sdrep==FALSE & !is.na(em$na_sdrep)) 1 else 0)
        
        if (!conv | !pdHess) warning("Assessment model is not converged!")
        
        # make the catch advice
        advice = advice_fn(em, pro.yr = assess_interval)
        
        advice <- matrix(c(advice*(2/3),advice*(1/3)), ncol = 2, byrow = TRUE)
        
        # set the catch for the next assess_interval years
        interval.info = list(catch = advice, years = y + 1:assess_interval)
        
        info = generate_basic_info(base.years = em.years)
        info = generate_NAA_where(basic_info = info, move.type = move.type)

        info$catch_info$agg_catch = om$input$data$agg_catch[1:length(em.years),,drop = F]
        info$index_info$agg_indices = om$input$data$agg_indices[1:length(em.years),,drop = F]
        info$catch_info$catch_paa = om$input$data$catch_paa[,1:length(em.years),,drop = F]
        info$index_info$index_paa = om$input$data$index_paa[,1:length(em.years),,drop = F]
        info$F_opts$F = exp(om$input$par$F_pars[1:length(em.years),])
        
        om_input = prepare_wham_input(basic_info = info, 
                                      selectivity = sel_om, 
                                      M = M_om, 
                                      NAA_re = NAA_re_om, 
                                      move = move_om)
        
        if(!is.null(mean_rec_weights)){
          om_input$data$SPR_weight_type = om_input$data$do_SPR_BRPs = 1
          om_input$data$SPR_weights     = SPR_weights
        }
        
        om2 <- fit_wham(om_input, do.fit = FALSE, do.retro = FALSE, do.osa = FALSE, MakeADFun.silent = TRUE)
        
        # update F in the operating model and re-simulate the data given the updated F
        
        om = update_om_fn(om, om2, interval.info, seed = seed)  
        
        em_list[[i]] <- em$rep
        sdrep_list[[i]] <- em$sdrep$par.fixed
        opt_list[[i]] <- em$opt
        converge_list[[i]] <- conv+pdHess
        if(save.sdrep) om_full[[i]] <- em
      }
    }
    
  } else {
    
    if (em.opt$do.move) {
      
      if (em.opt$est.move) { # equivalent to if(do.move & est.move)
        
        for(y in assess_years){
          
          i <- which(assess_years == y)
          
          em.years = base_years[1]:y
          
          # make the input for the estimation model
          em_input = make_em_input(om = om,
                                   M_em = M_em, 
                                   sel_em = sel_em, 
                                   NAA_re_em = NAA_re_em, 
                                   move_em,
                                   em.opt = em.opt,
                                   em_years = em.years,
                                   year.use = year.use)
          
          # fit the estimation model
          em = fit_wham(em_input, do.retro = FALSE, do.osa = FALSE, do.brps = TRUE, MakeADFun.silent = TRUE) #no feedback period yet
          
          # check convergence
          conv = as.logical(1-em$opt$convergence)
          pdHess = as.logical(if(em$na_sdrep==FALSE & !is.na(em$na_sdrep)) 1 else 0)
          
          if (!conv | !pdHess) warning("Assessment model is not converged!")
          
          # make the catch advice
          advice = advice_fn(em, pro.yr = assess_interval)
          
          # set the catch for the next assess_interval years
          interval.info = list(catch = advice, years = y + 1:assess_interval)
          
          info = generate_basic_info(base.years = em.years)
          info = generate_NAA_where(basic_info = info, move.type = move.type)
          
          info$catch_info$agg_catch = om$input$data$agg_catch[1:length(em.years),,drop = F]
          info$index_info$agg_indices = om$input$data$agg_indices[1:length(em.years),,drop = F]
          info$catch_info$catch_paa = om$input$data$catch_paa[,1:length(em.years),,drop = F]
          info$index_info$index_paa = om$input$data$index_paa[,1:length(em.years),,drop = F]
          info$F_opts$F = exp(om$input$par$F_pars[1:length(em.years),])
          
          om_input = prepare_wham_input(basic_info = info, 
                                        selectivity = sel_om, 
                                        M = M_om, 
                                        NAA_re = NAA_re_om, 
                                        move = move_om)
          
          if(!is.null(mean_rec_weights)){
            om_input$data$SPR_weight_type = om_input$data$do_SPR_BRPs = 1
            om_input$data$SPR_weights     = SPR_weights
          }
          
          om2 <- fit_wham(om_input, do.fit = FALSE, do.retro = FALSE, do.osa = FALSE, MakeADFun.silent = TRUE)
          
          # update F in the operating model and re-simulate the data given the updated F
          
          om = update_om_fn(om, om2, interval.info, seed = seed) 
          
          em_list[[i]] <- em$rep
          sdrep_list[[i]] <- em$sdrep$par.fixed
          opt_list[[i]] <- em$opt
          converge_list[[i]] <- conv+pdHess
          if(save.sdrep) om_full[[i]] <- em
        }
      } else { # equivalent to if(do.move & !est.move)
        
        for(y in assess_years){
          
          i <- which(assess_years == y)
          
          em.years = base_years[1]:y
          
          #make the input for the estimation model
          em_input = make_em_input(om = om,
                                   M_em = M_em, 
                                   sel_em = sel_em, 
                                   NAA_re_em = NAA_re_em, 
                                   move_em, 
                                   em.opt = em.opt,
                                   em_years = em.years,
                                   year.use = year.use)
          
          # Fix the movement (don't estimate movement)
          sigma_vals = 0.2
          em_input$par$mu_repars[,,,,1] = log(sigma_vals)
          em_input <- fix_move(em_input)
          
          # fit the estimation model
          em = fit_wham(em_input, do.retro = FALSE, do.osa = FALSE, do.brps = TRUE, MakeADFun.silent = TRUE) #no feedback period yet
          
          # check convergence
          conv = as.logical(1-em$opt$convergence)
          pdHess = as.logical(if(em$na_sdrep==FALSE & !is.na(em$na_sdrep)) 1 else 0)
          
          if (!conv | !pdHess) warning("Assessment model is not converged!")
          
          # make the catch advice
          advice = advice_fn(em, pro.yr = assess_interval)
          
          # set the catch for the next assess_interval years
          interval.info = list(catch = advice, years = y + 1:assess_interval)
          
          info = generate_basic_info(base.years = em.years)
          info = generate_NAA_where(basic_info = info, move.type = move.type)
          
          info$catch_info$agg_catch = om$input$data$agg_catch[1:length(em.years),,drop = F]
          info$index_info$agg_indices = om$input$data$agg_indices[1:length(em.years),,drop = F]
          info$catch_info$catch_paa = om$input$data$catch_paa[,1:length(em.years),,drop = F]
          info$index_info$index_paa = om$input$data$index_paa[,1:length(em.years),,drop = F]
          info$F_opts$F = exp(om$input$par$F_pars[1:length(em.years),])
          
          om_input = prepare_wham_input(basic_info = info, 
                                        selectivity = sel_om, 
                                        M = M_om, 
                                        NAA_re = NAA_re_om, 
                                        move = move_om)
          
          if(!is.null(mean_rec_weights)){
            om_input$data$SPR_weight_type = om_input$data$do_SPR_BRPs = 1
            om_input$data$SPR_weights     = SPR_weights
          }
          
          om2 <- fit_wham(om_input, do.fit = FALSE, do.retro = FALSE, do.osa = FALSE, MakeADFun.silent = TRUE)
          
          # update F in the operating model and re-simulate the data given the updated F
          
          om = update_om_fn(om, om2, interval.info, seed = seed) 
          
          em_list[[i]] <- em$rep
          sdrep_list[[i]] <- em$sdrep$par.fixed
          opt_list[[i]] <- em$opt
          converge_list[[i]] <- conv+pdHess
          if(save.sdrep) om_full[[i]] <- em
        }
      }
    } else { # equivalent to if(!do.move)
      
      for(y in assess_years){
        
        i <- which(assess_years == y)
        
        em.years = base_years[1]:y
        
        # make the input for the estimation model
        em_input = make_em_input(om = om,
                                 M_em = M_em, 
                                 sel_em = sel_em, 
                                 NAA_re_em = NAA_re_em, 
                                 move_em = NULL, 
                                 em.opt = em.opt,
                                 em_years = em.years,
                                 year.use = year.use)
        
        # fit the estimation model
        em = fit_wham(em_input, do.retro = FALSE, do.osa = FALSE, do.brps = TRUE, MakeADFun.silent = TRUE) #no feedback period yet
        
        # check convergence
        conv = as.logical(1-em$opt$convergence)
        pdHess = as.logical(if(em$na_sdrep==FALSE & !is.na(em$na_sdrep)) 1 else 0)
        
        if (!conv | !pdHess) warning("Assessment model is not converged!")
        
        # make the catch advice
        advice = advice_fn(em, pro.yr = assess_interval)
        
        # set the catch for the next assess_interval years
        interval.info = list(catch = advice, years = y + 1:assess_interval)
        
        info = generate_basic_info(base.years = em.years)
        info = generate_NAA_where(basic_info = info, move.type = move.type)
        
        info$catch_info$agg_catch = om$input$data$agg_catch[1:length(em.years),,drop = F]
        info$index_info$agg_indices = om$input$data$agg_indices[1:length(em.years),,drop = F]
        info$catch_info$catch_paa = om$input$data$catch_paa[,1:length(em.years),,drop = F]
        info$index_info$index_paa = om$input$data$index_paa[,1:length(em.years),,drop = F]
        info$F_opts$F = exp(om$input$par$F_pars[1:length(em.years),])
        
        om_input = prepare_wham_input(basic_info = info, 
                                      selectivity = sel_om, 
                                      M = M_om, 
                                      NAA_re = NAA_re_om, 
                                      move = move_om)
        
        if(!is.null(mean_rec_weights)){
          om_input$data$SPR_weight_type = om_input$data$do_SPR_BRPs = 1
          om_input$data$SPR_weights     = SPR_weights
        }
        
        om2 <- fit_wham(om_input, do.fit = FALSE, do.retro = FALSE, do.osa = FALSE, MakeADFun.silent = TRUE)
        
        # update F in the operating model and re-simulate the data given the updated F
        
        om = update_om_fn(om, om2, interval.info, seed = seed) 
        
        em_list[[i]] <- em$rep
        sdrep_list[[i]] <- em$sdrep$par.fixed
        opt_list[[i]] <- em$opt
        converge_list[[i]] <- conv+pdHess
        if(save.sdrep) om_full[[i]] <- em
      }
    }
  }
  if(save.sdrep) return(list(om = om, sdrep_list = sdrep_list, em_list  = em_list, opt_list = opt_list, converge_list = converge_list, om_full = om_full))
  return(list(om = om, sdrep_list = sdrep_list, em_list  = em_list, opt_list = opt_list, converge_list = converge_list, om_full = NULL))
}
