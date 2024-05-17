#' Function to perform management strategy evaluation 
#' 
#' a wrapper function to step through the feedback period and update the operating model, 
#' and refit the estimation model and generate the catch advice.
#' 
#' @param om Operating model 
#' @param M_em Natural mortality random effects
#' @param sel_em Selectivity random effects   
#' @param NAA_re_em Numbers-at-age random effects 
#' @param move_em Movement random effects
#' @param em.opt Movement random effects
#'   \itemize{
#'     \item \code{$separate.em} TRUE = spatially implicit, FALSE = spatially disaggregated
#'     \item \code{$separate.em.type} when separate.em = TRUE \cr
#'     {=1} fleets-as-areas (global SPR brps = FALSE) \cr
#'     {=2} fleets-as-areas (global SPR brps = TRUE) \cr
#'     {=3} panmictic (spatially-aggregated) \cr
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

loop_through_fn2 = function(om, 
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
  
  if (is.null(em.opt)) em.opt = list(separate.em = FALSE, separate.em.type = "none", do.move = FALSE, est.move = FALSE)
  if (em.opt$separate.em) move_em = NULL
  
  if (!em.opt$separate.em & em.opt$do.move & (is.null(move_em))) stop("movement structure (move_em) must be specified!")
  if (!em.opt$separate.em & !em.opt$do.move) move.type = 3 # no movement
  if (!em.opt$separate.em & em.opt$do.move & all(move_em$stock_move)) move.type = 2 # bidirectional
  if (!em.opt$separate.em & em.opt$do.move & !all(move_em$stock_move)) move.type = 1 # unidirectional
  
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
        
        em_input = make_em_input(om = om,
                                 M_em = M_em, 
                                 sel_em = sel_em, 
                                 NAA_re_em = NAA_re_em, 
                                 move_em = NULL,
                                 em.opt = em.opt,
                                 em_years = base_years[1]:y,
                                 year.use = year.use)
        
        n_stocks = om$input$data$n_stocks
        advice = NULL
        em = list()
        
        conv = rep(0,n_stocks)
        pdHess = rep(0,n_stocks)
        # fit the estimation model
        for(s in 1:n_stocks){
          em[[s]] = fit_wham(em_input[[s]], do.retro = FALSE, do.osa = FALSE, do.brps = TRUE, MakeADFun.silent = TRUE) #no feedback period yet
          
          conv[s] = as.logical(1-em[[s]]$opt$convergence)
          pdHess[s] = as.logical(if(em[[s]]$na_sdrep==FALSE & !is.na(em[[s]]$na_sdrep)) 1 else 0)
          
          if (!conv[s] | !pdHess[s]) warning("Assessment model is not converged!")
          
          mod <- em[[s]]
          tmp = advice_fn(em = mod, pro.yr = assess.interval)
          
          advice = cbind(advice,tmp)
        }
        
        # set the catch for the next assess_interval years
        interval.info = list(catch = advice, years = y + 1:assess_interval)
        
        # update the operating model with the right Fs and resimulate the data given those Fs
        info = generate_basic_info(base.years = base_years[1]:y)
        info = generate_NAA_where(basic_info = info, move.type = move.type)
        #fill in the data from the operating model simulation
        info$catch_info$agg_catch = om$input$data$agg_catch[1:length(base_years[1]:y),,drop = F]
        info$index_info$agg_indices = om$input$data$agg_indices[1:length(base_years[1]:y),,drop = F]
        info$catch_info$catch_paa = om$input$data$catch_paa[,1:length(base_years[1]:y),,drop = F]
        info$index_info$index_paa = om$input$data$index_paa[,1:length(base_years[1]:y),,drop = F]
        info$F_opts$F = exp(om$input$par$F_pars[1:length(base_years[1]:y),])
        # Selectivity Configuration
        fleet_pars <- c(5,1)
        index_pars <- c(2,1)
        sel <- list(model=rep("logistic",4),
                    initial_pars=c(rep(list(fleet_pars),2),rep(list(index_pars),2)),
                    fix_pars=rep(list(NULL),4))
        NAA_re <- list(N1_model=rep("equilibrium",2),sigma=rep("rec+1",2),cor=rep("iid",2))
        M <- list(model="constant",initial_means=array(0.2, dim = c(2,2,12)))
        
        om_input = prepare_wham_input(basic_info = info, selectivity = sel, M = M, NAA_re = NAA_re, move = move_em)
        # om_input$random = NULL
        om_input$data$SPR_weight_type = 1
        om_input$data$SPR_weights     = c(2/3,1/3)
        om_input$data$do_SPR_BRPs     = 1
        
        om2 <- fit_wham(om_input, do.fit = FALSE, do.retro = FALSE, do.osa = FALSE, MakeADFun.silent = TRUE)
        
        proj_opts = list(n.yrs=3, proj.catch = advice)
        em_proj = project_wham(om2, proj.opts = proj_opts, MakeADFun.silent=TRUE)
        updated_F = log(tail(em_proj$rep$Fbar,assess.interval))
        cat(paste0("\nFsolve: ", exp(updated_F),"\n"))
        years = y + 1:assess_interval
        year_ind = which(om$years %in% years)
        om$input$par$F_pars[year_ind,] = updated_F
        om = update_om_fn(om, seed = seed)  
        
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
      # one area model with area-specific fleet and survey info
      for(y in assess_years){
        
        i <- which(assess_years == y)
        
        em_input = make_em_input(om = om,
                                 M_em = M_em, 
                                 sel_em = sel_em, 
                                 NAA_re_em = NAA_re_em, 
                                 move_em = NULL,
                                 em.opt = em.opt,
                                 em_years = base_years[1]:y,
                                 year.use = year.use)
        
        # fit the estimation model
        em = fit_wham(em_input, do.retro = FALSE, do.osa = FALSE, do.brps = TRUE, MakeADFun.silent = TRUE) #no feedback period yet
        
        # check convergence
        conv = as.logical(1-em$opt$convergence)
        pdHess = as.logical(if(em$na_sdrep==FALSE & !is.na(em$na_sdrep)) 1 else 0)
        
        if (!conv | !pdHess) warning("Assessment model is not converged!")
        # make the catch advice
        advice = advice_fn(em = em, pro.yr = assess.interval)
        
        # set the catch for the next assess_interval years
        interval.info = list(catch = advice, years = y + 1:assess_interval)
        
        # update the operating model with the right Fs and resimulate the data given those Fs
        info = generate_basic_info(base.years = base_years[1]:y)
        info = generate_NAA_where(basic_info = info, move.type = move.type)
        #fill in the data from the operating model simulation
        info$catch_info$agg_catch = om$input$data$agg_catch[1:length(base_years[1]:y),,drop = F]
        info$index_info$agg_indices = om$input$data$agg_indices[1:length(base_years[1]:y),,drop = F]
        info$catch_info$catch_paa = om$input$data$catch_paa[,1:length(base_years[1]:y),,drop = F]
        info$index_info$index_paa = om$input$data$index_paa[,1:length(base_years[1]:y),,drop = F]
        info$F_opts$F = exp(om$input$par$F_pars[1:length(base_years[1]:y),])
        # Selectivity Configuration
        fleet_pars <- c(5,1)
        index_pars <- c(2,1)
        sel <- list(model=rep("logistic",4),
                    initial_pars=c(rep(list(fleet_pars),2),rep(list(index_pars),2)),
                    fix_pars=rep(list(NULL),4))
        NAA_re <- list(N1_model=rep("equilibrium",2),sigma=rep("rec+1",2),cor=rep("iid",2))
        M <- list(model="constant",initial_means=array(0.2, dim = c(2,2,12)))
        
        om_input = prepare_wham_input(basic_info = info, selectivity = sel, M = M, NAA_re = NAA_re, move = move_em)
        # om_input$random = NULL
        om_input$data$SPR_weight_type = 1
        om_input$data$SPR_weights     = c(2/3,1/3)
        om_input$data$do_SPR_BRPs     = 1
        
        om2 <- fit_wham(om_input, do.fit = FALSE, do.retro = FALSE, do.osa = FALSE, MakeADFun.silent = TRUE)
        
        proj_opts = list(n.yrs=3, proj.catch = advice)
        em_proj = project_wham(om2, proj.opts = proj_opts, MakeADFun.silent=TRUE)
        updated_F = log(tail(em_proj$rep$Fbar,assess.interval))
        cat(paste0("\nFsolve: ", exp(updated_F),"\n"))
        years = y + 1:assess_interval
        year_ind = which(om$years %in% years)
        om$input$par$F_pars[year_ind,] = updated_F
        om = update_om_fn(om, seed = seed)  
        
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
        
        em_input = make_em_input(om = om,
                                 M_em = M_em, 
                                 sel_em = sel_em, 
                                 NAA_re_em = NAA_re_em, 
                                 move_em = NULL,
                                 em.opt = em.opt,
                                 em_years = base_years[1]:y,
                                 year.use = year.use)
        
        n_stocks = om$input$data$n_stocks
        # fit the estimation model
        em = fit_wham(em_input, do.retro = FALSE, do.osa = FALSE, do.brps = TRUE, MakeADFun.silent = TRUE) #no feedback period yet
        
        # check convergence
        conv = as.logical(1-em$opt$convergence)
        pdHess = as.logical(if(em$na_sdrep==FALSE & !is.na(em$na_sdrep)) 1 else 0)
        
        if (!conv | !pdHess) warning("Assessment model is not converged!")
        # make the catch advice
        advice = advice_fn(em = em, pro.yr = assess.interval)
        
        # Warning this catch advice is divided by 2 (e.g. 2 regions)
        # cat("\nWarning catch advice is forced to be divided by number of regions!")
        tmp <- rep(advice/n_stocks,n_stocks) 
        advice <- matrix(tmp,ncol = n_stocks)
        
        # set the catch for the next assess_interval years
        interval.info = list(catch = advice, years = y + 1:assess_interval)
        
        # update the operating model with the right Fs and resimulate the data given those Fs
        info = generate_basic_info(base.years = base_years[1]:y)
        info = generate_NAA_where(basic_info = info, move.type = move.type)
        #fill in the data from the operating model simulation
        info$catch_info$agg_catch = om$input$data$agg_catch[1:length(base_years[1]:y),,drop = F]
        info$index_info$agg_indices = om$input$data$agg_indices[1:length(base_years[1]:y),,drop = F]
        info$catch_info$catch_paa = om$input$data$catch_paa[,1:length(base_years[1]:y),,drop = F]
        info$index_info$index_paa = om$input$data$index_paa[,1:length(base_years[1]:y),,drop = F]
        info$F_opts$F = exp(om$input$par$F_pars[1:length(base_years[1]:y),])
        # Selectivity Configuration
        fleet_pars <- c(5,1)
        index_pars <- c(2,1)
        sel <- list(model=rep("logistic",4),
                    initial_pars=c(rep(list(fleet_pars),2),rep(list(index_pars),2)),
                    fix_pars=rep(list(NULL),4))
        NAA_re <- list(N1_model=rep("equilibrium",2),sigma=rep("rec+1",2),cor=rep("iid",2))
        M <- list(model="constant",initial_means=array(0.2, dim = c(2,2,12)))
        
        om_input = prepare_wham_input(basic_info = info, selectivity = sel, M = M, NAA_re = NAA_re, move = move_em)
        # om_input$random = NULL
        om_input$data$SPR_weight_type = 1
        om_input$data$SPR_weights     = c(2/3,1/3)
        om_input$data$do_SPR_BRPs     = 1
        
        om2 <- fit_wham(om_input, do.fit = FALSE, do.retro = FALSE, do.osa = FALSE, MakeADFun.silent = TRUE)
        
        proj_opts = list(n.yrs=3, proj.catch = advice)
        em_proj = project_wham(om2, proj.opts = proj_opts, MakeADFun.silent=TRUE)
        updated_F = log(tail(em_proj$rep$Fbar,assess.interval))
        cat(paste0("\nFsolve: ", exp(updated_F),"\n"))
        years = y + 1:assess_interval
        year_ind = which(om$years %in% years)
        om$input$par$F_pars[year_ind,] = updated_F
        om = update_om_fn(om, seed = seed)  
        
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
          
          # make the input for the estimation model
          em_input = make_em_input(om = om,
                                   M_em = M_em, 
                                   sel_em = sel_em, 
                                   NAA_re_em = NAA_re_em, 
                                   move_em = move_em,
                                   em.opt = em.opt,
                                   em_years = base_years[1]:y,
                                   year.use = year.use)
          
          # fit the estimation model
          em = fit_wham(em_input, do.retro = FALSE, do.osa = FALSE, do.brps = TRUE, MakeADFun.silent = TRUE) #no feedback period yet
          
          # check convergence
          conv = as.logical(1-em$opt$convergence)
          pdHess = as.logical(if(em$na_sdrep==FALSE & !is.na(em$na_sdrep)) 1 else 0)
          
          if (!conv | !pdHess) warning("Assessment model is not converged!")
          
          # make the catch advice
          advice = advice_fn(em = em, pro.yr = assess.interval)
          
          # set the catch for the next assess_interval years
          interval.info = list(catch = advice, years = y + 1:assess_interval)
          
          # update the operating model with the right Fs and resimulate the data given those Fs
          # om = update_om_fn(om, interval.info = interval.info, seed = seed)  
          
          info = generate_basic_info(base.years = base_years[1]:y)
          info = generate_NAA_where(basic_info = info, move.type = move.type)
          #fill in the data from the operating model simulation
          info$catch_info$agg_catch = om$input$data$agg_catch[1:length(base_years[1]:y),,drop = F]
          info$index_info$agg_indices = om$input$data$agg_indices[1:length(base_years[1]:y),,drop = F]
          info$catch_info$catch_paa = om$input$data$catch_paa[,1:length(base_years[1]:y),,drop = F]
          info$index_info$index_paa = om$input$data$index_paa[,1:length(base_years[1]:y),,drop = F]
          info$F_opts$F = exp(om$input$par$F_pars[1:length(base_years[1]:y),])
          # Selectivity Configuration
          fleet_pars <- c(5,1)
          index_pars <- c(2,1)
          sel <- list(model=rep("logistic",4),
                      initial_pars=c(rep(list(fleet_pars),2),rep(list(index_pars),2)),
                      fix_pars=rep(list(NULL),4))
          NAA_re <- list(N1_model=rep("equilibrium",2),sigma=rep("rec+1",2),cor=rep("iid",2))
          M <- list(model="constant",initial_means=array(0.2, dim = c(2,2,12)))
          
          om_input = prepare_wham_input(basic_info = info, selectivity = sel, M = M, NAA_re = NAA_re, move = move_em)
          # om_input$random = NULL
          om_input$data$SPR_weight_type = 1
          om_input$data$SPR_weights     = c(2/3,1/3)
          om_input$data$do_SPR_BRPs     = 1
          
          om2 <- fit_wham(om_input, do.fit = FALSE, do.retro = FALSE, do.osa = FALSE, MakeADFun.silent = TRUE)
          
          proj_opts = list(n.yrs=3, proj.catch = advice)
          em_proj = project_wham(om2, proj.opts = proj_opts, MakeADFun.silent=TRUE)
          updated_F = log(tail(em_proj$rep$Fbar,assess.interval))
          cat(paste0("\nFsolve: ", exp(updated_F),"\n"))
          years = y + 1:assess_interval
          year_ind = which(om$years %in% years)
          om$input$par$F_pars[year_ind,] = updated_F
          om = update_om_fn(om, seed = seed)  
          
          em_list[[i]] <- em$rep
          sdrep_list[[i]] <- em$sdrep$par.fixed
          opt_list[[i]] <- em$opt
          converge_list[[i]] <- conv+pdHess
          if(save.sdrep) om_full[[i]] <- em
        }
      } else { # equivalent to if(do.move & !est.move)
        
        for(y in assess_years){
          
          i <- which(assess_years == y)
          
          #make the input for the estimation model
          em_input = make_em_input(om = om,
                                   M_em = M_em, 
                                   sel_em = sel_em, 
                                   NAA_re_em = NAA_re_em, 
                                   move_em = move_em, 
                                   em.opt = em.opt,
                                   em_years = base_years[1]:y,
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
          advice = advice_fn(em = em, pro.yr = assess.interval)
          
          # set the catch for the next assess_interval years
          interval.info = list(catch = advice, years = y + 1:assess_interval)
          
          # update the operating model with the right Fs and resimulate the data given those Fs
          info = generate_basic_info(base.years = base_years[1]:y)
          info = generate_NAA_where(basic_info = info, move.type = move.type)
          #fill in the data from the operating model simulation
          info$catch_info$agg_catch = om$input$data$agg_catch[1:length(base_years[1]:y),,drop = F]
          info$index_info$agg_indices = om$input$data$agg_indices[1:length(base_years[1]:y),,drop = F]
          info$catch_info$catch_paa = om$input$data$catch_paa[,1:length(base_years[1]:y),,drop = F]
          info$index_info$index_paa = om$input$data$index_paa[,1:length(base_years[1]:y),,drop = F]
          info$F_opts$F = exp(om$input$par$F_pars[1:length(base_years[1]:y),])
          # Selectivity Configuration
          fleet_pars <- c(5,1)
          index_pars <- c(2,1)
          sel <- list(model=rep("logistic",4),
                      initial_pars=c(rep(list(fleet_pars),2),rep(list(index_pars),2)),
                      fix_pars=rep(list(NULL),4))
          NAA_re <- list(N1_model=rep("equilibrium",2),sigma=rep("rec+1",2),cor=rep("iid",2))
          M <- list(model="constant",initial_means=array(0.2, dim = c(2,2,12)))
          
          om_input = prepare_wham_input(basic_info = info, selectivity = sel, M = M, NAA_re = NAA_re, move = move_em)
          # om_input$random = NULL
          om_input$data$SPR_weight_type = 1
          om_input$data$SPR_weights     = c(2/3,1/3)
          om_input$data$do_SPR_BRPs     = 1
          
          om2 <- fit_wham(om_input, do.fit = FALSE, do.retro = FALSE, do.osa = FALSE, MakeADFun.silent = TRUE)
          
          proj_opts = list(n.yrs=3, proj.catch = advice)
          em_proj = project_wham(om2, proj.opts = proj_opts, MakeADFun.silent=TRUE)
          updated_F = log(tail(em_proj$rep$Fbar,assess.interval))
          cat(paste0("\nFsolve: ", exp(updated_F),"\n"))
          years = y + 1:assess_interval
          year_ind = which(om$years %in% years)
          om$input$par$F_pars[year_ind,] = updated_F
          om = update_om_fn(om, seed = seed)  
          
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
        
        # make the input for the estimation model
        em_input = make_em_input(om = om,
                                 M_em = M_em, 
                                 sel_em = sel_em, 
                                 NAA_re_em = NAA_re_em, 
                                 move_em = NULL, 
                                 em.opt = em.opt,
                                 em_years = base_years[1]:y,
                                 year.use = year.use)
        
        # fit the estimation model
        em = fit_wham(em_input, do.retro = FALSE, do.osa = FALSE, do.brps = TRUE, MakeADFun.silent = TRUE) #no feedback period yet
        
        # check convergence
        conv = as.logical(1-em$opt$convergence)
        pdHess = as.logical(if(em$na_sdrep==FALSE & !is.na(em$na_sdrep)) 1 else 0)
        
        if (!conv | !pdHess) warning("Assessment model is not converged!")
        # make the catch advice
        advice = advice_fn(em = em, pro.yr = assess.interval)
        
        # set the catch for the next assess_interval years
        interval.info = list(catch = advice, years = y + 1:assess_interval)
        
        # update the operating model with the right Fs and resimulate the data given those Fs
        # om = update_om_fn(om, seed = seed, interval.info = interval.info) 
        
        info = generate_basic_info(base.years = base_years[1]:y)
        info = generate_NAA_where(basic_info = info, move.type = move.type)
        #fill in the data from the operating model simulation
        info$catch_info$agg_catch = om$input$data$agg_catch[1:length(base_years[1]:y),,drop = F]
        info$index_info$agg_indices = om$input$data$agg_indices[1:length(base_years[1]:y),,drop = F]
        info$catch_info$catch_paa = om$input$data$catch_paa[,1:length(base_years[1]:y),,drop = F]
        info$index_info$index_paa = om$input$data$index_paa[,1:length(base_years[1]:y),,drop = F]
        info$F_opts$F = exp(om$input$par$F_pars[1:length(base_years[1]:y),])
        # Selectivity Configuration
        fleet_pars <- c(5,1)
        index_pars <- c(2,1)
        sel <- list(model=rep("logistic",4),
                    initial_pars=c(rep(list(fleet_pars),2),rep(list(index_pars),2)),
                    fix_pars=rep(list(NULL),4))
        NAA_re <- list(N1_model=rep("equilibrium",2),sigma=rep("rec+1",2),cor=rep("iid",2))
        M <- list(model="constant",initial_means=array(0.2, dim = c(2,2,12)))
        
        om_input = prepare_wham_input(basic_info = info, selectivity = sel, M = M, NAA_re = NAA_re, move = move_em)
        # om_input$random = NULL
        om_input$data$SPR_weight_type = 1
        om_input$data$SPR_weights     = c(2/3,1/3)
        om_input$data$do_SPR_BRPs     = 1
        
        om2 <- fit_wham(om_input, do.fit = FALSE, do.retro = FALSE, do.osa = FALSE, MakeADFun.silent = TRUE)
        
        proj_opts = list(n.yrs=3, proj.catch = advice)
        em_proj = project_wham(om2, proj.opts = proj_opts, MakeADFun.silent=TRUE)
        updated_F = log(tail(em_proj$rep$Fbar,assess.interval))
        cat(paste0("\nFsolve: ", exp(updated_F),"\n"))
        years = y + 1:assess_interval
        year_ind = which(om$years %in% years)
        om$input$par$F_pars[year_ind,] = updated_F
        om = update_om_fn(om, seed = seed)  
        
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
