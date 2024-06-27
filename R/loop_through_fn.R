#' Function to perform management strategy evaluation 
#' 
#' a wrapper function to step through the feedback period and update the operating model, 
#' and refit the estimation model and generate the catch advice.
#' 
#' @param om Operating model including pseudo data
#' @param M_om Natural mortality configuration in the operating model
#' @param sel_om Selectivity configuration in the operating model
#' @param NAA_re_om Numbers-at-age (NAA) configuration in the operating model
#' @param mean_rec_weights A vector of weights given to each stock (default = NULL).
#' @param move_om Movement configuration in the operating model
#' @param age_comp_om Likelihood distribution of age composition data in the operating model
#'   \itemize{
#'     \item \code{"multinomial"} Default
#'     \item \code{"dir-mult"}
#'     \item \code{"dirichlet-miss0"}
#'     \item \code{"dirichlet-pool0"}
#'     \item \code{"dir-mult"}
#'     \item \code{"logistic-normal-miss0"}    
#'     \item \code{"logistic-normal-ar1-miss0"}
#'     \item \code{"logistic-normal-pool0"}
#'     \item \code{"logistic-normal-01-infl"}
#'     \item \code{"logistic-normal-pool0"}
#'     \item \code{"logistic-normal-01-infl-2par"}
#'     \item \code{"mvtweedie"}
#'     \item \code{"dir-mult-linear"}
#'     }
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
#' @param age_comp_em Likelihood distribution of age composition data in the assessment model
#'   \itemize{
#'     \item \code{"multinomial"} (Default)
#'     \item \code{"dir-mult"}
#'     \item \code{"dirichlet-miss0"}
#'     \item \code{"dirichlet-pool0"}
#'     \item \code{"dir-mult"}
#'     \item \code{"logistic-normal-miss0"}    
#'     \item \code{"logistic-normal-ar1-miss0"}
#'     \item \code{"logistic-normal-pool0"}
#'     \item \code{"logistic-normal-01-infl"}
#'     \item \code{"logistic-normal-pool0"}
#'     \item \code{"logistic-normal-01-infl-2par"}
#'     \item \code{"mvtweedie"}
#'     \item \code{"dir-mult-linear"}
#'     }
#' @param assess_years Year in which the assessment is conducted
#' @param assess_interval Assessment interval used in the MSE feedback loop
#' @param base_years Years used in the burn-in period
#' @param year.use Number of years used in the assessment model
#' @param hcr.type Type of harvest control rule
#'   \itemize{
#'     \item \code{"1"} Annual projected catch based on 75% of F40% (default)
#'     \item \code{"2"} Constant catch based on 75% of F40%
#'     \item \code{"3"} "Hockey stick" catch based on stock status
#'     }
#' @param hcr.opts a list of HCR information, only used if hcr.type = 3
#'   \itemize{
#'     \item \code{"max_percent"} maximum percent of F_XSPR to use for calculating catch in projections (default = 75)
#'     \item \code{"min_percent"} minimum percent of F_XSPR to use for calculating catch in projections (default = 0.01)
#'     \item \code{"BThresh_up"} Upper bound of overfished level (default = 0.5) 
#'     \item \code{"BThresh_low"} Lower bound of overfished level (default = 0.1)
#'     }
#' @param do.retro 	T/F do retrospective analysis? Default = TRUE.
#' @param do.osa T/F calculate one-step-ahead (OSA) residuals? Default = TRUE.
#' @param seed Seed used for generating data
#' @param save.sdrep T/F save the full report of the assessment model (memory intensive) 
#' 
#' @return a list of model output
#'   \describe{
#'     \item{\code{$om}}{Operating model}
#'     \item{\code{$sdrep_list}}{Parameter estimates from each assessment model}
#'     \item{\code{$em_list}}{Short report of each assessment model}
#'     \item{\code{$opt_list}}{Output from \code{\link[stats:nlminb]{stats::nlminb}}}
#'     \item{\code{$converge_list}}{Convergence from each assessment model}
#'     \item{\code{$em_full}}{Full report of each assessment model}
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
                           age_comp_om = "multinomial",
                           M_em, 
                           sel_em, 
                           NAA_re_em,
                           move_em,
                           em.opt = NULL,
                           age_comp_em = "multinomial",
                           assess_years = NULL, 
                           assess_interval = NULL, 
                           base_years = NULL, 
                           year.use = 30,
                           hcr.type = 1,
                           hcr.opts = NULL,
                           do.retro = FALSE,
                           do.osa = FALSE,
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
  if (!is.null(move_em) & em.opt$separate.em) warnings("move_em must be NULL if em.opt$separate.em = TRUE!")
  if (em.opt$separate.em) move_em = NULL
  
  em_list    <- list()
  par.est    <- list()
  par.se     <- list()
  adrep.est  <- list()
  adrep.se   <- list()
  opt_list   <- list()
  converge_list  <- list()
  catch_advice <- list()
  
  if(save.sdrep) em_full <- list()
  
  if (em.opt$separate.em) {
    
    if (em.opt$separate.em.type == 1) {
      
      for(y in assess_years){
        
        cat(paste0("\n-----\nStock Assessment in Year ",y,"\n"))
        
        i <- which(assess_years == y)
        
        em_list[[i]]   <- list()
        par.est[[i]]   <- list()
        par.se[[i]]    <- list()
        adrep.est[[i]] <- list()
        adrep.se[[i]]  <- list()
        opt_list[[i]]  <- list()
        converge_list[[i]] <- list()
        
        if(save.sdrep) em_full[[i]] <- list()
        
        em.years = base_years[1]:y
        
        em_input = make_em_input(om = om,
                                 M_em = M_em, 
                                 sel_em = sel_em, 
                                 NAA_re_em = NAA_re_em, 
                                 move_em = move_em,
                                 em.opt = em.opt,
                                 em_years = em.years,
                                 year.use = year.use,
                                 age_comp = age_comp_em)
        
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
          tmp = advice_fn(em = mod, pro.yr = assess_interval, hcr.type = hcr.type, hcr.opts = hcr.opts)
          
          advice = cbind(advice,tmp)
        }
        colnames(advice) = paste0("Region_",1:om$input$data$n_fleets)
        rownames(advice) = paste0("Year_",y + 1:assess_interval)
        
        cat("\n---------------------------\n")
        print(advice)
        cat("\n---------------------------\n")
        
        # set the catch for the next assess_interval years
        interval.info = list(catch = advice, years = y + 1:assess_interval)
        
        om = update_om_fn(om, interval.info, seed = seed)
        
        for(s in 1:n_stocks){
          em_list[[i]][[s]] <- em[[s]]$rep
          par.est[[i]][[s]] <- as.list(em[[s]]$sdrep, "Estimate")
          par.se[[i]] = as.list(em[[s]]$sdrep, "Std. Error")
          adrep.est[[i]] = as.list(em[[s]]$sdrep, "Estimate", report = TRUE)
          adrep.se[[i]] = as.list(em[[s]]$sdrep, "Std. Error", report = TRUE)
          opt_list[[i]][[s]] <- em[[s]]$opt
          converge_list[[i]][[s]] <- sum(conv,pdHess)
          catch_advice[[i]] <- advice
          if(save.sdrep) em_full[[i]][[s]] <- em[[s]]
        }
      }
    }
    
    if (em.opt$separate.em.type == 2) {
      
      for(y in assess_years){
        
        cat(paste0("\n-----\nStock Assessment in Year ",y,"\n"))
        
        i <- which(assess_years == y)
        
        em.years = base_years[1]:y
        
        em_input = make_em_input(om = om,
                                 M_em = M_em, 
                                 sel_em = sel_em, 
                                 NAA_re_em = NAA_re_em, 
                                 move_em = move_em,
                                 em.opt = em.opt,
                                 em_years = em.years,
                                 year.use = year.use,
                                 age_comp = age_comp_em)
        
        # fit the estimation model
        em = fit_wham(em_input, do.retro = FALSE, do.osa = FALSE, do.brps = TRUE, MakeADFun.silent = TRUE) #no feedback period yet
        
        # check convergence
        conv = as.logical(1-em$opt$convergence)
        pdHess = as.logical(if(em$na_sdrep==FALSE & !is.na(em$na_sdrep)) 1 else 0)
        
        if (!conv | !pdHess) warning("Assessment model is not converged!")
        
        # make the catch advice
        advice = advice_fn(em, pro.yr = assess_interval, hcr.type = hcr.type, hcr.opts = hcr.opts)
        
        colnames(advice) = paste0("Region_",1:om$input$data$n_fleets)
        rownames(advice) = paste0("Year_",y + 1:assess_interval)
        
        cat("\n---------------------------\n")
        print(advice)
        cat("\n---------------------------\n")
        
        # set the catch for the next assess_interval years
        interval.info = list(catch = advice, years = y + 1:assess_interval)
        
        # update F in the operating model and re-simulate the data given the updated F
        om = update_om_fn(om, interval.info, seed = seed)
        
        em_list[[i]] = em$rep
        par.est[[i]] = as.list(em$sdrep, "Estimate")
        par.se[[i]] = as.list(em$sdrep, "Std. Error")
        adrep.est[[i]] = as.list(em$sdrep, "Estimate", report = TRUE)
        adrep.se[[i]] = as.list(em$sdrep, "Std. Error", report = TRUE)
        opt_list[[i]] <- em$opt
        converge_list[[i]] <- conv+pdHess
        catch_advice[[i]] <- advice
        if(save.sdrep) em_full[[i]] <- em
      }
    }
    
    if (em.opt$separate.em.type == 3) {
      # one area model with combined fleet and survey info
      for(y in assess_years){
        
        cat(paste0("\n-----\nStock Assessment in Year ",y,"\n"))
        
        i <- which(assess_years == y)
        
        em.years = base_years[1]:y
        
        em_input = make_em_input(om = om,
                                 M_em = M_em, 
                                 sel_em = sel_em, 
                                 NAA_re_em = NAA_re_em, 
                                 move_em = move_em,
                                 em.opt = em.opt,
                                 em_years = em.years,
                                 year.use = year.use,
                                 age_comp = age_comp_em)
        
        n_stocks = om$input$data$n_stocks
        
        em = fit_wham(em_input, do.retro = FALSE, do.osa = FALSE, do.brps = TRUE, MakeADFun.silent = TRUE) #no feedback period yet
        
        # check convergence
        conv = as.logical(1-em$opt$convergence)
        pdHess = as.logical(if(em$na_sdrep==FALSE & !is.na(em$na_sdrep)) 1 else 0)
        
        if (!conv | !pdHess) warning("Assessment model is not converged!")
        
        # make the catch advice
        advice = advice_fn(em, pro.yr = assess_interval, hcr.type = hcr.type, hcr.opts = hcr.opts)
        
        advice <- matrix(c(advice*(1/om$input$data$n_fleets),advice*(1/om$input$data$n_fleets)), ncol =  om$input$data$n_fleets, byrow = TRUE)
        
        colnames(advice) = paste0("Region_",1:om$input$data$n_fleets)
        rownames(advice) = paste0("Year_",y + 1:assess_interval)
        
        cat("\n---------------------------\n")
        print(advice)
        cat("\n---------------------------\n")
        
        # set the catch for the next assess_interval years
        interval.info = list(catch = advice, years = y + 1:assess_interval)
        
        om = update_om_fn(om, interval.info, seed = seed)  
        
        em_list[[i]]   <- em$rep
        par.est[[i]]   <- as.list(em$sdrep, "Estimate")
        par.se[[i]]    <- as.list(em$sdrep, "Std. Error")
        adrep.est[[i]] <- as.list(em$sdrep, "Estimate", report = TRUE)
        adrep.se[[i]]  <- as.list(em$sdrep, "Std. Error", report = TRUE)
        opt_list[[i]]  <- em$opt
        converge_list[[i]] <- conv+pdHess
        catch_advice[[i]] <- advice
        if(save.sdrep) em_full[[i]] <- em
      }
    }
    
  } else {
    
    if (em.opt$do.move) {
      
      if (em.opt$est.move) { # equivalent to if(do.move & est.move)
        
        for(y in assess_years){
          
          cat(paste0("\n-----\nStock Assessment in Year ",y,"\n"))
          
          i <- which(assess_years == y)
          
          em.years = base_years[1]:y
          
          # make the input for the estimation model
          em_input = make_em_input(om = om,
                                   M_em = M_em, 
                                   sel_em = sel_em, 
                                   NAA_re_em = NAA_re_em, 
                                   move_em = move_em,
                                   em.opt = em.opt,
                                   em_years = em.years,
                                   year.use = year.use,
                                   age_comp = age_comp_em)
          
          # fit the estimation model
          em = fit_wham(em_input, do.retro = FALSE, do.osa = FALSE, do.brps = TRUE, MakeADFun.silent = TRUE) #no feedback period yet
          
          # check convergence
          conv = as.logical(1-em$opt$convergence)
          pdHess = as.logical(if(em$na_sdrep==FALSE & !is.na(em$na_sdrep)) 1 else 0)
          
          if (!conv | !pdHess) warning("Assessment model is not converged!")
          
          # make the catch advice
          advice = advice_fn(em, pro.yr = assess_interval, hcr.type = hcr.type, hcr.opts = hcr.opts)
          
          colnames(advice) = paste0("Region_",1:om$input$data$n_fleets)
          rownames(advice) = paste0("Year_",y + 1:assess_interval)
          
          cat("\n---------------------------\n")
          print(advice)
          cat("\n---------------------------\n")
          
          # set the catch for the next assess_interval years
          interval.info = list(catch = advice, years = y + 1:assess_interval)
          
          # update F in the operating model and re-simulate the data given the updated F
          om = update_om_fn(om, interval.info, seed = seed) 
          
          em_list[[i]]   <- em$rep
          par.est[[i]]   <- as.list(em$sdrep, "Estimate")
          par.se[[i]]    <- as.list(em$sdrep, "Std. Error")
          adrep.est[[i]] <- as.list(em$sdrep, "Estimate", report = TRUE)
          adrep.se[[i]]  <- as.list(em$sdrep, "Std. Error", report = TRUE)
          opt_list[[i]]  <- em$opt
          converge_list[[i]] <- conv+pdHess
          catch_advice[[i]] <- advice
          if(save.sdrep) em_full[[i]] <- em
        }
      } else { # equivalent to if(do.move & !est.move) # Correct model
        
        for(y in assess_years){
          
          cat(paste0("\n-----\nStock Assessment in Year ",y,"\n"))
          
          i <- which(assess_years == y)
          
          em.years = base_years[1]:y
          
          #make the input for the estimation model
          em_input = make_em_input(om = om,
                                   M_em = M_em, 
                                   sel_em = sel_em, 
                                   NAA_re_em = NAA_re_em, 
                                   move_em = move_em, 
                                   em.opt = em.opt,
                                   em_years = em.years,
                                   year.use = year.use,
                                   age_comp = age_comp_em)
          
          # Fix the movement (don't estimate movement)
          # sigma_vals = 0.2
          # em_input$par$mu_repars[,,,,1] = log(sigma_vals)
          em_input <- fix_move(em_input)
          
          # fit the estimation model
          em = fit_wham(em_input, do.retro = FALSE, do.osa = FALSE, do.brps = TRUE, MakeADFun.silent = TRUE) #no feedback period yet
          
          # check convergence
          conv = as.logical(1-em$opt$convergence)
          pdHess = as.logical(if(em$na_sdrep==FALSE & !is.na(em$na_sdrep)) 1 else 0)
          
          if (!conv | !pdHess) warning("Assessment model is not converged!")
          
          # make the catch advice
          advice = advice_fn(em, pro.yr = assess_interval, hcr.type = hcr.type, hcr.opts = hcr.opts)
          
          colnames(advice) = paste0("Region_",1:om$input$data$n_fleets)
          rownames(advice) = paste0("Year_",y + 1:assess_interval)
          
          cat("\n---------------------------\n")
          print(advice)
          cat("\n---------------------------\n")
          
          # set the catch for the next assess_interval years
          interval.info = list(catch = advice, years = y + 1:assess_interval)
          
          # update F in the operating model and re-simulate the data given the updated F
          om = update_om_fn(om, interval.info, seed = seed) 
          
          em_list[[i]]   <- em$rep
          par.est[[i]]   <- as.list(em$sdrep, "Estimate")
          par.se[[i]]    <- as.list(em$sdrep, "Std. Error")
          adrep.est[[i]] <- as.list(em$sdrep, "Estimate", report = TRUE)
          adrep.se[[i]]  <- as.list(em$sdrep, "Std. Error", report = TRUE)
          opt_list[[i]]  <- em$opt
          converge_list[[i]] <- conv+pdHess
          catch_advice[[i]] <- advice
          if(save.sdrep) em_full[[i]] <- em
        }
      }
    } else { # equivalent to if(!do.move)
      
      for(y in assess_years){
        
        cat(paste0("\n-----\nStock Assessment in Year ",y,"\n"))
        
        i <- which(assess_years == y)
        
        em.years = base_years[1]:y
        
        # make the input for the estimation model
        em_input = make_em_input(om = om,
                                 M_em = M_em, 
                                 sel_em = sel_em, 
                                 NAA_re_em = NAA_re_em, 
                                 move_em = move_em, 
                                 em.opt = em.opt,
                                 em_years = em.years,
                                 year.use = year.use,
                                 age_comp = age_comp_em)
        
        # fit the estimation model
        em = fit_wham(em_input, do.retro = FALSE, do.osa = FALSE, do.brps = TRUE, MakeADFun.silent = TRUE) #no feedback period yet
        
        # check convergence
        conv = as.logical(1-em$opt$convergence)
        pdHess = as.logical(if(em$na_sdrep==FALSE & !is.na(em$na_sdrep)) 1 else 0)
        
        if (!conv | !pdHess) warning("Assessment model is not converged!")
        
        # make the catch advice
        advice = advice_fn(em, pro.yr = assess_interval, hcr.type = hcr.type, hcr.opts = hcr.opts)
        
        colnames(advice) = paste0("Region_",1:om$input$data$n_fleets)
        rownames(advice) = paste0("Year_",y + 1:assess_interval)
        
        cat("\n---------------------------\n")
        print(advice)
        cat("\n---------------------------\n")
        
        # set the catch for the next assess_interval years
        interval.info = list(catch = advice, years = y + 1:assess_interval)
        
        # update F in the operating model and re-simulate the data given the updated F
        om = update_om_fn(om, interval.info, seed = seed) 
        
        em_list[[i]]   <- em$rep
        par.est[[i]]   <- as.list(em$sdrep, "Estimate")
        par.se[[i]]    <- as.list(em$sdrep, "Std. Error")
        adrep.est[[i]] <- as.list(em$sdrep, "Estimate", report = TRUE)
        adrep.se[[i]]  <- as.list(em$sdrep, "Std. Error", report = TRUE)
        opt_list[[i]]  <- em$opt
        converge_list[[i]] <- conv+pdHess
        catch_advice[[i]] <- advice
        if(save.sdrep) em_full[[i]] <- em
      }
    }
  }
  if (save.sdrep) {
    return(list(om = om, 
                em_list   = em_list, 
                par.est   = par.est, 
                par.se    = par.se, 
                adrep.est = adrep.est,
                adrep.se  = adrep.se,
                opt_list  = opt_list, 
                converge_list = converge_list, 
                catch_advice = catch_advice,
                em_full = em_full))
  } else {
    return(list(om = om, 
                em_list   = em_list, 
                par.est   = par.est, 
                par.se    = par.se, 
                adrep.est = adrep.est,
                adrep.se  = adrep.se,
                opt_list  = opt_list, 
                converge_list = converge_list, 
                catch_advice = catch_advice,
                em_full = list()))
  }
}
