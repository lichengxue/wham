generate_config <- function(EM.opt = 1, move) {
  fleet_pars <- c(5,1)
  index_pars <- c(2,1)
  n_ages     <- 12
  
  # ---1----
  if (EM.opt == 1) {
    n_stocks = n_regions = n_fleets = n_indices = 1
    sel_em <- list(model=rep("logistic",n_fleets+n_indices),
                   initial_pars=c(rep(list(fleet_pars),n_fleets),rep(list(index_pars),n_indices)),
                   fix_pars=rep(list(NULL),n_fleets+n_indices))
    NAA_re_em <- list(N1_model="equilibrium",sigma="rec+1",cor="iid")
    M_em <- list(model="constant",initial_means=array(0.2, dim = c(n_stocks,n_regions,n_ages)))
    em_opt <- list(separate.em = TRUE, separate.em.type = 1, do.move = FALSE, est.move = FALSE)
    move_em <- NULL
    config <- list(n_stocks = n_stocks, n_regions = n_regions, n_fleets = n_fleets, n_indices = n_indices,
                   sel_em = sel_em, NAA_re_em = NAA_re_em, M_em = M_em, em_opt = em_opt, move_em = move_em)
  }
  # ---2----
  if (EM.opt == 2) {
    n_stocks = n_regions = 1
    n_fleets = n_indices = 2
    sel_em <- list(model=rep("logistic",n_fleets+n_indices),
                   initial_pars=c(rep(list(fleet_pars),n_fleets),rep(list(index_pars),n_indices)),
                   fix_pars=rep(list(NULL),n_fleets+n_indices))
    NAA_re_em <- list(N1_model="equilibrium",sigma="rec+1",cor="iid")
    M_em <- list(model="constant",initial_means=array(0.2, dim = c(n_stocks,n_regions,n_ages)))
    em_opt <- list(separate.em = TRUE, separate.em.type = 2, do.move = FALSE, est.move = FALSE)
    move_em <- NULL
    config <- list(n_stocks = n_stocks, n_regions = n_regions, n_fleets = n_fleets, n_indices = n_indices,
                   sel_em = sel_em, NAA_re_em = NAA_re_em, M_em = M_em, em_opt = em_opt, move_em = move_em)
  }
  # ---3----
  if (EM.opt == 3) {
    n_stocks = n_regions = 1
    n_fleets = n_indices = 1
    sel_em <- list(model=rep("logistic",n_fleets+n_indices),
                   initial_pars=c(rep(list(fleet_pars),n_fleets),rep(list(index_pars),n_indices)),
                   fix_pars=rep(list(NULL),n_fleets+n_indices))
    NAA_re_em <- list(N1_model="equilibrium",sigma="rec+1",cor="iid")
    M_em <- list(model="constant",initial_means=array(0.2, dim = c(n_stocks,n_regions,n_ages)))
    em_opt <- list(separate.em = TRUE, separate.em.type = 3, do.move = FALSE, est.move = FALSE)
    move_em <- NULL
    config <- list(n_stocks = n_stocks, n_regions = n_regions, n_fleets = n_fleets, n_indices = n_indices,
                   sel_em = sel_em, NAA_re_em = NAA_re_em, M_em = M_em, em_opt = em_opt, move_em = move_em)
  }
  # ---4----
  if (EM.opt == 4) {
    n_stocks = n_regions = 2
    n_fleets = n_indices = 2
    sel_em <- list(model=rep("logistic",n_fleets+n_indices),
                   initial_pars=c(rep(list(fleet_pars),n_fleets),rep(list(index_pars),n_indices)),
                   fix_pars=rep(list(NULL),n_fleets+n_indices))
    NAA_re_em <- list(N1_model=rep("equilibrium",n_stocks),
                      sigma=rep("rec+1",n_stocks),
                      cor=rep("iid",n_stocks))
    M_em <- list(model="constant",initial_means=array(0.2, dim = c(n_stocks,n_regions,n_ages)))
    em_opt <- list(separate.em = FALSE, separate.em.type = 3, do.move = TRUE, est.move = FALSE)
    move_em <- move
    config <- list(n_stocks = n_stocks, n_regions = n_regions, n_fleets = n_fleets, n_indices = n_indices,
                   sel_em = sel_em, NAA_re_em = NAA_re_em, M_em = M_em, em_opt = em_opt, move_em = move_em)
  }
  # ---5----
  if (EM.opt == 5) {
    n_stocks = n_regions = 2
    n_fleets = n_indices = 2
    sel_em <- list(model=rep("logistic",n_fleets+n_indices),
                   initial_pars=c(rep(list(fleet_pars),n_fleets),rep(list(index_pars),n_indices)),
                   fix_pars=rep(list(NULL),n_fleets+n_indices))
    NAA_re_em <- list(N1_model=rep("equilibrium",n_stocks),
                      sigma=rep("rec+1",n_stocks),
                      cor=rep("iid",n_stocks))
    M_em <- list(model="constant",initial_means=array(0.2, dim = c(n_stocks,n_regions,n_ages)))
    em_opt <- list(separate.em = FALSE, separate.em.type = 3, do.move = FALSE, est.move = FALSE)
    move_em <- NULL
    config <- list(n_stocks = n_stocks, n_regions = n_regions, n_fleets = n_fleets, n_indices = n_indices,
                   sel_em = sel_em, NAA_re_em = NAA_re_em, M_em = M_em, em_opt = em_opt, move_em = move_em)
  }
  # ---6----
  if (EM.opt == 6) {
    n_stocks = n_regions = 2
    n_fleets = n_indices = 2
    sel_em <- list(model=rep("logistic",n_fleets+n_indices),
                   initial_pars=c(rep(list(fleet_pars),n_fleets),rep(list(index_pars),n_indices)),
                   fix_pars=rep(list(NULL),n_fleets+n_indices))
    NAA_re_em <- list(N1_model=rep("equilibrium",n_stocks),
                      sigma=rep("rec",n_stocks),
                      cor=rep("iid",n_stocks))
    M_em <- list(model="constant",initial_means=array(0.2, dim = c(n_stocks,n_regions,n_ages)))
    em_opt <- list(separate.em = FALSE, separate.em.type = 3, do.move = FALSE, est.move = FALSE)
    move_em <- NULL
    config <- list(n_stocks = n_stocks, n_regions = n_regions, n_fleets = n_fleets, n_indices = n_indices,
                   sel_em = sel_em, NAA_re_em = NAA_re_em, M_em = M_em, em_opt = em_opt, move_em = move_em)
  }

  return(config)
}

