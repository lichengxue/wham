#' Generate input data for the assessment model
#' 
#' a function to generate the input for the estimation model for management strategy evaluation. 
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
#' @param em_years Years used in the assessment model
#' @param year.use Number of years used in the assessment model
#' 
#' @return a wham input 
#'   
#' @export
#'
#' @seealso \code{\link{loop_through_fn}}
#'

make_em_input = function(om, 
                         M_em = NULL, 
                         sel_em = NULL, 
                         NAA_re_em = NULL, 
                         move_em = NULL,
                         em.opt = em.opt,
                         em_years = NULL,
                         year.use = NULL) {
  if (em.opt$separate.em) em.opt$do.move = FALSE 
  if (!em.opt$separate.em & !em.opt$do.move) move.type = 3 # no movement
  if (!em.opt$separate.em & em.opt$do.move & all(move_em$stock_move)) move.type = 2 # bidirectional
  if (!em.opt$separate.em & em.opt$do.move & !all(move_em$stock_move)) move.type = 1 # unidirectional
  
  data = om$input$data
  
  if (!is.null(year.use)) {
    if (year.use > length(em_years)) stop("Warning: year.use must be > em_years!")
    ind_em = (length(em_years)-year.use+1):length(em_years)
    em_years = tail(em_years,year.use)
  } else {
    year.use = length(em_years)
  }
  
  if (em.opt$separate.em){
    if (em.opt$separate.em.type == 1) {
      n_stocks = data$n_stocks
      em_input = list()
      for (s in 1:n_stocks){
        info = generate_basic_info(n_stocks = 1,
                                   n_regions = 1,
                                   n_indices = 1,
                                   n_fleets = 1,
                                   base.years = em_years)
        
        #fill in the data from the operating model simulation
        info$catch_info$agg_catch = data$agg_catch[ind_em,s,drop = F]
        info$index_info$agg_indices = data$agg_indices[ind_em,s,drop = F]
        info$catch_info$catch_paa = data$catch_paa[s,ind_em,,drop = F]
        info$index_info$index_paa = data$index_paa[s,ind_em,,drop = F]
        em_input[[s]] = prepare_wham_input(basic_info = info, selectivity = sel_em, M = M_em, NAA_re = NAA_re_em)
      } 
    }
    
    if (em.opt$separate.em.type == 2) { # use areas-as-fleets model 
      n_fleets = data$n_fleets
      n_indices = data$n_indices
      
      info = generate_basic_info(n_stocks = 1,
                                 n_regions = 1,
                                 n_indices = n_indices,
                                 n_fleets = n_fleets,
                                 base.years = em_years)
      
      #fill in the data from the operating model simulation
      info$catch_info$agg_catch = data$agg_catch[ind_em,,drop = F]
      info$index_info$agg_indices = data$agg_indices[ind_em,,drop = F]
      info$catch_info$catch_paa = data$catch_paa[,ind_em,,drop = F]
      info$index_info$index_paa = data$index_paa[,ind_em,,drop = F]
      
      em_input = prepare_wham_input(basic_info = info, selectivity = sel_em, M = M_em, NAA_re = NAA_re_em)
    }
    
    if (em.opt$separate.em.type == 3) { # use one area with fleets and survey indices combined
      
      info = generate_basic_info(n_stocks = 1,
                                 n_regions = 1,
                                 n_indices = 1,
                                 n_fleets = 1,
                                 base.years = em_years)
      
      #fill in the data from the operating model simulation
      info$catch_info$agg_catch = data$agg_catch[ind_em,,drop = F]
      info$catch_info$agg_catch = matrix(rowSums(info$catch_info$agg_catch),ncol = 1)
      info$index_info$agg_indices = data$agg_indices[ind_em,,drop = F]
      info$index_info$agg_indices = matrix(rowSums(info$index_info$agg_indices),ncol = 1)
      
      info$catch_info$catch_paa = data$catch_paa[,ind_em,,drop = F]
      catch = data$agg_catch[ind_em,,drop = F]
      ratio = data$catch_paa[,ind_em,]
      result = 0
      for (i in 1:dim(ratio)[1]){
        tmp <- ratio[i,,]*catch[,i]
        result <- result+tmp
      }
      result <- t(apply(result,1,function(row)row/sum(row)))
      info$catch_info$catch_paa <- array(result,dim = c(1, nrow(result), ncol(result)))
      
      info$index_info$index_paa = data$index_paa[,ind_em,,drop = F]
      catch = data$agg_indices[ind_em,,drop = F]
      ratio = data$index_paa[,ind_em,]
      result = 0
      for (i in 1:dim(ratio)[1]){
        tmp <- ratio[i,,]*catch[,i]
        result <- result+tmp
      }
      result <- t(apply(result,1,function(row)row/sum(row)))
      info$index_info$index_paa <- array(result,dim = c(1, nrow(result), ncol(result)))
      
      em_input = prepare_wham_input(basic_info = info, selectivity = sel_em, M = M_em, NAA_re = NAA_re_em)
    }
    
  } else {
    info = generate_basic_info(base.years = em_years)
    info = generate_NAA_where(basic_info = info, move.type = move.type)
    #fill in the data from the operating model simulation
    info$catch_info$agg_catch = data$agg_catch[ind_em,,drop = F]
    info$index_info$agg_indices = data$agg_indices[ind_em,,drop = F]
    info$catch_info$catch_paa = data$catch_paa[,ind_em,,drop = F]
    info$index_info$index_paa = data$index_paa[,ind_em,,drop = F]
    
    if (em.opt$do.move) {
      
      em_input = prepare_wham_input(basic_info = info, selectivity = sel_em, M = M_em, NAA_re = NAA_re_em, move = move_em)
      
      if (!em.opt$est.move) em_input = fix_move(em_input)
      
    } else {
      
      em_input = prepare_wham_input(basic_info = info, selectivity = sel_em, M = M_em, NAA_re = NAA_re_em, move = NULL)
    }
  }
  
  return(em_input)
}

