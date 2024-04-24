# Function to generate EM input
make_em_input = function(om, 
                         M_em = NULL, 
                         sel_em = NULL, 
                         NAA_re_em = NULL, 
                         move_em = NULL,
                         em.opt = em.opt,
                         em_years = NULL,
                         year.use = NULL) {
  #em.opt = list(separate.em = TRUE, separate.em.type = 2, do.move = FALSE, est.move = FALSE)
  #em.opt = list(separate.em = FALSE, separate.em.type = 1, do.move = TRUE, est.move = FALSE)
  
  #if (is.null(em.opt)) em.opt = list(separate.em = FALSE, separate.em.type = NULL, do.move = FALSE, move_em = NULL, est.move = FALSE)
  # Fit EMs separately
  #if (em.opt$separate.em & !em.opt$separate.em.type %in% 1:3) warning("separate.em.type must be 1:3!")
  # Fit EMs simultaneously
  #if (!em.opt$separate.em & !em.opt$separate.em.type %in% 1:3) separate.em.type = NULL
  #if (!em.opt$separate.em & em.opt$do.move & (is.null(em.opt$move))) stop("movement structure (move_em) must be specified!")
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
        info$agg_catch = data$agg_catch[ind_em,s,drop = F]
        info$agg_indices = data$agg_indices[ind_em,s,drop = F]
        info$catch_paa = data$catch_paa[s,ind_em,,drop = F]
        info$index_paa = data$index_paa[s,ind_em,,drop = F]
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
      info$agg_catch = data$agg_catch[ind_em,,drop = F]
      info$agg_indices = data$agg_indices[ind_em,,drop = F]
      info$catch_paa = data$catch_paa[,ind_em,,drop = F]
      info$index_paa = data$index_paa[,ind_em,,drop = F]
      
      em_input = prepare_wham_input(basic_info = info, selectivity = sel_em, M = M_em, NAA_re = NAA_re_em)
    }
    
    if (em.opt$separate.em.type == 3) { # use one area with fleets and survey indices combined
      
      info = generate_basic_info(n_stocks = 1,
                                 n_regions = 1,
                                 n_indices = 1,
                                 n_fleets = 1,
                                 base.years = em_years)
      
      #fill in the data from the operating model simulation
      info$agg_catch = data$agg_catch[ind_em,,drop = F]
      info$agg_catch = matrix(rowSums(info$agg_catch),ncol = 1)
      info$agg_indices = data$agg_indices[ind_em,,drop = F]
      info$agg_indices = matrix(rowSums(info$agg_indices),ncol = 1)
      
      info$catch_paa = data$catch_paa[,ind_em,,drop = F]
      catch = data$agg_catch[ind_em,,drop = F]
      ratio = data$catch_paa[,ind_em,]
      result = 0
      for (i in 1:dim(ratio)[1]){
        tmp <- ratio[i,,]*catch[,i]
        result <- result+tmp
      }
      result <- t(apply(result,1,function(row)row/sum(row)))
      info$catch_paa <- array(result,dim = c(1, nrow(result), ncol(result)))
      
      info$index_paa = data$index_paa[,ind_em,,drop = F]
      catch = data$agg_indices[ind_em,,drop = F]
      ratio = data$index_paa[,ind_em,]
      result = 0
      for (i in 1:dim(ratio)[1]){
        tmp <- ratio[i,,]*catch[,i]
        result <- result+tmp
      }
      result <- t(apply(result,1,function(row)row/sum(row)))
      info$index_paa <- array(result,dim = c(1, nrow(result), ncol(result)))
      
      em_input = prepare_wham_input(basic_info = info, selectivity = sel_em, M = M_em, NAA_re = NAA_re_em)
    }
    
  } else {
    info = generate_basic_info(base.years = em_years)
    info = generate_NAA_where(basic_info = info, move.type = move.type)
    #fill in the data from the operating model simulation
    info$agg_catch = data$agg_catch[ind_em,,drop = F]
    info$agg_indices = data$agg_indices[ind_em,,drop = F]
    info$catch_paa = data$catch_paa[,ind_em,,drop = F]
    info$index_paa = data$index_paa[,ind_em,,drop = F]
    
    if (em.opt$do.move) {
      
      em_input = prepare_wham_input(basic_info = info, selectivity = sel_em, M = M_em, NAA_re = NAA_re_em, move = move_em)
      
      if (!em.opt$est.move) em_input = fix_move(em_input)
      
    } else {
      
      em_input = prepare_wham_input(basic_info = info, selectivity = sel_em, M = M_em, NAA_re = NAA_re_em, move = NULL)
    }
  }
  
  return(em_input)
}

