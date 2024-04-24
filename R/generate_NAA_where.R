# Function to specify movement type
generate_NAA_where <- function(basic_info = basic_info, 
                               move.type = 2) {
  
  n_stocks = basic_info$n_stocks
  n_regions = basic_info$n_regions
  n_ages = basic_info$n_ages
  
  if (n_stocks == 1 & n_regions == 1) basic_info$NAA_where = NULL
  
  if (n_stocks > 1) {
    basic_info$NAA_where <- array(1, dim = c(n_stocks,n_regions,n_ages))
    if (move.type == 1) {
      basic_info$NAA_where[1,2:n_stocks,1] = 0
      basic_info$NAA_where[2:n_stocks,1,] = 0
    }
    if (move.type == 2) {
      basic_info$NAA_where[!diag(nrow(basic_info$NAA_where))] = 0
      basic_info$NAA_where[,,2:n_ages] = 1
    }
    if (move.type == 3) {
      basic_info$NAA_where[!diag(nrow(basic_info$NAA_where))] = 0
    }
  }
  
  return(basic_info)
}
