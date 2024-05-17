#' Fix movement parameter
#' 
#' a function to fix movement rate in the input. 
#' 
#' @param input wham input

#' @return a wham input with movement rate treated as known
#'   
#' @export
#'
fix_move <- function(input) {
  temp <- array(as.integer(input$map$trans_mu), dim = dim(input$par$trans_mu))
  temp[] <- NA
  input$map$trans_mu <- factor(temp)
  
  if(!is.null(input$map$mu_prior_re)) {
    cat("\nMovement is treated as random effects and fixed as known.")
    temp <- array(as.integer(input$map$mu_prior_re), dim = dim(input$par$mu_prior_re))
    temp[] <- NA
    input$map$mu_prior_re <- factor(temp)
  }
  
  if(!is.null(input$map$mu_re)) {
    temp <- array(as.integer(input$map$mu_re), dim = dim(input$par$mu_re))
    temp[] <- NA
    input$map$mu_re <- factor(temp)
  }
  
  if(!is.null(input$map$mu_repars)) {
    temp <- array(as.integer(input$map$mu_repars), dim = dim(input$par$mu_repars))
    temp[] <- NA
    input$map$mu_repars <- factor(temp)
  }

  return(input)
}