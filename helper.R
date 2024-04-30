sim_fn <- function(om, self.fit = FALSE){
  input <- om$input
  input$data = om$simulate(complete=TRUE)
  if(self.fit) {
    fit <- fit_wham(input, do.osa = F, do.retro = F, do.brp = TRUE, MakeADFun.silent = FALSE)
    return(fit)
  } else return(input) 
}

Generate_Maturity <- function(life_history = NULL, na) {
  if (is.null(life_history)){
    warning("Life history is not specified and default is used!")
    maturity <- t(matrix(1/(1 + exp(-1*(1:na - na/2))), na))
  } else if (life_history == "short"){
    m50 = 1.75; mslope = 1; 
    maturity <- t(matrix(1/(1 + exp(-(1:na-m50)/mslope)), na))
  } else if (life_history == "medium"){
    m50 = 3.5; mslope = 1; 
    maturity <- t(matrix(1/(1 + exp(-(1:na-m50)/mslope)), na))
  } else if (life_history == "long"){
    m50 = 7; mslope = 1; 
    maturity <- t(matrix(1/(1 + exp(-(1:na-m50)/mslope)), na))
  } 
  return(maturity)
}

Generate_Len <- function(Linf,k,n_ages) {
  Len <- Linf*(1-exp(-k*1:n_ages))
  return(Len)
}

Generate_WAA <- function(life_history = NULL, na) {
  if (is.null(life_history)){
    warning("Life history is not specified and default is used!")
    Len <- 100*(1-exp(-0.3*(1:na - 0)))
    W <- 3e-6*Len^3
  } else if (life_history == "short"){
    k = 0.27; Linf = 90
    Len <- Generate_Len(Linf,k,na)
    LWexp = 3; LWscaler = 3e-6
    W <- LWscaler*Len^LWexp
  } else if (life_history == "medium"){
    k = 0.13; Linf = 90
    Len <- Generate_Len(Linf,k,na)
    LWexp = 3; LWscaler = 3e-6
    W <- LWscaler*Len^LWexp
  } else if (life_history == "long"){
    k = 0.07; Linf = 90
    Len <- Generate_Len(Linf,k,na)
    LWexp = 3; LWscaler = 3e-6
    W <- LWscaler*Len^LWexp
  } 
  return(W)
}

