Overbeck2 <-
function(Loss, Capital, alpha=0.95, model=c("normal", "t-student", "both"), df=NULL){
  K <- as.numeric(Capital)
  L <- as.matrix(Loss)
  alpha <- as.numeric(alpha)
  S <- rowSums(L)
  
  # VaR alpha%: F^{-1}_{S}(p)=VaR
  VaRp <- as.numeric(VaR(mean(S), varcov=var(S), alpha=alpha, model=model, df=df))
  
  # Funcion indicador: S > VaR
  ind <- S>VaRp
  
  # Capital allocation
  return(K*(colMeans(L[ind, ,drop=FALSE]) / mean(S[ind])))
    
}
