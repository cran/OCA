hap <-
function(Loss, Capital, alpha=0.95, model='normal', df=NULL){
  K <- as.numeric(Capital)
  Loss <- as.matrix(Loss)
  alpha <- as.numeric(alpha)
  
  V <- sapply(seq_len(ncol(Loss)), function(L, i){
    VaR(Loss=colMeans(L)[i], variance=diag(cov(L))[i], weights=1, alpha=alpha, model=model, df=df) 
  }, L=Loss)
  
  return(matrix((K / sum(V)) * V))
    
}
