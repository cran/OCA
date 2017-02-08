cap <-
function(Loss, Capital){
  K <- as.numeric(Capital)
  L <- as.matrix(Loss)
  S <- rowSums(Loss)
  Ki <- (K/var(S)) * apply(L, 2, function(x) cov(x, S))
  return(as.matrix(Ki))
}
