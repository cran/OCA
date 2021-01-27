Risk <-
function(Loss, variance, alpha=0.95, measure=c('VaR', 'ES', 'both'),
         weights=NULL, model=c('normal', 't-student', 'both'), 
         df=NULL, percentage = FALSE)
  { # inicia la función
  measure <- match.arg(measure)
    
  if(measure=='VaR' | measure=='both'){
    VaR. <- VaR(Loss=Loss, variance=variance, alpha=alpha, weights=weights, model=model, df=df, percentage = percentage)
  }
  
  if(measure=='ES' | measure=='both'){
    ES. <- ES(Loss=Loss, variance=variance, alpha=alpha, weights=weights, model=model, df=df, percentage = percentage)
  }
  
  if(measure=='VaR'){
    return(VaR.)
  }
  if(measure=='ES'){
    return(ES.)
  }
  if(measure=='both'){
    return(rbind(VaR., ES.))
  }
}
