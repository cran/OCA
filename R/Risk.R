Risk <-
function(Loss, varcov, alpha=0.95, measure=c('VaR', 'ES', 'both'),
                 weights=NULL, model=c('normal', 't-student', 'both'), df=NULL)
  { # inicia la función
  measure <- match.arg(measure)
    
  if(measure=='VaR' | measure=='both'){
    VaR. <- VaR(Loss=Loss, varcov=varcov, alpha=alpha, weights=weights, model=model, df=df)
  }
  
  if(measure=='ES' | measure=='both'){
    ES. <- ES(Loss=Loss, varcov=varcov, alpha=alpha, weights=weights, model=model, df=df)
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
