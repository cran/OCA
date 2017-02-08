ES <-
function(Loss, varcov, alpha=0.95, weights=NULL, model=c('normal', 't-student', 'both'), df=NULL)
{ # inicia la funcion
  # alpha <- as.numeric(alpha)
  model <- match.arg(model)
  L <- Loss
  w <-if(is.null(weights)){
    w <- rep(1, length(L))
  } else{
    w <- weights
  }
  alpha <- as.numeric(alpha)
  mu <- crossprod(w, L)
  sigma <- sqrt(tcrossprod(w, crossprod(w, varcov)))
                  
  
  if(model=='normal' | model=='both'){
    ES.n <- as.matrix(sapply(1:length(alpha), function(x, y, i) {
      x + y *dnorm(qnorm(alpha[i]))/(1-alpha[i])
    },  x=mu, y=sigma))
    ES.n <- t(data.frame(ES.n))
    colnames(ES.n) <- paste(alpha, '%', sep='')
    rownames(ES.n) <- 'ES normal'
           }
  
  if(model=='t-student' | model=='both'){
    ES.t <- as.matrix(sapply(1:length(alpha), function(sigma, alpha, df, i) 
      mu + .5* sqrt(sigma)*( dt(qt(alpha[i], df), df)/(1-alpha[i]) )*(df+qt(alpha[i], df)*qt(alpha[i], df))/(df-1),
                           sigma=(df*sigma^2)/(df-2), alpha=alpha, df=df))
    ES.t <- t(data.frame(ES.t))
    colnames(ES.t) <- paste(alpha, '%', sep='')
    rownames(ES.t) <- 'ES t-student'
    
  }
  
    
  if(model=='normal'){
    return(ES.n)
  }
  
  if(model=='t-student'){
    return(ES.t)
  }
  
  if(model=='both'){
  .ES <- rbind(ES.n, ES.t)
  return(.ES)
  }
  

  
}
