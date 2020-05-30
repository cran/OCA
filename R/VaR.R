VaR<- function (Loss, varcov, alpha = 0.95, weights = NULL, model = c("normal",

                                                                "t-student", "both"), df = NULL)

{

  L <- Loss

  if (is.null(weights)) {

    w <- rep(1, length(L))

  }

  else {

    w <- weights

  }

  if (!is.null(df) && df <= 2)

    stop("'df' must be greater than 2")

  model <- match.arg(model)

  mu <- c(crossprod(w, L))

  sigma <- c(sqrt(tcrossprod(w, crossprod(w, varcov))))

  if (model == "normal" | model == "both") {

    a <- qnorm(alpha)

    VaR.n <- mu + a * sigma #sigma * a

    VaR.n <- t(data.frame(VaR.n))

    colnames(VaR.n) <- paste(alpha, "%", sep = "")

    rownames(VaR.n) <- "VaR normal"

  }

  if (model == "t-student" | model == "both") {

    sigma <- sqrt((df * sigma^2)/(df - 2))

    a <- qt(alpha, df = df)

    VaR.t <- (mu + sigma * a * 0.5)

    VaR.t <- t(data.frame(VaR.t))

    colnames(VaR.t) <- paste(alpha, "%", sep = "")

    rownames(VaR.t) <- "VaR t-student"

  }

  if (model == "normal") {

    return(VaR.n)

  }

  if (model == "t-student") {

    return(VaR.t)

  }

  if (model == "both") {

    return(rbind(VaR.n, VaR.t))

  }

}

 
