\name{cap}
\alias{cap}

\title{
 Covariance Allocation Principle 
}
\description{
 This function implements the covariance allocation principle for optimal capital allocation.
}

\usage{
 cap(Loss, Capital)
}

\arguments{
  \item{Loss}{
    A matrix containing the individual losses in each column
 }
  \item{Capital}{
   A scalar representing the capital to be allocated to each loss. 
 }
}

\details{
 The Covariance Allocation Principle correspond to the following expression:
 
 \deqn{
 K_{i} = \frac{K}{Var[S]} Cov(X_{i}, S), \quad i=1, \ldots, n,    
}{
K_i = (K / Var[S]) Cov(X_i, S),  i=1, \ldots, n,    
}


 where \eqn{K_{i}}{K_i}  is the capital to be allocated to the \emph{ith} loss, \emph{K} is the total capital to be allocated, \eqn{X_{i}}{X_i} is the individual unit loss and \emph{S} is the total (aggretate) loss, this comes from \eqn{\sum_{i}X_{i}}{\sum X_i}. \eqn{Cov(X_{i}, S)}{Cov(X_i, S)} is the covariance between the individual loss \eqn{X_{i}}{X_i} and the aggregate loss \emph{S}; and \eqn{Var(S)}{Var(S)} is the variance of the aggregate loss. 
}

\value{
 A \eqn{n \times 1}{n x 1} matrix containing each asset and the corresponding capital allocation. If \code{Capital=1}, then the returned value will be the proportions of capital required by each loss to be faced.
}
\references{
Dhaene J., Tsanakas A., Valdez E. and Vanduffel S. (2011). \emph{Optimal Capital Allocation Principles}. The Journal of Risk and Insurance. Vol. 00, No. 0, 1-28.


Urbina, J. (2013) \emph{Quantifying Optimal Capital Allocation Principles based on Risk Measures.} Master Thesis, Universitat Politècnica de Catalunya.

Urbina, J. and Guillén, M. (2014). \emph{An application of capital allocation principles to operational risk and the cost of fraud}. Expert Systems with Applications. 41(16):7023-7031.

}
\author{
Jilber Urbina
}


\seealso{
 \code{\link{Overbeck2}}, \code{\link{hap}}
}
\examples{
data(dat1, dat2)
Loss <- cbind(Loss1=dat1[1:400, ], Loss2=unname(dat2))
# Proportions of capital to be allocated to each bussines unit
cap(Loss, Capital=1) 

# Capital allocation,
# capital is determined as the empirical VaR of the losses at 99\%  
K <- quantile(rowSums(Loss),  probs = 0.99)
cap(Loss, Capital=K)

}

\keyword{manip}
