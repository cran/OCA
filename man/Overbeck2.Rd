\name{Overbeck2}
\alias{Overbeck2}

\title{
 Overbeck type II Allocation Principle 
}
\description{
 This function implements the Overbeck type II allocation principle for optimal capital allocation.
}

\usage{
Overbeck2(Loss, Capital, alpha = 0.95, 
          model = c("normal", "t-student", "both"), df = NULL)
}


\arguments{
  \item{Loss}{
  Either a scalar or a vector of size \emph{N} containing the mean losses.
}
  \item{Capital}{
  A scalar representing the capital to be allocated to each loss.
}
  \item{alpha}{
   A numeric value (either a single one or a vector) consisting of the significance level at which the allocation has to be computed, it can either be a single numeric value or a vector of numeric values.
}
  \item{model}{
  A character string indicating which distribution is to be used for computing the VaR underlying the Overbeck type II principle, the default value is the \code{normal} distribution, the other alternative is \code{t-student} distribution with \eqn{\upsilon}{\upsilon} degrees of freedom. When \code{model='both'} \code{'normal'} as well as \code{'t-student'} are used when computing the allocations, see examples.
}
  \item{df}{
An integer indicating the degrees of freedom for the t-student distribution when setting \code{model='t-student'} and \code{model='both'}. \code{df} must be greater than 2. 
 }
}

\details{
\code{Overbeck2} computes the  capital allocation based on the following formulation:

 \deqn{
  K_{i} = \frac{K}{CTE_{p}[S]} E \left[ X_{i}|S > F_{X_{S}}^{-1}(p) \right], \quad i=1, \ldots, n. 
 }{
 K_i =K/CTE_p[S] E [ X_i|S > F_X_S^-1(p)],  i=1, ..., n. 
 }
 
Where \eqn{$K$}{K} is the aggregate capital to be allocated, \eqn{CTE_{p}[S]}{CTE_p[S] } is the Conditional Tail Expectation of the aggregate loss at level \emph{p}, \eqn{$X_{i}$}{X_i} is the individual loss, \emph{S} is the aggregate loss and \eqn{F_{X}^{-1}(p)}{ F_X^-1(p)} is the quantile function of \eqn{$X$}{X} at level  \emph{p}



}
\value{
A real-valued \eqn{n \times 1}{n x 1} matrix containing the optimal capital allocation, if \code{Capital} is set to 1, then the returned matrix will consist of the proportions of capital each individual loss needs to be optimally faced.
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
\code{\link{hap}}, \code{\link{cap}}
}
\examples{
data(dat1, dat2)
Loss <- cbind(Loss1=dat1[1:400, ], Loss2=unname(dat2))
# Proportions of capital to be allocated to each bussines unit
Overbeck2(Loss, Capital=1) 

# Capital allocation,
# capital is determined as the empirical VaR of the losses at 99\%  
K <- quantile(rowSums(Loss),  probs = 0.99)
Overbeck2(Loss, Capital=K)
}
\keyword{manip}