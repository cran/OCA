\name{VaR}
\alias{VaR}

\title{
Value at Risk
}
\description{
 Computes Value at Risk based on both normal and t-student distribution.
}
\usage{
VaR(Loss, varcov, alpha = 0.95, weights = NULL,
    model = c("normal", "t-student", "both"), 
    df = NULL)
}

\arguments{
  \item{Loss}{
 It could be either a scalar or a $m x 1$ matrix containing the mean losses.
}
  \item{varcov}{
 A scalar corresponding to the variance of the loss, if \code{Loss} is a $m x1$ matrix, then \code{varcov} must be a $m xm$ matrix containing the variances and covariances of the losses.
}
  \item{alpha}{
 The confidence level at which either the VaR or the ES will be computed, by default \code{alpha} is set to 0.95.
}
  \item{weights}{
  A vector of weights of size \emph{N} for computing both the mean and the variance of the vector of Losses, it is applicable only when \code{Loss} is a vector. When \code{weights=NULL} mean and variaces used to compute ES are the original values supplied to \code{Losses} and \code{varcov}.
}
  \item{model}{
    A character string indicating which probability model has to be used for computing the risk measures, it could only be a normal distribution or a t-student distribution with $v$ degrees of freedom. The normal distibution is the default model for this funcion. \code{model} also allows the user to set \code{'both'} if she wishes both normal and t-student VaR or ES depending on what she choses in \code{measure}. See example below.
}
  \item{df}{
   An integer (df>2) denoting the degrees of freedom, only required if \code{model='t-student'}. Otherwise it has to be \code{NULL}.
}
}

\value{
A \code{data.frame} containing each risk measure at its corresponding confidence level
}
\references{
McNeal A., Frey R. and Embrechts P (2005). Quantitative Risk Management: Concepts, Techniques and Tools. Princeton Series of Finance. ISBN 0-691-12255-5


Urbina, J. (2013) \emph{Quantifying Optimal Capital Allocation Principles based on Risk Measures.} Master Thesis, Universitat Politècnica de Catalunya.

Urbina, J. and Guillén, M. (2014). \emph{An application of capital allocation principles to operational risk and the cost of fraud}. Expert Systems with Applications. 41(16):7023-7031.
}
\author{
Jilber Urbina.
}


\seealso{
\code{\link{Risk}}
}
\examples{
# Reproducing VaR from Table 2.1 in page 47 of 
# McNeal A., Frey R. and Embrechts P (2005).

alpha <- c(.90, .95, .975, .99, .995)
VaR(Loss=0, varcov=(10000*0.2/sqrt(250))^2, alpha=alpha, model='both', df=4)
 
 # only normal VaR results
VaR(Loss=0, varcov=(10000*0.2/sqrt(250))^2, alpha=alpha)
 


}

\keyword{manip}
