\name{Risk}
\alias{Risk}

\title{
 Risk measures suchs as Value at Risk (VaR) and Expected Shortfall (ES) with normal and t-student distributions.
}
\description{
Standard risk measures such VaR and ES are provided by \code{Risk}. Both VaR and ES can be computed using either the normal or t-student distribution.
}
\usage{
Risk(Loss, varcov, alpha = 0.95, 
     measure = c("VaR", "ES", "both"), 
     weights = NULL, 
     model = c("normal", "t-student", "both"),
     df = NULL)
}

\arguments{
  \item{Loss}{
   It could be either a scalar or a $m x 1$ matrix containing the mean losses.
}
  \item{varcov}{
   A scalar corresponding to the variance of the loss, if \code{Loss} is a $m x 1$ matrix, then \code{varcov} must be a $m x m$ matrix containing the variances and covariances of the losses.
}
  \item{alpha}{
  The confidence level at which either the VaR or the ES will be computed, by default \code{alpha} is set to 0.95.
}
  \item{measure}{
   An optional character string giving a measure for computing the risk. \code{"VaR"} stands for Value at Risk, \code{"ES"} stands for Expected Shortfall, and if both is chosen, then the function returns both the VaR and the ES as a result. By default \code{measure} is set to be \code{"VaR"}.
}
  \item{weights}{
   A vector cointaining the weights. It is only needed if \code{Loss} is a matrix, if it is not then \code{weights} is set to 1.
}
  \item{model}{
    A character string indicating which probability model has to be used for computing the risk measures, it could only be a normal distribution or a t-student distribution with $v$ degrees of freedom. The normal distibution is the default model for this funcion. \code{model} also allows the user to set \code{'both'} if she wishes both normal and t-student VaR or ES depending on what she choses in \code{measure}. See example below.
}
  \item{df}{
   An integer (df>2) denoting the degrees of freedom, only required if \code{model='t-student'}. Otherwise it has to be \code{NULL}.
}
}



\value{
A \code{data.frame} containing each risk measure at its corresponding confidence level.
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
\code{\link{VaR}}
}

\examples{

# Reproducing Table 2.1 in page 47 of 
# McNeal A., Frey R. and Embrechts P (2005).
 alpha <- c(.90, .95, .975, .99, .995)
 (Risk(Loss=1, varcov=(0.2/sqrt(250))^2, alpha=alpha, 
       measure='both', model='both', df=4)-1)*10000
 
 # only VaR results
 (Risk(Loss=1, varcov=(0.2/sqrt(250))^2, alpha=alpha, 
       measure='VaR', model='both', df=4)-1)*10000
 
 # only normal VaR results
 (Risk(Loss=1, varcov=(0.2/sqrt(250))^2, alpha=alpha)-1)*10000
 
 # only SE based on a 4 degrees t-student.
 (Risk(Loss=1, varcov=(0.2/sqrt(250))^2, alpha=alpha, 
       measure='ES', model='t-student', df=4)-1)*10000
}

\keyword{manip}
