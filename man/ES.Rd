\name{ES}
\alias{ES}

\title{
Expected Shortfall
}
\description{
 Computes the Expected Shortfall of a given amount of loss.
}
\usage{
ES(Loss, varcov, alpha = 0.95, weights = NULL,
   model = c("normal", "t-student", "both"), 
   df = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Loss}{
  Either a single-numeric value or a vector representing the mean loss(es) to which the ES is to be calculated.
}
  \item{varcov}{
  If \code{Loss} is a single-numeric value, then \code{varcov} must be a scalar denoting the variance of the loss, otherwise, if \code{Loss} is a vector of \emph{N} elements, then \code{varcov} must be a variance-covariance matrix of dimension \eqn{N \times N}{N x N}.
}
  \item{alpha}{
  A numeric value (either a single one or a vector) consisting of the significance level at which ES has to be computed, it can either be a single numeric value or a vector of numeric values.
}

\item{weights}{
  A vector of weights of size \emph{N} for computing both the mean and the variance of the vector of Losses, it is applicable only when \code{Loss} is a vector. When \code{weights=NULL} mean and variaces used to compute ES are the original values supplied to \code{Losses} and \code{varcov}.
}
  \item{model}{
  A character string indicating which distribution is to be used for computing the ES, the default value is the \code{normal} distribution, the other alternative is \code{t-student} distribution with \eqn{\upsilon}{\upsilon} degrees of freedom. When \code{model='both'} \code{'normal'} as well as \code{'t-student'} are used when computing the ES, see examples.
}
  \item{df}{
 An integer indicating the degrees of freedom for the t-student distribution when setting \code{model='t-student'} and \code{model='both'}. \code{df} must be greater than 2. 
}
}
\details{
\code{ES} computes the Expected Shortfall (ES) of a certaing amount of loss based upon the following general formulation:

 \deqn{
 ES_{\alpha} = \frac{1}{(1-\alpha)} \int_{\alpha}^{1} VaR_{u}(X)du = E[X|X > F_{X}^{-1}(\alpha)].
 }{
 ES_\alpha = 1/(1-\alpha) \int_{\alpha}^{1} VaR_u(X)du = E[X|X > F_X^-1(\alpha)].
 }

where \eqn{\alpha}{\alpha} is the significance level, \eqn{VaR_{u}(X)}{VaR_u(X)} is the Value-at-Risk of \emph{X}.

\code{ES} for the normal case is based on the following expression:
  
  \deqn{
  ES_{\alpha} = \mu + \sigma \frac{\phi(\Phi^{-1}(\alpha))}{1-\alpha}
  }{
  ES_{\alpha} = \mu + \sigma [\phi(\Phi^-1 (\alpha))] / (1-\alpha)
  }
  
  Meanwhile, \code{ES} for the t-student distribution takes comes from:
  
  \deqn{
  ES_{\alpha}(\tilde{X}) = \frac{g_{\upsilon}(t_{\upsilon}^{-1}(\alpha))}{1-\alpha} \left( \frac{\upsilon+(t_{\upsilon}^{-1}(\alpha))^{2}}{\upsilon - 1} \right)
  }{
    ES_\alpha \tilde{X} = g_\upsilon(t_\upsilon^-1(\alpha)) /(1-\alpha) (\upsilon+(t_\upsilon^-1 (\alpha))^2 / (\upsilon - 1))
  }
}



\value{
A \code{data.frame} containing the ES for each significance level specified.
}
\references{
Dhaene J., Tsanakas A., Valdez E. and Vanduffel S. (2011). \emph{Optimal Capital Allocation Principles}. The Journal of Risk and Insurance. Vol. 00, No. 0, 1-28.

McNeil, A. J.; Frey, R. & Embrechts, P. \emph{Quantitative risk management: concepts, techniques and tools}. Princeton University Press, 2005


Urbina, J. (2013) \emph{Quantifying Optimal Capital Allocation Principles based on Risk Measures.} Master Thesis, Universitat Politècnica de Catalunya.

Urbina, J. and Guillén, M. (2014). \emph{An application of capital allocation principles to operational risk and the cost of fraud}. Expert Systems with Applications. 41(16):7023-7031.

}
\author{
 Jilber Urbina
}


\seealso{
 \code{\link{VaR}},  \code{\link{Risk}}
}

\examples{
# Exercise 2.21, page 46 in McNeil et al (2005) 
alpha <- c(.90, .95, .975, .99, .995) 
(ES(Loss=1, varcov=(0.2/sqrt(250))^2, alpha=alpha, model='normal')-1)*10000
(ES(Loss=1, varcov=(0.2/sqrt(250))^2, alpha=alpha, model='t-student', df=4)-1)*10000

# Both type of models at once.
(ES(Loss=1, varcov=(0.2/sqrt(250))^2, alpha=alpha, model='both', df=4)-1)*10000

# A vector of losses
L <- c(10,40)                           # a vector of two (mean) losses
varcov <- matrix(c(100,150,150,900), 2) # covariance matrix
w <- c(0.5, 0.5)                        # a vector weights
ES(Loss=L, varcov=varcov, weights=w, alpha=0.95) 
}





\keyword{manip}
