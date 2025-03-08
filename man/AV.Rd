% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AV.R
\name{AV}
\alias{AV}
\title{Average rule}
\usage{
AV(E, d, name = FALSE)
}
\arguments{
\item{E}{The endowment.}

\item{d}{The vector of claims.}

\item{name}{A logical value.}
}
\value{
The awards vector selected by the AV rule. If \code{name = TRUE}, the name of the function (AV) as a character string.
}
\description{
This function returns the awards vector assigned by the average rule (AV) to a claims problem.
}
\details{
Let \eqn{N=\{1,\ldots,n\}} be the set of claimants, \eqn{E\ge 0} the endowment to be divided and \eqn{d\in \mathbb{R}_+^N} the vector of claims
such that \eqn{\sum_{i \in N} d_i\ge E}.

The average rule (AV) is the average of constrained equal awards (CEA) and constrained equal losses (CEL) rules.
That is,
\deqn{ \text{AV}(E,d)=\frac{\text{CEA}(E,d)+\text{CEL}(E,d)}{2}.}
}
\examples{
E=10
d=c(2,4,7,8)
AV(E,d)
}
\references{
Thomson, W. (2019). How to divide when there isn't enough. From Aristotle, the Talmud, and Maimonides to the axiomatics of resource allocation. Cambridge University Press.
}
\seealso{
\link{allrules}, \link{axioms}, \link{CEA}, \link{CEL}, \link{Talmud}, \link{RTalmud}.
}
