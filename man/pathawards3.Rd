% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pathawards3.R
\name{pathawards3}
\alias{pathawards3}
\title{The path of awards for three claimants}
\usage{
pathawards3(d, claimants, Rule, col = "red", points = 300)
}
\arguments{
\item{d}{The vector of claims.}

\item{claimants}{Three claimants.}

\item{Rule}{The rule: AA, APRO, CE, CEA, CEL, DT, MO, PIN, PRO, RA, Talmud.}

\item{col}{The colour of the path, by default, col="red".}

\item{points}{The number of values of the endowment to draw the path.}
}
\value{
The graphical representation of the path of awards of a rule for the given claims and three claimants.
}
\description{
This function returns the graphical representation of the path of awards of any rule for a claims vector and three claimants.
}
\details{
Let \eqn{d\in \mathcal{R}^n}{d}, with \eqn{d\ge 0}, be a vector of claims and
 denote  \eqn{D=\sum_{i=1}^{n} d_i}{D=\sum di} the sum of claims.

The path of awards of a rule \eqn{R} for three claimants \eqn{i}, \eqn{j}, and \eqn{k} is the parametric curve:
\deqn{p(E)=\{(R_i(E,d),R_j(E,d),R_k(E,d))\in \mathcal{R}^3:\;E\in[0,D]\}.}{p(E)=\{(Ri(E,d),Rj(E,d),Rk(E,d)): 0\le E \le D\}.}
}
\examples{
d=c(2,4,7,8)
claimants=c(1,3,4)
Rule=Talmud
pathawards3(d,claimants,Rule)
}
\seealso{
\link{pathawards}, \link{schedrule}, \link{schedrules}, \link{verticalruleplot}
}
