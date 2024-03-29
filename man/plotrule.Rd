% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotrule.R
\name{plotrule}
\alias{plotrule}
\title{Plot of an awards vector}
\usage{
plotrule(E, d, Rule = NULL, awards = NULL, set = TRUE, col = "blue")
}
\arguments{
\item{E}{The endowment.}

\item{d}{The vector of claims.}

\item{Rule}{A rule: AA, APRO, CE, CEA, CEL, DT, MO, PIN, PRO, RA, Talmud.}

\item{awards}{An awards vector.}

\item{set}{A logical value.}

\item{col}{The colour.}
}
\value{
If set = TRUE, the function creates a new figure plotting both the set of awards vectors for the claims problem and the given awards vector.
Otherwise, it just adds to the current picture the point representing the given awards vector. The function only plots one awards vector at a time.

The awards vector can be introduced directly as a vector. Alternatively, we can provide a rule and then the awards vector to be plotted is the one selected by the rule for the claims problem.
Therefore, if Rule = NULL it plots the given awards vector.
Otherwise, it plots the awards vector selected by the given rule for the claims problem.
In order to plot two (or more) awards vectors, draw the first one with the option set = TRUE and add the others, one by one, with the option set = FALSE.
}
\description{
This function plots an awards vector in the set of awards vectors for a claims problem.
}
\details{
Let \eqn{E\ge 0} be the endowment to be divided and \eqn{d\in \mathcal{R}^n}{d} the vector of claims
with \eqn{d\ge 0} and such that \eqn{\sum_{i=1}^{n} d_i\ge E,\;}{} the sum of claims exceeds the endowment.

A vector \eqn{x=(x_1,\dots,x_n)}{x=(x1,...,xn)} is an awards vector for the claims problem \eqn{(E,d)} if \eqn{0\le x \le d}
and satisfies the balance requirement, that is, \eqn{\sum_{i=1}^{n}x_i=E}{x1+\dots+xn=E} the sum of its coordinates is equal to \eqn{E}.
Let \eqn{X(E,d)} be the set of awards vectors for the problem \eqn{(E,d)}.

A rule is a function that assigns to each claims problem \eqn{(E,d)} an awards vector for \eqn{(E,d)},
that is, a division between the claimants of the amount available.
}
\examples{
E=10
d=c(2,4,7,8)
plotrule(E,d,Rule=AA,col="red")
# Plotting the awards vector (1,3,5,1) and the AA rule
# First, plot the awards vector (1,3,5,1) and the set of awards
plotrule(E,d,awards=c(1,3,5,1),col="green")
# Second, add the AA rule with the option set=FALSE
plotrule(E,d,Rule=AA,set=FALSE,col="red")
}
\seealso{
\link{setofawards}, \link{allrules}
}
