% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CE.R
\name{CE}
\alias{CE}
\title{Constrained egalitarian rule}
\usage{
CE(E, d, name = FALSE)
}
\arguments{
\item{E}{The endowment.}

\item{d}{The vector of claims.}

\item{name}{A logical value.}
}
\value{
The awards vector selected by the CE rule. If name = TRUE, the name of the function (CE) as a character string.
}
\description{
This function returns  the awards vector assigned by the constrained egalitarian rule (CE) rule to a claims problem.
}
\details{
Let \eqn{E\ge 0} be the endowment to be divided and \eqn{d\in \mathcal{R}^n}{d} the vector of claims
with \eqn{d\ge 0} and such that \eqn{\sum_{i=1}^{n} d_i\ge E,\;}{} the sum of claims exceeds the endowment.

Assume that the claims are ordered from small to large, \eqn{0 \le d_1 \le...\le d_n}{%
0 \le d1 \le...\le dn}.
The constrained egalitarian rule coincides with the constrained equal awards rule (CEA) applied to
the problem \eqn{(E, d/2)} if the endowment is less or equal than the half-sum of the claims \eqn{D/2}.
Otherwise, any additional unit is assigned to claimant \eqn{1} until she/he receives the minimum
of the claim and half of \eqn{d_2}{%
d2}. If this minimun is \eqn{d_1}{%
d1}, she/he stops there. If it is not, the next increment is
divided equally between claimants \eqn{1} and \eqn{2} until claimant \eqn{1} receives \eqn{d_1}{%
d1} (in this case she drops out) or they reach \eqn{d_3/2}{%
d3/2}.
If claimant \eqn{1} leaves, claimant \eqn{2} receives any aditional increment until she/he reaches \eqn{d_2}{%
d2} or \eqn{d_3/2}{%
d3/2}. In the case that claimant \eqn{1} and \eqn{2} reach \eqn{d_3/2}{%
d3/2}, any additional unit is divided between claimants \eqn{1}, \eqn{2}, and \eqn{3} until the first one receives \eqn{d_1}{%
d1} or they reach \eqn{d_4/2}{%
d4/2}, and so on.

Therefore:

If \eqn{E \le D/2} then  \eqn{CE(E,d) = CEA(E,d/2)=(\min\{\frac{d_i}{2},\lambda\})_{i\in N}}{CE(E,d)=CEA(E,d/2)=(min\{di/2,\lambda\})} where \eqn{\lambda \ge 0} is chosen so as to achieve balance.

If \eqn{E \ge D/2} then the CE rule assigns to claimant \eqn{i} the maximum of two quantities: the half-claim and the minimum of the claim and a value \eqn{\lambda \ge 0}
 chosen so as to achieve balance.
\deqn{CE_i(E,d)=\max\{\frac{d_i}{2},\min\{d_i,\lambda\}\},\ i=1,\dots,n, \ where \ \sum_{i=1}^{n}	CE_i(E,d)=E.}{CE(E,d) = (max\{ di/2 , min\{di,\lambda\} \}).}
}
\examples{
E=10
d=c(2,4,7,8)
CE(E,d)
}
\references{
Chun, Y., Schummer, J., Thomson, W. (2001). Constrained egalitarianism: a new solution for claims problems. Seoul J. Economics 14, 269–297.

Thomson, W. (2019). How to divide when there isn't enough. From Aristotle, the Talmud, and Maimonides to the axiomatics of resource allocation. Cambridge University Press.
}
\seealso{
\link{allrules}, \link{CEA}, \link{Talmud}, \link{PIN}
}
