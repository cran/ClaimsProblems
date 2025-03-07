#' @title Reverse Talmud rule
#' @description This function returns the awards vector assigned by the reverse Talmud rule to a claims problem.
#' @param E The endowment.
#' @param d The vector of claims.
#' @param name A logical value.
#' @return The awards vector selected by the reverse Talmud rule.
#' If \code{name = TRUE}, the name of the function (RTalmud) as a character string.
#' @details Let \eqn{N=\{1,\ldots,n\}} be the set of claimants, \eqn{E\ge 0} the endowment to be divided and \eqn{d\in \mathbb{R}_+^N} the vector of claims
#' such that \eqn{D=\sum_{i \in N} d_i\ge E}.
#'
#' The reverse Talmud rule (RTalmud) coincides with the constrained equal losses rule (CEL)
#' applied to the problem \eqn{(E, d/2)} if the endowment is less or equal than the half-sum of the claims, \eqn{D/2}.
#' Otherwise, the reverse Talmud rule assigns \eqn{d/2} and
#' the remainder, \eqn{E-D/2}, is awarded with the constrained equal awards rule with claims \eqn{d/2}. Therefore, for each \eqn{i\in N},
#'
#' \deqn{\text{RTalmud}_i(E,d) = \begin{cases}
#' \max\{\frac{d_i}{2}-\lambda,0\}   & \text{if }  E\leq \tfrac{1}{2}D\\[3pt]
#' \frac{d_i}{2}+\min\{\frac{d_i}{2},\lambda\} & \text{if }  E \geq \tfrac{1}{2}D
#' \end{cases},}
#'
#' where \eqn{\lambda \geq 0} is chosen such that  \eqn{\underset{i\in N}{\sum}	\text{RTalmud}_i(E,d)=E}.
#'
#' @seealso \link{AA}, \link{allrules}, \link{APRO}, \link{Talmud}, \link{CEA}, \link{CEL}, \link{CD}, \link{RA}.
#' @examples
#' E=10
#' d=c(2,4,7,8)
#' RTalmud(E,d)
#' @references Chun, Y., Schummer, J., and Thomson, W. (2001). Constrained egalitarianism: a new solution for claims problems. Seoul Journal of Economics 14, 269-297.
#' @references Thomson, W. (2019). How to divide when there isn't enough. From Aristotle, the Talmud, and Maimonides to the axiomatics of resource allocation. Cambridge University Press.
#' @export

RTalmud = function(E, d, name = FALSE) {
  if (name == TRUE) {
    rule = "RTalmud"
    return(rule)
  }
  ########################################
  # Required: (E,d) must be a claims problem, i.e., E >=0, d >=0, E <= sum(d)
  ########################################
  n = length(d)
  D = sum(d) #The number of claims and the total claim
  if (E < 0 || sum((d < 0)) > 0 || E > D)
    stop('(E,d) is not a claims problem.',call.=F)

  ########## THE TALMUD RULE ##############
  if (E <= D / 2) {
    rule = CEL(E, d / 2)
  } else {
    rule = d - CEL(D - E, d / 2)
  }
  return(rule)
}
