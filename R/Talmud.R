#' @title Talmud rule
#' @description This function returns the awards vector assigned by the Talmud rule to a claims problem.
#' @param E The endowment.
#' @param d The vector of claims.
#' @param name A logical value.
#' @return The awards vector selected by the Talmud rule.
#' If \code{name = TRUE}, the name of the function (Talmud) as a character string.
#' @details Let \eqn{N=\{1,\ldots,n\}} be the set of claimants, \eqn{E\ge 0} the endowment to be divided and \eqn{d\in \mathbb{R}_+^N} the vector of claims
#' such that \eqn{D=\sum_{i \in N} d_i\ge E}.
#'
#' The Talmud rule (Talmud) coincides with the constrained equal awards rule (CEA)
#' applied to the problem \eqn{(E, d/2)} if the endowment is less or equal than the half-sum of the claims, \eqn{D/2}.
#' Otherwise, the Talmud rule assigns \eqn{d/2} and
#' the remainder, \eqn{E-D/2}, is awarded with the constrained equal losses rule with claims \eqn{d/2}. Therefore, for each \eqn{i\in N},
#'
#' \deqn{\text{Talmud}_i(E,d) = \begin{cases}
#' \min\{\frac{d_i}{2},\lambda\}   & \text{if }  E\leq \tfrac{1}{2}D\\[3pt]
#' d_i-\min\{\frac{d_i}{2},\lambda\} & \text{if }  E \geq \tfrac{1}{2}D
#' \end{cases},}
#'
#' where \eqn{\lambda \geq 0} is chosen such that  \eqn{\underset{i\in N}{\sum}	\text{Talmud}_i(E,d)=E}.
#'
#' The Talmud rule when applied to a two-claimant problem is often referred to as the contested garment rule and coincides with concede-and-divide rule.
#' The Talmud rule corresponds to the nucleolus of the associated (pessimistic) coalitional game.
#' @seealso \link{AA}, \link{allrules}, \link{APRO}, \link{axioms}, \link{CEA}, \link{CEL}, \link{CD}, \link{RA}, \link{RTalmud}.
#' @examples
#' E=10
#' d=c(2,4,7,8)
#' Talmud(E,d)
#' D=sum(d)
#' #The Talmud rule is self-dual
#' d-Talmud(D-E,d)
#' @references Aumann, R. and Maschler, M. (1985). Game theoretic analysis of a bankruptcy problem from the Talmud. Journal of Economic Theory 36, 195-213.
#' @references Thomson, W. (2019). How to divide when there isn't enough. From Aristotle, the Talmud, and Maimonides to the axiomatics of resource allocation. Cambridge University Press.
#' @export

Talmud = function(E, d, name = FALSE) {
  if (name == TRUE) {
    rule = "Talmud"
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
    rule = CEA(E, d / 2)
  } else {
    rule = d - CEA(D - E, d / 2)
  }
  return(rule)
}
