#' @title Talmud rule
#' @description This function returns the awards vector assigned by the Talmud rule to a claims problem.
#' @param E The endowment.
#' @param d The vector of claims.
#' @param name A logical value.
#' @return The awards vector selected by the Talmud rule.
#' If name = TRUE, the name of the function (Talmud) as a character string.
#' @details Let \eqn{E\ge 0} be the endowment to be divided and \eqn{d\in \mathcal{R}^n}{d} the vector of claims
#' with \eqn{d\ge 0} and such that \eqn{D=\sum_{i=1}^{n} d_i\ge E}{D=\sum di \ge E}, the sum of claims \eqn{D} exceeds the endowment.
#'
#' The Talmud rule coincides with the constrained equal awards rule (CEA)
#' applied to the problem \eqn{(E, d/2)} if the endowment is less or equal than the half-sum of the claims, \eqn{D/2}.
#' Otherwise, the Talmud rule assigns \eqn{d/2} and
#' the remainder, \eqn{E-D/2}, is awarded with the constrained equal losses rule with claims \eqn{d/2}. Therefore:
#'
#' If \eqn{E \le \frac{D}{2}}{E\le D/2} then:
#' \deqn{Talmud(E,d) = CEA(E,d/2).}{Talmud(E,d)=CEA(E,d/2).}
#'
#' If \eqn{E \ge \frac{D}{2}}{E\ge D/2} then:
#' \deqn{Talmud(E,d) =d/2+ CEL(E-D/2,d/2) = d-CEA(D-E,d/2).}{Talmud(E,d) =d/2+ CEL(E-D/2,d/2) = d-CEA(D-E,d/2).}
#'
#' The Talmud rule when applied to a two-claimant problem is often referred to as the contested garment rule and coincides with concede-and-divide rule.
#' The Talmud rule corresponds to the nucleolus of the associated (pessimistic) coalitional game.
#' @seealso \link{allrules}, \link{CEA}, \link{CEL}, \link{AA}, \link{APRO}, \link{RA}, \link{CD}.
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