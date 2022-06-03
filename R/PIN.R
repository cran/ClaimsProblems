#' @title Piniles' rule
#' @description This function returns the awards vector assigned by the Piniles' rule (PIN) to a claims problem.
#' @param E The endowment.
#' @param d The vector of claims.
#' @param name A logical value.
#' @return The awards vector selected by the PIN rule. If name = TRUE, the name of the function (PIN) as a character string.
#' @details Let \eqn{E\ge 0} be the endowment to be divided and \eqn{d\in \mathcal{R}^n}{d} the vector of claims
#' with \eqn{d\ge 0} and such that \eqn{D=\sum_{i=1}^{n} d_i\ge E}{D=\sum di \ge E}, the sum of claims \eqn{D} exceeds the endowment.
#'
#' The Piniles' rule coincides with the constrained equal awards rule (CEA) applied to
#' the problem \eqn{(E, d/2)} if the endowment is less or equal than the half-sum of the claims, \eqn{D/2}.
#' Otherwise it assigns to each claimant \eqn{i} half of the claim, \eqn{d_i/2}{di/2} and,
#' then, it distributes the remainder with the CEA rule. Therefore:
#'
#' If \eqn{E \le \frac{D}{2}}{E\le D/2} then,
#' \deqn{PIN(E,d)  = CEA(E,d/2).}{PIN(E,d)=CEA(E,d/2).}
#'
#' If \eqn{E \ge \frac{D}{2}}{E\ge D/2} then,
#' \deqn{PIN(E,d)=d/2+CEA(E-D/2,d/2).}{PIN(E,d)=d/2+CEA(E-D/2,d/2).}
#' @seealso \link{allrules}, \link{CEA}, \link{Talmud}
#' @examples
#' E=10
#' d=c(2,4,7,8)
#' PIN(E,d)
#' @references Piniles, H.M. (1861). Darkah shel Torah. Forester, Vienna.
#' @references Thomson, W. (2019). How to divide when there isn't enough. From Aristotle, the Talmud, and Maimonides to the axiomatics of resource allocation. Cambridge University Press.
#' @export

PIN = function(E, d, name = FALSE) {
  if (name == TRUE) {
    rule = "PIN"
    return(rule)
  }
  ########################################
  # Required: (E,d) must be a claims problem, i.e., E >=0, d >=0, E <= sum(d)
  ########################################
  n = length (d)
  D = sum(d) #The number of claims and the total claim
  if (E < 0 || sum((d < 0)) > 0 || E > D)
    stop('(E,d) is not a claims problem.',call.=F)


  ######### THE PINILES RULE ############
  if (D / 2 >= E) {
    rule = CEA(E, d / 2)

  } else {
    rule = d / 2 + CEA(E - D / 2, d / 2)

  }
  return(rule)
}
