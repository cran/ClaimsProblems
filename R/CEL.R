#' @title Constrained equal losses rule
#' @description This function returns the awards vector assigned by the constrained equal losses rule (CEL) to a claims problem.
#' @param E The endowment.
#' @param d The vector of claims.
#' @param name A logical value.
#' @return The awards vector selected by the CEL rule. If name = TRUE, the name of the function (CEL) as a character string.
#' @details Let \eqn{E\ge 0} be the endowment to be divided and let \eqn{d\in \mathcal{R}^n}{d} be the vector of claims
#' with \eqn{d\ge 0} and such that \eqn{\sum_{i=1}^{n} d_i\ge E,}{} the sum of claims exceeds the endowment.
#'
#' The constrained equal losses rule (CEL) equalizes losses under the constraint that no award
#' is negative. Then, claimant \eqn{i} receives the maximum of zero and the claim minus a number \eqn{\lambda \ge 0} chosen so as to achieve balance.
#' \deqn{CEL_i(E,d)=\max\{0,d_i-\lambda\},\ i=1,\dots,n, \ such \  that  \ \sum_{i=1}^n CEL_i(E,d)=E.}{CEL(E,d)=(max\{0,di-\lambda\}).}
#'
#' CEA and CEL are dual rules.
#' @seealso \link{allrules}, \link{CEA}
#' @examples
#' E=10
#' d=c(2,4,7,8)
#' CEL(E,d)
#' # CEL and CEA are dual: CEL(E,d)=d-CEA(D-E,d)
#' D=sum(d)
#' d-CEA(D-E,d)
#' @references Maimonides, Moses, 1135-1204. Book of Judgements, Moznaim Publishing Corporation, New York, Jerusalem (Translated by Rabbi Elihahu Touger, 2000).
#' @references  Thomson, W. (2019). How to divide when there isn't enough. From Aristotle, the Talmud, and Maimonides to the axiomatics of resource allocation. Cambridge University Press.
#' @export

CEL = function(E, d, name = FALSE) {
  if (name == TRUE) {
    rule = "CEL"
    return(rule)
  }
  ########################################
  # Required: (E,d) must be a claims problem, i.e., E >=0, d >=0, E <= sum(d)
  ########################################
  n = length(d)
  D = sum(d) #The number of claims and the total claim
  if (E < 0 || sum((d < 0)) > 0 || E > D)
    stop('(E,d) is not a claims problem.',call.=F)

  ############## THE CEL RULE ###############
  rule = d - CEA(D - E, d)
  return(rule)
}
