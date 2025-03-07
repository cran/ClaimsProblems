#' @title Constrained equal losses rule
#' @description This function returns the awards vector assigned by the constrained equal losses rule (CEL) to a claims problem.
#' @param E The endowment.
#' @param d The vector of claims.
#' @param name A logical value.
#' @return The awards vector selected by the CEL rule. If \code{name = TRUE}, the name of the function (CEL) as a character string.
#' @details Let \eqn{N=\{1,\ldots,n\}} be the set of claimants, \eqn{E\ge 0} the endowment to be divided and \eqn{d\in \mathbb{R}_+^N} the vector of claims
#' such that \eqn{\sum_{i \in N} d_i\ge E}.
#'
#' The constrained equal losses rule (CEL) equalizes losses under the constraint that no award
#' is negative. Then, claimant \eqn{i} receives the maximum of zero and the claim minus a number \eqn{\lambda \ge 0} chosen so as to achieve balance. That is, for each \eqn{i\in N},
#' \deqn{\text{CEL}_i(E,d)=\max\{0,d_i-\lambda\},}
#'
#'  where \eqn{\lambda\geq 0} is chosen  such that \eqn{\sum_{i\in N} \text{CEL}_i(E,d)=E.}
#'
#' CEA and CEL are dual rules.
#' @seealso \link{allrules}, \link{axioms}, \link{CE}, \link{CEA}, \link{AV}, \link{PIN}, \link{Talmud}, \link{RTalmud}.
#' @examples
#' E=10
#' d=c(2,4,7,8)
#' CEL(E,d)
#' # CEL and CEA are dual: CEL(E,d)=d-CEA(D-E,d)
#' D=sum(d)
#' d-CEA(D-E,d)
#' @references Maimonides, Moses, [1135-1204], Book of Judgements (translated by Rabbi Elihahu Touger, 2000), New York and Jerusalem: Moznaim Publishing Corporation, 2000.
#' @references Thomson, W. (2019). How to divide when there isn't enough. From Aristotle, the Talmud, and Maimonides to the axiomatics of resource allocation. Cambridge University Press.
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
