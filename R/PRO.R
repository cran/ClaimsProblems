#' @title Proportional rule
#' @description This function returns the awards vector assigned by the proportional rule (PRO) to a claims problem.
#' @param E The endowment.
#' @param d The vector of claims.
#' @param name A logical value.
#' @return The awards vector selected by the PRO rule. If name = TRUE, the name of the function (PRO) as a character string.
#' @details Let \eqn{E\ge 0} be the endowment to be divided and \eqn{d\in \mathcal{R}^n}{d} the vector of claims
#' with \eqn{d\ge 0} and such that \eqn{D=\sum_{i=1}^{n} d_i\ge E}{D=\sum di \ge E}, the sum of claims \eqn{D} exceeds the endowment.
#'
#' The proportional rule distributes awards proportional to claims.
#' \deqn{PRO(E,d)=\frac{E}{D}d}{PRO(E,d)=(E/D)*d}
#' @seealso \link{allrules}, \link{APRO}
#' @examples
#' E=10
#' d=c(2,4,7,8)
#' PRO(E,d)
#' @references Aristotle, Ethics, Thompson, J.A.K., tr. 1985. Harmondsworth: Penguin.
#' @references Thomson, W. (2019). How to divide when there isn't enough. From Aristotle, the Talmud, and Maimonides to the axiomatics of resource allocation. Cambridge University Press.
#' @export
PRO = function(E, d, name = FALSE) {
  if (name == TRUE) {
    rule = "PRO"
    return(rule)
  }
  ########################################
  # Required: (E,d) must be a claims problem, i.e., E >=0, d >=0, E <= sum(d)
  ########################################
  n = length (d)
  D = sum(d) #The number of claims and the total claim
  if (E < 0 || sum((d < 0)) > 0 || E > D)
    stop('(E,d) is not a claims problem.',call.=F)


  ############## THE PROPORTIONAL RULE #########
  rule = d / D * E
  return(rule)
}
