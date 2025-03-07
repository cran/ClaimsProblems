#' @title Dominguez-Thomson rule
#' @description This function returns the awards vector assigned by the Dominguez-Thomson rule (DT) to a claims problem.
#' @param E The endowment.
#' @param d The vector of claims.
#' @param name A logical value.
#' @return The awards vector selected by the DT rule. If \code{name = TRUE}, the name of the function (DT) as a character string.
#' @details Let \eqn{N=\{1,\ldots,n\}} be the set of claimants, \eqn{E\ge 0} the endowment to be divided and \eqn{d\in \mathbb{R}_+^N} the vector of claims
#' such that \eqn{\sum_{i \in N} d_i\ge E}.
#'
#' The truncated claim of claimant \eqn{i\in N} in \eqn{(E,d)} is the minimum of the claim and the endowment:
#' \deqn{t_i(E,d)=\min\{d_i,E\},\ i=1,\dots,n.}
#' Let \eqn{t(E,d)=(t_1(E,d),\dots,t_n(E,d))}{t(E,d)=(t1,\dots,tn)} be the vector of truncated claims and \eqn{b(E,d)=\frac{1}{n}t(E,d)}.
#'
#' Let \eqn{(E^1,d^1)=(E,d)}. For each \eqn{k\ge 2} define:
#'  \deqn{(E^k,d^k)=\Bigl(E^{k-1}-\sum_{i=1}^n b_i(E^{k-1},d^{k-1}),d^{k-1}-b(E^{k-1},d^{k-1})\Bigr).}
#'
#' In step 1, the endowment is \eqn{E} and the claims vector is \eqn{d}.
#' For \eqn{k \ge 2}, the endowment is the remainder once all the claimants have received the amount of the previous steps and the new vector of claims is readjusted accordingly.
#' Observe that neither the endowment nor each agent's claim ever increases from one step to the next.
#' This recursive process exhausts \eqn{E} in the limit.
#'
#'  For each \eqn{(E,d)}, the Dominguez-Thomson rule (DT) assigns the awards vector:
#' \deqn{\text{DT}(E,d)=\sum_{k=1}^{\infty} b(E^k,d^k).}
#' @seealso \link{allrules}.
#' @examples
#' E=10
#' d=c(2,4,7,8)
#' DT(E,d)
#' @references Domínguez, D. and Thomson, W. (2006). A new solution to the problem of adjudicating conflicting claims. Economic Theory 28(2), 283-307.
#' @references Thomson, W. (2019). How to divide when there isn't enough. From Aristotle, the Talmud, and Maimonides to the axiomatics of resource allocation. Cambridge University Press.
#' @export

DT = function(E, d, name = FALSE) {
  if (name == TRUE) {
    rule = "DT"
    return(rule)
  }
  ########################################
  # Required: (E,d) must be a claims problem, i.e., E >=0, d >=0, E <= sum(d)
  ########################################
  n = length(d)
  D = sum(d) #The number of claims and the total claim
  if (E < 0 || sum((d < 0)) > 0 || E > D)
    stop('(E,d) is not a claims problem.',call.=F)


  ###TRIVIAL cases#######
  if (E == 0) {
    rule = rep(0, n)
    return(rule)
  } else if (E == D) {
    rule = d
    return(rule)
  }
  ###### THE DOMINGUEZ - THOMSON RULE #####
  rule = rep(0, n)

  noncero = which(d > 0)
  t = rep(0, n)
  m = max(which(d == min(d[noncero])))
  while (E > d[m]) {
    #Truncated claims
    for (i in 1:n) {
      t[i] = min(d[i], E) / n
    }
    rule = rule + t
    d = d - t
    E = E - sum(t)
  }
  rule[noncero] = rule[noncero] + E / length(noncero)

  return(rule)
}
