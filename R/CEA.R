#' @title Constrained equal awards rule
#' @description This function returns the awards vector assigned by the constrained equal awards rule (CEA) to a claims problem.
#' @param E The endowment.
#' @param d The vector of claims.
#' @param name A logical value.
#' @return The awards vector selected by the CEA rule. If name = TRUE, the name of the function (CEA) as a character string.
#' @details Let \eqn{E\ge 0} be the endowment to be divided and let \eqn{d\in \mathcal{R}^n}{d} be the vector of claims
#' with \eqn{d\ge 0} and such that \eqn{\sum_{i=1}^{n} d_i\ge E,}{} the sum of claims exceeds the endowment.
#'
#' The constrained equal awards rule (CEA) equalizes awards under the constraint that no individual's
#' award exceeds his/her claim. Then, claimant \eqn{i} receives the minimum of the claim and a value \eqn{\lambda \ge 0} chosen so as to achieve balance.
#' \deqn{ CEA_i(E,d)=\min\{d_i,\lambda\},\ i=1,\dots,n, \ such \  that \ \sum_{i=1}^{n} CEA_i(E,d)=E.}{CEA(E,d)=(min\{di,\lambda\}).}
#'
#' The constrained equal awards rule corresponds to the Dutta-Ray solution to the associated (pessimistic) coalitional game.
#' The CEA and CEL rules are dual.
#' @seealso  \link{allrules}, \link{CE}, \link{CEL}, \link{PIN}, \link{Talmud}
#' @examples
#' E=10
#' d=c(2,4,7,8)
#' CEA(E,d)
#' # CEA and CEL are dual: CEA(E,d)=d-CEL(D-E,d)
#' D=sum(d)
#' d-CEL(D-E,d)
#' @references Maimonides, Moses, 1135-1204. Book of Judgements, Moznaim Publishing Corporation, New York, Jerusalem (Translated by Rabbi Elihahu Touger, 2000).
#' @references Thomson, W. (2019). How to divide when there isn't enough. From Aristotle, the Talmud, and Maimonides to the axiomatics of resource allocation. Cambridge University Press.
#' @export

CEA = function(E, d, name = FALSE) {
  if (name == TRUE) {
    rule = "CEA"
    return(rule)
  }
  ########################################
  # Required: (E,d) must be a claims problem, i.e., E >=0, d >=0, E <= sum(d)
  ########################################
  n = length(d)
  D = sum(d) #The number of claims and the total claim
  if (E < 0 || sum((d < 0)) > 0 || E > D)
    stop('(E,d) is not a claims problem.',call.=F)

  ###################
  # Claims in ascending order
  do = sort(d, index.return = T)$x
  ordenI = sort(d, index.return = T)$ix
  orden = sort(ordenI, index.return = T)$ix
  dnull = sum(do == 0)

  ####################
  # Trivial cases:
  if (E == 0) {
    #Null endowment
    rule = rep(0, length(d))

    return(rule)
  } else if (E == D) {
    # Endowment equal to the sum of the claims
    rule = d

    ruleNull = rep(0, dnull)

    return(rule)
  } else if (dnull > 0) {
    #Some claims (but not all) are zero
    do = do[(dnull + 1):n]

    ruleNull = rep(0, dnull)

  } else {
    ruleNull = c()

  }
  ############## The CEA RULE ##########################
  n = length(do)
  S = cumsum(do) - do
  # The accumulated claims S(1)=0; S(i)=d(1)+...+d(i-1) for i>1
  dM = seq(n, 1) * do # (n+1-i)*d(i), ii=1,...,n
  control = as.numeric(E < S + dM)
  # Check whether or not (E-(d(1)+...+d(i-1)))/(n+1-i)<di
  ii = which(control == 1)[1] # The first claim that satisfies the control condition
  ############## CEA computation ##############
  rule = as.numeric(ii == 1) * (E / n * rep(1, n)) + as.numeric(ii > 1) *
    (control * (E - S[ii]) / (n + 1 - ii) + (1 - control) * do)

  # if i=1,  CEA is the egalitarian rule
  #if i>1,  the claimants before i get its claim. (control=0)
  # the others get (E-(d(1)+...+d(i-1)))/(n+1-i)
  #######ADDING the null claimants and REORDERING #####
  rule = c(ruleNull, rule)
  #Adding the null claimants
  rule = rule[orden] #Reordering the claimants
  return(rule)
}
