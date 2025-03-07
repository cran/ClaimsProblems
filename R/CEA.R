#' @title Constrained equal awards rule
#' @description This function returns the awards vector assigned by the constrained equal awards rule (CEA) to a claims problem.
#' @param E The endowment.
#' @param d The vector of claims.
#' @param name A logical value.
#' @return The awards vector selected by the CEA rule. If \code{name = TRUE}, the name of the function (CEA) as a character string.
#' @details Let \eqn{N=\{1,\ldots,n\}} be the set of claimants, \eqn{E\ge 0} the endowment to be divided and \eqn{d\in \mathbb{R}_+^N} the vector of claims
#' such that \eqn{\sum_{i \in N} d_i\ge E}.
#'
#' The constrained equal awards rule (CEA) equalizes awards under the constraint that no individual's
#' award exceeds his/her claim. Then, claimant \eqn{i} receives the minimum of the claim and a value \eqn{\lambda \ge 0} chosen so as to achieve balance. That is, for each \eqn{i\in N},
#' \deqn{ \text{CEA}_i(E,d)=\min\{d_i,\lambda\},}
#'
#'  where \eqn{\lambda\geq 0} is chosen  such that \eqn{\sum_{i\in N} \text{CEA}_i(E,d)=E.}
#'
#' The constrained equal awards rule corresponds to the Dutta-Ray solution to the associated (pessimistic) coalitional game.
#' The CEA and CEL rules are dual.
#' @seealso  \link{allrules}, \link{axioms}, \link{CE}, \link{CEL}, \link{AV}, \link{PIN}, \link{Talmud}, \link{RTalmud}.
#' @examples
#' E=10
#' d=c(2,4,7,8)
#' CEA(E,d)
#' # CEA and CEL are dual: CEA(E,d)=d-CEL(D-E,d)
#' D=sum(d)
#' d-CEL(D-E,d)
#' @references Maimonides, Moses, [1135-1204], Book of Judgements (translated by Rabbi Elihahu Touger, 2000), New York and Jerusalem: Moznaim Publishing Corporation, 2000.
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
