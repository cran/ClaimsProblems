#' @title Average rule
#' @description This function returns the awards vector assigned by the average rule (AV) to a claims problem.
#' @param E The endowment.
#' @param d The vector of claims.
#' @param name A logical value.
#' @return The awards vector selected by the AV rule. If \code{name = TRUE}, the name of the function (AV) as a character string.
#' @details Let \eqn{N=\{1,\ldots,n\}} be the set of claimants, \eqn{E\ge 0} the endowment to be divided and \eqn{d\in \mathbb{R}_+^N} the vector of claims
#' such that \eqn{\sum_{i \in N} d_i\ge E}.
#'
#' The average rule (AV) is the average of constrained equal awards (CEA) and constrained equal losses (CEL) rules.
#' That is,
#' \deqn{ \text{AV}(E,d)=\frac{\text{CEA}(E,d)+\text{CEL}(E,d)}{2}.}
#'
#' @seealso  \link{allrules}, \link{axioms}, \link{CEA}, \link{CEL}, \link{Talmud}, \link{RTalmud}.
#' @examples
#' E=10
#' d=c(2,4,7,8)
#' AV(E,d)
#' @references Thomson, W. (2019). How to divide when there isn't enough. From Aristotle, the Talmud, and Maimonides to the axiomatics of resource allocation. Cambridge University Press.
#' @export

AV = function(E, d, name = FALSE) {
  if (name == TRUE) {
    rule = "AV"
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
  ############## The AV RULE ##########################
  rule <- (CEA(E, d) + CEL(E, d)) / 2
  return(rule)
}