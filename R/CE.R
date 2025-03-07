#' @title Constrained egalitarian rule
#' @description This function returns  the awards vector assigned by the constrained egalitarian rule (CE) rule to a claims problem.
#' @param E The endowment.
#' @param d The vector of claims.
#' @param name A logical value.
#' @return The awards vector selected by the CE rule. If \code{name = TRUE}, the name of the function (CE) as a character string.
#' @details Let \eqn{N=\{1,\ldots,n\}} be the set of claimants, \eqn{E\ge 0} the endowment to be divided and \eqn{d\in \mathbb{R}_+^N} the vector of claims
#' such that \eqn{D=\sum_{i \in N} d_i\ge E}.
#'
#' Rearrange the claims from small to large, \eqn{0 \le d_1 \le...\le d_n}.
#' The constrained egalitarian rule (CE) coincides with the constrained equal awards rule (CEA) applied to
#' the problem \eqn{(E, d/2)} if the endowment is less or equal than the half-sum of the claims, \eqn{D/2}.
#' Otherwise, any additional unit is assigned to claimant \eqn{1} until she/he receives the minimum
#' of the claim and half of \eqn{d_2}. If this minimun is \eqn{d_1}, she/he stops there. If it is not, the
#' next increment is divided equally between claimants \eqn{1} and \eqn{2} until claimant \eqn{1} receives
#'  \eqn{d_1} (in this case she drops out) or they reach \eqn{d_3/2}.
#' If claimant \eqn{1} leaves, claimant \eqn{2} receives any additional increment until she/he reaches \eqn{d_2}
#' or \eqn{d_3/2}. In the case that claimant \eqn{1} and \eqn{2} reach \eqn{d_3/2}, any additional unit is
#' divided between claimants \eqn{1}, \eqn{2}, and \eqn{3} until the first one receives \eqn{d_1} or they
#' reach \eqn{d_4/2}, and so on. Therefore, for each \eqn{i\in N},
#'
#' \deqn{\text{CE}_i(E,d)=\begin{cases}
#' \min\{\frac{d_i}{2},\lambda\}     & \text{if } E\leq \tfrac{1}{2}D\\[3pt]
#' \max \bigl\{ \frac{d_i}{2},\min\{d_i,\lambda \} \bigr\} & \text{if }  E \geq \tfrac{1}{2}D
#' \end{cases},}
#'
#' where \eqn{\lambda \geq 0} is chosen such that  \eqn{\underset{i\in N}{\sum}	\text{CE}_i(E,d)=E}.
#'
#' @seealso \link{allrules}, \link{axioms}, \link{CEA}, \link{PIN}, \link{Talmud}.
#' @examples
#' E=10
#' d=c(2,4,7,8)
#' CE(E,d)
#' @references Chun, Y., Schummer, J., Thomson, W. (2001). Constrained egalitarianism: a new solution for claims problems. Seoul Journal of Economics 14, 269–297.
#' @references Thomson, W. (2019). How to divide when there isn't enough. From Aristotle, the Talmud, and Maimonides to the axiomatics of resource allocation. Cambridge University Press.
#' @export

CE = function(E, d, name = FALSE) {
  if (name == TRUE) {
    rule = "CE"
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

  ##################
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
  } else{
    ruleNull = c()
  }
  ############ THE CE RULE #########
  n = length(do)
  if (E <= D / 2) {
    #Lower-half domain
    rule = CEA(E, do / 2)

  } else{
    # Higher-half domain
    rule = do / 2
    # Each claimant gets at least di/2
    Ns = 0
    while (sum(rule) < E) {
      R = E - sum(rule)#Extra units
      pendientes1 = which(rule != do)#The claimants with award is not equal to the claim
      pendientes = which(rule[pendientes1] == min(rule[pendientes1])) +
        Ns #The first claimant
      #with award is not equal to the claim
      Requitativo = R / (length(pendientes))
      #The equitable quantity to give to each claimant
      for (j in (sort(pendientes, decreasing = TRUE))) {
        if (j < n) {
          rule[j] = min(min(do[j], rule[j] + Requitativo), rule[j + 1])#We give to each claimant the equitative award
        } else {
          rule[j] = min(do[j], rule[j] + Requitativo)
        }
      }
      saturados = as.numeric(do == rule)
      Ns = sum(saturados)
    }
  }

  #######ADDING the null claimants and REORDERING #####
  rule = c(ruleNull, rule)
  #Adding the null claimants
  rule = rule[orden]#Reordering the claimants
  return(rule)
}
