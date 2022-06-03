#' @title Adjusted proportional rule
#' @description This function returns the awards vector assigned by the adjusted proportional rule (APRO) to a claims problem.
#' @param E The endowment.
#' @param d The vector of claims.
#' @param name A logical value.
#' @return The awards vector selected by the APRO rule. If name = TRUE, the name of the function (APRO) as a character string.
#' @details Let \eqn{E\ge 0} be the endowment to be divided and \eqn{d\in \mathcal{R}^n}{%
#'  d} the vector of claims
#' with \eqn{d\ge 0} and such that \eqn{\sum_{i=1}^{n} d_i\ge E,}{%
#' } the sum of claims exceeds the endowment.
#'
#' For each subset \eqn{S} of the set of claimants \eqn{N}, let \eqn{d(S)=\sum_{j\in S}d_j}{d(S)} be the sum of claims of the members of \eqn{S}
#'  and let \eqn{N\backslash S}{N-S} be the complementary coalition of \eqn{S}.
#'
#' The minimal right of claimant \eqn{i} in \eqn{(E,d)} is whatever is left after every other claimant has received his claim, or 0 if that is not possible:
#' \deqn{m_i(E,d)=\max\{0,E-d(N\backslash\{i\})\},\ i=1,\dots,n.}{mi = max\{ 0 , E-d(N-\{i\}) \}, i=1,\dots,n.}
#' Let \eqn{m(E,d)=(m_1(E,d),\dots,m_n(E,d))}{m(E,d)=(m1,\dots,mn)} be the vector of minimal rights.
#'
#' The truncated claim of claimant \eqn{i} in \eqn{(E,d)} is the minimum of the claim and the endowment:
#' \deqn{t_i(E,d)=\min\{d_i,E\},\ i=1,\dots,n.}{ti = min\{di,E\}, i=1,\dots,n.}
#' Let \eqn{t(E,d)=(t_1(E,d),\dots,t_n(E,d))}{t(E,d)=(t1,\dots,tn)} be the vector of truncated claims.
#'
#' The adjusted proportional rule first gives to each claimant the minimal right, and then divides the remainder
#' of the endowment \eqn{E'=E-\sum_{i=1}^n m_i(E,d)}{%
#' }
#' proportionally with respect to the new claims. The vector of the new claims \eqn{d'} is determined by the minimum of the remainder and the lowered claims,
#' \eqn{d_i'=\min\{E-\sum_{j=1}^n m_j(E,d),d_i-m_i\},\  i=1,\dots,n}{d'i=min\{E-\sum mj,di-mi\}, i=1,\dots,n
#' }. Therefore:
#' \deqn{APRO(E,d)=m(E,d)+PRO(E',d').}{%
#' APRO(E,d)=m(E,d)+PRO(E-\sum mi,d').}
#'
#' The adjusted proportional rule corresponds to the \eqn{\tau}-value of the associated (pessimistic) coalitional game.
#' @seealso \link{allrules}, \link{CD}, \link{PRO}, \link{coalitionalgame}
#' @examples
#' E=10
#' d=c(2,4,7,8)
#' APRO(E,d)
#' #The adjusted proportional rule is self-dual: APRO(E,d)=d-APRO(D-E,d)
#' D=sum(d)
#' d-APRO(D-E,d)
#' @references Curiel, I. J., Maschler, M., and Tijs, S. H. (1987). Bankruptcy games. Zeitschrift für operations research, 31(5), A143-A159.
#' @references Mirás Calvo, M.Á., Núñez Lugilde, I., Quinteiro Sandomingo, C., and Sánchez-Rodríguez, E. (2021). The adjusted proportional and the minimal overlap rules restricted to the lower-half, higher-half, and middle domains. Working paper 2021-02, ECOBAS.
#' @references Thomson, W. (2019). How to divide when there isn't enough. From Aristotle, the Talmud, and Maimonides to the axiomatics of resource allocation. Cambridge University Press.
#' @export

APRO = function(E, d, name = FALSE) {
  if (name == TRUE) {
    rule = "APRO"
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

  ########################
  # Trivial cases:
  if (E == 0) {#Null endowment
    rule = rep(0, n)
    return(rule)
  } else if (E == D) { #Endowment equal to the sum of the claims
    rule = d
    return(rule)
  } else if (dnull > 0) { #Some claims (but not all) are zero
    do = do[(dnull + 1):n]
    ruleNull = rep(0, dnull)
  } else {
    ruleNull = c()
  }
  ########### ADJUSTED PROPORTIONAL RULE #############

  n = length(do)
  Ad = rep(0, n)
  m = E-D+do #Minimal rights
  m[m<0]=0 #Minimal rights
  AE = E - sum(m) #The remaining endowment
  for (i in 1:n) { #Adjusted claims
    Ad[i] = min(do[i] - m[i], E - sum(m))
  }
  rule = m + AE / sum(Ad) * Ad #Proportional rule adding the minimal rights

  ##### ADDING the null claimants and REORDERING #####
  rule = c(ruleNull, rule) #Adding the null claimants
  rule = rule[orden]#Reordering the claimants
  return(rule)
}
