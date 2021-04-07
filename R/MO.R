#' @title Minimal overlap rule
#' @description This function returns the awards vector assigned by the minimal overlap rule rule (MO) to a claims problem.
#' @param E The endowment.
#' @param d The vector of claims.
#' @param name A logical value.
#' @return The awards vector selected by the MO rule. If name = TRUE, the name of the function (MO) as a character string.
#' @details Let \eqn{E\ge 0} be the endowment to be divided and \eqn{d\in \mathcal{R}^n}{d} the vector of claims
#' with \eqn{d\ge 0} and such that \eqn{\sum_{i=1}^{n} d_i\ge E,\;}{} the sum of claims exceeds the endowment.
#'
#' The truncated claim of a claimant \eqn{i} is the minimum of the claim and the endowment:
#' \deqn{t_i(E,d)=t_i=\min\{d_i,E\},\ i=1,\dots,n}{ti=min\{di,E\}, i=1,\dots,n}
#'
#' Suppose that each agent claims specific parts of E equal to her/his
#' claim. After arranging which parts agents claim so as to “minimize
#' conflict”, equal division prevails among all agents claiming a
#' specific part and each agent receives the sum of the compensations
#' she/he gets from the various parts that he claimed.
#'
#' Let \eqn{d_0=0}{d0=0}. The minimal overlap rule is defined, for each problem \eqn{(E,d)} and each claimant \eqn{i}, as:
#'
#' If \eqn{E\le d_n} then
#' \deqn{MO_i(E,d)=\frac{t_1}{n}+\frac{t_2-t_1}{n-1}+\dots+\frac{t_i-t_{i-1}}{n-i+1}.}{%
#' MOi(E,d) = t1/n + (t2-t1)/(n-1) + \dots + (ti-t(i-1))/(n-i+1).}
#'
#' If \eqn{E>d_n} let \eqn{s\in (d_k,d_{k+1}]}{dk<s \le d(k+1)}, with \eqn{k\in \{0,1,\dots,n-2\}}{0\le k \le n-2},
#' be the unique solution to the equation \eqn{\sum_{i \in N} \max\{d_i-s,0\} =E-s}{max\{d1-s,0\}+\dots+max\{dn-s,0\}=E-s}. Then:
#' \deqn{MO_i(E,d)=\frac{d_1}{n}+\frac{d_2-d_1}{n-1}+\dots+\frac{d_i-d_{i-1}}{n-i+1}, \  i\in\{1,\dots,k\}}{%
#' MOi(E,d) = d1/n + (d2-d1)/(n-1) + \dots + (di-d(i-1))/(n-i+1), if i\le k}
#' \deqn{MO_i(E,d)=MO_i(s,d)+d_i-s, \  i\in\{k+1,\dots,n\}.}{MOi(E,d) = MOi(s,d)+di-s,  if  i>k+1.}
#' @seealso \link{allrules}, \link{CD}.
#' @examples
#' E=10
#' d=c(2,4,7,9)
#' MO(E,d)
#' @references Mirás Calvo, M.A., Núñez Lugilde, I., Quinteiro Sandomingo, C., and Sánchez-Rodríguez, E. (2021). The adjusted proportional and the minimal overlap rules restricted to the lower-half, higher-half, and middle domains. Working paper 2021-02, ECOBAS.
#' @references O'Neill, B. (1982). A problem of rights arbitration from the Talmud. Math. Social Sci. 2, 345-371.
#' @references Thomson, W. (2019). How to divide when there isn't enough. From Aristotle, the Talmud, and Maimonides to the axiomatics of resource allocation. Cambridge University Press.
#' @export

 MO = function(E, d,name = FALSE) {
  if (name == TRUE) {
    rule = "MO"
    return(rule)
  }
  ########################################
  # Required: (E,d) must be a claims problem, i.e., E >=0, d >=0, E <= sum(d)
  ########################################
  n = length (d)
  D = sum(d) #The number of claims and the total claim
    if (E < 0 || sum((d < 0)) > 0 || E > D)
    stop('(E,d) is not a claims problem.',call.=F)

  #####################
  # Claims in ascending order
  do = sort(d, index.return = T)$x
  ordenI = sort(d, index.return = T)$ix
  orden = sort(ordenI, index.return = T)$ix
  dnull = sum(do == 0)

  ########################
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
  ############## THE MINIMAL OVERLAP RULE ###############
  n = length(do)
  rule = rep(0, n)

  if (sum(do >= E) == n) {
    #All of the claims are bigger than E
    rule = rep(1, n) * E / n
    sol = E
  } else if (sum(do >= E) >= 1) {
    #Unless one claimant is bigger than E
    j = sum(do > E)
    kestr = n - j
    #First claimant with d>E
    sol = do[kestr]
    rule[1] = do[1] / n
    #First claimant
    if (kestr > 1) {
      for (i in 2:kestr) {
        rule[i] = rule[i - 1] + (do[i] - do[i - 1]) / (n - i + 1)
        #Awards for claimants with d<E
      }
    }
    if (kestr < n) {
      for (i in (kestr + 1):n) {
        rule[i] = rule[kestr] + (E - do[kestr]) / (n - kestr)
        #Awards for claimants with d>E
      }
    }
  } else {
    #All the claims are smaller than E
    t = rep(0, n)

    for (j in 0:(n - 1)) {
      t[j + 1] = (sum(do[(j + 1):n]) - E) / (n - (j + 1))
    }
    t = max(t)
    sol = t
    kestr = n - sum(do > t)
    #We calculate the player k
    if (kestr == 0) {
      rule = ((do - t) + t / n) * rep(1, n)
    } else {
      if (kestr > 1) {
        rule[1] = do[1] / n

        for (i in 2:kestr) {
          #Awards for players smaller than k
          rule[i] = rule[i - 1] + (do[i] - do[i - 1]) / (n - i + 1)
        }
      }
      if (kestr < n) {
        rule[1] = do[1] / n

        for (i in (kestr + 1):n) {
          #Awards for players bigger than k
          rule[i] = rule[kestr] + (do[i] - t) + (t - do[kestr]) / (n - kestr)

        }
      }
    }
  }
  ####### ADDING the null claimants and REORDERING #####
  rule = c(ruleNull, rule)
  #Adding the null claimants
  rule = rule[orden]#Reordering the claimants
  return(rule)
}
