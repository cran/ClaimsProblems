#' @title Random arrival rule
#' @description This function returns the awards vector assigned by the random arrival rule (RA) to a claims problem.
#' @param E The endowment.
#' @param d The vector of claims.
#' @param name A logical value.
#' @return The awards vector selected by the RA rule. If name = TRUE, the name of the function (RA) as a character string.
#' @details Let \eqn{E\ge 0} be the endowment to be divided and let \eqn{d\in \mathcal{R}^n}{d} be the vector of claims
#' with \eqn{d\ge 0} and such that \eqn{\sum_{i=1}^{n} d_i\ge E,\ }{} the sum of claims exceeds the endowment.
#'
#' For each subset \eqn{S} of the set of claimants \eqn{N}, let \eqn{d(S)=\sum_{j\in S}d_j}{d(S)} be the sum of claims of the members of \eqn{S}.
#'
#' The random arrival rule considers all the possible arrivals of the claimants
#' and applies the principle ``first to arrive, first to be served".
#' Then, for each order, the corresponding marginal worth vector assigns to each claimant the minimum of her/his claim
#'  and what remains of the endowment. The rule averages all the marginal worth vectors  considering all the permutations of the elements of \eqn{N}.
#'
#' Let \eqn{\Pi^N}{\Pi} denote the set of permutations of the set of claimants \eqn{N} and \eqn{|\Pi^N|}{|\Pi|} its cardinality.
#' Given a permutation \eqn{\pi \in \Pi}{\pi} and a claimant \eqn{i\in N}{i} let
#' \eqn{\pi_{\le i}}{\pi(\le i)} denote the set of claimants that precede \eqn{i} in the order \eqn{\pi},
#' that is, \eqn{\pi_{\le i}=\{ j \in N :\pi(j)<\pi(i) \}}{\pi(\le i)=\{j: \pi(j)<\pi(i)\}}.
#'
#' The random arrival rule assigns to each \eqn{(E,d)} and each \eqn{i}
#' the value:
#' \deqn{RA_i(E,d)=\frac{1}{|\Pi^N|}\sum_{\pi\in \Pi^N}\min\{d_i,\max\{ 0,E-d(\pi_{\le i}) \}\}, \  i=1,\dots,n}{%
#' RAi(E,d)=1/|\Pi| \sum min\{ di, max\{0,E-d(\pi(\le i))\} \}, where the sum is taken over the set of permutations \Pi.}
#'
#'
#' The random arrival rule corresponds to the Shapley value of the associated (pessimistic) coalitional game.
#' @seealso \link{allrules}, \link{setofawards}, \link{Talmud}, \link{AA}, \link{CD}, \link{APRO}
#' @examples
#' E=10
#' d=c(2,4,7,8)
#' RA(E,d)
#' D=sum(d)
#' #The random arrival rule is self-dual: RA(E,d)= d-RA(D-E,d)
#' d-RA(D-E,d)
#' @references O'Neill, B. (1982). A problem of rights arbitration from the Talmud. Math. Social Sci. 2, 345-371.
#' @references Thomson, W. (2019). How to divide when there isn't enough. From Aristotle, the Talmud, and Maimonides to the axiomatics of resource allocation. Cambridge University Press.
#' @export
RA = function(E, d, name = FALSE ) {
  if (name == TRUE) {
    rule = "RA"
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
  #Trivial cases:  Null endowment or all claims zero
  if (E == 0 | sum(as.numeric(d == 0)) == n) {
    rule = rep(0, n)
    return(rule)
  } else if (E == D) {
    rule = d
    return(rule)
  }

  ############# THE RA RULE ######################

  ########## AUXILARY VALUES ###########
  metade = ceiling(n / 2)
  valores = 1 / n * rep(1, metade)

  k = 1
  if (n > 2) {
    for (ii in 2:metade) {
      valores[ii] = valores[ii - 1] * (ii - 1) / (n - k)
      k = k + 1
    }
  }
  if (n %% 2 == 0) {
    mS = c(valores, sort(valores))
  } else {
    mS = c(valores, sort(valores[1:length(valores) - 1]))
  }
  #########
  sconxunta = 2 * E - D
  rule = rep(0, n)
  for (ii in 1:(2 ^ (n - 1) - 1)) {
    coalicions = as.numeric(intToBits(ii)[1:n])
    v1 = E - D + sum(d * coalicions)
    v2 = sconxunta - v1
    a = sum(coalicions)
    coalicions = coalicions - mS[a + 1]
    coalicions[coalicions == 1 - mS[a + 1]] = mS[a]
    rule = rule + (max(0, v1) - max(0, v2)) * coalicions

    if (sum(is.na(rule)) > 0) {
      return()
    }
  }
  rule = rule + E / n
  return(rule)
}
