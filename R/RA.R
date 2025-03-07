#' @title Random arrival rule
#' @description This function returns the awards vector assigned by the random arrival rule (RA) to a claims problem.
#' @param E The endowment.
#' @param d The vector of claims.
#' @param name A logical value.
#' @return The awards vector selected by the RA rule. If \code{name = TRUE}, the name of the function (RA) as a character string.
#' @details Let \eqn{N=\{1,\ldots,n\}} be the set of claimants, \eqn{E\ge 0} the endowment to be divided and \eqn{d\in \mathbb{R}_+^N} the vector of claims
#' such that \eqn{\sum_{i \in N} d_i\ge E}. For each coalition \eqn{S\in 2^N}, \eqn{d(S)=\sum_{j\in S}d_j}.
#'
#' The random arrival rule (RA) considers all the possible arrivals of the claimants
#' and applies the principle ``first to arrive, first to be served".
#' Then, for each order, the corresponding marginal worth vector assigns to each claimant the minimum of her/his claim
#'  and what remains of the endowment. The rule averages all the marginal worth vectors  considering all the permutations of the elements of \eqn{N}.
#'
#' Let \eqn{\Pi^N} denote the set of permutations of the set of claimants \eqn{N} and \eqn{|\Pi^N|} its cardinality.
#' Given a permutation \eqn{\pi \in \Pi} and a claimant \eqn{i\in N} let
#' \eqn{\pi_{\le i}} be the set of claimants that precede \eqn{i} in the order \eqn{\pi},
#' that is, \eqn{\pi_{\le i}=\{ j \in N :\pi(j)<\pi(i) \}}.
#'
#' The random arrival rule assigns to each \eqn{(E,d)} and each \eqn{i\in N},
#'
#' \deqn{\text{RA}_i(E,d)=\frac{1}{|\Pi^N|}\sum_{\pi\in \Pi^N}\min\Bigl\{d_i,\max\{ 0,E-d(\pi_{\le i}) \}\Bigr\}.}
#'
#'
#' The random arrival rule corresponds to the Shapley value of the associated (pessimistic) coalitional game.
#'
#' This function is programmed following the algorithm of Le Creurer et al. (2022).
#' @seealso \link{AA}, \link{allrules}, \link{APRO}, \link{axioms}, \link{CD}, \link{setofawards}, \link{Talmud}.
#' @examples
#' E=10
#' d=c(2,4,7,8)
#' RA(E,d)
#' D=sum(d)
#' #The random arrival rule is self-dual: RA(E,d)= d-RA(D-E,d)
#' d-RA(D-E,d)
#' @references Le Creurer, I.J, Mirás Calvo, M. A., Núñez Lugilde, I., Quinteiro Sandomingo, C., and Sánchez Rodríguez, E. (2022). On the computation of the Shapley value and the random arrival rule.  Available at \url{https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4293746}.
#' @references O’Neill, B. (1982) A problem of rights arbitration from the Talmud. Mathematical Social Sciences 2, 345–371.
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
