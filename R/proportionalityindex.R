#' @title Proportionality deviation index
#' @description This function returns the proportionality deviation index of a rule for a claims problem.
#' @param E The endowment.
#' @param d The vector of claims.
#' @param Rule A rule: AA, APRO, CE, CEA, CEL, DT, MO, PIN, PRO, RA, Talmud.
#' @return The proportionality deviation index of a rule for a claims problem.
#' @details Let \eqn{E> 0} be the endowment to be divided and \eqn{d\in \mathcal{R}^n}{d} the vector of claims
#' with \eqn{d\ge 0} and such that \eqn{D=\sum_{i=1}^{n} d_i\ge E}{D=\sum di \ge E}, the sum of claims \eqn{D} exceeds the endowment.
#'
#' Rearrange the claims from small to large, \eqn{0 \le d_1 \le...\le d_n}{%
#' 0 \le d1 \le...\le dn}.
#' The proportionality deviation index of the rule \eqn{R} for the problem \eqn{(E,d)}, denoted by \eqn{I(R,E,d)}, is
#' the ratio of the area that lies between the identity line and the cumulative claims-awards curve over the total area under the identity line.
#'
#' Let \eqn{d_0=0}{d0=0} and \eqn{R_0(E,d)=0}{R0(E,d)=0}. For each \eqn{k=1,\dots,n} define
#' \eqn{X_k=\frac{1}{D} \sum_{j=0}^{k} d_j}{Xk=(d0+\dots+dk)/D} and
#' \eqn{Y_k=\frac{1}{E} \sum_{j=0}^{k} R_j(E,d)}{Yk=(R0+\dots+Rk)/E}. Then
#' \deqn{I(R,E,d)=1-\sum_{k=1}^{n}(X_{k}-X_{k-1})(Y_{k}+Y_{k-1}).}{I(R,E,d)=1-\sum (Xk-X(k-1))(Yk+Y(k-1)) where the sum goes from k=1 to n.}
#'
#' The proportionality deviation index of the proportional rule is zero for all claims problems.
#' In general  \eqn{-1 \le I(R,E,d) \le 1}.
#'
#' @seealso \link{indexpath}, \link{cumulativecurve}, \link{lorenzcurve}, \link{giniindex}, \link{lorenzdominance}, \link{PRO}.
#' @examples
#' E=10
#' d=c(2,4,7,8)
#' Rule=AA
#' proportionalityindex(E,d,Rule)
#' #The proportionality deviation index of the proportional rule is 0
#' proportionalityindex(E,d,PRO)
#' @references  Ceriani, L. and Verme, P. (2012). The origins of the Gini index: extracts from Variabilitá e Mutabilitá (1912) by Corrado Gini. The Journal of Economic Inequality, 10(3), 421-443.
#' @references Mirás Calvo, M.A., Núñez Lugilde, I., Quinteiro Sandomingo, C., and Sánchez-Rodríguez, E. (2021). Deviation from proportionality and Lorenz-dominance between
#' the average of awards and the standard rules for claims problems. Working paper 2021-01, ECOBAS.
#' @export

proportionalityindex = function(E, d, Rule) {
  n = length(d)
  D = sum(d) #The number of claims and the sum of the claims

  ##############################################################
  # Required: (E,d) must be a claims problem, i.e., E >0, d >0,
  #E < sum(d) and the claims vector must be in increasing order
  ##############################################################
  do = sort(d)
  if (sum((d < 0)) > 0)
    stop('d is not a claims vector.',call.=F)


  if (E < 0 || E > D) {
    stop('(E,d) is not a claims problem.',call.=F)
  } else if (E == 0) {
    #claims index for the rule is zero
    stop('We can not compute the proportionality index if E=0.',call.=F)
  } else {
    #claims index for the rule and for the claims
    rule = Rule(E, do)
    claimsawards = 1-1/D*(sum(do*rule)/E+2/E* sum(rule*(D-cumsum(do))))
    if (sum(do == d) < n)
      message('The proportionality deviation index is computed by rearranging the claims in increasing order.\n')
    return(claims_awards = claimsawards)
  }

}
