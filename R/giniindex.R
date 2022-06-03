#' @title Gini index
#' @description  This function returns the Gini index of any rule for a claims problem.
#' @param E The endowment.
#' @param d The vector of claims.
#' @param Rule A rule: AA, APRO, CE, CEA, CEL, DT, MO, PIN, PRO, RA, Talmud.
#' @return The Gini index of a rule for a claims problem and the Gini index of the vector of claims.
#' @details Let \eqn{E> 0} be the endowment to be divided and \eqn{d\in \mathcal{R}^n}{d} the vector of claims
#' with \eqn{d\ge 0} and such that \eqn{D=\sum_{i=1}^{n} d_i\ge E}{D=\sum di \ge E}, the sum of claims \eqn{D} exceeds the endowment.
#'
#' Rearrange the claims from small to large, \eqn{0 \le d_1 \le...\le d_n}{%
#' 0 \le d1 \le...\le dn}. The Gini index is a number aimed at measuring the degree of inequality in a distribution.
#' The Gini index of the rule \eqn{R} for the problem \eqn{(E,d)}, denoted by \eqn{G(R,E,d)}, is
#' the ratio of the area that lies between the identity line and the Lorenz curve of the rule over the total area under the identity line.
#'
#' Let \eqn{R_0(E,d)=0}{R0(E,d)=0}. For each \eqn{k=0,\dots,n} define
#' \eqn{X_k=\frac{k}{n}}{Xk=k/n} and
#' \eqn{Y_k=\frac{1}{E} \sum_{j=0}^{k} R_j(E,d)}{Yk=(R0+\dots+Rk)/E}. Then
#' \deqn{G(R,E,d)=1-\sum_{k=1}^{n}(X_{k}-X_{k-1})(Y_{k}+Y_{k-1}).}{G(R,E,d)=1-\sum (Xk-X(k-1))(Yk+Y(k-1) where the sum goes from k=1 to n.}
#' In general \eqn{0\le G(R,E,d) \le 1}.
#' @seealso \link{lorenzcurve}, \link{cumawardscurve}, \link{deviationindex}, \link{indexgpath}, \link{lorenzdominance}.
#' @examples
#' E=10
#' d=c(2,4,7,8)
#' Rule=AA
#' giniindex(E,d,Rule)
#' # The Gini index of the proportional awards coincides with the Gini index of the vector of claims
#' giniindex(E,d,PRO)
#' @references  Ceriani, L. and Verme, P. (2012). The origins of the Gini index: extracts from Variabilitá e Mutabilitá (1912) by Corrado Gini. The Journal of Economic Inequality, 10(3), 421-443.
#' @references Mirás Calvo, M.Á., Núñez Lugilde, I., Quinteiro Sandomingo, C., and Sánchez Rodríguez, E. (2022). Deviation from proportionality and Lorenz-domination for claims problems. Rev Econ Design. \doi{10.1007/s10058-022-00300-y}
#' @export

giniindex = function(E, d, Rule) {
  n = length(d)
  D = sum(d) #The number of claims and the sum of the claims

  ##############################################################
  # Required: (E,d) must be a claims problem, i.e., E >0, d >0,
  #E < sum(d) and the claims vector must be in increasing order
  ##############################################################
  do = sort(d)
  if (sum((d < 0)) > 0)
    stop('d is not a claims vector.',call.=F)

  if (sum(do == d) < n)
    message('The Gini index is computed by rearranging the claims in increasing order.\n')
    giniclaims = 1 - 1 / n - (2 / n) * sum((n - (1:n)) * do / D)
    if (E < 0 || E > D) {
      warning('(E,d) is not a claims problem. \n')

      message('The Gini index for the claims vector:\n')
      return (Gini_claims = giniclaims)
    } else if (E == 0) {
      #Gini index for the rule is zero
      giniawards = 0
      return(list(Gini_awards = giniawards, Gini_claims = giniclaims))
    } else {
      #Gini index for the rule and for the claims
      rule = Rule(E, do)
      giniawards = 1 - 1 / n - (2 / n) * sum((n - (1:n)) * rule / E)
      return(list(Gini_awards = giniawards, Gini_claims = giniclaims))
    }
}
