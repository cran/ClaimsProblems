#' @title Gini index
#' @description  This function returns the Gini index of any rule for a claims problem.
#' @param E The endowment.
#' @param d The vector of claims.
#' @param Rule A rule: AA, APRO, CE, CEA, CEL, AV, DT, MO, PIN, PRO, RA, Talmud, RTalmud.
#' @return The Gini index of a rule for a claims problem and the Gini index of the vector of claims.
#' @details Let \eqn{N=\{1,\ldots,n\}} be the set of claimants, \eqn{E\ge 0} the endowment to be divided and \eqn{d\in \mathbb{R}_+^N} the vector of claims
#' such that \eqn{\sum_{i \in N} d_i\ge E}.
#'
#' Rearrange the claims from small to large, \eqn{0 \le d_1 \le...\le d_n}.
#' The Gini index is a number aimed at measuring the degree of inequality in a distribution.
#' The Gini index of the rule \eqn{\mathcal{R}} for the problem \eqn{(E,d)}, denoted by \eqn{G(\mathcal{R},E,d)}, is
#' the ratio of the area that lies between the identity line and the Lorenz curve of the rule over the total area under the identity line.
#'
#' Let \eqn{\mathcal{R}_0(E,d)=0}. For each \eqn{k=0,\dots,n} define
#' \eqn{X_k=\frac{k}{n}} and \eqn{Y_k=\frac{1}{E} \sum_{j=0}^{k} \mathcal{R}_j(E,d)}. Then,
#' \deqn{G(\mathcal{R},E,d)=1-\sum_{k=1}^{n}\Bigl(X_{k}-X_{k-1}\Bigr)\Bigl(Y_{k}+Y_{k-1}\Bigr).}
#' In general \eqn{0\le G(\mathcal{R},E,d) \le 1}.
#' @seealso \link{cumawardscurve}, \link{deviationindex}, \link{indexgpath},  \link{lorenzcurve}, \link{lorenzdominance}.
#' @examples
#' E=10
#' d=c(2,4,7,8)
#' Rule=AA
#' giniindex(E,d,Rule)
#' # The Gini index of the proportional awards coincides with the Gini index of the vector of claims
#' giniindex(E,d,PRO)
#' @references  Ceriani, L. and Verme, P. (2012). The origins of the Gini index: extracts from Variabilitá e Mutabilitá (1912) by Corrado Gini. The Journal of Economic Inequality 10(3), 421-443.
#' @references Mirás Calvo, M.Á., Núñez Lugilde, I., Quinteiro Sandomingo, C., and Sánchez Rodríguez, E. (2023). Deviation from proportionality and Lorenz-domination for claims problems. Review of Economic Design 27, 439-467.
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
