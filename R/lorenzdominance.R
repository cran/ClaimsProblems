#' @title Lorenz-dominance relation
#' @description This function checks whether or not the awards assigned by two rules to a claims problem are Lorenz-comparable.
#' @param E The endowment.
#' @param d The vector of claims.
#' @param Rules The two rules: AA, APRO, CE, CEA, CEL, DT, MO, PIN, PRO, RA, Talmud.
#' @param Info A logical value.
#' @return If Info = FALSE, the Lorenz-dominance relation between the awards vectors selected by both rules.
#' If both awards vectors are equal then cod = 2. If the awards vectors are not Lorenz-comparable then cod = 0.
#' If the awards vector  selected by the first rule Lorenz-dominates the awards vector  selected by the second rule then cod = 1; otherwise cod = -1.
#' If Info = TRUE, it also gives the corresponding cumulative sums.
#' @details Let \eqn{E\ge 0} be the endowment to be divided and \eqn{d\in \mathcal{R}^n}{d} the vector of claims
#' with \eqn{d\ge 0} and such that \eqn{\sum_{i=1}^{n} d_i\ge E,\;}{} the sum of claims exceeds the endowment.
#'
#' A vector \eqn{x=(x_1,\dots,x_n)}{x=(x1,...,xn)} is an awards vector for the claims problem \eqn{(E,d)} if \eqn{0\le x \le d}
#' and satisfies the balance requirement, that is, \eqn{\sum_{i=1}^{n}x_i=E}{x1+\dots+xn=E} the sum of its coordinates is equal to \eqn{E}.
#' Let \eqn{X(E,d)} be the set of awards vectors for \eqn{(E,d)}.
#'
#' Given a claims problem \eqn{(E,d)}, in order to compare a pair of awards vectors \eqn{x,y\in X(E,d)}{x,y in X(E,d)} with the Lorenz criterion,
#' first one has to rearrange the coordinates of each allocation in a non-decreasing order. Then we say that \eqn{x} Lorenz-dominates \eqn{y} (or, that \eqn{y} is Lorenz-dominated by \eqn{x})
#' if all the cumulative sums of the rearranged coordinates are greater with \eqn{x} than with \eqn{y}. That is,
#' \eqn{x} Lorenz-dominates \eqn{y} if for each \eqn{k=1,\dots,n-1} we have that
#' \deqn{\sum_{j=1}^{k}x_j \geq \sum_{j=1}^{k}y_j}{x1+\dots+xk \ge y1+\dots+yk.}
#'
#' Let \eqn{R} and \eqn{R'} be two rules. We say that \eqn{R} Lorenz-dominates \eqn{R'} if \eqn{R(E,d)} Lorenz-dominates \eqn{R'(E,d)} for all \eqn{(E,d)}.
#'
#' @seealso \link{cumawardscurve}, \link{deviationindex}, \link{indexgpath}, \link{lorenzcurve}, \link{giniindex}.
#' @examples
#' E=10
#' d=c(2,4,7,8)
#' Rules=c(AA,CEA)
#' lorenzdominance(E,d,Rules)
#' @references  Lorenz, M. O. (1905). Methods of measuring the concentration of wealth. Publications of the American statistical association, 9(70), 209-219.
#' @references Mirás Calvo, M.Á., Núñez Lugilde, I., Quinteiro Sandomingo, C., and Sánchez Rodríguez, E. (2022). Deviation from proportionality and Lorenz-domination for claims problems. Rev Econ Design. \doi{10.1007/s10058-022-00300-y}
#' @references Mirás Calvo, M.Á., Núñez Lugilde, I., Quinteiro Sandomingo, C., and Sánchez-Rodríguez, E. (2021). The adjusted proportional and the minimal overlap rules restricted to the lower-half, higher-half, and middle domains. Working paper 2021-02, ECOBAS.
#' @export

lorenzdominance = function(E, d, Rules, Info = FALSE) {
  ########################################
  # Required: (E,d) must be a claims problem, i.e., E >=0, d >=0, E <= sum(d)
  ########################################
  n = length (d)
  D = sum(d) #The number of claims and the total claim
  if (E < 0 || sum((d < 0)) > 0 || E > D)
    stop('(E,d) is not a claims problem.',call.=F)

  if(length(Rules)>2)
    warning('We check if the first two rules are Lorenz-comparable. \n',call.=F,immediate.=T)

  #####################
  do = sort(d)
  if (sum(do == d) < n){
    message('The result is computed for the rearranged vector of claims.\n')
  }
  Rule1char = Rules[[1]](0, 0, name=TRUE)
  Rule2char = Rules[[2]](0, 0, name=TRUE)
  rule1 = Rules[[1]](E, do)
  rule2 = Rules[[2]](E, do) #Award of Rule 1 and Rule2
  sum1 = cumsum(rule1)
  sum2 = cumsum(rule2)
  dom1 = sum(sum1 >= sum2)
  dom2 = sum(sum2 >= sum1)

  if (dom1 == n & dom2 < n) {
    cod=1
  if (Info == TRUE){
    message('The awards vector selected by the ',
        Rule1char,
        ' rule Lorenz dominates the awards vector selected by the ',
        Rule2char,
        ' rule.\n')
                  }
  } else if (dom2 == n & dom1 < n) {
    cod=-1
    if (Info == TRUE){
    message('The awards vector selected by the ',
        Rule1char,
        ' rule is Lorenz-dominated by the awards vector selected by the ',
        Rule2char,
        ' rule. \n')
                  }
  } else if (dom1 == n & dom2 == n) {
    cod=2
    if (Info == TRUE){
    message('The awards vectors selected by the ',
        Rule1char,
        ' and the ',
        Rule2char,
        ' rules are equal.\n')
                    }
    }else{
      cod=0
      if (Info == TRUE){
    message('The awards vectors selected by the',
        Rule1char,
        'and the',
        Rule2char,
        'rules are not Lorenz-comparable.\n')
                      }
    }
  if (Info == TRUE){
  message('The cumulative sums for both awards vectors are:\n')
  message(paste(
    format(Rule1char, width = 17, justify = c("right")),
    format(Rule2char, width = 17, justify = c("right")),
    '\n'
  ))
  for (i in 1:n) {
    message(paste(
      format(
        round(sum1[i], 3),
        nsmall = 3,
        width = 17,
        justify = c("right")
      ),
      format(
        round(sum2[i], 3),
        nsmall = 3,
        width = 17,
        justify = c("right")
      ),
      '\n'
    ))
              }
  }
return(list(cod=cod))
}
