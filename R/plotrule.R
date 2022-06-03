#' @title Plot of an awards vector
#' @description This function plots an awards vector in the set of awards vectors for a claims problem.
#' @param E The endowment.
#' @param d The vector of claims.
#' @param awards An awards vector.
#' @param set A logical value.
#' @param Rule A rule: AA, APRO, CE, CEA, CEL, DT, MO, PIN, PRO, RA, Talmud.
#' @param col The colour.
#' @return If set = TRUE, the function creates a new figure plotting both the set of awards vectors for the claims problem and the given awards vector.
#' Otherwise, it just adds to the current picture the point representing the given awards vector. The function only plots one awards vector at a time.
#'
#' The awards vector can be introduced directly as a vector. Alternatively, we can provide a rule and then the awards vector to be plotted is the one selected by the rule for the claims problem.
#' Therefore, if Rule = NULL it plots the given awards vector.
#' Otherwise, it plots the awards vector selected by the given rule for the claims problem.
#' In order to plot two (or more) awards vectors, draw the first one with the option set = TRUE and add the others, one by one, with the option set = FALSE.
#' @details Let \eqn{E\ge 0} be the endowment to be divided and \eqn{d\in \mathcal{R}^n}{d} the vector of claims
#' with \eqn{d\ge 0} and such that \eqn{\sum_{i=1}^{n} d_i\ge E,\;}{} the sum of claims exceeds the endowment.
#'
#' A vector \eqn{x=(x_1,\dots,x_n)}{x=(x1,...,xn)} is an awards vector for the claims problem \eqn{(E,d)} if \eqn{0\le x \le d}
#' and satisfies the balance requirement, that is, \eqn{\sum_{i=1}^{n}x_i=E}{x1+\dots+xn=E} the sum of its coordinates is equal to \eqn{E}.
#' Let \eqn{X(E,d)} be the set of awards vectors for the problem \eqn{(E,d)}.
#'
#' A rule is a function that assigns to each claims problem \eqn{(E,d)} an awards vector for \eqn{(E,d)},
#' that is, a division between the claimants of the amount available.
#' @seealso \link{setofawards}, \link{allrules}
#' @examples
#' E=10
#' d=c(2,4,7,8)
#' plotrule(E,d,Rule=AA,col="red")
#' # Plotting the awards vector (1,3,5,1) and the AA rule
#' # First, plot the awards vector (1,3,5,1) and the set of awards
#' plotrule(E,d,awards=c(1,3,5,1),col="green")
#' # Second, add the AA rule with the option set=FALSE
#' plotrule(E,d,Rule=AA,set=FALSE,col="red")
#' @importFrom graphics points
#' @importFrom graphics text
#' @importFrom rgl text3d
#' @importFrom rgl points3d
#' @export
plotrule = function(E, d, Rule=NULL, awards=NULL, set=TRUE, col = "blue") {
  ########################################
  # Required: (E,d) must be a claims problem, i.e., E >=0, d >=0, E <= sum(d)
  ########################################
  n = length(d)
  D = sum(d) #The number of claims and the total claim
  if (E < 0 | sum((d < 0)) > 0 | E > D)
    stop('d is not a claims vector. \n',call.=F)

  # MORE than 4 claimants
  if (n>4)
    stop('There are more than 4 claimants. \n',call.=F)

  #####
  ##### If given a rule, then awards = Rule(E,d)
  if (is.null(Rule)==FALSE){
    awards=Rule(E, d)
    }
  ########################################
  # Required: awards must be an awards vector
  ########################################
  na = length(awards)
  if (na != n | sum((awards < 0)) > 0 | abs(sum(awards)-E)>0.001)
    stop('The given vector is not an awards vector for the claims problem. \n')

  #
  #
  # CASE n=2
  if (n == 2) {
if (set==TRUE){setofawards(E,d)}
    points(awards[1], awards[2], col = col, pch = 8)
  }
  #
  #
  # CASE n=3
  else if (n == 3) {
    # We need to compute the matrix to project the awards vector in our equilateral triangle
    m =c(max(0, E - d[2] - d[3]), max(0, E - d[1] - d[3]), max(0, E - d[1] - d[2]))
    Delta = E - sum(m)
    # the real imputation set (in R^3)
    imputation = matrix(
      c(E - m[2] - m[3], m[2], m[3],
        m[1], E - m[1] - m[3], m[3],
        m[1], m[2], E - m[1] - m[2]),
      ncol = 3,
      byrow = T
    )
    # The equilateral triangle (in R^2)
    equilatero = matrix(c(0, 0, Delta, 0, Delta / 2, sqrt(3) / 2 * Delta),
                        ncol = 2,
                        byrow = T)
    # The projection matrix
    P = t(equilatero) %*% solve(t(imputation))
    # The projected coordinates of the vector of awards
    awards = t(P %*% t(t(awards)))
    # Plot the set of awards (if necessary)
    if (set==TRUE){setofawards(E,d)}
    points(awards, col = col, pch = 8)
  }
  #
  #
  # CASE n=4
  else if (n == 4) {
    if (set==TRUE){setofawards(E,d)}
    text3d(awards[1],awards[2],awards[3],"*",col=col,font=4)
  }
}
