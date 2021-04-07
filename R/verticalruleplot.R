#' @title Vertical rule plot
#' @description For each claimant, it plots a vertical line with his claim and a point on the awards vector of the chosen rules.
#' @param E The endowment.
#' @param d The vector of claims
#' @param Rules The rules: AA, APRO, CE, CEA, CEL, DT, MO, PIN, PRO, RA, Talmud.
#' @param col The colors. If col=NULL then the sequence of default colors is:
#' c("red", "blue", "green", "yellow", "pink", "coral4", "darkgray", "burlywood3", "black", "darkorange", "darkviolet").
#' @param legend A logical value. The color legend is shown if legend=TRUE.
#' @return This function represents the claims vector and the awards vector assigned by several rules as vertical segments.
#' @details Let \eqn{E\ge 0} be the endowment to be divided and \eqn{d\in \mathcal{R}^n}{%
#' d} the vector of claims with \eqn{d\ge 0} and such that \eqn{\sum_{i=1}^{n} d_i\ge E,\ }{}
#' the sum of claims exceeds the endowment.
#'
#' A vector \eqn{x=(x_1,\dots,x_n)}{x=(x1,...,xn)} is an awards vector for the claims problem \eqn{(E,d)} if:
#' no claimant is asked to pay (\eqn{0\le x});
#' no claimant  receives more than his claim (\eqn{x\le d});
#' and the balance requirement is satisfied, that is, the sum of the awards is equal to the endowment (\eqn{\sum_{i=1}^{n} x_i= E}{x1+...+xn=E}).
#'
#' A rule is a function that assigns to each claims problem \eqn{(E,d)} an awards vector for \eqn{(E,d)},
#' that is, a division between the claimants of the amount available.
#'
#' The formal definitions of the main rules are given in the corresponding function help.
#'
#' @seealso \link{allrules}, \link{pathawards}, \link{pathawards3}, \link{schedrule}, \link{schedrules}
#' @examples
#' E=10
#' d=c(2,4,7,8)
#' Rules=c(Talmud,RA,AA)
#' col=c("red","green","blue")
#' verticalruleplot(E,d,Rules,col)
#' @references Thomson, W. (2019). How to divide when there isn't enough. From Aristotle, the Talmud, and Maimonides to the axiomatics of resource allocation. Cambridge University Press.
#' @importFrom graphics points
#' @importFrom graphics lines
#' @importFrom graphics legend
#' @export
verticalruleplot = function(E,d, Rules, col = NULL, legend = TRUE) {
  #
  n = length(d) #The number of claims
  D = sum(d) #The sum of claims
  ########################################
  # Required: (E,d) must be a claims problem, i.e., E >=0, d >=0, E <= sum(d)
  ########################################
  if (E < 0 || sum((d < 0)) > 0 || E > D)
    stop('(E,d) is not a claims problem.',call.=F)

  # Default colors
  if (is.null(col)) {
    col=c("red","blue","green","yellow","pink","coral4","darkgray","burlywood3","black","darkorange","darkviolet")
  }
  #Names of the rules as strings
  numberrules = length(Rules)
  Rulechar=rep(0,numberrules)
  for(i in 1:numberrules){
    Rulechar[i]=Rules[[i]](0,0,name=TRUE)
  }
# The plot
  plot(
    0,
    0,
    type = "n",
    xlim = c(0.3, n+0.5),
    ylim = c(0, max(d)),
    main = c("Vertical awards plot"),
    xlab = paste("E=",toString(E),"; d=(",toString(d),")",sep=""),
    ylab = ""
  )
  grid()
  #Draw the line of the claims for each claimant.
  for(i in 1:n){
    lines(c(i,i),c(0,d[i]),col="coral")
  }
  #Draw the point of each claimant for each rule.
  for(j in 1:numberrules){
    rule=Rules[[j]](E,d)
    for(i in 1:n){
      points(i,rule[i],col=col[j],pch=8)
    }
  }
  #Legend of the rule
  if (legend==TRUE){
  legend(x = "topleft",
         legend = c(Rulechar),
         col = col[1:numberrules],
         pch=8,
         cex=1.05,
         y.intersp=0.5,
         bty = "n"
         )
}
}
