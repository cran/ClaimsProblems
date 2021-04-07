#' @title Cumulative claims-awards curve
#' @description This function returns the cumulative claims-awards curve of a rule for a claims problem.
#' @param E The endowment.
#' @param d The vector of claims.
#' @param Rules The rules: AA, APRO, CE, CEA, CEL, DT, MO, PIN, PRO, RA, Talmud.
#' @param col The colors. If col = NULL then the sequence of default colors is:
#' c("red", "blue", "green", "yellow", "pink", "coral4", "darkgray", "burlywood3", "black", "darkorange", "darkviolet").
#' @param legend A logical value. The color legend is shown if legend = TRUE.
#' @return The graphical representation of the cumulative claims-awards curves of a rule (or several rules) for a claims problem.
#' @details Let \eqn{E> 0} be the endowment to be divided and \eqn{d\in \mathcal{R}^n}{d} the vector of claims
#' with \eqn{d\ge 0} and such that the sum of claims \eqn{D=\sum_{i=1}^{n} d_i\ge E}{D=\sum di} exceeds the endowment.
#'
#' Rearrange the claims from small to large, \eqn{0 \le d_1 \le...\le d_n}{%
#' 0 \le d1 \le...\le dn}. The cumulative claims-awards curve allows us
#' to compare the division recommended by a specific rule \eqn{R} with the division that preserves the proportions of the
#' claims, the proportional rule.
#'
#' The cumulative claims-awards curve of a rule \eqn{R} for the claims problem \eqn{(E,d)} is the polygonal path connecting the \eqn{n+1} points
#' \deqn{(0,0), (\frac{d_1}{D},\frac{R_1(E,d)}{E}),\dots,(\frac{\sum_{i=1}^{n-1}d_i}{D},\frac{\sum_{i=1}^{n-1}R_i(E,d)}{E}),(1,1).}{%
#' (0,0) , (d1/D,R1(E,d)/E) , ((d1+d2)/D , (R1(E,d)+R2(E,d))/E ,\dots , (1,1).}
#' Basically, it represents the percentage of the endowment assigned by the rule to each cumulative proportion of claims.
#'
#' The cumulative claims-awards curve fully captures the Lorenz ranking of rules:
#' if a rule \eqn{R} Lorenz-dominates a rule \eqn{R'} then, for each claims problem, the cumulative claims-awards  curve of \eqn{R}
#' lies above the cumulative curve of \eqn{R'}.
#' @seealso \link{proportionalityindex}, \link{indexpath}, \link{lorenzcurve}, \link{giniindex}, \link{lorenzdominance},  \link{PRO}.
#' @examples
#' E=10
#' d=c(2,4,7,8)
#' Rules=c(AA,RA,Talmud,CEA,CEL)
#' cumulativecurve(E,d,Rules)
#' @references  Lorenz, M. O. (1905). Methods of measuring the concentration of wealth. Publications of the American statistical association, 9(70), 209-219.
#' @references Mirás Calvo, M.A., Núñez Lugilde, I., Quinteiro Sandomingo, C., and Sánchez-Rodríguez, E. (2021). Deviation from proportionality and Lorenz-dominance between
#' the average of awards and the standard rules for claims problems. Working paper 2021-01, ECOBAS.
#' @importFrom graphics lines
#' @importFrom graphics legend
#' @importFrom graphics grid
#' @importFrom graphics mtext
#' @export

cumulativecurve = function(E, d, Rules, col = NULL, legend = TRUE) {

  # Default colors
  if (is.null(col)) {
  col=c("red","blue","green","yellow","pink","coral4","darkgray","burlywood3","black","darkorange","darkviolet")
  }
  ########################################
  # Required: (E,d) must be a claims problem, i.e., E >=0, d >=0, E <= sum(d)
  ########################################
  n = length(d)
  D = sum(d) #The number of claims and the total claim
  if (E < 0 || sum((d < 0)) > 0 || E > D)
    stop('(E,d) is not a claims problem.',call.=F)

  if (E ==0)
    stop('The endowment must be strictly positive, E>0.',call.=F)

  ###################
  do = sort(d)
  if (sum(do == d) < n){
    message('The result is computed for the rearranged vector of claims.\n')
}
  ### THE IDENTITY LINE
  plot(
    c(0, 1), c(0, 1),
    type = "l",
    xlim = c(0, 1),
    ylim = c(0, 1),
    xlab = c("Percentage of claims"),
    ylab = c("Percentage of awards"),
    main = c("Cumulative claims-awards curve")
    )
subtitle=paste("E=",toString(E),"; d=(",toString(d),")",sep="")
mtext(subtitle,side=3,line=0.5,cex=0.7)
grid()
  ###### claims CURVE (DATA) ######
  numberrules = length(Rules)
  percentAwards = matrix(0, numberrules, n)
  percentClaims = matrix(0, numberrules, n)
 # THE CURVES
for (ii in 1:numberrules) {
    # THE CUMULATIVE PERCENTAGES
      rule = Rules[[ii]](E, do)
      percentAwards[ii, ] = cumsum(rule)/E
      percentClaims[ii, ] = cumsum(do)/D
     # THE POLIGONAL CURVE
      lines(
        c(0,percentClaims[ii,]),
        c(0, percentAwards[ii,]),
        lwd = 2,
        type = "o",
        col = col[ii]
      )
  }
    name = rep(0, numberrules)
    for (kk in 1:numberrules) {
      name[kk] = Rules[[kk]](0,0,TRUE )
    }
    if (legend==TRUE) {
    legend(
      x = "topleft",
      legend = c(name),
      col=col[1:numberrules],
      lty=1,
      lwd=2,
      seg.len=0.5,
      cex=0.8,
      y.intersp=0.6,
      bty = "n"
      )
}
}
