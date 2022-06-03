#' @title Cumulative awards curve
#' @description The graphical representation of the cumulative curves of a rule (or several rules) with respect to a given rule, for a claims problem.
#' @param E The endowment.
#' @param d The vector of claims.
#' @param Rule Principal Rule: AA, APRO, CE, CEA, CEL, DT, MO, PIN, PRO, RA, Talmud. By default, Rule = PRO.
#' @param Rules The rules: AA, APRO, CE, CEA, CEL, DT, MO, PIN, PRO, RA, Talmud.
#' @param col The colours. If col = NULL then the sequence of default colours is:
#' c("red", "blue", "green", "yellow", "pink", "coral4", "darkgray", "burlywood3", "black", "darkorange", "darkviolet").
#' @param legend A logical value. The colour legend is shown if legend = TRUE.
#' @return The graphical representation of the cumulative curves of a rule (or several rules) for a claims problem.
#' @details Let \eqn{E> 0} be the endowment to be divided and \eqn{d\in \mathcal{R}^n}{d} the vector of claims
#' with \eqn{d\ge 0} and such that the sum of claims \eqn{D=\sum_{i=1}^{n} d_i\ge E}{D=\sum di} exceeds the endowment.
#'
#' Rearrange the claims from small to large, \eqn{0 \le d_1 \le...\le d_n}{%
#' 0 \le d1 \le...\le dn}. The cumulative curve allows us
#' to compare the division recommended by a specific rule \eqn{R} with the division the division recommended by another specific rule \eqn{S}.
#'
#' The cumulative awards curve of a rule \eqn{S} with respect of a rule \eqn{R} for the claims problem \eqn{(E,d)} is the polygonal path connecting the \eqn{n+1} points
#' \deqn{(0,0), (\frac{R_1}{E},\frac{S_1}{E}),\dots,(\frac{\sum_{i=1}^{n-1}R_i}{E},\frac{\sum_{i=1}^{n-1}S_i}{E}),(1,1).}{%
#' (0,0) , (R1/E,S1/E) , ((R1+R2)/E , (S1+S2)/E ,\dots , (1,1).}
#'
#' The cumulative awards curve fully captures the Lorenz ranking of rules:
#' if a rule \eqn{R} Lorenz-dominates a rule \eqn{S} then, for each claims problem, the cumulative curve of \eqn{R}
#' lies above the cumulative curve of \eqn{S}.
#' If \eqn{R = PRO}, the cumulative curve coincides with the cumulative claims-awards curve.
#'
#' \eqn{cumulativecurve} function of version 0.1.0 returned the cumulative claims-awards curve with respect to the proportional rule.
#' @seealso \link{deviationindex}, \link{indexgpath}, \link{lorenzcurve}, \link{giniindex}, \link{lorenzdominance},  \link{allrules}.
#' @examples
#' E=10
#' d=c(2,4,7,8)
#' Rule=PRO
#' Rules=c(AA,RA,Talmud,CEA,CEL)
#' cumawardscurve(E,d,Rule,Rules)
#' @references Lorenz, M. O. (1905). Methods of measuring the concentration of wealth. Publications of the American statistical association, 9(70), 209-219.
#' @references Mirás Calvo, M.Á., Núñez Lugilde, I., Quinteiro Sandomingo, C., and Sánchez Rodríguez, E. (2022). Deviation from proportionality and Lorenz-domination for claims problems. Rev Econ Design. \doi{10.1007/s10058-022-00300-y}
#' @importFrom graphics lines
#' @importFrom graphics legend
#' @importFrom graphics grid
#' @importFrom graphics mtext
#' @export
cumawardscurve = function(E, d,Rule=PRO,Rules, col = NULL, legend = TRUE) {

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
  R=Rule(0,0,name=TRUE)

  R1=c()
  for(i in 1:length(Rules)){
    R1=c(R1,paste(Rules[[i]](0,0,name=TRUE),sep=""))
  }



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
    xlab = paste("Percentage of awards of ",toString(R)," rule",sep=""),
    ylab = paste("Percentage of awards of the rules: ",toString(R1),sep=""),
    main = paste(R," curve",sep="")
  )
  subtitle=paste("E=",toString(E),"; d=(",toString(d),")",sep="")
  mtext(subtitle,side=3,line=0.5,cex=0.7)
  grid()
  ###### claims CURVE (DATA) ######
  numberrules = length(Rules)
  percentAwards = matrix(0, numberrules, n)
  percentAwards2= matrix(0, numberrules, n)
  # THE CURVES
  for (ii in 1:numberrules) {
    # THE CUMULATIVE PERCENTAGES
    rule = Rules[[ii]](E, do)
    percentAwards[ii, ] = cumsum(rule)/E
    r=Rule(E, do)
    percentAwards2[ii, ] = cumsum(r)/E
    # THE POLIGONAL CURVE
    lines(
      c(0,percentAwards2[ii,]),
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
