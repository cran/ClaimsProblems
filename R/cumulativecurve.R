cumulativecurve = function(E, d, Rules, col = NULL, legend = TRUE) {
  .Deprecated("cumawardscurve")
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
