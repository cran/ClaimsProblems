#' @title The Lorenz curve
#' @description This function returns the Lorenz curve of any rule for a claims problem.
#' @param E The endowment.
#' @param d The vector of claims.
#' @param Rules The rules: AA, APRO, CE, CEA, AV, DT, MO, PIN, PRO, RA, Talmud, RTalmud.
#' @param col The colours. If \code{col=NULL} then the sequence of default colors is:
#' c("red", "blue", "green", "yellow", "pink", "orange", "coral4", "darkgray", "burlywood3", "black", "darkorange", "darkviolet").
#' @param legend A logical value. The colour legend is shown if \code{legend=TRUE}.
#' @return The graphical representation of the Lorenz curve of a rule (or several rules) for a claims problem.
#' @details Let \eqn{N=\{1,\ldots,n\}} be the set of claimants, \eqn{E\ge 0} the endowment to be divided and \eqn{d\in \mathbb{R}_+^N} the vector of claims
#' such that \eqn{\sum_{i \in N} d_i\ge E}.
#'
#' Rearrange the claims from small to large, \eqn{0 \le d_1 \le...\le d_n}.
#' The Lorenz curve represents the proportion of the awards given to each subset of claimants by a specific rule \eqn{\mathcal{R}}  as a function of the
#' cumulative distribution of population.
#'
#' The Lorenz curve of a rule \eqn{\mathcal{R}} for the claims problem \eqn{(E,d)} is the polygonal path connecting the \eqn{n+1} points,
#' \deqn{(0,0), \Bigl(\frac{1}{n},\frac{\mathcal{R}_1(E,d)}{E}\Bigr),\dots,\Bigl(\frac{n-1}{n},\frac{\sum_{i=1}^{n-1}\mathcal{R}_i(E,d)}{E}\Bigl),(1,1).}
#' Basically, it represents the cumulative percentage of the endowment assigned by the rule to each cumulative percentage of claimants.
#'
#' @seealso \link{cumawardscurve}, \link{deviationindex}, \link{giniindex}, \link{indexgpath}, \link{lorenzdominance}.
#' @examples
#' E=10
#' d=c(2,4,7,8)
#' Rules=c(AA,RA,Talmud,CEA,CEL)
#' col=c("red","blue","green","yellow","pink")
#' lorenzcurve(E,d,Rules,col)
#' @references  Lorenz, M. O. (1905). Methods of measuring the concentration of wealth. Publications of the American statistical association 9(70), 209-219.
#' @references Mirás Calvo, M.Á., Núñez Lugilde, I., Quinteiro Sandomingo, C., and Sánchez Rodríguez, E. (2023a). Deviation from proportionality and Lorenz-domination for claims problems. Review of Economic Design 27, 439-467.
#' @references Mirás Calvo, M.Á., Núñez Lugilde, I., Quinteiro Sandomingo, C., and Sánchez-Rodríguez, E. (2023b). Refining the Lorenz‐ranking of rules for
#' claims problems on restricted domains. International Journal of Economic Theory 19(3), 526-558.
#' @importFrom graphics lines
#' @importFrom graphics legend
#' @importFrom graphics grid
#' @importFrom graphics mtext
#' @export

lorenzcurve = function(E, d, Rules, col = NULL, legend = TRUE) {

# Default colors
  if (is.null(col)) {
  col=c("red","blue","green","yellow","pink","coral4", "orange", "darkgray","burlywood3","black","darkorange","darkviolet")
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
    xlab = c("Percentage of claimants"),
    ylab = c("Percentage of awards"),
    main = c("Lorenz curve")
  )
  subtitle=paste("E=",toString(E),"; d=(",toString(d),")",sep="")
  mtext(subtitle,side=3,line=0.5,cex=0.7)
  grid()
  ###### claims CURVE (DATA) ######
  numberrules = length(Rules)
  percentAwards = matrix(0, numberrules, n)
  percentClaimants = matrix(0, numberrules, n)
  # THE CURVES
  for (ii in 1:numberrules) {
    # THE CUMULATIVE PERCENTAGES
    rule = Rules[[ii]](E, do)
    percentAwards[ii, ] = cumsum(rule)/E
    percentClaimants[ii, ] =c(1:n)/n
    # THE POLIGONAL CURVE
    lines(
      c(0,percentClaimants[ii,]),
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
  if (legend==TRUE){
  legend(
    x = "topleft",
    legend = c(name),
    col=col[1:numberrules],
    lty=1,
    lwd=2,
    seg.len=0.5,
    cex=0.8,
    y.intersp=0.6,
    bty ="n"
  )
}
}
