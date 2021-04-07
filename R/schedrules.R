#' @title Schedules of awards of several rules
#' @description This function returns the graphical representation of the schedules of awards of different rules for a claims vector and the same claimant.
#' @param d A vector of claims.
#' @param claimant A claimant.
#' @param Rules The rules: AA, APRO, CE, CEA, CEL, DT, MO, PIN, PRO, RA, Talmud.
#' @param col The colors. If col = NULL then the sequence of default colors is:
#' c("red", "blue", "green", "yellow", "pink", "coral4", "darkgray", "burlywood3", "black", "darkorange", "darkviolet").
#' @param points The number of endowment values to draw the path.
#' @param legend A logical value. The color legend is shown if legend = TRUE.
#' @return The graphical representation of the schedules of awards of the rules for the claims vector and the same claimant.
#' @details Let \eqn{d\in \mathcal{R}^n}{d}, with \eqn{d\ge 0}, be a vector of claims and
#'  denote  \eqn{D=\sum_{i=1}^{n} d_i}{D=\sum di} the sum of claims.
#'
#' The schedules of awards of a rule \eqn{R} for claimant \eqn{i} is the function \eqn{S} that assigns to each \eqn{E\in [0,D]}{0\le E \le D} the  value:
#' \eqn{S(E)=R_i(E,d)\in \mathcal{R}}{S(E)=Ri(E,d)}.
#' Therefore, the schedules of awards of a rule plots each claimants's award as a function of \eqn{E}.
#'
#' @seealso \link{schedrule}, \link{pathawards}, \link{pathawards3}, \link{verticalruleplot}
#' @examples
#' d=c(2,4,7,8)
#' claimant=2
#' Rules=c(Talmud,RA,AA)
#' col=c("red","green","blue")
#' schedrules(d,claimant,Rules,col)
#' @references Thomson, W. (2019). How to divide when there isn't enough. From Aristotle, the Talmud, and Maimonides to the axiomatics of resource allocation. Cambridge University Press.
#' @importFrom graphics points
#' @importFrom graphics lines
#' @importFrom graphics legend
#' @importFrom graphics axis
#' @importFrom graphics abline
#' @export
schedrules = function(d, claimant, Rules, col = NULL, points = 201, legend = TRUE) {

  if (sum((d < 0)) > 0)
    stop('d is not a claims vector.',call.=F)

  # Number of claimants
  n = length(d)
  # Sum of the claims
  D = sum(d)
  # Number of rules
  numberrules = length(Rules)
  # Default colors
  if (is.null(col)) {
    col=c("red","blue","green","yellow","pink","coral4","darkgray","burlywood3","black","darkorange","darkviolet")
  }
  # The values of the endowment (we make sure that D/2 is one of these values)
  endowms = c(seq(0, D/2, length.out = ceiling(points/2)),seq(D/2, D, length.out = ceiling(points/2)))
  le = length(endowms)
  # The values of the rules for the claimant
  rulevalues = matrix(0, numberrules, le)
  for (kk in 1:numberrules) {
    for (ii in 1:le) {
      r = Rules[[kk]](endowms[ii], d)
      rulevalues[kk, ii] = t(r[claimant])
    }
  }
  # The name of the rules as a string
  Rulechar = rep(0, numberrules)
  for (kk in 1:numberrules) {
    Rulechar[kk] = Rules[[kk]](0, 0, name=TRUE)
  }
  ##################################
  # PLOT: THE SCHEDULES OF AWARDS
  plot(
    0,
    0,
    type = "n",
    xlim = c(0, D),
    ylim = c(0, d[claimant]),
    xaxt="n",
    main = paste("Schedules of awards for claimant",claimant),
    xlab = paste("d=(",toString(d),")",sep=""),
    ylab = ""
  )
  # Tickmarks and grid
  grid(nx=0,ny=NULL)
  do = sort(d)
  axis(side=1,at=c(0,do[1],do[n],D/2,D-do[n],D-do[1],D))
  abline(a = NULL, b = NULL, v = c(0,do[1],do[n],D/2,D-do[n],D-do[1],D), lty="dotted")
  # The curves for each rule
  for (j in 1:dim(rulevalues)[1]) {
    lines(endowms, rulevalues[j, ], col = col[j])
   # points(D / 2, d[claimant[j]] / 2, col = "black", lwd = 2)
  }
  #The legend
  if (legend==TRUE){
    legend(x = "topleft",
           legend = c(Rulechar),
           col = col[1:numberrules],
           lty=1,
           lwd=2,
           seg.len=0.5,
           cex=0.8,
           y.intersp=0.5,
           bty = "n"
    )
  }
}
