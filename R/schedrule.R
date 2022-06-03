#' @title Schedules of awards of a rule
#' @description This function returns the graphical representation of the schedules of awards of any rule for a claims vector.
#' @param d A vector of claims.
#' @param Rule The rule: AA, APRO, CE, CEA, CEL, DT, MO, PIN, PRO, RA, or Talmud.
#' @param claimants A subset of claimants.
#' @param col The colours. If col = NULL then the sequence of default colours is chosen randomly.
#' @param points The number of values of the endowment to draw the path.
#' @param legend A logical value. The colour legend is shown if legend = TRUE.
#' @return The graphical representation of the schedules of awards of a rule for a claims vector and a group of claimants.
#' @details  Let \eqn{d\in \mathcal{R}^n}{d}, with \eqn{d\ge 0}, be a vector of claims and
#'  denote  \eqn{D=\sum_{i=1}^{n} d_i}{D=\sum di} the sum of claims.
#'
#' The schedules of awards of a rule \eqn{R} for claimant \eqn{i} is the function \eqn{S} that assigns to each \eqn{E\in [0,D]}{0\le E \le D} the  value:
#' \eqn{S(E)=R_i(E,d)\in \mathcal{R}}{S(E)=Ri(E,d)}.
#' Therefore, the schedules of awards of a rule plots each claimants's award as a function of \eqn{E}.
#'
#' @seealso \link{schedrules}, \link{pathawards}, \link{pathawards3}, \link{verticalruleplot}
#' @examples
#' d=c(2,4,7,8)
#' Rule=Talmud
#' claimants=c(1,2,3,4)
#' col=c("red","green","yellow","blue")
#' schedrule(d,claimants,Rule,col)
#' # The schedules of awards of the concede-and-divide rule.
#' schedrule(c(2,4),c(1,2),CD)
#' @references Thomson, W. (2019). How to divide when there isn't enough. From Aristotle, the Talmud, and Maimonides to the axiomatics of resource allocation. Cambridge University Press.
#' @importFrom graphics points
#' @importFrom graphics lines
#' @importFrom graphics legend
#' @importFrom graphics axis
#' @importFrom graphics abline
#' @importFrom grDevices rgb
#' @importFrom stats runif
#' @export
#'
schedrule = function(d, claimants, Rule, col = NULL, points = 201, legend = TRUE) {

  if (sum((d < 0)) > 0)
    stop('d is not a claims vector.',call.=F)

  # Number of claimants
  n = length(d)
  # Sum of the claims
  D = sum(d)
  # Number of claimants in the subset
  claimantsnumber = length(claimants)
  # Default colors: chosen randomly
  if (is.null(col)) {
    col=rgb(runif(claimantsnumber),runif(claimantsnumber),runif(claimantsnumber))
  }
  # The values of the endowment (we make sure that D/2 is one of these values)
  endowms = c(seq(0, D/2, length.out = ceiling(points/2)),seq(D/2, D, length.out = ceiling(points/2)))
  le = length(endowms)
 # The values of the rule for the grupo of claimants
  rulevalues = matrix(0, claimantsnumber, le)
  for (ii in 1:le) {
    rule = Rule(endowms[ii], d)
    rulevalues[, ii] = t(rule[claimants])
  }
  # The name of the rule as a string
  Rulechar = Rule(0, 0, name=TRUE)
  ##################################
  # PLOT: THE SCHEDULES OF AWARDS
  plot(
    0,
    0,
    type = "n",
    xlim = c(0, D),
    ylim = c(0, max(d[claimants])),
    xaxt="n",
    main = paste("Schedules of awards of the",Rulechar,"rule"),
    xlab = paste("d=(",toString(d),")",sep=""),
    ylab = ""
  )
  # Tickmarks and grid
  grid(nx=0,ny=NULL)
  do = sort(d)
  axis(side=1,at=c(0,do[1],do[n],D/2,D-do[n],D-do[1],D))
  abline(a = NULL, b = NULL, v = c(0,do[1],do[n],D/2,D-do[n],D-do[1],D), lty="dotted")
  # The curves for each claimant
  for (j in 1:dim(rulevalues)[1]) {
    lines(endowms, rulevalues[j, ], col = col[j])
    # points(D / 2, d[claimants[j]] / 2, col = "black", lwd = 2)
  }
  #The legend
  if (legend==TRUE){
  legend(
    x = "topleft",
    legend = c(claimants),
    col = col,
    title = "Claimants",
    lty=1,
    lwd=2,
    seg.len=0.5,
    cex=0.8,
    y.intersp=0.7,
    bty = "n",
    ncol=2
  )
          }
}
