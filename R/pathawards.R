#' @title The path of awards for two claimants
#' @description This function returns the graphical representation of the path of awards of any rule for a claims vector and a pair of claimants.
#' @param d The vector of claims.
#' @param claimants Two claimants.
#' @param Rule The rule: AA, APRO, CE, CEA, CEL, DT, MO, PIN, PRO, RA, Talmud.
#' @param col The colour.
#' @param points The number of values of the endowment to draw the path.
#' @return The graphical representation of the path of awards of a rule for the given claims and a pair of claimants.
#' @details Let \eqn{d\in \mathcal{R}^n}{d}, with \eqn{d\ge 0}, be a vector of claims and
#'  denote  \eqn{D=\sum_{i=1}^{n} d_i}{D=\sum di} the sum of claims.
#'
#' The path of awards of a rule \eqn{R} for two claimants \eqn{i} and \eqn{j} is the parametric curve:
#' \deqn{p(E)=\{(R_i(E,d),R_j(E,d))\in \mathcal{R}^2:\;E\in[0,D]\}.}{p(E)=\{(Ri(E,d),Rj(E,d)): 0\le E \le D\}.}
#'
#' @seealso \link{pathawards3}, \link{schedrule}, \link{schedrules}, \link{verticalruleplot}
#' @examples
#' d=c(2,4,7,8)
#' claimants=c(1,2)
#' Rule=Talmud
#' pathawards(d,claimants,Rule)
#' # The path of awards of the concede-and-divide rule
#' pathawards(c(2,3),c(1,2),CD)
#' #The path of awards of the DT rule for d=(d1,d2) with d2<2d1
#' pathawards(c(1,1.5),c(1,2),DT,col="blue",points=1001)
#' #The path of awards of the DT rule for d=(d1,d2) with d2>2d1
#' pathawards(c(1,2.5),c(1,2),DT,col="blue",points=1001)
#' @references Thomson, W. (2019). How to divide when there isn't enough. From Aristotle, the Talmud, and Maimonides to the axiomatics of resource allocation. Cambridge University Press.
#' @importFrom graphics points
#' @importFrom graphics mtext
#' @importFrom graphics lines
#' @importFrom grDevices dev.new
#' @export

pathawards = function(d, claimants, Rule, col = "red", points = 201) {
  # CONTROL of d and claimants
  if (sum((d < 0)) > 0)
    stop('d is not a claims vector.',call.=F)

  if (length(claimants) != 2)
    stop("The function is defined for two claimants.",call.=F)

  # Te sum of claims
  D = sum(d)
  # The values of the endowment (we make sure that D/2 is one of these values)
  endowms = c(seq(0, D/2, length.out = ceiling(points/2)),seq(D/2, D, length.out = ceiling(points/2)))
  le = length(endowms)
  claim1 = rep(0, le)
  claim2 = rep(0, le)
  for (ii in 1:le) {
    r = Rule(endowms[ii], d)
    claim1[ii] = r[claimants[1]]
    claim2[ii] = r[claimants[2]]
  }
  ### PATH OF AWARDS, GRAPHICAL REPRESENTATION ###
  Rulechar = Rule(0, 0, name=TRUE)
  plot(
    claim1,
    claim2,
    xlim = c(0, d[claimants[1]]),
    ylim = c(0, d[claimants[2]]),
    type = "l",
    col = col,
    main = c("Path of awards"),
    xlab = bquote(paste(.(Rulechar)[.(claimants[1])])*'(E,d)'),
    ylab = bquote(paste(.(Rulechar)[.(claimants[2])])*'(E,d)')
  )
  subtitle=paste("d=(",toString(d),")",sep="")
  mtext(subtitle,side=3,line=0.5,cex=0.7)
  grid()
  lines(claim1, claim2, col = col)
}
