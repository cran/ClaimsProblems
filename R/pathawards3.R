#' @title The path of awards for three claimants
#' @description This function returns the graphical representation of the path of awards of any rule for a claims vector and three claimants.
#' @param d The vector of claims.
#' @param claimants Three claimants.
#' @param Rule The rule: AA, APRO, CE, CEA, CEL, DT, MO, PIN, PRO, RA, Talmud.
#' @param col The colour of the path, by default, col="red".
#' @param points The number of values of the endowment to draw the path.
#' @return The graphical representation of the path of awards of a rule for the given claims and three claimants.
#' @details Let \eqn{d\in \mathcal{R}^n}{d}, with \eqn{d\ge 0}, be a vector of claims and
#'  denote  \eqn{D=\sum_{i=1}^{n} d_i}{D=\sum di} the sum of claims.
#'
#' The path of awards of a rule \eqn{R} for three claimants \eqn{i}, \eqn{j}, and \eqn{k} is the parametric curve:
#' \deqn{p(E)=\{(R_i(E,d),R_j(E,d),R_k(E,d))\in \mathcal{R}^3:\;E\in[0,D]\}.}{p(E)=\{(Ri(E,d),Rj(E,d),Rk(E,d)): 0\le E \le D\}.}
#' @seealso \link{pathawards}, \link{schedrule}, \link{schedrules}, \link{verticalruleplot}
#' @examples
#' d=c(2,4,7,8)
#' claimants=c(1,3,4)
#' Rule=Talmud
#' pathawards3(d,claimants,Rule)
#' @importFrom rgl plot3d
#' @importFrom rgl points3d
#' @importFrom rgl grid3d
#' @importFrom rgl par3d
#' @importFrom rgl view3d
#' @export

pathawards3 = function(d, claimants, Rule, col = "red", points = 300)  {
  # CONTROL of d and claimants
  if (sum((d < 0)) > 0)
    stop('d is not a claims vector.',call.=F)

  if (length(claimants) != 3)
    stop("The function is defined for three claimants.",call.=F)

  # Te sum of claims
  D = sum(d)
  # The values of the endowment (we make sure that D/2 is one of these values)
  endowms = c(seq(0, D/2, length.out = ceiling(points/2)),seq(D/2, D, length.out = ceiling(points/2)))
  le = length(endowms)
  claim1 = rep(0, le)
  claim2 = rep(0, le)
  claim3 = rep(0, le)
  for (ii in 1:le) {
    r = Rule(endowms[ii], d)
    claim1[ii] = r[claimants[1]]
    claim2[ii] = r[claimants[2]]
    claim3[ii] = r[claimants[3]]
  }
  ##### PATH AWARDS, GRAPHICAL REPRESENTATION ####
  #windows(12, 8, pointsize = 10)
  par3d(windowRect = c(100, 100, 800, 800))
  Rulechar = Rule(0, 0, name=TRUE)
  plot3d(
    claim1,
    claim2,
    claim3,
    col = col,
    main = paste("Path of awards of the ",Rulechar," rule for d=(",toString(d),")",sep=""),
    xlab = toString(claimants[1]),
    ylab = toString(claimants[2]),
    zlab = toString(claimants[3]),
    axes = T,
    box = F
  )
  #
  view3d(180,90)
  grid3d(c("x-+", "y-+", "z-+"))
}
