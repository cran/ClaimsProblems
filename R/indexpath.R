#' @title Proportionality index path
#' @description The function returns the proportionality deviation index path of a rule for a vector of claims.
#' @param d The vector of claims.
#' @param Rules The rules: AA, APRO, CE, CEA, CEL, DT, MO, PIN, PRO, RA, Talmud.
#' @param col The colors. If col = NULL then the sequence of default colors is:
#' c("red", "blue", "green", "yellow", "pink", "coral4", "darkgray", "burlywood3", "black", "darkorange", "darkviolet").
#' @param points The number of endowment values to be drawn.
#' @param legend A logical value. The color legend is shown if legend = TRUE.
#' @return This function returns the proportionality index path of a rule (or several rules) for a vector of claims.
#' @details Let \eqn{d\in \mathcal{R}^n}{d} be  a vector of claims rearranged from small to large, \eqn{0 \le d_1 \le...\le d_n}{%
#' 0 \le d1 \le...\le dn}.
#'
#' Given a rule \eqn{R}, consider the function \eqn{J} that assigns to each \eqn{E\in (0,D]}{0<E\le D}
#' the value \eqn{J(E)=I(R,E,d)}, that is, the proportionality deviation index of the rule \eqn{R} for the problem \eqn{(E,d)}.
#' The graph of \eqn{J} is the index path of \eqn{R} for the vector of claims \eqn{d}.
#'
#' The index path is a simple tool to visualize the discrepancy of the divisions
#' recommended by a rule for a vector of claims with respect to the proportional distribution and to compare it with other rules.
#' @seealso \link{proportionalityindex}, \link{cumulativecurve}, \link{giniindex}, \link{lorenzcurve}, \link{lorenzdominance}, \link{PRO}.
#' @examples
#' d=c(2,4,7,8)
#' Rules=c(Talmud,RA,AA)
#' col=c("red","green","blue")
#' indexpath(d,Rules,col)
#' @references Mirás Calvo, M.A., Núñez Lugilde, I., Quinteiro Sandomingo, C., and Sánchez-Rodríguez, E. (2021). Deviation from proportionality and Lorenz-dominance between
#' the average of awards and the standard rules for claims problems. Working paper 2021-01, ECOBAS.
#' @references Thomson, W. (2019). How to divide when there isn't enough. From Aristotle, the Talmud, and Maimonides to the axiomatics of resource allocation. Cambridge University Press.
#' @importFrom graphics points
#' @importFrom graphics lines
#' @importFrom graphics legend
#' @importFrom graphics axis
#' @importFrom graphics abline
#' @export
indexpath = function(d, Rules, col = NULL, points = 201, legend = TRUE) {

  # Default colors
  if (is.null(col)) {
    col=c("red","blue","green","yellow","pink","coral4","darkgray","burlywood3","black","darkorange","darkviolet")
  }
  #
  n = length(d) #The number of claims
  D = sum(d) #The sum of claims
  ########################################
  # Required: d must be a claims vector.
  ########################################
  if (sum((d < 0)) > 0)
    stop('d is not a claims vector.',call.=F)

  ###################
  do = sort(d)
  if (sum(do == d) < n) {
    message('The path is shown for the rearranged vector of claims.\n')
  }
  #The endowment domain
  endowms = c(seq(0, D/2, length.out = ceiling(points/2)),seq(D/2, D, length.out = ceiling(points/2)))
  #Names of the rules for the legend
  numberrules = length(Rules)
  Rulechar=rep(0,numberrules)
  for(i in 1:numberrules){
    Rulechar[i]=Rules[[i]](0,0,name=TRUE)
  }
  #The growth of the index for the rules.
  f=do/D
  H=c()
  for(j in 1:numberrules){
    G=c()
    for(E in endowms){
      regla=Rules[[j]](E,d)
      Y=cumsum(regla)/E
      Y0=c(0,Y[1:n-1])
      G=c(G,1-f%*%t(t(Y0+Y)));
    }
    H=rbind(H,G)
    }
W=c(min(H,na.rm=TRUE)-0.05,max(H,na.rm=TRUE)+0.05)
# The index path for the proportional rule
plot(
  c(0,D),
  c(0,0),
  type = "l",
  xlim = c(0, D),
  ylim= W,
  xaxt="n",
  main = c("Proportionality index path"),
  xlab = paste("d=(",toString(d),")",sep=""),
  ylab = ""
)
# Tickmarks and grid
axis(side=1,at=c(0,do[1],do[n],D/2,D-do[n],D-do[1],D))
abline(a = NULL, b = NULL, v = c(0,do[1],do[n],D/2,D-do[n],D-do[1],D), lty="dotted")
grid(nx=0,ny=NULL)
#The index paths for the selected rules
for(i in 1:numberrules){
  lines(endowms,H[i,],col=col[i])
}
#The legend
if (legend==TRUE){
  legend(x = "topright",
         legend = c(Rulechar),
         col = col[1:numberrules],
         lty=1,
         lwd=2,
         seg.len=0.5,
         cex=0.8,
         y.intersp=0.6,
         bty = "n"
         )
}
}
