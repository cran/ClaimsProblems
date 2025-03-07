#' @title Index path
#' @description The function returns the deviation index path or the signed deviation index path for a rule with respect to another rule for a vector of claims.
#' @param d The vector of claims.
#' @param Rule Principal Rule: AA, APRO, CE, CEA, CEL, AV, DT, MO, PIN, PRO, RA, Talmud, RTalmud. By default, \code{Rule = PRO}.
#' @param Rules The rules: AA, APRO, CE, CEA, CEL, AV, DT, MO, PIN, PRO, RA, Talmud, RTalmud.
#' @param signed A logical value. If \code{signed = FALSE}, it draws the deviation index path and, if \code{signed = TRUE} it draws the signed deviation index path. By default, \code{signed = TRUE}.
#' @param col The colours. If \code{col = NULL} then the sequence of default colours is:
#' c("red", "blue", "green", "yellow", "pink", "orange", "coral4", "darkgray", "burlywood3", "black", "darkorange", "darkviolet").
#' @param points The number of endowment values to be drawn.
#' @param legend A logical value. The legend is shown if \code{legend = TRUE}.
#' @return This function returns the deviation index path of a rule (or several rules) for a vector of claims.
#' @details Let \eqn{N=\{1,\ldots,n\}} be the set of claimants, \eqn{d\in \mathbb{R}^N} a vector of claims rearranged from small to large, \eqn{0 \le d_1 \le...\le d_n} and \eqn{D=\sum_{i\in N}d_i}.
#'
#' Given two rules \eqn{\mathcal{R}} and \eqn{\mathcal{S}}, consider the function \eqn{J} that assigns to each \eqn{E\in (0,D]}
#' the value \eqn{J(E)=I\Bigl(\mathcal{R}(E,d),\mathcal{S}(E,d)\Bigr)}, that is, the signed deviation index of the rules \eqn{\mathcal{R}} and \eqn{\mathcal{S}} for the problem \eqn{(E,d)}.
#' The graph of \eqn{J} is the signed index path of \eqn{\mathcal{S}} in function of the rule \eqn{\mathcal{R}} for the vector of claims \eqn{d}.
#'
#' Given two rules \eqn{\mathcal{R}} and \eqn{\mathcal{S}}, consider the function \eqn{J^{+}} that assigns to each \eqn{E\in (0,D]}
#' the value \eqn{J^{+}(E)=I^{+}\Bigl(\mathcal{R}(E,d),\mathcal{S}(E,d)\Bigr)}, that is, the deviation index of the rules \eqn{\mathcal{R}} and \eqn{\mathcal{S}} for the problem \eqn{(E,d)}.
#' The graph of \eqn{J^{+}} is the index path of \eqn{\mathcal{S}} in function of the rule \eqn{\mathcal{R}} for the vector of claims \eqn{d}.
#'
#' The signed index path and the index path are simple tools to visualize the discrepancy of the divisions
#' recommended by a rule for a vector of claims with respect to the divisions recommended by another rule.
#' If \eqn{\mathcal{R} = \text{PRO}}, the function draws the proportionality deviation index path or the signed proportionality deviation index path.
#'
#' @seealso  \link{allrules}, \link{cumawardscurve}, \link{deviationindex}, \link{giniindex}, \link{lorenzcurve}, \link{lorenzdominance}.
#' @examples
#' d=c(2,4,7,8)
#' Rule=PRO
#' Rules=c(Talmud,RA,AA)
#' col=c("red","green","blue")
#' indexgpath(d,Rule,Rules,signed=TRUE,col)
#' @references Ceriani, L. and Verme, P. (2012). The origins of the Gini index: extracts from Variabilitá e Mutabilitá (1912) by Corrado Gini. The Journal of Economic Inequality 10(3), 421-443.
#' @references Mirás Calvo, M.Á., Núñez Lugilde, I., Quinteiro Sandomingo, C., and Sánchez Rodríguez, E. (2023). Deviation from proportionality and Lorenz-domination for claims problems. Review of Economic Design 27, 439-467.
#' @references Thomson, W. (2019). How to divide when there isn't enough. From Aristotle, the Talmud, and Maimonides to the axiomatics of resource allocation. Cambridge University Press.
#' @importFrom graphics lines
#' @importFrom graphics legend
#' @importFrom graphics axis
#' @importFrom graphics abline
#' @export
indexgpath = function(d,Rule=PRO,Rules,signed=TRUE, col = NULL, points = 201, legend = TRUE) {

  # Default colors
  if (is.null(col)) {
    col=c("red","blue","green","yellow","pink","orange","coral4","darkgray","burlywood3","black","darkorange","darkviolet")
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
    R=Rule(0,0,name=TRUE)

  #The growth of the index for the rules.
  H=c()
  for(j in 1:numberrules){
    G=c()
    for(E in endowms[2:points]){
      if(signed==TRUE){
      G=c(G,deviationindex(E,do,Rule,Rules[[j]])$index_signed)
      }else{
      G=c(G,deviationindex(E,do,Rule,Rules[[j]])$index)};
    }
    H=rbind(H,G)
  }
  W=c(min(H,na.rm=TRUE)-0.05,max(H,na.rm=TRUE)+0.05)
  # The index path for the proportional rule
  if(signed==TRUE){
  title=paste(R," signed index path",sep="")}else{
    title=paste(R," index path",sep="")
  }
  plot(
    c(0,D),
    c(0,0),
    type = "l",
    xlim = c(0, D),
    ylim= W,
    xaxt="n",
    main = title,
    xlab = paste("d=(",toString(d),")",sep=""),
    ylab = ""
  )
  # Tickmarks and grid
  axis(side=1,at=c(0,do[1],do[n],D/2,D-do[n],D-do[1],D))
  abline(a = NULL, b = NULL, v = c(0,do[1],do[n],D/2,D-do[n],D-do[1],D), lty="dotted")
  grid(nx=0,ny=NULL)
  #The index paths for the selected rules
  for(i in 1:numberrules){
    lines(endowms[2:points],H[i,],col=col[i])
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
