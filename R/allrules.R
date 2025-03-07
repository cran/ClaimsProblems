#' @title Summary of the division rules
#' @description This function returns the awards vectors selected, for a given claims problem, by the rules:  AA, APRO, CE, CEA, CEL, AV, DT, MO, PIN, PRO, RA, Talmud, and RTalmud.
#' @param E The endowment.
#' @param d The vector of claims.
#' @param draw A logical value.
#' @param col The colours (useful only if \code{draw=TRUE}). If \code{col=NULL} then the sequence of default colours is:
#' c("red", "blue", "green", "yellow", "pink", "orange", "coral4", "darkgray", "burlywood3", "black", "darkorange", "darkviolet","darkgreen").
#' @return A data-frame with the awards vectors selected by the main division rules. If \code{draw = TRUE}, it
#' displays a mosaic plot representing the data-frame.
#' @details Let \eqn{N=\{1,\ldots,n\}} be the set of claimants, \eqn{E\ge 0} the endowment to be divided and \eqn{d\in \mathbb{R}_+^N} the vector of claims
#' such that \eqn{\sum_{i \in N} d_i\ge E}.
#'
#' A vector \eqn{x=(x_1,\dots,x_n)} is an awards vector for the claims problem \eqn{(E,d)} if \eqn{0\le x \le d}
#' and satisfies the balance requirement, that is, \eqn{\sum_{i=1}^{n}x_i=E}.
#'
#' A rule is a function that assigns to each claims problem \eqn{(E,d)} an awards vector.
#' The formal definitions of the main rules are given in the corresponding function help.
#' @seealso \link{AA}, \link{APRO}, \link{axioms}, \link{CD}, \link{CE}, \link{CEA}, \link{CEL}, \link{AV}, \link{DT}, \link{MO}, \link{PIN}, \link{PRO},  \link{RA},  \link{Talmud}, \link{RTalmud}, \link{verticalruleplot}.
#' @examples
#' E=10
#' d=c(2,4,7,8)
#' allrules(E,d)
#' @references Mirás Calvo, M.A., Quinteiro Sandomingo, C., and Sánchez-Rodríguez, E. (2022). The average-of-awards rule for claims problems. Social Choice and Welfare 59, 863-888.
#' @references Thomson, W. (2019). How to divide when there isn't enough. From Aristotle, the Talmud, and Maimonides to the axiomatics of resource allocation. Cambridge University Press.
#' @importFrom graphics mosaicplot
#' @export

allrules = function(E, d, draw = TRUE, col = NULL) {
  n = length(d) #The number of claimants
  D = sum(d) # The sum of claims
  ########################################
  # Required: (E,d) must be a claims problem, i.e., E >=0, d >=0, E <= sum(d)
  ########################################
  if (E < 0 || sum((d < 0)) > 0 || E > D)
    stop('(E,d) is not a claims problem.')

  ######### THE 13 RULES ###################
  CEL = CEL(E, d)
  CE = CE(E, d)
  MO = MO(E, d)
  PRO = PRO(E, d)
  APRO = APRO(E, d)
  RA = RA(E, d)
  AA = AA(E, d)
  Talmud = Talmud(E, d)
  PIN = PIN(E, d)
  CEA = CEA(E, d)
  AV = AV(E,d)
  DT = DT(E, d)
  RTalmud = RTalmud(E, d)


  ##########  THE TABLE ######################
  names = c("AA", "APRO", "CE", "CEA", "CEL", "AV", "DT", "MO", "PIN", "PRO", "RA","Talmud","RTalmud")
  table = data.frame(rbind(AA, APRO, CE, CEA, CEL, AV, DT, MO, PIN, PRO, RA, Talmud, RTalmud))
  claimants = rep(0, n)
  for (i in 1:n) {
    claimants[i] = c(toString(i, 0))
  }
  colnames(table) = c(claimants)
  if (draw == TRUE){
  ###########  THE MOSAIC GRAPH ################
    if (is.null(col)) {
      col=c("red","blue","green","yellow","pink","coral4","orange","darkgray","burlywood3","black","darkorange","darkviolet","darkgreen")
    }
  mosaicplot(table,main="",color=col)
  }

  return(table)
}
