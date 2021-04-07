#' @title Summary of the division rules
#' @description This function returns the awards vectors selected, for a given claims problem, by the rules:  AA, APRO, CE, CEA, CEL, DT, MO, PIN, PRO, RA, and Talmud.
#' @param E The endowment.
#' @param d The vector of claims.
#' @param draw A logical value.
#' @return A data-frame with the awards vectors selected by the main division rules. If draw = TRUE,
#' a bar chart for each claimant representing the amounts that the claimant receives.
#' @details  Let \eqn{E\ge 0} be the endowment to be divided and \eqn{d\in \mathcal{R}^n}{%
#' d} the vector of claims with \eqn{d\ge 0} and such that \eqn{\sum_{i=1}^{n} d_i\ge E,\ }{}
#' the sum of claims exceeds the endowment.
#'
#' A vector \eqn{x=(x_1,\dots,x_n)}{x=(x1,...,xn)} is an awards vector for the claims problem \eqn{(E,d)} if:
#' no claimant is asked to pay (\eqn{0\le x});
#' no claimant  receives more than his claim (\eqn{x\le d});
#' and the balance requirement is satisfied, that is, the sum of the awards is equal to the endowment (\eqn{\sum_{i=1}^{n} x_i= E}{x1+...+xn=E}).
#'
#' A rule is a function that assigns to each claims problem \eqn{(E,d)} an awards vector for \eqn{(E,d)},
#' that is, a division between the claimants of the amount available.
#'
#' The formal definitions of the main rules are given in the corresponding function help.
#' @seealso \link{AA}, \link{APRO}, \link{CD}, \link{CE}, \link{CEA}, \link{CEL}
#' , \link{DT}, \link{MO}, \link{PIN}, \link{PRO}, \link{RA}, \link{Talmud},  \link{verticalruleplot}
#' @examples
#' E=10
#' d=c(2,4,7,8)
#' allrules(E,d)
#' @references Mirás Calvo, M.A., Quinteiro Sandomingo, C., and Sánchez-Rodríguez, E. (2020). The core-center rule for the bankruptcy problem. Working paper 2020-02, ECOBAS.
#' @references Thomson, W. (2019). How to divide when there isn't enough. From Aristotle, the Talmud, and Maimonides to the axiomatics of resource allocation. Cambridge University Press.
#' @importFrom graphics par
#' @importFrom graphics barplot
#' @export

allrules = function(E, d, draw = TRUE) {
  n = length(d) #The number of claimants
  D = sum(d) # The sum of claims
  ########################################
  # Required: (E,d) must be a claims problem, i.e., E >=0, d >=0, E <= sum(d)
  ########################################
  if (E < 0 || sum((d < 0)) > 0 || E > D)
    stop('(E,d) is not a claims problem.')

  ######### THE 11 RULES ###################
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
  DT = DT(E, d)

  ##########  THE TABLE ######################
  names = c("AA", "APRO", "CE", "CEA", "CEL", "DT", "MO", "PIN", "PRO", "RA","Talmud")
  table = data.frame(rbind(AA, APRO, CE, CEA, CEL, DT, MO, PIN, PRO, RA,Talmud))
  claimants = rep(0, n)
  for (i in 1:n) {
    claimants[i] = c(toString(i, 0))
  }
  colnames(table) = c(claimants)
  if (draw == TRUE){
  ###########  THE BAR GRAPH ################
  if(n==2){
    par(mfrow = c(2, 1))
    for (i in 1:n) {
      barplot(table[, i],
              names.arg = names,
              main = paste("Claimant", i))
    }
  } else if(n==3|n==4){
    par(mfrow = c(2, 2))
    for (i in 1:n) {
      barplot(table[, i],
              names.arg = names,
              main = paste("Claimant", i))
    }
  } else if(n==5|n==6){
    par(mfrow = c(3, 2))
    for (i in 1:n) {
      barplot(table[, i],
              names.arg = names,
              main = paste("Claimant", i))
    }
  } else if(n==7|n==8|n==9){
    par(mfrow = c(3,3))
    for (i in 1:n) {
      barplot(table[, i],
              names.arg = names,
              main = paste("Claimant", i))
    }
  } else {
    par(mar = rep(3, 4))
    par(mfrow = c(3, 4))

    for (i in 1:n) {
      barplot(table[, i],
              names.arg = names,
              main = paste("Claimant", i))
                  }
        }
  }
  on.exit(par(mfrow = c(1, 1)))

  return(table)
}
