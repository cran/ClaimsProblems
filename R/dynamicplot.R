#' @title Dynamic plot
#' @description For each claimaint, it plots the awards of the chosen rules for a dynamic model with t periods.
#' @param E The endowment.
#' @param d The vector of claims
#' @param Rules The rules: AA, APRO, CE, CEA, CEL, DT, MO, PIN, PRO, RA, Talmud.
#' @param claimant A claimant.
#' @param percentage A number in (0,1).
#' @param times Number of iterations.
#' @param col The colours. If col=NULL then the sequence of default colours is:
#' c("red", "blue", "green", "yellow", "pink", "coral4", "darkgray", "burlywood3", "black", "darkorange", "darkviolet").
#' @param legend A logical value. The colour legend is shown if legend=TRUE.
#' @return This function represents the awards proposed by different rules for a claimant if the resource decreases in each iteration by a given percentage.
#' @details Let \eqn{E\ge 0} be the endowment to be divided and \eqn{d\in \mathcal{R}^n}{%
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
#'
#' Given \eqn{l} a natural number, the function solves each claims problem in time \eqn{t}, which is \eqn{(E_t,d)}{(Et,d)}, with \eqn{E_t=(1-p)^t E}{Et=E(1-p)^t}, \eqn{p} \eqn{\in}{ in } \eqn{(0,1)} and \eqn{t=1,\ldots,l}.
#'
#' @seealso \link{allrules}, \link{pathawards}, \link{pathawards3}, \link{schedrule}, \link{schedrules}
#' @examples
#' E=10
#' d=c(2,4,7,8)
#' Rules=c(Talmud,RA,AA,PRO)
#' claimant=1
#' percentage=0.076
#' times=10
#' dynamicplot(E,d,Rules,claimant,percentage,times)
#' @references Thomson, W. (2019). How to divide when there isn't enough. From Aristotle, the Talmud, and Maimonides to the axiomatics of resource allocation. Cambridge University Press.
#' @references Mirás Calvo, M.Á., Núñez Lugilde, I., Quinteiro Sandomingo, C., and Sánchez-Rodríguez, E. (2020). An algorithm to compute the core-center rule of a claims problem with an application to
#' the allocation of CO2 emissions. Working paper.
#' @importFrom graphics matplot
#' @importFrom graphics matlines
#' @importFrom graphics grid
#' @importFrom graphics legend
#' @export
dynamicplot= function(E,d, Rules, claimant, percentage, times, col = NULL, legend = TRUE) {
  n = length(d) #The number of claimants
  D = sum(d) # The sum of claims
  ########################################
  # Required: (E,d) must be a claims problem, i.e., E >=0, d >=0, E <= sum(d)
  ########################################
  if (E < 0 || sum((d < 0)) > 0 || E > D)
    stop('(E,d) is not a claims problem.')
  # Required: times>0.
  if(times<=0)
    stop('times must be a positive number')
  # Required: percentage between 0 and 1.
  if(percentage<=0||percentage>=1)
    stop('percentage must be a number between 0 and 1')
  if(claimant>n)
    stop('The chosen claimant does not exist')
  # Default colors
  if (is.null(col)) {
    col=c("red","blue","green","yellow","pink","coral4","darkgray","burlywood3","black","darkorange","darkviolet")
  }
  #Names of the rules as strings
  numberrules = length(Rules)
  table=matrix(0,numberrules,times)
  Rulechar=rep(0,numberrules)
  for(i in 1:numberrules){
    Rulechar[i]=Rules[[i]](0,0,name=TRUE)
    R=Rules[[i]](E,d)
    table[i,1]=R[claimant]
  }
  for(j in 2:times){
    E=(1-percentage)*E
    for(i in 1:numberrules){
      R=Rules[[i]](E,d)
      table[i,j]=R[claimant]
    }
  }
  #Graph of each rule
  matplot(seq(1,times),table[1,],ylim=c(min(table),max(table)),xlab="", ylab="",col = col[1],  type = "b", cex = 1, pch = 21, bg = "black",main = paste("Claimant ",toString(claimant),sep=""))
  for(i in 2:numberrules){
    matlines(seq(1,times),table[i,], type = "b", cex = 1, pch = 21, bg = "black", col = col[i])
  }
  grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 1)
  #Legend
  legend("topright", legend = c(Rulechar), lty = 2, col = col[1:numberrules])
  }

