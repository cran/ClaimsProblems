#' @title Concede-and-divide rule
#' @description This function returns the awards vector assigned by the concede-and-divide (CD) rule to a two-claimant problem.
#' @param E The endowment.
#' @param d The vector of two claims.
#' @param name A logical value.
#' @return The awards vector selected by the CD rule. If \code{name = TRUE}, the name of the function (CD) as a character string.
#' @details Let \eqn{E\ge 0} be the endowment to be divided and \eqn{d=(d_1,d_2)\in \mathbb{R}_+^2}
#'  the vector of claims such that  \eqn{d_1+d_2 \ge E}.
#'
#' The concede-and-divide rule (CD) first assigns to each of the two claimants the difference between the endowment and the other agent’s claim (or 0 if this
#' difference is negative), and divides the remainder equally. That is, for each \eqn{i\in \{1,2\}},
#' \deqn{\text{CD}_i(E,d)=\max\{E-d_j,0\}+\frac{E-\max\{E-d_i,0\}-\max\{E-d_j,0\}}{2}.}
#' Several rules are extensions of the concede-and-divide rule to general populations: AA, APRO, MO, RA, and Talmud.
#' @seealso \link{allrules}, \link{AA}, \link{APRO}, \link{MO}, \link{pathawards}, \link{RA}, \link{Talmud}.
#' @examples
#' E=10
#' d=c(7,8)
#' CD(E,d)
#' # Talmud, RA, MO, APRO, and AA coincide with CD for two-claimant problems
#' Talmud(E,d)
#' RA(E,d)
#' MO(E,d)
#' APRO(E,d)
#' AA(E,d)
#' @references Aumann, R. and Maschler, M., (1985). Game theoretic analysis of a bankruptcy problem from the Talmud. Journal of Economic Theory 36, 195–213.
#' @references Mirás Calvo,M.Á., Núñez Lugilde, I., Quinteiro Sandomingo, C., and Sánchez Rodríguez,E. (2025). On how the rules that extend the concede-and-divide principle
#' differ for pairs of claimants. Preprint.
#' @references Thomson, W. (2019). How to divide when there isn't enough. From Aristotle, the Talmud, and Maimonides to the axiomatics of resource allocation. Cambridge University Press.
#' @export

CD = function (E, d, name = FALSE) {
  if (name == TRUE) {
    rule = "CD"
    return(rule)
  }
  ########################################
  # Required: (E,d) must be a claims problem with two claimants
  ########################################
  n = length(d)
  D = sum(d) #The number of claims and the total claim
  if (E < 0 || sum((d < 0)) > 0 || E > D)
    stop('(E,d) is not a claims problem.',call.=F)

  if (n != 2)
    stop('The concede-and-divide rule is defined for two claimants.',call.=F)

  ########################################
  # Claims in ascending order: permutation P
  do = sort(d, index.return = T)$x
  ordenI = sort (d, index.return = T)$ix #The permutation of the claims
  orden = sort(ordenI, index.return = T)$ix #The inverse permutation

  ######## THE CD RULE ###################
  rule =
    c(E / 2, E / 2) * as.numeric (E <= do[1]) + c(do[1] / 2, E - do[1] /
                                                    2) *
    as.numeric (do[1] < E & E <= do[2]) +
    c((E + do[1] - do[2]) / 2, ((E - do[1] + do[2]) / 2)) * as.numeric (E >
                                                                          do[2])
  rule = rule[orden] #Reorder the awards
  return(rule)
}
