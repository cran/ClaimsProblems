#' @title Piniles' rule
#' @description This function returns the awards vector assigned by the Piniles' rule (PIN) to a claims problem.
#' @param E The endowment.
#' @param d The vector of claims.
#' @param name A logical value.
#' @return The awards vector selected by the PIN rule. If \code{name = TRUE}, the name of the function (PIN) as a character string.
#' @details Let \eqn{N=\{1,\ldots,n\}} be the set of claimants, \eqn{E\ge 0} the endowment to be divided and \eqn{d\in \mathbb{R}_+^N} the vector of claims
#' such that \eqn{D=\sum_{i \in N} d_i\ge E}.
#'
#' The Piniles' rule (PIN) coincides with the constrained equal awards rule (CEA) applied to
#' the problem \eqn{(E, d/2)} if the endowment is less or equal than the half-sum of the claims, \eqn{D/2}.
#' Otherwise it assigns to each claimant \eqn{i} half of the claim, \eqn{d_i/2}, and,
#' then, it distributes the remainder with the CEA rule. Therefore, for each \eqn{i\in N},
#'
#' \deqn{\text{PIN}_i(E,d) = \begin{cases}
#' \min\{\frac{d_i}{2},\lambda\}      & \text{if } E\leq \tfrac{1}{2}D\\[3pt]
#' \frac{d_i}{2}+\min\{\frac{d_i}{2},\lambda\} & \text{if }  E \geq \tfrac{1}{2}D
#' \end{cases},}
#'
#' where \eqn{\lambda \geq 0} is chosen such that  \eqn{\underset{i\in N}{\sum}	\text{PIN}_i(E,d)=E}.
#' @seealso \link{allrules}, \link{axioms}, \link{CEA}, \link{Talmud}.
#' @examples
#' E=10
#' d=c(2,4,7,8)
#' PIN(E,d)
#' @references Piniles, H.M. (1861). Darkah shel Torah. Forester, Vienna.
#' @references Thomson, W. (2019). How to divide when there isn't enough. From Aristotle, the Talmud, and Maimonides to the axiomatics of resource allocation. Cambridge University Press.
#' @export

PIN = function(E, d, name = FALSE) {
  if (name == TRUE) {
    rule = "PIN"
    return(rule)
  }
  ########################################
  # Required: (E,d) must be a claims problem, i.e., E >=0, d >=0, E <= sum(d)
  ########################################
  n = length (d)
  D = sum(d) #The number of claims and the total claim
  if (E < 0 || sum((d < 0)) > 0 || E > D)
    stop('(E,d) is not a claims problem.',call.=F)


  ######### THE PINILES RULE ############
  if (D / 2 >= E) {
    rule = CEA(E, d / 2)

  } else {
    rule = d / 2 + CEA(E - D / 2, d / 2)

  }
  return(rule)
}
