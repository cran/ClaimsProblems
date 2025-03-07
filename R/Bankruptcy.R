#' Bankruptcy data
#' @name Bankruptcy
#' @docType data
#' @description
#' Bankruptcy data: creditors and claims
#' @usage
#' Bankruptcy
#' @format A data frame with 8 rows and 2 variables:
#'
#'
#' \tabular{rlll}{
#' [,1] \tab \code{Creditor}   \tab categorical \tab Creditor name \cr
#' [,2] \tab \code{Claim}  \tab numeric \tab Claim (millions of euro)\cr
#' }
#' @references Fiestras-Janeiro, M.G., Sánchez-Rodríguez, E., and Schuster, M. (2016). A precedence constraint value revisited. Top, 25, 156–179
#' @source The data were obtained from Fiestras et al (2016)
#' @examples
#' data(Bankruptcy)
#' Bankruptcy
#' E= 230
#' allrules(E,Bankruptcy$Claim)
"Bankruptcy"



