#' CO2 emissions (kt) data
#' @name CO2emissions
#' @docType data
#' @description
#' CO2 emissions (kt) data from different countries and regions in 2014
#'
#' @usage
#' CO2emissions
#' @format A data frame with 20 rows and 2 variables:
#' \tabular{rlll}{
#' [,1] \tab \code{Region}   \tab categorical \tab Country or Region name \cr
#' [,2] \tab \code{Emissions}  \tab numeric \tab CO2 emissions (kt)\cr
#' }
#' @references Mirás Calvo, M.A., Núñez Lugilde, I., Quinteiro Sandomingo, C., and Sánchez-Rodríguez, E. (2024). An algorithm to compute the average-of-awards rule for claims problems with an application to
#' the allocation of CO\eqn{_2} emissions. Annals of Operations Research 336, 1435-1459.
#' @source The data were obtained from Climate Change Data, World Bank Group \url{https://climateknowledgeportal.worldbank.org/}
#' @examples
#' data(CO2emissions)
#' head(CO2emissions)
#' E=31284288 #Emissions for 2015
#' allrules(E,CO2emissions$Emissions)
#' @examples
#' par(mfrow = c(2, 3))
#' E0 <- 33857455
#' Rules=c(Talmud,CEA,CEL,PRO)
#' percentage = 0.076
#' times = 20
#' for (claimant in 1:6) {
#' dynamicplot(E0, CO2emissions$Emissions, Rules, claimant, percentage, times, legend=FALSE)
#' }
"CO2emissions"



