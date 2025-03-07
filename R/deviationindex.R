#' @title Deviation index
#' @description This function returns the deviation index and the signed deviation index for a rule with respect to another rule.
#' @param E The endowment.
#' @param d The vector of claims.
#' @param R A rule : AA, APRO, CE, CEA, CEL, AV, DT, MO, PIN, PRO, RA, Talmud, RTalmud.
#' @param S A rule: AA, APRO, CE, CEA, CEL, AV, DT, MO, PIN, PRO, RA, Talmud, RTalmud.
#' @return The deviation index and the signed deviation index of a rule for a claims problem.
#' @details Let \eqn{N=\{1,\ldots,n\}} be the set of claimants, \eqn{E\ge 0} the endowment to be divided and \eqn{d\in \mathbb{R}_+^N} the vector of claims
#' such that \eqn{\sum_{i \in N} d_i\ge E}.
#'
#' Rearrange the claims from small to large, \eqn{0 \le d_1 \le...\le d_n}.
#' The signed deviation index of the rule \eqn{\mathcal{S}} with respect to the rule \eqn{\mathcal{R}} for the problem \eqn{(E,d)}, denoted by \eqn{I(\mathcal{R}(E,d),\mathcal{S}(E,d))}, is
#' the ratio of the area that lies between the identity line and the cumulative curve over the total area under the identity line.
#'
#' Let \eqn{\mathcal{R}_0=0} and \eqn{\mathcal{S}_0=0}. For each \eqn{k=1,\dots,n} define
#' \eqn{X_k=\frac{1}{E} \sum_{j=0}^{k}\mathcal{R}_j} and \eqn{Y_k=\frac{1}{E} \sum_{j=0}^{k} \mathcal{S}_j}. Then,
#' \deqn{I(\mathcal{R}(E,d),\mathcal{S}(E,d))=1-\sum_{k=1}^{n}\Bigl(X_{k}-X_{k-1}\Bigr)\Bigl(Y_{k}+Y_{k-1}\Bigr).}
#'  In general  \eqn{-1 \le I(\mathcal{R}(E,d),\mathcal{S}(E,d)) \le 1}.
#'
#' The deviation index of the rule \eqn{\mathcal{S}} with respect to the rule \eqn{\mathcal{R}} for the problem \eqn{(E,d)}, denoted by \eqn{I^{+}(\mathcal{R}(E,d),\mathcal{S}(E,d))}, is
#' the ratio of the area between the line of the cumulative sum of the distribution proposed by the rule \eqn{\mathcal{R}} and the cumulative curve over the area under the line \eqn{x=y}.
#'
#' In general  \eqn{0 \le I^{+}(\mathcal{R}(E,d),\mathcal{S}(E,d)) \le 1}.
#'
#' The proportionality deviation index is the deviation index when \eqn{\mathcal{R} = \text{PRO}}. The proportionality deviation index of the proportional rule is zero for all claims problems.
#' The signed proportionality deviation index is the signed deviation index with \eqn{\mathcal{R} = \text{PRO}}.
#'
#' @seealso \link{allrules}, \link{cumawardscurve}, \link{giniindex}, \link{indexgpath}, \link{lorenzcurve}, \link{lorenzdominance}.
#' @examples
#' E=10
#' d=c(2,4,7,8)
#' R=CEA
#' S=AA
#' deviationindex(E,d,R,S)
#' #The deviation index of rule R with respect of the rule R is 0.
#' deviationindex(E,d,PRO,PRO)
#' @references  Ceriani, L. and Verme, P. (2012). The origins of the Gini index: extracts from Variabilitá e Mutabilitá (1912) by Corrado Gini. The Journal of Economic Inequality 10(3), 421-443.
#' @references Mirás Calvo, M.Á., Núñez Lugilde, I., Quinteiro Sandomingo, C., and Sánchez Rodríguez, E. (2023). Deviation from proportionality and Lorenz-domination for claims problems. Review of Economic Design 27, 439-467.
#' @export


deviationindex = function(E,d,R,S) {
  n = length(d)
  D = sum(d) #The number of claims and the sum of the claims

  ##############################################################
  # Required: (E,d) must be a claims problem, i.e., E >0, d >0,
  #E < sum(d) and the claims vector must be in increasing order
  ##############################################################
  do = sort(d)
  if (sum((d < 0)) > 0)
    stop('d is not a claims vector.',call.=F)


  if (E < 0 || E > D) {
    stop('(E,d) is not a claims problem.',call.=F)
  } else if (E == 0) {
    #claims index for the rule is zero
    stop('We can not compute the deviation index if E=0.',call.=F)
  }else {
      #claims index for the rule and for the claims
      x = R(E, do)
      y = S(E, do)
      crule1=c(0,cumsum(x)/E)
      crule2=c(0,cumsum(y)/E)
      if(sum(crule1>=crule2)==(n+1)){
        index = (1-1/E*(sum(x*y)/E+2/E* sum(y*(E-cumsum(x)))))
      }else if(sum(crule1<=crule2)==(n+1)){
        index = -(1-1/E*(sum(x*y)/E+2/E* sum(y*(E-cumsum(x)))))
      }else{
        alpha=rep(0,n+1)
        alpha[crule1>crule2]=-1
        alpha[crule1<crule2]=1
        index=0
        for(i in 2:(n+1)){
          if(alpha[i-1]>=0 & alpha[i]>=0){
            index=index+(crule2[i]-crule2[i-1])*(crule2[i-1]-crule1[i-1]+crule2[i]-crule1[i])
          }else if(alpha[i-1]<=0 & alpha[i]<=0){
            index=index+(crule2[i]-crule2[i-1])*(crule1[i-1]-crule2[i-1]+crule1[i]-crule2[i])
          }else if(alpha[i-1]==1 & alpha[i]==-1){
            z=(crule2[i]*crule1[i-1]-crule2[i-1]*crule1[i])/((crule2[i]-crule2[i-1])-(crule1[i]-crule1[i-1]))
            index=index+(z-crule2[i-1])*(crule2[i-1]-crule1[i-1])+(crule2[i]-z)*(crule1[i]-crule2[i])
          }else if(alpha[i-1]==-1 & alpha[i]==1){
            z=(crule2[i]*crule1[i-1]-crule2[i-1]*crule1[i])/((crule2[i]-crule2[i-1])-(crule1[i]-crule1[i-1]))
            index=index+(z-crule2[i-1])*(crule1[i-1]-crule2[i-1])+(crule2[i]-z)*(crule2[i]-crule1[i])
          }
        }
      }
      index_signed = 1-(sum(x*y)+2*sum(y*(E-cumsum(x))))/E^2
      if (sum(do == d) < n)
        message('The deviation index is computed by rearranging the claims in increasing order.\n')
      return(list(index=index,index_signed=index_signed))
    }

}
