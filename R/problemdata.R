#' @title Claims problem data
#' @description The function returns which of the following subdomains the claims problem belongs to:  the lower-half, higher-half, and  midpoint domains. In addittion, the function returns
#' the minimal rights vector, the truncated claims vector, the sum and the half-sum of claims.
#' @param E The endowment.
#' @param d The vector of claims.
#' @param draw A logical value.
#' @return The minimal rights vector; the truncated claims vector; the sum, the half-sum of the claims, and the class (lower-half, higher-half, and midpoint domains) to which the claims problem belongs. It returns \code{cod  = 1} if the claims problem belong to the lower-half domain, \code{cod  =  -1}  if it belongs to the higher-half domain, and \code{cod = 0}  for the midpoint domain.  Moreover, if  \code{draw = TRUE} a plot of the claims, from small to large in the interval [0,D], is given.
#' @details Let \eqn{N=\{1,\ldots,n\}} be the set of claimants, \eqn{E\ge 0} the endowment to be divided and \eqn{d\in \mathbb{R}_+^N} the vector of claims
#' such that \eqn{D=\sum_{i \in N} d_i\ge E}.
#'
#' The lower-half domain is the subdomain of claims problems for which the endowment is less or equal than the half-sum of claims, \eqn{E \le  D/2}.
#'
#' The higher-half domain is the subdomain of claims problems for which the endowment is greater or equal than the half-sum of claims, \eqn{E \ge  D/2}.
#'
#' The midpoint domain is the subdomain of claims problems for which the endowment is equal to the half-sum of claims, \eqn{E =  D/2}.
#'
#' The minimal right of claimant \eqn{i\in N} in \eqn{(E,d)} is whatever is left after every other claimant has received his claim, or 0 if that is not possible:
#' \deqn{m_i(E,d)=\max\{0,E-d(N\backslash\{i\})\}.}
#' Let \eqn{m(E,d)=\Bigl(m_1(E,d),\dots,m_n(E,d)\Bigr)} be the vector of minimal rights.
#'
#' The truncated claim of claimant \eqn{i\in N} in \eqn{(E,d)} is the minimum of the claim and the endowment:
#' \deqn{t_i(E,d)=\min\{d_i,E\}.}
#' Let \eqn{t(E,d)=\Bigl(t_1(E,d),\dots,t_n(E,d)\Bigr)} be the vector of truncated claims.
#'
#' @seealso \link{allrules}, \link{setofawards}.
#' @examples
#' E=10
#' d=c(2,4,7,8)
#' problemdata(E,d,draw=TRUE)
#' @export
problemdata = function(E,d,draw=FALSE){

  ########################################
  # Required: (E,d) must be a claims problem, i.e., E >=0, d >=0, E <= sum(d)
  ########################################
  n= length(d); D=sum(d) #The number of claims and the total claim
  if (E < 0 || sum((d < 0)) > 0 || E > D)
    stop('(E,d) is not a claims problem.',call.=F)

  ###################

  t = rep(0,n)
  Dh = D/2
  m = E-D+d #Minimal rights
  m[m<0]=0

  for(i in 1:n){#Truncated claims
    t[i]=min(E,d[i]);
  }

  if(E<Dh){
    cod=1
  } else if(E>Dh){
    cod=-1

  } else {
    cod=0
  }


  if (draw==TRUE){

  #Graph with the claims on the ascending order.
  dcrece=sort(d,index.return=T)$x


  Ordencrece=sort(d,index.return=T)$ix
  Dm=D-dcrece
  inter=sort(c(0,dcrece,Dh,Dm,D))
  plot(
    0,
    0,
    type = "n",
    xlim = c(0,D),
    # ylim = c(-1,0),
    main = c("Claims in ascending order on the interval [0,D]"),
    xlab = paste("E=",as.character(E),"; d=[",toString(dcrece),"]",sep=""),
    ylab = "",axes=F
  )
  lines(c(0,D),c(0,0),col="blue")
  for(i in 1:length(inter)){
    points(inter[i],0,pch=3)
    text(inter[i],-0.15,toString(inter[i]),font=5)
  }
  text(0,-0.4,"0",font=3)
  text(max(d),-0.4,"dn",font=3)
  text(Dh,-0.4,"D/2",font=3)
  text(min(Dm),-0.4,"D-n",font=3)
  text(min(D),-0.4,"D",font=3)
  points(E,0,pch=8,col="orange")
  lines(c(E,E),c(0,0.3),col="orange")
  text(E,0.4,"E",font=3,col="orange")
}
####
  return(list(minimal_rights=m,truncated_claims=t,sum_claims=D,half_sum_claims=Dh,cod_domain=cod))
}

