#' @title Volume of the set of awards vectors
#' @description This function computes the volume of the set of award vectors of a claims problem and the projected volume.
#' @param E The endowment.
#' @param d The vector of claims.
#' @param real Logical parameter. By default, \code{real = TRUE}.
#' @return The volume of the set of awards vectors. If \code{real = FALSE}, it returns the volume of the projection into the last coordinate.
#' @details Let \eqn{N=\{1,\ldots,n\}} be the set of claimants, \eqn{E\ge 0} the endowment to be divided and \eqn{d\in \mathbb{R}_+^N} the vector of claims
#' such that \eqn{\sum_{i \in N} d_i\ge E}.
#'
#' A vector \eqn{x=(x_1,\dots,x_n)} is an awards vector for the claims problem \eqn{(E,d)} if \eqn{0\le x \le d}
#' and satisfies the balance requirement, that is, \eqn{\sum_{i=1}^{n}x_i=E}.
#' Let \eqn{X(E,d)} be the set of awards vectors for \eqn{(E,d)}.
#'
#' Let \eqn{\mu} be the \eqn{(n-1)}-dimensional Lebesgue measure. We define by \eqn{V(E,d)=\mu (X(E,d))} the
#' measure (volume) of the set of awards \eqn{X(E,d)} and \eqn{\hat{V}(E,d)} the volume of the projection onto an (\eqn{n-1)}-dimensional space. 
#' \deqn{V(E,d)=\sqrt{n}\hat{V}(E,d).}
#'
#' The function is programmed following the procedure explained in Mirás Calvo et al. (2024b).
#'
#' @seealso \link{setofawards}.
#' @examples
#' E=10
#' d=c(2,4,7,10)
#' volume(E,d)
#' #The volume function is a symmetric function.
#' D=sum(d)
#' volume(D-E,d)
#' @references Mirás Calvo, M.A., Núñez Lugilde, I., Quinteiro Sandomingo, C., and Sánchez-Rodríguez, E. (2024a). An algorithm to compute the average-of-awards rule for claims problems with an application to
#' the allocation of CO\eqn{_2} emissions. Annals of Operations Research, 336: 1435-1459.
#' @references Mirás Calvo, M.A., Núñez Lugilde, I., Quinteiro Sandomingo, C., and Sánchez-Rodríguez, E. (2024b). On properties of the set of awards vectors for a claims problem. TOP, 32: 137-167.
#' @export

volume=function(E,d,real=TRUE){
  n=length(d);
  D=sum(d)
  factor=sqrt(n)/factorial(n-1)
  ########################################
  # Required: (E,d) must be a claims problem, i.e., E >=0, d >=0, E <= sum(d)
  ########################################
  if (E < 0 || sum((d < 0)) > 0 || E > D)
    stop('(E,d) is not a claims problem.\n',call.=F)
  #####################
  # Sort the claims in ascending order
  do=sort(d,index.return=T)$x

  ############ Trivial cases #############
  ######TWO CLAIMANTS#################
  if (n==2){
    v1=max(0,E-do[2])
    v2=max(0,E-do[1])
    vol=(E-v1-v2)*factor
    if(real==FALSE){
      vol=vol/sqrt(n)
    }
    return(vol)}

  Dn=D-do[n] #D_{-n}
  if (E==0 || sum(d==0)>0){# Null endowment or any claims zero
    vol=0;
    return(vol)
  }else if(E>=Dn & E<=do[n]){#When D-do(n)<=E <= do(n)
    vol=sqrt(n)*prod(do[1:n-1])
    if(real==FALSE){
      vol=vol/sqrt(n)
    }
    return(vol)
  }else if(E<do[1]){#When E<min(d). The set of awards coincides with the imputation set of the associated game.
    vol=E^(n-1)*factor
    if(real==FALSE){
      vol=vol/sqrt(n)
    }
    return(vol)
  }
  ###############
  ##CASE E=D/2###
  ###############
  if(E==D/2){
    vol= (prod(do[1:(n-1)])*factorial(n-1))*factor;#Hyperrectangle of the product of the claims.
    if (do[n]<E){
      PesoC=Peso(E-do[n],c(do[1:n-1],E-do[n]))*factor#Problems Pn and P*n coincide.
      #We compute their volume with de decomposition of the imputation set (Peso.R function).
      vol=(vol-2*PesoC)
    }
    if(real==FALSE){
      vol=vol/sqrt(n)
    }
    return(vol)}

  ########################
  #######SELF-DUALITY#####
  ########################
  ###### When E>D/2, we apply self-duality
  control =as.numeric (E>D/2)
  if ( control ==1){E=D-E}

  ########################
  #####IMPUTATION SET#####
  ########################
  if(E<Dn/2){#E>do(1), E<D/2, and E<Dn/2
    # Then , the minimum rights are all zero .
    # In this case, we decompose the imputation set.
    vol=Peso(E,do)*factor#We compute its volume with de decomposition of the imputation set (Peso.R function).
    if(real==FALSE){
      vol=vol/sqrt(n)
    }
    return(vol)}else if(E==Dn/2 & E<=do[n]){#Direct computation.
      vol= (prod(do[1:(n-1)])*factorial(n-1)/2)*factor
      if(real==FALSE){
        vol=vol/sqrt(n)
      }
      return(vol)}else{#E>do(1), E<D/2, and E>Dn/2
        # In this case we decompose the hyperrectangle of the product of the claims.
        if(do[n]<E){#Problem Pn. (E-dn,(d_{-n},E-dn))
          #We compute its volume with de decomposition of the imputation set (Peso.R function).
          PesoB=Peso(E-do[n],c(do[1:n-1],E-do[n]))
        }else{PesoB=0}
        if(D-do[n]>E){##Problem P*n. (D_{-n},(d_{-n},D_{-n}-E))
          #We compute its volume with de decomposition of the imputation set (Peso.R function).
          PesoA=Peso(Dn-E,c(do[1:n-1],Dn-E))
        }else{PesoA=0}
        #The volume of the set of awards ir the volume of the hyperrectangle of the product of the claims
        #minus the volume of problems Pn and P*n.
        vol = (factorial(n-1)*prod(do[1:n-1])-(PesoA+PesoB))*factor;
        if(real==FALSE){
          vol=vol/sqrt(n)
        }
        return(vol)}
}
