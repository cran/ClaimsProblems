#' @title Average-of-awards rule
#' @description This function returns the awards vector assigned by the average-of-awards rule (AA) to a claims problem.
#' @param E The endowment.
#' @param d The vector of claims.
#' @param name A logical value.
#' @return The awards vector selected by the AA rule. If \code{name = TRUE}, the name of the function (AA) as a character string.
#' @details Let \eqn{N=\{1,\ldots,n\}} be the set of claimants, \eqn{E\ge 0} the endowment to be divided and \eqn{d\in \mathbb{R}_+^N} the vector of claims
#' such that \eqn{\sum_{i \in N} d_i\ge E}.
#'
#' A vector \eqn{x=(x_1,\dots,x_n)} is an awards vector for the claims problem \eqn{(E,d)} if \eqn{0\le x \le d}
#' and satisfies the balance requirement, that is, \eqn{\sum_{i=1}^{n}x_i=E}.
#' Let \eqn{X(E,d)} be the set of awards vectors for \eqn{(E,d)}.
#'
#' The average-of-awards rule (AA) assigns to each claims problem \eqn{(E,d)}
#' the expectation of the uniform distribution defined over the set of awards vectors, that is,
#' the centroid of \eqn{X(E,d)}.
#'
#' Let \eqn{\mu} be the \eqn{(n-1)}-dimensional Lebesgue measure and \eqn{V(E,d)=\mu (X(E,d))} be the
#' measure (volume) of the set of awards \eqn{X(E,d)}. The average-of-awards rule assigns to each claims problem \eqn{(E,d)} the awards vector given by:
#' \deqn{\text{AA}(E,d)=\frac{1}{V(E,d)}\int_{X(E,d)} x d\mu.}
#'
#' The average-of-awards rule corresponds to the core-center solution of the associated coalitional (pessimistic) game.
#'
#' The function AA is programmed with the algorithm of Mirás Calvo et al. (2024b), which is an improved version of the algorithm of Mirás Calvo et al. (2024a).
#' @seealso \link{allrules}, \link{axioms}, \link{CD}, \link{coalitionalgame},  \link{setofawards}, \link{volume}.
#' @examples
#' E=10
#' d=c(2,4,7,8)
#' AA(E,d)
#' #The average-of-awards rule is self-dual: AA(E,d)=d-AA(D-E,d)
#' D=sum(d)
#' d-AA(D-E,d)
#' @references Gonzalez-Díaz, J. and Sánchez-Rodríguez, E. (2007). A natural selection from the core of a TU game: the core-center. International Journal of Game Theory 36(1), 27-46.
#' @references Mirás Calvo, M.A., Núñez Lugilde, I., Quinteiro Sandomingo, C., and Sánchez-Rodríguez, E. (2024a). An algorithm to compute the average-of-awards rule for claims problems with an application to
#' the allocation of CO\eqn{_2} emissions. Annals of Operations Research 336, 1435-1459.
#' @references Mirás Calvo, M.A., Núñez Lugilde, I., Quinteiro Sandomingo, C., and Sánchez-Rodríguez, E. (2024b). On properties of the set of awards vectors for a claims problem. TOP 32, 137-167.
#' @references Mirás Calvo, M.A., Quinteiro Sandomingo, C., and Sánchez-Rodríguez, E. (2022). The average-of-awards rule for claims problems. Social Choice and Welfare 59, 863-888.
#' @export

AA=function(E,d,name=FALSE){
  if(name==TRUE){
    rule="AA"
    return(rule)
  }
  ########################################
  # Required: (E,d) must be a claims problem, i.e., E >=0, d >=0, E <= sum(d)
  ########################################
  n= length (d); D=sum(d) #The number of claims and the total claim
  if (E < 0 || sum((d < 0)) > 0 || E > D)
    stop('(E,d) is not a claims problem.\n',call.=F)
  #####################
  # Sort the claims in ascending order
  do=sort(d,index.return=T)$x
  ordenI = sort (d, index.return =T)$ix #The permutation of the claims
  orden = sort(ordenI,index.return =T)$ix #The inverse permutation
  dnull =sum (do ==0); #The null claims

  ############ Trivial cases #############
  if (E==0 | dnull == length (d)){# Null endowment or all claims zero
    rule= rep(0,length (d)); return (rule)
  } else if (E==D){# Endowment equal to the sum of the claims
    rule=d;
    return (rule)
  } else if (dnull >0) {# Some claims (but not all) are zero
    do=do [( dnull +1):n]; ruleNull =rep(0,dnull);
  } else { #0<E<D and d>0
    ruleNull =c();
  }
  ############# THE AVERAGE OF AWARDS RULE ################
  # From now on: 0<E<D, do >0
  n = length (do) # Number of strictly positive claims
  rule= rep(0,n) # Initially mu=0
  ###### DIRECT CASES ######################
  ###### 1, TWO CLAIMANTS: The concede and divide rule ####
  if(n==2){
    rule=c(E/2,E/2)* as.numeric (E <= do[1])+c(do[1]/2,E-do[1]/2)* as.numeric (do[1]<E & E <= do[2])+c((E+do[1]-do[2])/2, ((E-do[1]+ do[2])/2))* as.numeric (E>do[2])
    rule=c(ruleNull ,rule) #Add the null claims
    rule=rule[ orden ] #Re - order the awards
    return (rule)
  }
  ###### 2, When D-do(n)<=E <= do(n) or E=D/2 ######
  if ((E >= D-do[n] & E <= do[n]) | E==D/2){
    rule[1:(n-1)]= do[1:(n-1)]/2;rule[n]=E-sum(rule[1:(n-1)])
    rule=c( ruleNull ,rule) #Add the null claims
    rule=rule[ orden ] #Re - order the awards
    return (rule)
  }
  ###### 3. When E>D/2, we apply self-duality
  control =as.numeric (E>D/2)
  if ( control ==1){E=D-E}
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # THE CORE - CENTER ALGORITHM
  #1.- 0<E<D/2 %
  #2.- Positive claims : do >0 %
  #3.- Claims in ascending order : do(i)<= do(i+1) %
  #4.- E<D-do(n) %
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # CASE 1: E<do(1), that is , E<do(i) for all i
  #The core and the imputation set coincide: E<d_i for all i\in N
  # The family $\mathcal{F}$ consists of the emptyset
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  if (E <= do[1]){
    rule=E/n*rep(1,n);
    rule= control *do +(-1)^( control )* rule # Apply self-duality
    rule=c( ruleNull ,rule) #Add the null claims
    rule=rule[ orden ] #Re - order the awards
    return (rule)
  }
  Dn=D-do[n] #D_{-n}
  if(E<Dn/2){
    # CASE 2: E>do(1), E<D/2, and E<Dn/2
    # Then , the minimum rights are all zero .
    # In this case, we decompose the imputation set.
    Info=Peso(E,do,TRUE) #Ejecutamos la función Peso una vez y después ponemos los nombres.
    VolInicial=Info$Peso# Weight of the Set of awards vectors.
    VolUtopia=Info$PesoUtopia #Weight of the set of awards vectors of the utopia games.
    rule=rep(1/n*(E+sum(VolUtopia*do)/VolInicial),n)-do*VolUtopia/VolInicial#Formula of the greedy algotithm.
    rule= control *do +(-1)^( control )* rule # Apply self - duality
    #ADDING the null claimants and REORDERING
    rule =c(ruleNull ,rule) #Add the null claimants
    rule = rule[ orden ] #Re - order the awards
    return(rule)}
  if(E>=Dn/2){

    # CASE 3: E>do(1), E<D/2, and E>Dn/2
    # In this case we decompose the hyperrectangle of the product of the claims.
    Prod= prod(do[1:(n-1)])*factorial(n-1);# Volume hyperrectangle of the product of the claims.
    if(do[n]<E){#Problem Pn. (E-dn,(d_{-n},E-dn))
      EA=E-do[n]
      dA=do
      dA[n]=EA
      Info=Peso(EA,dA,TRUE)#Ejecutamos la función Peso una vez y después ponemos los nombres.
      VolA=Info$Peso# Weight of the Set of awards vectors.
      VolUtopiaA=Info$PesoUtopia#Weight of the set of awards vectors of the utopia games.
      #Calculamos el centroide del problema Pn:
      ruleA=rep(1/n*(EA+sum(VolUtopiaA*dA)/VolA),n)-dA*VolUtopiaA/VolA#Formula of the greedy algotithm.
    }else{VolA=0
    ruleA=rep(0,n)}
    if(D-do[n]>E){#Problem P*n. (D_{-n},(d_{-n},D_{-n}-E))
      EB=D-E-do[n] #We decompose the imputation set of the associated game of problem P*n.
      dB=do
      dB[n]=EB
      Info=Peso(EB,dB,TRUE)#Ejecutamos la función Peso una vez y después ponemos los nombres.
      VolB=Info$Peso# Weight of the Set of awards vectors.
      VolUtopiaB=Info$PesoUtopia #Weight of the set of awards vectors of the utopia games.
      #Calculamos el centroide del problema P*n:
      ruleB=rep(1/n*(EB+sum(VolUtopiaB*dB)/VolB),n)-dB*VolUtopiaB/VolB#Formula of the greedy algotithm.
      ruleB=do-ruleB#Self-duality
    }else{PesoB=0
    ruleB=rep(0,n)}
    VolInicial=Prod-VolA-VolB #Volume of the set of awards vectors.

    rule=(do*Prod/2-VolA*ruleA-VolB*ruleB)/VolInicial#Formula of the hyperrectangle.
    rule[n]=E-sum(rule[1:n-1])#Add the last claimant by efficiency.
    rule= control *do +(-1)^( control )* rule#Self-duality
    rule =c(ruleNull ,rule) #Add the null claimants
    rule = rule[ orden ] #Re - order the awards
    return(rule)}
}
