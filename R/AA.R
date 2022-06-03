#' @title Average of awards rule
#' @description This function returns the awards vector assigned by the average of awards rule (AA) to a claims problem.
#' @param E The endowment.
#' @param d The vector of claims.
#' @param name A logical value.
#' @return The awards vector selected by the AA rule. If name = TRUE, the name of the function (AA) as a character string.
#' @details Let \eqn{E\ge 0} be the endowment to be divided and \eqn{d\in \mathcal{R}^n}{d} the vector of claims
#' with \eqn{d\ge 0} and such that \eqn{\sum_{i=1}^{n} d_i\ge E,\;}{} the sum of claims exceeds the endowment.
#'
#' A vector \eqn{x=(x_1,\dots,x_n)}{x=(x1,...,xn)} is an awards vector for the claims problem \eqn{(E,d)} if \eqn{0\le x \le d}
#' and satisfies the balance requirement, that is, \eqn{\sum_{i=1}^{n}x_i=E}{x1+\dots+xn=E} the sum of its coordinates is equal to \eqn{E}.
#' Let \eqn{X(E,d)} be the set of awards vectors for \eqn{(E,d)}.
#'
#' The average of awards rule assigns to each claims problem \eqn{(E,d)}
#' the expectation of the uniform distribution defined over the set of awards vectors, that is,
#' the centroid of \eqn{X(E,d)}.
#'
#' Let \eqn{\mu} be the (n-1)-dimensional Lebesgue measure and \eqn{V(E,d)=\mu (X(E,d))} the measure (volume) of the set of awards \eqn{X(E,d)}.
#' The average of awards rule assigns to each problem \eqn{(E,d)} the awards vector given by:
#' \deqn{AA(E,d)=\frac{1}{V(E,d)}\int_{X(E,d)} x d\mu}{AA(E,d)=1/V(E,d) ∫xd\mu, where the integral is taken over X(E,d).}
#'
#' The average of awards rule corresponds to the core-center of the associated coalitional (pessimistic) game.
#' @seealso \link{allrules}, \link{CD}, \link{setofawards}, \link{coalitionalgame}
#' @examples
#' E=10
#' d=c(2,4,7,8)
#' AA(E,d)
#' #The average of awards rule is self-dual: AA(E,d)=d-AA(D-E,d)
#' D=sum(d)
#' d-AA(D-E,d)
#' @references Gonzalez-Díaz, J. and Sánchez-Rodríguez, E. (2007). A natural selection from the core of a TU game: the core-center. International Journal of Game Theory, 36(1), 27-46.
#' @references Mirás Calvo, M.Á., Quinteiro Sandomingo, C., and Sánchez-Rodríguez, E. (2022). The average-of-awards rule for claims problems. Soc Choice Welf. \doi{10.1007/s00355-022-01414-6}
#' @references Mirás Calvo, M.Á., Núñez Lugilde, I., Quinteiro Sandomingo, C., and Sánchez-Rodríguez, E. (2020). An algorithm to compute the core-center rule of a claims problem with an application to
#' the allocation of CO2 emissions. Working paper.
#' @export
AA= function (E,d,name=FALSE){
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
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # CASE 2: E>do(1), E<D/2, and E<D-do(n)
  #Then , the minimum rights are all zero .
  #
  #First, compute the maximum cardinal of the members of the family F
  #c=max {|T|:T in F}
  # Observe that since vector do is sorted in increasing order
  # then c is the maximum index i such that do(1 )+...+ do(i)<E.
  #Also, since we are in CASE 2, we know that c<n-1
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  c=sum( cumsum (do)<E)
  #We compute CT, the binary number associated with coalition T={n,n-1 ,...,n -(c-1)},
  #the " biggest " of cardinality c
  CT=2^n-2^(n-c)
  # Some initial values
  Family =c(); Peso =c(); cont =0;
  #For each coalition in F with binary number less than CT
  #we compute the corresponding weight : Peso (T)
  for (T in seq(CT ,1)){ # Backwards recurrence ( from bigger coalitions down )
    Tvector =as.logical ( intToBits (T)[1:n]); #The claims in T
    dT= sum(do[ Tvector ]); #The sum of the claims in T
    if (dT <E){ #If T belongs to F
      #T is the member number cont of F
      cont = cont +1; Family [ cont ]=T
      #The members of F that contained T
      A=as.numeric (( bitwAnd ( Family [1: cont ],T)==T));
      #The weight of coalition T ( peso (T))
      #If T is maximal : Peso (T )=(E-dT )^(n-1)
      #Otherwise , Peso (T) is (E-dT )^(n-1) minus the sum of the
      #weights of the coalition in F that contained T
      if( cont ==1){B=(E-dT )^(n-1)}
      else {B=c(- Peso [1:( cont -1)], (E-dT )^(n-1 ))};
      Peso [ cont ]=A%*%t(t(B));
    }
  }
  #
  # Final computations
  #
  # Weights of all the single claimant coalitions : T={i}
  PesoU =rep (0,n); # Null initial weight
  #If T belongs to F then T has a positive weight (already computed)
  for (ii in 1:n){
    if (do[ii]<E){#If player ii belongs to family F
      #The binary number of coalition T={ ii} is 2^(ii -1)
      PesoU [ii]= Peso [ which ( Family ==2^(ii -1 ))];
    }
  }
  #The weight of the core of the associated coalitional game
  PesoC =E^(n-1)- sum( Peso );
  #The core - center simplified formula
  rule=- PesoU *do/ PesoC +rep(1/n*(E+ PesoU %*% t(t(do ))/ PesoC ),n)
  rule= control *do +(-1)^( control )* rule # Apply self - duality
  #ADDING the null claimants and REORDERING
  rule =c(ruleNull ,rule) #Add the null claimants
  rule = rule[ orden ] #Re - order the awards
  #
  return (rule)
}
