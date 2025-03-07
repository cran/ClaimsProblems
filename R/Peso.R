Peso=function(E,d,Utopia=FALSE){
  #This function computes the weight being Volume = Weight x sqrt(n)/(n-1)! 
  n=length(d);
  d=sort(d,index.return=T)$x
  ordenI = sort (d, index.return =T)$ix #The permutation of the claims
  orden = sort(ordenI,index.return =T)$ix #The inverse permutation
  cardinal=sum(d<E) #Number of claimants such that di<E.
  Dcardinal=sum(d[1:cardinal]);
  Peso=0
  if(Utopia==TRUE)
  {
    PesoUtopia=rep(0,n)
  }
  d = d[d<E]; #We work with the claims such that di<E.
  if(cardinal==0)
  {
    Peso=E^(n-1)
  }else{
  S=0
  while (S<=(2^(cardinal-1) - 1)){#By duality, we go through half of the coalitions.
    Svector=as.numeric(intToBits(S)[1:n])
    cT = length(d[Svector]);
    dT=sum(d[which(Svector==1)]);
    if (dT < E){#If the coalition belongs to family F, we compute the volume of its utopia game.
    Peso = Peso+(-1)^cT*(E-dT)^(n-1);

    if(Utopia==TRUE)
    {
      # Aproveitamos para gardar resultados necesarios para calcular xa o peso
      # dos correspondentes xogos de utopía (os xogos de utopía de cada un dos xogadores de T):
      axentesS = which(Svector==1); # Os xogadores de T
      PesoUtopia[axentesS] = PesoUtopia[axentesS]+ (-1)^(cT-1)*(E-dT)^(n-1)
    }
    }
   dCT = Dcardinal-dT; #Complementary coalition.
    if (dCT < E){#If the complementary coalition belongs to family F, we compute the volume of its utopia game.
    cCT = cardinal-cT;
    Peso = Peso+(-1)^cCT*(E-dCT)^(n-1);
    if(Utopia==TRUE)
    {
      #Igual que antes:
      axentesNnoS = which(Svector[1:cardinal] == 0)# Os xogadores do complementario de T no xogo truncado
      PesoUtopia[axentesNnoS] = PesoUtopia[axentesNnoS]+ (-1)^(cCT-1)*(E-dCT)^(n-1)
    }
    }
    S=S+1
  }}

  if(Utopia==TRUE)
  {
    PesoUtopia=PesoUtopia[orden]
    return(list(Peso=Peso,PesoUtopia=PesoUtopia))
  }else
  {
    return(Peso)
  }
}

