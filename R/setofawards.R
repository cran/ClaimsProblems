#' @title Set of awards vectors for a claims problem
#' @description This function plots the set of awards vectors for a claims problem with 2, 3, or 4 claimants and returns its vertices.
#' @param E The endowment.
#' @param d The vector of claims.
#' @param draw A logical value.
#' @param col The color.
#' @return The vertices of the set of awards vectors for a claims problem with 2, 3, or 4 claimants. For two-claimant and three-claimant problems, if draw = TRUE it plots the set of awards vectors.
#' For a four-claimant problem, if draw = TRUE, it plots the projection of the set of awards vector over the euclidean space of the first three coordinates.
#' The default colors (col = NULL) are: red for two-claimant problems, beige for three-claimant problems, and white for four-claimant problems.
#' @details Let \eqn{E\ge 0} be the endowment to be divided and \eqn{d\in \mathcal{R}^n}{d} the vector of claims
#' with \eqn{d\ge 0} and such that \eqn{\sum_{i=1}^{n} d_i\ge E,\;}{} the sum of claims exceeds the endowment.
#'
#' A vector \eqn{x=(x_1,\dots,x_n)}{x=(x1,...,xn)} is an awards vector for the claims problem \eqn{(E,d)} if \eqn{0\le x \le d}
#' and satisfies the balance requirement, that is, \eqn{\sum_{i=1}^{n}x_i=E}{x1+\dots+xn=E} the sum of its coordinates is equal to \eqn{E}.
#' Let \eqn{X(E,d)} be the set of awards vectors for the problem \eqn{(E,d)}.
#'
#' For each subset \eqn{S} of the set of claimants \eqn{N}, let \eqn{d(S)=\sum_{j\in S}d_j}{d(S)} be the sum of claims of the members of \eqn{S}
#'  and let \eqn{N\backslash S}{N-S} be the complementary coalition of \eqn{S}.
#'
#' The minimal right of claimant \eqn{i} in \eqn{(E,d)} is whatever is left after every other claimant has received his claim, or 0 if that is not possible:
#' \deqn{m_i(E,d)=\max\{0,E-d(N\backslash\{i\})\},\ i=1,\dots,n.}{mi = max\{ 0 , E-d(N-\{i\}) \}, i=1,\dots,n.}
#' Let \eqn{m(E,d)=(m_1(E,d),\dots,m_n(E,d))}{m(E,d)=(m1,\dots,mn)} be the vector of minimal rights.
#'
#' The truncated claim of claimant \eqn{i} in \eqn{(E,d)} is the minimum of the claim and the endowment:
#' \deqn{t_i(E,d)=\min\{d_i,E\},\ i=1,\dots,n.}{ti = min\{di,E\}, i=1,\dots,n}
#' Let \eqn{t(E,d)=(t_1(E,d),\dots,t_n(E,d))}{t(E,d)=(t1,\dots,tn)} be the vector of truncated claims.
#'
#' A vector \eqn{x} is efficient if the sum of its coordinates coincides with the endowment. The set of awards is the the set of all efficient vectors bounded by the minimal right and trucated claim vectors.
#'
#' The set of awards vectors for the claims problem \eqn{(E,d)} can be given in terms of the minimal rights and truncated claims vectors:
#' \deqn{X(E,d)=\bigl\{x \in \mathcal{R}^n: \sum_{i=1}^n x_i=E,  m_i(E,d) \le x_i \le t_i(E,d),\ i=1,\dots,n \bigr\}}{%
#' X(E,d)=\{x=(x1,\dots,xn): m(E,d) \le x \le t(E,d), x1+\dots+xn=E\}.}
#'
#' The set of awards vectors for a problem coincides with the core of its associated coalitional (pessimistic) game.
#'
#' The vertices of the set of awards are the marginal worth vectors. For each order of the claimants, the marginal worth vectors are obtained applying  the principle ``first to arrive, first to be served".
#' Then, for each order, the corresponding marginal worth vector assigns to each claimant the minimum of her/his claim
#'  and what remains of the endowment.
#'
#' @references Thomson, W. (2019). How to divide when there isn't enough. From Aristotle, the Talmud, and Maimonides to the axiomatics of resource allocation. Cambridge University Press.
#' @seealso \link{plotrule}, \link{problemdata}, \link{AA}, \link{RA}
#' @examples
#' E=10
#' d=c(2,4,7,8)
#' setofawards(E,d,col="darkgreen")
#' @importFrom graphics lines
#' @importFrom graphics points
#' @importFrom graphics polygon
#' @importFrom graphics mtext
#' @importFrom rgl lines3d
#' @importFrom rgl plot3d
#' @importFrom rgl grid3d
#' @importFrom rgl rgl.triangles
#' @importFrom rgl par3d
#' @importFrom rgl view3d
#' @importFrom rgl points3d
#' @importFrom geometry convhulln
#' @export
setofawards=function(E,d,draw=TRUE,col=NULL){
  ########################################
  # Required: (E,d) must be a claims problem, i.e., E >=0, d >=0, E <= sum(d)
  ########################################
  n= length(d); D=sum(d) #The number of claims and the total claim
  if (E<0|sum((d<0))>0|E>D)
    stop('(E,d) is not a claims problem.',call.=F)

  if(D==E){
    return(paste('The set of awards is a single-point: (',toString(d),')',sep=""))
  }
  #
  #
  ####### CASE n=2
  if(n==2){
    # Draws the set of awards of a 2-claimant problem.
    #The minimal rights
    m=c(max(0,E-d[2]),max(0,E-d[1]))
    # The length of the set of awards
    Delta=E-sum(m)
    V=matrix(c(m[1],E-m[1],E-m[2],m[2]),ncol=2,byrow=T)
    #The extreme points of the set of awards (no repeated points)
    V=unique(V)
    if (draw==TRUE){
        #If the set of awards is a line segment
        #The axis are determined by the minimal rights
        plot(m[1],
             m[2],
             type="n",
             xlim=c(m[1],min(E,d[1])*1.2),
             ylim=c(m[2],min(E,d[2])),
             xlab=expression("x"[1]),
             ylab=expression("x"[2]),
             main=c("Set of awards")
             )
      subtitle=paste("E=",toString(E),"; d=(",toString(d),")",sep="")
      mtext(subtitle,side=3,line=0.5,cex=0.7)
      grid()
        lines(c(m[1],E-m[2]),c(m[2],m[2]))
        lines(c(m[1],m[1]),c(E-m[1],m[2]))
        # Default color for the set fo awards vector: RED
        if (is.null(col)){col="red"}
        lines(t(V[,1]),t(V[,2]),col=col)
    }
    return(V)
  }
    #
    #
 ####### CASE n=3
 else if(n==3){
    m=c(max(0,E-d[2]-d[3]),max(0,E-d[1]-d[3]),max(0,E-d[1]-d[2]))
    #The coalitional game
    v12=max(0,E-d[3])
    v13=max(0,E-d[2])
    v23=max(0,E-d[1])
    #The extreme points of the set of awards (no repeated points)
    V=matrix(c(E-v23,v23-m[3],m[3],
               v13-m[3],E-v13,m[3],
               m[1],E-v13,v13-m[1],
               m[1],v12-m[1],E-v12,
               v12-m[2],m[2],E-v12,
               E-v23,m[2],v23-m[2]),
             ncol=3,
             byrow=T
             )
    V=unique(V)
    if (draw==TRUE){
      # The minimal rights
      imputation=matrix(c(E-m[2]-m[3],m[2],m[3],
                          m[1],E-m[1]-m[3], m[3],
                          m[1],m[2],E-m[1]-m[2]),ncol=3,byrow=T);
      # The extreme points of the imputation set
      #The length of the imputation set is sqrt(2)*Delta
      Delta=E-sum(m)
      #The corresponding extreme points of the equilateral triangle in our window
      equilatero=matrix(c(0,0,Delta,0,Delta/2,sqrt(3)/2*Delta),ncol=2,byrow=T);
      P=t(equilatero)%*%solve(t(imputation));
      plot(0,
           0,
           xlim=c(0,max(equilatero[,1])),
           ylim=c(0,max(equilatero[,2])*1.1),
           type="n",
           main="Set of awards",
           axes=F,
           xlab=paste("E=",toString(E),"; d=(",toString(d),")",sep=""),
           ylab="",
           asp=1)
      text(-max(equilatero[,1])*0.12,0, paste("(",toString(imputation[1,]),")"))
      text(max(equilatero[,1])+max(equilatero[,1])*0.12,0, paste("(",toString(imputation[2,]),")"))
      text(max(equilatero[,1])/2,max(equilatero[,2])*1.05,paste("(",toString(imputation[3,]),")"))
      polygon(t(equilatero[,1]),t(equilatero[,2]))
      vertP=t(P%*%t(V));
      if(dim(V)[1]==2){
        # Default color for the set fo awards vector: RED
        if (is.null(col)){col="red"}
          lines(t(vertP[,1]),t(vertP[,2]),col=col,lwd=3)
      }else{
        # Default color for the set fo awards vector: BEIGE
        if (is.null(col)){col="beige"}
            polygon((vertP),col=col)
      }
    }
  return(V)
  }
  #
  #
  ####### CASE n=4
  else if(n==4){
    # The coalitional game
    m=c(max(0,E-d[2]-d[3]-d[4]),max(0,E-d[1]-d[3]-d[4]),max(0,E-d[1]-d[2]-d[4]),max(0,E-d[1]-d[2]-d[3]))
    v1=m[1];v2=m[2];v3=m[3];v4=m[4]; v1234=E;
    v12=max(0,E-d[3]-d[4]);v13=max(0,E-d[2]-d[4]);v23=max(0,E-d[1]-d[4]);
    v14=max(0,E-d[2]-d[3]);v24=max(0,E-d[1]-d[3]);v34=max(0,E-d[1]-d[2]);
    v123=max(0,E-d[4]);v124=max(0,E-d[3]);v134=max(0,E-d[2]);v234=max(0,E-d[1]);
    # The extreme points of the set of awards:
    # Order:
    #  (1)1234,  (2)1243,  (3)1324,  (4)1342,  (5)1423,  (6)1432
    #  (7)2134,  (8)2143,  (9)2314, (10)2341, (11)2413, (12)2431
    # (13)3124, (14)3142, (15)3214, (16)3241, (17)3412, (18)3421
    # (19)4123, (20)4132, (21)4213, (22)4231, (23)4312, (24)4321
    WP=matrix(0,24,4)
    WP[1,]=c(v1,v12-v1, v123-v12, v1234-v123);
    WP[2,]=c(v1, v12-v1, v1234-v124, v124-v12);
    WP[3,]=c(v1, v123-v13, v13-v1, v1234-v123);
    WP[4,]=c(v1, v1234-v134, v13-v1, v134-v13);
    WP[5,]=c(v1, v124-v14, v1234-v124, v14-v1);
    WP[6,]=c(v1, v1234-v134, v134-v14, v14-v1);
    WP[7,]=c(v12-v2, v2, v123-v12, v1234-v123);
    WP[8,]=c(v12-v2, v2, v1234-v124, v124-v12);
    WP[9,]=c(v123-v23, v2, v23-v2, v1234-v123);
    WP[10,]=c(v1234-v234, v2, v23-v2, v234-v23);
    WP[11,]=c(v124-v24, v2, v1234-v124, v24-v2);
    WP[12,]=c(v1234-v234, v2, v234-v24, v24-v2);
    WP[13,]=c(v13-v3, v123-v13, v3, v1234-v123);
    WP[14,]=c(v13-v3, v1234-v134 ,v3, v134-v13);
    WP[15,]=c(v123-v23, v23-v3, v3, v1234-v123);
    WP[16,]=c(v1234-v234, v23-v3, v3, v234-v23);
    WP[17,]=c(v134-v34, v1234-v134, v3, v34-v3);
    WP[18,]=c(v1234-v234, v234-v34, v3, v34-v3);
    WP[19,]=c(v14-v4, v124-v14, v1234-v124, v4);
    WP[20,]=c(v14-v4, v1234-v134, v134-v14, v4);
    WP[21,]=c(v124-v24, v24-v4, v1234-v124, v4);
    WP[22,]=c(v1234-v234, v24-v4, v234-v24, v4);
    WP[23,]=c(v134-v34, v1234-v134, v34-v4, v4);
    WP[24,]=c(v1234-v234, v234-v34, v34-v4, v4);
    V=WP[,1:3]
    V=unique(V);
    if (draw==TRUE){
    par3d(windowRect = c(100,100,900,900),zoom=2)
    um=matrix(c(-0.862371,0.5039079,-0.04891979,0,-0.1309225,-0.1286247,0.98301315,0,0.4890557,0.8541268,0.17689507,0,0,0,0,1),ncol=4,byrow=T)
    view3d(userMatrix=um)
      # The faces of the set of awards
      # We used the face-game notation
      #
      # The faces according to Shapley's article
      #1.- F{1}: 3241, 3421, 4321, 4231, 2431, 2341 (16,18,24,22,12,10)
      F1=rbind(WP[16,],WP[18,],WP[24,],WP[22,],WP[12,],WP[10,],WP[16,])
      #2.- F{124}: 3241, 3214, 3124, 3142, 3412, 3421 (16,15,13,14,17,18)
      F124=rbind(WP[16,],WP[15,],WP[13,],WP[14,],WP[17,],WP[18,],WP[16,])
      #3.- F{24}: 3124, 1324, 1342, 3142 (13,3,4,14)
      #4.- F{2}: 3142, 1342, 1432, 4132, 4312, 3412 (14,4,6,20,23,17)
      F2=rbind(WP[14,],WP[4,],WP[6,],WP[20,],WP[23,],WP[17,],WP[14,])
      #5.- F{23}: 4132, 1432, 1423, 4123 (20,6,5,19)
      #6.- F{123}: 4132, 4123, 4213, 4231, 4321, 4312 (20,19,21,22,24,23)
      #7.- F{12}: 3421, 3412, 4312, 4321 (18,17,23,24)
      #8.- F{134}: 2341, 2314, 2134, 2143, 2413, 2431 (10,9,7,8,11,12)
      F134=rbind(WP[10,],WP[9,],WP[7,],WP[8,],WP[11,],WP[12,],WP[10,])
      #9.- F{14}: 2341, 3241, 3214, 2314 (10,16,15,9)
      #10.- F{4}: 3214, 3124, 1324, 1234, 2134, 2314 (15,13,3,1,7,9)
      #11.- F{234}: 1324, 1342, 1432, 1423, 1243, 1234 (3,4,6,5,2,1)
      F234=rbind(WP[3,],WP[4,],WP[6,],WP[5,],WP[2,],WP[1,],WP[3,])
      #12.- F{3}: 1423, 4123, 4213, 2413, 2143, 1243 (5,19,21,11,8,2)
      F3=rbind(WP[5,],WP[19,],WP[21,],WP[11,],WP[8,],WP[2,],WP[5,])
      #13.- F{13}: 4213, 4231, 2431, 2413 (21,22,12,11)
      #14.- F{34}: 2134, 1234, 1243, 2143 (7,1,2,8)
      #
      # SINGLETON CASES
      imputation=matrix(c(v1, v2, v3,
                          v1, v2, v1234-v1-v2-v4,
                          v1234-v2-v3-v4, v2, v3,
                          v1, v1234-v1-v3-v4, v3,
                          v1, v2, v3,
                          v1234-v2-v3-v4, v2, v3,
                          v1, v1234-v1-v3-v4, v3,
                          v1, v2, v1234-v1-v2-v4),ncol=3,byrow=T);
      D1=sum(d[1:n-1])
      #
      plot3d(imputation,
             type="l",
             xlab="",
             ylab="",
             zlab = "",
             col="black",
             lwd=1,
             main=paste("Set of awards vectors for ","E=",toString(E),", d=(",toString(d),")",sep=""),
             box=F)
# The set of awards is a line segment
      if(dim(V)[1]==2){
        # Default color for the set fo awards vector: RED
        if (is.null(col)){col="red"}
            lines3d(t(V[,1]),t(V[,2]),t(V[,3]),col=col,lwd=5)
        grid3d(c("x-+", "y-+", "z-+"))
# The set of awards has dimension 2 or 3
      }else{
        if(sum(WP[,1])==0|sum(WP[,2])==0|sum(WP[,3])==0|sum(WP[,4])==0){
          # Default color for the set fo awards vector: RED
          if (is.null(col)){col="red"}
              points3d(t(V[,1]),t(V[,2]),t(V[,3]),col=col)
              lines3d(F234,col=col,lwd=3)
              lines3d(F1,col=col,lwd=3)
              lines3d(F134,col=col,lwd=3)
              lines3d(F2,col=col,lwd=3)
              lines3d(F124,col=col,lwd=3)
              lines3d(F3,col=col,lwd=3)
              #
          grid3d(c("x-+", "y-+", "z-+"))
        }else{
          points3d(t(V[,1]),t(V[,2]),t(V[,3]),col="black")
          lines3d(F234,col="black",lwd=3)
          lines3d(F1,col="black",lwd=3)
          lines3d(F134,col="black",lwd=3)
          lines3d(F2,col="black",lwd=3)
          lines3d(F124,col="black",lwd=3)
          lines3d(F3,col="black",lwd=3)
          if(is.null(col)==FALSE){
            ts.surf1 <- t(convhulln(V))
            convex1 <-  rgl.triangles(V[ts.surf1,1],V[ts.surf1,2],V[ts.surf1,3],col=col,alpha=0.2)}
          grid3d(c("x-+", "y-+", "z-+"))
        }
      }
    }
    return(unique(WP))
      }
  else{
    stop("This claims problem has more than four claimants.")}
}
