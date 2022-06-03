#' @title Coalitional game associated with a claims problem
#' @description This function returns the pessimistic and optimistic coalitional games associated with a claims problem.
#' @param E The endowment.
#' @param d The vector of claims.
#' @param lex Logical parameter. If lex = TRUE, coalitions of claimants are ordered lexicographically. By default, lex = FALSE, and coalitions are ordered using their binary representations.
#' @param opt Logical parameter. If opt = TRUE, both the pessimist and optimistic associated coalitional games are given.
#' By default, opt = FALSE, and only the associated pessimistic coalitional game is computed.
#' @return The pessimistic (and optimistic) associated coalitional game(s).
#' @details Let \eqn{E\ge 0} be the endowment to be divided and \eqn{d\in \mathcal{R}^n}{d} the vector of claims
#' with \eqn{d\ge 0} and such that \eqn{\sum_{i=1}^{n} d_i\ge E,\;}{} the sum of claims exceeds the endowment.
#'
#'  For each subset \eqn{S} of the set of claimants \eqn{N}, let \eqn{d(S)=\sum_{j\in S}d_j}{d(S)} be the sum of claims of the members of \eqn{S}
#'  and let \eqn{N\backslash S}{N-S} be the complementary coalition of \eqn{S}.
#'
#'  Given a claims problem \eqn{(E,d)}, its associated pessimistic coalitional game is the game  \eqn{v_{pes}:2^N\rightarrow \mathcal{R}}{vp} assigning to each coalition \eqn{S\in 2^N}{S}
#'  the real number:
#' \deqn{v_{pes}(S)=\max\{0,E-d(N\backslash S)\}.}{vp(S)=max \{0,E-d(N-S)\}.}
#'
#' Given a claims problem \eqn{(E,d)}, its associated optimistic coalitional game  is the game \eqn{v_{opt}:2^N\rightarrow \mathcal{R}}{vo}
#' assigning to each coalition \eqn{S\in 2^N}{S}
#'  the real number:
#' \deqn{v_{opt}(S)=\min\{E,d(S)\}.}{vo(S)=min \{E,d(S)\}.}
#'
#' The optimistic and the pessimistic coalitional games are dual games, that is, for all \eqn{S\in 2^N}{S}:
#' \deqn{v_{opt}(S)=E-v_{pes}(N\backslash S).}{vo(S)=E-vp(N-S).}
#'
#'
#' An efficient way to represent a nonempty coalition \eqn{S\in 2^N}{S} is by identifying it with the binary sequence
#' \eqn{a_{n}a_{n-1}\dots a_{1}}{a(n)a(n-1) \dots a(1)} where \eqn{a_i=1}{a(i)=1} if \eqn{i\in S}{i belongs to S}
#' and \eqn{a_i=0}{a(i)=0} otherwise.
#' Therefore, each coalition \eqn{S} is represented by the number associated with its binary representation: \eqn{\sum_{i\in T}2^{i-1}}{\sum a(i)2^(i-1)}.
#' Then coalitions can be ordered by their associated numbers.
#'
#' Alternatively, coalitions can be ordered lexicographically.
#'
#'
#' Given a claims problem \eqn{(E,d)}, its associated coalitional game \eqn{v} can be represented by the vector whose coordinates are the values assigned by \eqn{v} to all the nonempty coalitions.
#' For instance. if \eqn{n=3}, the associated coalitional game can be represented by the vector of the values of all the 7 nonempty coalitions, ordered using the binary representation:
#' \deqn{v = [v(\{1\}),v(\{2\}),v(\{1,2\}),v(\{3\}),v(\{1,3\}),v(\{2,3\}),v(\{1,2,3\})]}{v = [v(\{1\}), v(\{2\}), v(\{1,2\}), v(\{3\}), v(\{1,3\}), v(\{2,3\}), v(\{1,2,3\})]}
#' Alternatively, the coordinates can be ordered lexicographically:
#'  \deqn{v = [v(\{1\}),v(\{2\}),v(\{3\}),v(\{1,2\}),v(\{1,3\}),v(\{2,3\}),v(\{1,2,3\})]}{v = [v(\{1\}), v(\{2\}), v(\{3\}), v(\{1,2\}), v(\{1,3\}), v(\{2,3\}), v(\{1,2,3\})]}
#'
#' When \eqn{n=4}, the associated coalitional game can be represented by the vector of the values of all the 15 nonempty coalitions, ordered using the binary representation:
#'
#' \eqn{v = [v(\{1\}),v(\{2\}),v(\{1,2\}),v(\{3\}),v(\{1,3\}),v(\{2,3\}),v(\{1,2,3\}),v(\{4\}),}{%
#' }
#'
#' \eqn{v(\{1,4\}),v(\{2,4\}),v(\{1,2,4\}),v(\{3,4\}),v(\{1,3,4\}),v(\{2,3,4\}),v(\{1,2,3,4\})]}{%
#' v = [v(\{1\}), v(\{2\}), v(\{1,2\}), v(\{3\}), v(\{1,3\}), v(\{2,3\}), v(\{1,2,3\}), v(\{4\}), v(\{1,4\}), v(\{2,4\}), v(\{1,2,4\}), v(\{3,4\}), v(\{1,3,4\}), v(\{2,3,4\}), v(\{1,2,3,4\})]}
#'
#' Alternatively, the coordinates can be ordered lexicographically:
#'
#' \eqn{v=[v(\{1\}),v(\{2\}),v(\{3\}),v(\{4\}),v(\{1,2\}),v(\{1,3\}),v(\{1,4\}),v(\{2,3\}),\dots}{%
#' }
#'
#' \eqn{\dots v(\{2,4\}),v(\{3,4\}),v(\{1,2,3\}),v(\{1,2,4\}),v(\{1,3,4\}),v(\{2,3,4\}),v(\{1,2,3,4\})]}{%
#' v=[v(\{1\}), v(\{2\}), v(\{3\}), v(\{4\}), v(\{1,2\}), v(\{1,3\}), v(\{1,4\}), v(\{2,3\}), v(\{2,4\}), v(\{3,4\}), v(\{1,2,3\}), v(\{1,2,4\}), v(\{1,3,4\}), v(\{2,3,4\}), v(\{1,2,3,4\})]}
#'
#' @seealso \link{setofawards}
#' @examples
#' E=10
#' d=c(2,4,7,8)
#' v=coalitionalgame(E,d,opt=TRUE,lex=TRUE)
#' #The pessimistic and optimistic coalitional games are dual games
#' v_pes=v$v_pessimistic_lex
#' v_opt=v$v_optimistic_lex
#' v_opt[1:14]==10-v_pes[14:1]
#' @references O’Neill B (1982) A problem of rights arbitration from the Talmud. Math Soc Sci 2:345–371.
#' @export

coalitionalgame = function(E, d, opt = FALSE,lex=FALSE) {
  v = NULL
  ########################################
  # Required: (E,d) must be a claims problem, i.e., E >=0, d >=0, E <= sum(d)
  ########################################
  n = length(d); D = sum(d) #The number of claims and the total claim
  if (E < 0 || sum((d < 0)) > 0 || E > D)
    stop('(E,d) is not a claims problem.',call.=F)

  ################### PESSIMISTIC CHARACTERISTIC FUNCTION #############
  end1 = 2 ^ n - 1
  v = rep(0, end1)
  sconxunta = 2 * E - D

  if (E == 0) {
    if (opt == 1) {
      v_opt = v
      return(list(v_pessimistic = v, v_optimistic = v_opt))
    }
  } else {
    for (S in 1:((2 ^ (n - 1)) - 1)) {
      v[S] = E - D + sum(d * as.numeric(intToBits(S)[1:n]))
      v[end1 - S] = sconxunta - v[S]
      v[S] = max(0, v[S])
      v[end1 - S] = max(0, v[end1 - S])
    }
  }
  v[end1] = E
  if(lex==TRUE){
    vlex=v
    basebin=matrix(0,2^n-1,n)
    for (ii in 1:2^n-1){
      basebin[ii,]=as.numeric(intToBits(ii)[1:n])
    }
    suma4fila=rowSums(basebin)
    basebin=cbind(suma4fila,basebin)#Matrix of coalitions in binary order
    baselex=c()
    for(i in 1:(n-1)){
      t=which(basebin[,1]==i)
      m=basebin[t,]
      m=1-m[,2:(n+1)]
      ms=m[order(m[,1]),]
      baselex=rbind(baselex,1-ms)
    }
    suma4fila=rowSums(baselex)
    baselex=cbind(suma4fila,baselex)
    baselex=rbind(baselex,basebin[2^n-1,])#Matrix of coalitions in lexicographic order

    for (players in 1:(n-1)){
      B = which(basebin[,1]==players); L=which(baselex[,1]==players);
      for(ii in 1:length(B)){
        for(iii in 1:length(L)){
          if(sum(bitwAnd(basebin[B[ii],2:(n+1)],baselex[L[iii],2:(n+1)]))==players){
            vlex[L[iii]]=v[B[ii]]#vlex: v in lexicographic order
          }
        }
      }
    }
    if(opt==FALSE){#We display only the pessimistic characteristic function in the lexicographic form
      return(list(v_pessimistic_lex = vlex))}}
  ################### OPTIMISTIC CHARACTERISTIC FUNCTION #############
  if (opt == TRUE) {
    v_opt = c(matrix(E - v[1:(end1-1)][(end1-1):1], 1, (end1-1)), E)
    if(lex==TRUE){#We display the two characteristic functions in the lexicographic form
      vlex_opt=v_opt
      for (players in 1:(n-1)){
        B = which(basebin[,1]==players); L=which(baselex[,1]==players);
        for(ii in 1:length(B)){
          for(iii in 1:length(L)){
            if(sum(bitwAnd(basebin[B[ii],2:(n+1)],baselex[L[iii],2:(n+1)]))==players){
              vlex_opt[L[iii]]=v_opt[B[ii]]#vlex_opt: v optimistic in lexicographic order
            }
          }
        }
      }
      return(list(v_pessimistic_lex = vlex, v_optimistic_lex = vlex_opt))}
    return(list(v_pessimistic_bin = v, v_optimistic_bin = v_opt))}
  return(list(v_pessimistic_bin = v))
}
