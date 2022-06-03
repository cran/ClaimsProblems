proportionalityindex = function(E, d, Rule) {
  .Deprecated("deviationindex")
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
    stop('We can not compute the proportionality index if E=0.',call.=F)
  } else {
    #claims index for the rule and for the claims
    rule = Rule(E, do)
    claimsawards = 1-1/D*(sum(do*rule)/E+2/E* sum(rule*(D-cumsum(do))))
    if (sum(do == d) < n)
      message('The proportionality deviation index is computed by rearranging the claims in increasing order.\n')
    return(claims_awards = claimsawards)
  }

}
