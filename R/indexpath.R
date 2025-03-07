indexpath = function(d, Rules, col = NULL, points = 201, legend = TRUE) {
  .Deprecated("indexgpath")
  # Default colors
  if (is.null(col)) {
    col=c("red","blue","green","yellow","pink","coral4","orange","darkgray","burlywood3","black","darkorange","darkviolet")
  }
  #
  n = length(d) #The number of claims
  D = sum(d) #The sum of claims
  ########################################
  # Required: d must be a claims vector.
  ########################################
  if (sum((d < 0)) > 0)
    stop('d is not a claims vector.',call.=F)

  ###################
  do = sort(d)
  if (sum(do == d) < n) {
    message('The path is shown for the rearranged vector of claims.\n')
  }
  #The endowment domain
  endowms = c(seq(0, D/2, length.out = ceiling(points/2)),seq(D/2, D, length.out = ceiling(points/2)))
  #Names of the rules for the legend
  numberrules = length(Rules)
  Rulechar=rep(0,numberrules)
  for(i in 1:numberrules){
    Rulechar[i]=Rules[[i]](0,0,name=TRUE)
  }
  #The growth of the index for the rules.
  f=do/D
  H=c()
  for(j in 1:numberrules){
    G=c()
    for(E in endowms){
      regla=Rules[[j]](E,d)
      Y=cumsum(regla)/E
      Y0=c(0,Y[1:n-1])
      G=c(G,1-f%*%t(t(Y0+Y)));
    }
    H=rbind(H,G)
    }
W=c(min(H,na.rm=TRUE)-0.05,max(H,na.rm=TRUE)+0.05)
# The index path for the proportional rule
plot(
  c(0,D),
  c(0,0),
  type = "l",
  xlim = c(0, D),
  ylim= W,
  xaxt="n",
  main = c("Proportionality index path"),
  xlab = paste("d=(",toString(d),")",sep=""),
  ylab = ""
)
# Tickmarks and grid
axis(side=1,at=c(0,do[1],do[n],D/2,D-do[n],D-do[1],D))
abline(a = NULL, b = NULL, v = c(0,do[1],do[n],D/2,D-do[n],D-do[1],D), lty="dotted")
grid(nx=0,ny=NULL)
#The index paths for the selected rules
for(i in 1:numberrules){
  lines(endowms,H[i,],col=col[i])
}
#The legend
if (legend==TRUE){
  legend(x = "topright",
         legend = c(Rulechar),
         col = col[1:numberrules],
         lty=1,
         lwd=2,
         seg.len=0.5,
         cex=0.8,
         y.intersp=0.6,
         bty = "n"
         )
}
}
