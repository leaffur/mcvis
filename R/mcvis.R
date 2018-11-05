#' @author Chen Lin
#' @title Multi-collinearity Visualization
#' @param Y respose numeric vector
#' @param X design matrix
#' @import igraph
#' @export mcvis
#' @examples
#' library(mplot)
#' data("artificialeg")
#' p=dim(artificialeg)[2]-1
#' X=artificialeg[,1:p]
#' mcvis(X)


mcvis <- function(X, tau = 1.5,
                          col.names,
                          col.one= FALSE,
                          eig.max=dim(X)[2]-col.one, vol.max=dim(X)[2]-col.one,
                          method="bootstrap"
                          )
{
  n<-dim(X)[1]
  n1<-as.matrix(rep(1,n))
  p<-dim(X)[2]-col.one
  if (col.one) {col.names<-colnames(X)[2:(p+1)]} else {col.names<-colnames(X)}
  eig.max<-min(p,eig.max)
  vol.max<-min(p,vol.max)
  #one can choose the max variables and eigenvectors he want to plot.

  #
  vif=v2=matrix(0,p,steps)
  mv2=numeric(0)
  if (method=="cv") {method=2}
  if (method=="bootstrap") {method=1}

  for (i in 1:steps) {
    index.b <- switch(
      method,
      sample(n,replace=TRUE),
      sample(n,replace = FALSE)[1:(floor(sqrt(p*n)))]
    )
    #use bootstrap or cross-validation

    X.b <- as.matrix(X[index.b,])
    n1<-as.matrix(rep(1,dim(X.b)[1]))
    if (col.one==FALSE) {X1<-X.b} else {X1<-cbind(X.b[,2:(p+1)])}
    X2<-X1-n1%*%colMeans(X1)

    s<-as.matrix(sqrt(diag(t(X2)%*%X2)))
    Z<-X2[,1]/s[1,]
    for (j in 2:p)   { Z<-as.matrix(cbind(Z,X2[,j]/s[j,])) }
    #Z is the centering and standarding of X1

    v<-numeric(0)
    for (j in 1:p){ v[j]<-s[j,]/sqrt(sum(X1[,j]^2)) }
    D<-diag(v)
    Z1<-Z%*%D
    #note! I use Z*D rather than D to calculate the eigenvalue and variance inflation factors.

    v2[,i]<-1/t(eigen(crossprod(Z1,Z1))$values)
    vif[,i]<-(t(diag(solve(t(Z1)%*%Z1)))) #inverse of vif of d
  }

  steps=1000
  g<-tor<-matrix(0,p,p)
  for (j in 1:p){
    t<-matrix(0,p+1,10)
    for (i in 1:10) {
      ji<-(i-1)*steps/10+1
      ki<-i*steps/10
      da <- lm(v2[j,ji:ki] ~ t(vif[,ji:ki]))
      t[,i] <- coef(da) / sqrt(diag(vcov(da))) # t-value
    }
    for (i in 2:(p+1)) {
      tor[j,i-1]<-mean(t[i,]^2)
    }
    g[j,]<-rep(1,p)-tor[j,]/sum(tor[j,])
  }
  #g[j,i]: jth small eigenvalue with ith column vector

  or<-order(g[p,])
  or<-or[1:vol.max]
  g.or<-g[,or]
  if (vol.max >1) {g.or<-g.or[p:(p-eig.max+1),]} else {g.or<-t(t(g.or[p:(p-eig.max+1)]))}
  if (eig.max ==1 ) {g.or<-t(g.or)}
  g.or[g.or>1-tau/p]<-1
  col.names<-col.names[or]
  #reorder the variables by the connection with the smallest eigenvalue

  M<-matrix(1,eig.max,vol.max)
  G<-graph_from_incidence_matrix(M)

  vec<-NULL
  for (i in 1:eig.max) {vec<-c(vec,g.or[i,])}
  graph_attr(G,'weight') <- vec
  par(bg="white")
  G.text<-paste('x',or," -- ", col.names,sep="")
  val<-paste('v',1:eig.max,sep="")
  col<-paste('x',or,sep="")
  plot(G,
       edge.color = grey(graph_attr(G,'weight')),
       vertex.size=20,
       vertex.label=c(val,col),
       vertex.color=c("yellow","cyan")[V(G)$type+1],
       edge.width=(rep(1,length(vec))-graph_attr(G,'weight'))*10,
       layout=layout_as_bipartite,
       main="Visualization"
  )
  if (vol.max>1)
  {text(x=rep(2,vol.max),y=(1:vol.max)*2/(1-vol.max)+(vol.max+1)/(vol.max-1),G.text)}
  if (vol.max==1) {text(x=2,y=0,G.text)}
  text(x=-2,y=0,"v1: the smallest eigenvalue")
}
