#' @author Chen Lin, Kevin Wang, Samuel Mueller
#' @title Multi-collinearity Visualization
#' @param mcvis_result Output of the mcvis function
#' @param tau A parameter determining below which thickness the plotting lines hide.
#' @param eig.max The maximum number of eigenvalues to be displayed on the plot.
#' @param vol.max The maximum number of variables to be displayed on the plot.
#' @import igraph
#' @export
#' @examples
#' set.seed(1)
#' p = 10
#' n = 100
#' X = matrix(rnorm(n*p), ncol = p)
#' X[,1] = X[,2] + X[,3] + rnorm(n, 0, 0.1)
#' mcvis_result = mcvis(X)
#' igraph_mcvis(mcvis_result)


igraph_mcvis <- function(mcvis_result,
                         tau = 2/3,
                         eig.max = ncol(mcvis_result$MC),
                         vol.max = ncol(mcvis_result$MC))
{
  #####################
  g = 1-mcvis_result$MC
  col.names = mcvis_result$col.names
  #####################

  p = ncol(g)
  eig.max = min(p, eig.max)
  vol.max = min(p, vol.max)
  or = order(g[p,]) ## Order the columns of g by the smallest eigenvalue
  or = or[1:vol.max]
  g.or = g[,or]
  if (vol.max > 1) {g.or = g.or[p:(p-eig.max+1),]} else {g.or = as.matrix(g.or[p:(p-eig.max+1)])}
  if (eig.max == 1) {g.or = t(g.or)}

 ######################
  ## igraph plotting
  g.or[g.or > 1-tau/p] = 1
  ## For plotting purpose, if the values of g.or is above a certain threshold, then we set it to 1
  col.names = col.names[or]
  ## reorder the variables by the connection with the smallest eigenvalue

  M = matrix(1,eig.max,vol.max)
  G = igraph::graph_from_incidence_matrix(M)

  vec = as.vector(t(g.or))


  graph_attr(G,'weight') = vec
  par(bg="white")
  G.text<-paste('x',or," -- ", col.names,sep="")
  val<-paste('tau',p:(p-eig.max+1),sep="")
  col<-paste('x',or,sep="")

  plot(G,
       edge.color = grey(graph_attr(G,'weight')),
       vertex.size=20,
       vertex.label=c(val,col),
       vertex.color=c("yellow","cyan")[V(G)$type+1],
       edge.width=(rep(1,length(vec))-graph_attr(G,'weight'))*10,
       layout=layout_as_bipartite,
       main="mcvis"
  )

  if (vol.max>1)
  {text(x=rep(1.7,vol.max),y=(1:vol.max)*2/(1-vol.max)+(vol.max+1)/(vol.max-1),G.text)}

  if (vol.max==1) {text(x=2,y=0,G.text)}
  text(x=0,y=-1.4,"tau1: the inverse of the smallest eigenvalue")
}
