#' @title Multi-collinearity Visualization plots
#' @import igraph
#' @rdname plot.mcvis
#' @importFrom grDevices grey
#' @export
igraph_mcvis <- function(mcvis_result, eig_max = 1L, var_max = ncol(mcvis_result$MC)) {
  #####################
  g = 1 - mcvis_result$MC
  col_names = mcvis_result$col_names
  #####################

  p = ncol(g)
  eig_max = min(p, eig_max)
  var_max = min(p, var_max)
  or = order(g[p, ])  ## Order the columns of g by the smallest eigenvalue
  or = or[1:var_max]
  g.or = g[, or]
  if (var_max > 1) {
    g.or = g.or[p:(p - eig_max + 1), ]
  } else {
    g.or = as.matrix(g.or[p:(p - eig_max + 1)])
  }
  if (eig_max == 1) {
    g.or = t(g.or)
  }

  ###################### igraph plotting
  g.or[g.or > 1 - (2/3)/p] = 1
  ## For plotting purpose, if the values of g.or is above a certain threshold, then we set it to 1
  col_names = col_names[or]
  ## reorder the variables by the connection with the smallest eigenvalue

  M = matrix(1, eig_max, var_max)
  G = igraph::graph_from_incidence_matrix(M)

  vec = as.vector(t(g.or))


  graph_attr(G, "weight") = vec
  opar <- par(bg = "white")
  on.exit(par(opar))
  G.text <- paste0("x", or, " -- ", col_names)
  # val <- as.expression(lapply((p-eig_max+1):p, function(i) bquote(tau[.(i)])))
  val <- as.expression(lapply(p:(p - eig_max + 1), function(i) bquote(tau[.(i)])))
  col <- paste0("x", or)

  plot(G, edge.color = grDevices::grey(graph_attr(G, "weight")), vertex.size = 20, vertex.label = c(val, col), vertex.color = c("yellow", "cyan")[V(G)$type +
                                                                                                                                                    1], edge.width = (rep(1, length(vec)) - graph_attr(G, "weight")) * 10, layout = layout_as_bipartite, main = "mcvis")

  if (var_max > 1) {
    text(x = rep(1.7, var_max), y = (1:var_max) * 2/(1 - var_max) + (var_max + 1)/(var_max - 1), G.text)
  }

  if (var_max == 1) {
    text(x = 2, y = 0, G.text)
  }
  # text(x=0,y=-1.4, expression(paste(tau, '1: the inverse of the smallest eigenvalue')))
}
