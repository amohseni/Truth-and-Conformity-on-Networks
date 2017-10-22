# Network Space
# by Aydin Mohseni
# www.aydinmohseni.com

# Load libraries
library(igraph)

N <- 10 # Number of agents
nodes <- c(1:N)

# WANT: algorithm that does the following:
# creating a sequence of graphs
# (1) regular networks from degree 2 to N
# (2) from regular networks of centrality 0 to deformed

# Create regular networks of degree 2 to N
# create an empty list to store the networks
regularNetworks.List <- list()

for (degree in 2:(N - 1)) {
  
  # Create regular network
  if (degree %% 2 == 0) { # If the degree is even
    x <- rep(1:N, each = degree / 2)
    y <- x + c(1:(degree / 2))
    y[y > N] <- y[y > N] - N
    edges <- data.frame(from = x, to = y)
  } else { # If the degree is odd
    x <- rep(1:N, each = ceiling(degree / 2))
    y <- x + c(1:floor(degree / 2), 0)
    y[seq(
      from = ceiling(degree / 2),
      to = ceiling(degree / 2) * N,
      by = ceiling(degree / 2)
    )] <-
      y[seq(
        from = ceiling(degree / 2),
        to = ceiling(degree / 2) * N,
        by = ceiling(degree / 2)
      )] + N / 2
    y[y > N] <- y[y > N] - N
    edges <- data.frame(from = x, to = y)
  }
  
  adjacencyMatrix <- data.frame(matrix(NA, nrow = N, ncol = N - 1))
  
  # PLOT: format the data for plotting
  for (j in 1:N) {
    adjMatSub <- subset(edges, from == j | to == j)
    adjMatSubUnique <- unique(adjMatSub[adjMatSub != j])
    adjacencyMatrix[j,] <-
      c(adjMatSubUnique, rep(NA, N - 1 - length(adjMatSubUnique)))
  }
  
  regularNetworks.List[[degree-1]] <- adjacencyMatrix
  
  net <- graph_from_data_frame(d = edges, vertices = 1:N, directed = F)
  net <- simplify(net, remove.multiple = T, remove.loops = T)
  l <- layout_in_circle(net)

  # plot network
  plot(net, edge.arrow.size = .4, vertex.label = NA, layout = l,
       vertex.frame.color = "black", 
       vertex.color = "black",
       edge.color="gray")

}

# Create a list for the network space
networkSpace.List <- rep(list(list()), N - 1)

for (k in 2:(N - 1)) {
  # Deform the regular networks,
  # increasing centrality with each step
  x <- regularNetworks.List[[k - 1]]
  
  # Create a list in which to save each of the deformed network
  # for the particular starting regular network
  deformedNetworks.List <- list()
  deformedNetworks.List[[1]] <- regularNetworks.List[[k - 1]]
  
  # Identiify all of the links can can be redirected to the hub agent
  # First, find all the nodes that DO link to the hub (so they do no 'double-link')
  u <- c(1)
  for (i in 1:N) {
    if (any(x[i,] == 1, na.rm = TRUE)) {
      u <- append(u, i, after = length(u))
    }
  }
  # find the set of links NOT to the hub (agent 1)
  w <- which(x != 1, arr.ind = TRUE)
  # take the subset of these that belong to nodes not already linked to the hub agent
  v <- w[-which(w[, 1] %in% u), ]
  
  # Proceed only if there are some links that CAN be redirected to a central agent
  if (length(v) > 0) {
    # and limit these so that each agent shifts ONLY ONE
    # of her links to the central agent
    v <- v[c(1:length(unique(v[, 1]))),]
    if (length(v) > 2) {
      deformations <- dim(v)[1]
    } else {
      demormations <- 1
    }
    
    # Find all the nodes that NOW link to the hub (so they do no 'double-link')
    for (j in 1:deformations) {
      # pick the next link to be deformed
      if (length(v) > 2) {
        z <- v[j,]
      } else {
        z <- v
      }
      # and replace this link with a link to the hub (agent 1)
      x[z[1], z[2]] <- 1
      deformedNetworks.List[[j + 1]] <- x
      
      ### Format the data for the NETWORK PLOT
      edges <- data.frame(from = x, to = y)
      
      # PLOT: format the data for plotting
      for (j in 1:N) {
        adjMatSub <- subset(edges, from == j | to == j)
        adjMatSubUnique <- unique(adjMatSub[adjMatSub != j])
        adjacencyMatrix[j,] <-
          c(adjMatSubUnique, rep(NA, N - 1 - length(adjMatSubUnique)))
      }
      
      net <- graph_from_data_frame(d = edges, vertices = 1:N, directed = F)
      net <- simplify(net, remove.multiple = T, remove.loops = T)
      l <- layout_in_circle(net)
      
      # plot network
      plot(net, edge.arrow.size = .4, vertex.label = NA, layout = l,
           vertex.frame.color = "black", 
           vertex.color = "black",
           edge.color="gray")
      
    }
  }
  # Save the list of deformed networks for the network space
  networkSpace.List[[k]] <- deformedNetworks.List
}

# plot all of the networks
plot(net, edge.arrow.size = .4, vertex.label = NA, layout = l,
     vertex.frame.color = "black", 
     vertex.color = "black",
     edge.color="gray")


