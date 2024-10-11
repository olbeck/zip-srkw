### Functions for simulating data ---------------------------

## Simulaiton of GLM and GLMMAdaptive 
sim_results <- function(mod,  design.mat , nsim = 10){
  sim.dat <- stats::simulate(mod, nsim = nsim)
  
  
  datsim4 <-
    foreach(n = 1:nsim,
            .packages = c( "igraph", "centiserve")) %dopar% {
              
              dat.temp <- design.mat
              dat.temp$Val <- sim.dat[, n]
              
              
              #make adjacency matrix
              A <- matrix(0, nrow = 22, ncol = 22)
              for(i in 1:nrow(dat.temp)){
                a <- dat.temp$V1[i]
                b <- dat.temp$V2[i]
                A[a,b] <- dat.temp$Val[i]
              }
              
              A <- A + t(A)
              
              #make an igraph 
              g.temp <- graph_from_adjacency_matrix(A, mode = "undirected", weighted = TRUE)
              
              
              
              #get centrality measures
              # degree.sim <- igraph::degree(g.temp)
              # strength.sim <- igraph::strength(g.temp)
              eigen.sim <- igraph::eigen_centrality(g.temp)$vector
              close.sim <- igraph::closeness(g.temp, normalized = T) #igraph::betweenness(g.temp, normalized = TRUE) #needs to be normalized
              between.sim <- igraph::betweenness(g.temp, normalized = T)
              
              #Return
              list(sim.dat = sim.dat[, n],
                   # degree.sim = degree.sim,
                   # strength.sim = strength.sim,
                   eigen.sim = eigen.sim,
                   close.sim = close.sim,
                   between.sim = between.sim)
              
            }
  
  results4 <- as.data.frame(do.call(rbind, datsim4))
  return(results4)
}


####### Mallows distance functions #########


mallow_test <- function(results, 
                        trimming_bound = 1/22){
  n.sim <- nrow(results)
  
  close.ts <- rep(NA, n.sim)
  eigen.ts <- rep(NA, n.sim)
  between.ts <- rep(NA, n.sim)
  
  for(i in 1:n.sim){
    
    close.ts[i] <- (Gamma_alpha(results$close.sim[[i]], whale.close, trimming_bound))^2
    eigen.ts[i] <- (Gamma_alpha(results$eigen.sim[[i]], whale.eigen, trimming_bound))^2
    between.ts[i] <- (Gamma_alpha(results$between.sim[[i]], whale.between, trimming_bound))^2
    
  }
  
  return(list(closeness = close.ts,
              eigen =eigen.ts,
              between = between.ts))
  
  
}

