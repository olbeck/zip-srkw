## For when alpha = 0, and n=m
Gamma_0 <- function(Xdist, Ydist ){
  
  n <-length(Xdist)
  
  Xsort <- sort(Xdist)
  Ysort <- sort(Ydist)
  
  ret <- sqrt(sum(abs(Xsort-Ysort)^2)/(n))
  return(ret)
  
}


#´Alvarez-Esteban et al. (2008)

# Minimax confidence intervals for the
# Sliced Wasserstein distance
# Tudor Manole, Sivaraman Balakrishnan, Larry Wasserman
# page 2257


## For when alpha =/= 0, and n=m
Gamma_alpha <- function(Xdist, Ydist, alpha ){
  
  Xsort <- sort(Xdist)
  Ysorted <- sort(Ydist)
  
  # Calculate the number of elements to trim
  n <- length(Xsort)
  trim_count <- max(floor(alpha * n), 1)
  alpha.corrected <- trim_count / n
  
  # Trim both vectors by keeping the middle (1 - 2a) proportion
  i <- (trim_count + 1):(n - trim_count)
  Xtrim <- Xsort[i]
  Ytrim <- Ysorted[i]
  
  ret <- sqrt(sum(abs(Xtrim-Ytrim)^2)/(n) / (1-2*alpha.corrected)) 
  return(ret)
  
} 
