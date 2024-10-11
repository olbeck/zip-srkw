### Surfacing Simulation Plots ----------------

### ECDF Plots 

plot_ecdf <- function(sim_dat){
  
  num_sims <- length(sim_dat)
  
  plot_eigen <- vector(mode = "list", length = num_sims)
  plot_close <- vector(mode = "list", length = num_sims)
  for(i in 1:num_sims){
    
    plot_eigen[[i]] <- 
      sim_dat[[i]] %>% 
      mutate(id = row_number()) %>% 
      dplyr::select(eigen.sim, id) %>%
      unnest(cols = c(eigen.sim)) %>% 
      mutate(id = as.factor(id)) %>%
      #filter(id == 1) %>%
      ggplot(aes(x=eigen.sim, group = id)) +
      stat_ecdf(geom = "step", alpha = 0.01) +
      stat_ecdf(data = dat.plot.true, aes(x = eigen.sim), color = "red") +
      theme_minimal()+
      ggtitle("ECDF Of Eigen Centrality") +
      xlab("Eigen Centrality")+
      ylab("Empirical Cumulative Density")
    
    
    
    plot_close[[i]] <- 
      sim_dat[[i]] %>% 
      mutate(id = row_number()) %>% 
      dplyr::select(close.sim, id) %>%
      unnest(cols = c(close.sim)) %>% 
      mutate(id = as.factor(id)) %>%
      #filter(id == 1) %>%
      ggplot(aes(x=close.sim, group = id)) +
      stat_ecdf(geom = "step", alpha = 0.01) +
      stat_ecdf(data = dat.plot.true, aes(x = close.sim), color = "red") +
      theme_minimal()+
      ggtitle("ECDF Of Closeness Centrality")+
      xlab("Closeness Centrality") +
      ylab("Empirical Cumulative Density")
  }
  
  return(list(eigen = plot_eigen,
              close = plot_close))
  
  
}



### Wasserstein Distance - Surfacing

plot_wass_density_surface <- function(mallow_results_testing){
  
  mallow_eigen_testing <- lapply(mallow_results_testing, "[[", "eigen")
  mallow_close_testing <- lapply(mallow_results_testing, "[[", "closeness")
  
  plot_eigen_testing <- 
    do.call(data.frame, mallow_eigen_testing) %>%
    `colnames<-`(c("Model 1",  "Model 2")) %>% 
    pivot_longer(everything(), names_to = "type", values_to = "value") %>% 
    ggplot(aes(x = value, color = type)) +
    #geom_density() +
    stat_density(geom="line",position="identity")+
    theme_minimal()+
    ggtitle("Density of Trimmed Mallows' Distance",
            subtitle = "Eigenvector Centrality") +
    xlab("Distance") + 
    ylab("Density")+
    xlim(0, 0.04)+
    guides(color=guide_legend(title="Model",
                              override.aes = list(shape = "line", linetype = c("solid")))) +
    theme(legend.position = c(0.8, 0.6), 
          legend.background = element_rect(fill="white", 
                                           size=0.5, linetype="blank"),
          legend.text = element_text(size = 8), 
          legend.title = element_text(size = 10), 
          axis.line = element_line())  +
    scale_y_continuous(expand=c(0,1.5)) +
    scale_x_continuous(expand=c(0,0))
  
  
  plot_close <- 
    do.call(data.frame, mallow_close_testing ) %>%
    `colnames<-`(c("Model 1",  "Model 2")) %>% 
    pivot_longer(everything(), names_to = "type", values_to = "value") %>% 
    ggplot(aes(x = value, color = type)) +
    #geom_density() +
    stat_density(geom="line",position="identity")+
    theme_minimal()+
    ggtitle("Density of Trimmed Mallows' Distance",
            subtitle = "Closeness Centrality") +
    xlab("Distance") + 
    ylab("Density")+
    # xlim(0, 0.004)+
    guides(color=guide_legend(title="Model",
                              override.aes = list(shape = "line", linetype = c("solid")))) +
    theme(legend.position = c(0.8, 0.6), 
          legend.background = element_rect(fill="white", 
                                           size=0.5, linetype="blank"),
          legend.text = element_text(size = 8), 
          legend.title = element_text(size = 10),
          axis.line = element_line()) +
    scale_y_continuous(expand=c(0,1.5)) +
    scale_x_continuous(expand=c(0,0)) 
  
  
  
  return(list(eigen = plot_eigen_testing ,
              close = plot_close))
  
}



### Contact Network Plots -----------------------------------


### Wasserstein Distance Contact 

plot_wass_density_contact <- function(mallow_results){
  
  mallow_eigen <- lapply(mallow_results, "[[", "eigen")
  mallow_close <- lapply(mallow_results, "[[", "closeness")
  
  plot_eigen <- 
    do.call(data.frame, mallow_eigen) %>%
    `colnames<-`(c("Walktrap", "Leading Eigenvalue", "Infomap", "Fluid Communities")) %>% 
    pivot_longer(everything(), names_to = "type", values_to = "value") %>% 
    ggplot(aes(x = value, color = type)) +
    #geom_density() +
    stat_density(geom="line",position="identity")+
    theme_minimal()+
    ggtitle("Density of Trimmed Mallows' Distance",
            subtitle = "Eigenvector Centrality") +
    xlab("Distance") + 
    ylab("Density")+
    xlim(0, 0.07)+
    guides(color=guide_legend(title="Clustering Method",
                              override.aes = list(shape = "line", linetype = c("solid")))) +
    theme(legend.position = c(0.8, 0.6), 
          legend.background = element_rect(fill="white", 
                                           size=0.5, linetype="blank"),
          legend.text = element_text(size = 8), 
          legend.title = element_text(size = 10)
    ) + 
    scale_color_discrete(breaks=c("Walktrap", "Leading Eigenvalue", "Infomap", "Fluid Communities"))
  
  plot_close <- 
    do.call(data.frame, mallow_close) %>%
    `colnames<-`(c("Walktrap", "Leading Eigenvalue", "Infomap", "Fluid Communities")) %>% 
    pivot_longer(everything(), names_to = "type", values_to = "value") %>% 
    ggplot(aes(x = value, color = type)) +
    #geom_density() +
    stat_density(geom="line",position="identity")+
    xlim(0, 0.02)+
    theme_minimal()+
    ggtitle("Density of Trimmed Mallows' Distance",
            subtitle = "Closeness Centrality") +
    xlab("Distance") + 
    ylab("Density")+
    guides(color=guide_legend(title="Clustering Method",
                              override.aes = list(shape = "line", linetype = c("solid")))) +
    theme(legend.position = c(0.8, 0.6), 
          legend.background = element_rect(fill="white", 
                                           size=0.5, linetype="blank"),
          legend.text = element_text(size = 8), 
          legend.title = element_text(size = 10)
    ) + 
    scale_color_discrete(breaks=c("Walktrap", "Leading Eigenvalue", "Infomap", "Fluid Communities"))
  
  
  
  return(list(eigen = plot_eigen,
              close = plot_close))
  
}

