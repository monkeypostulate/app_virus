model_infectious<-function(g,perc)
{
  n<-length(V(g))
  V(g)$state <- 0 # non- infected
  V(g)$period <- 0 # non- infected
  infected <- sample(1:n, round(n * perc), replace = F) # simulate infections
  
  V(g)$name <- 1:length(V(g))
  t1 <- V(g)$name %in% infected
  V(g)[ t1]$state <- 1
  V(g)[ t1]$period <- 1
  
  return(g)
}

g_infected <- function(g, perc, prob, period, period_cured,n_contacts) {
  # #########################
  # Select neighbor
  n <- length(V(g))
  
  # individuals with interactions that can cause a transmition
  nodes_interacting <- sample(1:n, round(n * perc), replace = F) # simulate infections
  
  nodes_interacting <- V(g)[(V(g)$name %in% nodes_interacting &
                               V(g)$state == 1)]$name
  
  if (length(nodes_interacting) > 0) {
  #  for (i in 1:length(nodes_interacting)) {
      current_node <- V(g)[V(g)$name  %in% nodes_interacting ]
      neigh <- ego(g,
                   order = 1, nodes = current_node,
                   mindist = 0)

      potential_infected <- as.character(neigh[[1]])
      
      if(length(neigh)>1){
     for( ne in 2:length(neigh))
       potential_infected <- unique(c(potential_infected,as.character(neigh[[ne]]) ) )
      }
      
      
      if (length(potential_infected) > 1) {
        potential_infected <- potential_infected[!potential_infected %in% current_node ]
      #  potential_infected <- sample(potential_infected,
       #                              min(n_contacts,length(potential_infected)))
        
        # #########################
        # infected
       # for (k in 1:length(potential_infected)) {
          new_infectious<-rbinom(length(potential_infected), 1, prob) 
        potential_infected<-potential_infected[new_infectious==1]
        potential_infected<-potential_infected[!is.na(potential_infected)]
        #  if (rbinom(1, 1, prob) == 1) {
        t1 <- V(g)$name %in% potential_infected
        t2 <- V(g)$state==0
        # if (V(g)[ t1]$state == 0) {
              
              V(g)[ t1 & t2]$state <- 1
              V(g)[ t1 & t2]$period <- period
         #   }
          #}
       # }
      }
    }
  
  V(g)[(V(g)$period <= period_cured
        & V(g)$state == 1)]$state <- 2
  
  return(g)
}




divisors <- function(x){
  #  Vector of numberes to test against
  y <- seq_len(x)
  #  Modulo division. If remainder is 0 that number is a divisor of x so return it
  y[ x%%y == 0 ]
}
generate_network<-function(n_size, n_g=NA,
                           pn,p_comm,
                           gamma_param=NA){
  
  
  g <- erdos.renyi.game(n_size, p = pn / n_size, 
                              loops=F,directed=F)  
  if(!is.na(gamma_param)){
  g_temp<-barabasi.game(n_size,power=1,directed=F,m=1)
  n_g_temp<-length(E(g_temp))
  
  edge_delete<-sample(1:n_g_temp,round(n_g_temp*3/4),replace=F)
  g_temp<-g_temp%>%
    delete_edges( edges=edge_delete)
  
  g<-g_temp%>%
    add_edges(as.vector(t(as_edgelist(g))))
  
  }
  
  matr<-matrix(0,ncol=n_g,nrow=n_g)
  
  if(!is.na(n_g)){
  comm_size=n_size/n_g
  p_comm<-p_comm/comm_size
  diag(matr)<-p_comm
  
  g_temp22<-sample_sbm(n_size,
                       pref.matrix=matr,
                       block.sizes=rep(comm_size,n_g),
                       directed=F, loops=F)
  g<-g%>%
    add_edges(as.vector(t(as_edgelist(g_temp22))))
  g<-igraph::simplify(g,remove.multiple = TRUE)
  ly<-layout_with_fr(g_temp22)
  
  
  }
  return(list(g=g,ly=ly))
}




simulate_virus_spread<-function(g,
                               t_time,
                               lockdown,
                               duration,prob){
  
  n<-length(V(g))
  V(g)$state <- 0 # non- infected
  V(g)$period <- 0 # non- infected
  
  V(g)$name <- 1:length(V(g))
  t1 <- round(n*2/3) # Patient zero is node 80
  V(g)[t1]$state <- 1
  V(g)[ t1]$period <- 1
  
  

  for (t in 2:t_time) {
    t1 <- t - duration
    if(t>=lockdown$start & t<=lockdown$end){
      perc<- lockdown$perc_lockdown
    }else{ 
      perc<-lockdown$perc}
    g<- g_infected(g, prob =prob, perc =perc,
                   period = t, period_cured = t1,
                    n_contacts=4)
    
  }
  return(g)
  
}


