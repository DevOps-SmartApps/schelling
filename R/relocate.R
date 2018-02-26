#' Relocate agents to a random empty location 
#' 
#' Relocate agents to a random empty location with similarity ratio greater than or equal to the similarity threshold t.
#' If no empty locations satisfy the agent's requirement, move the agent to the empty location 
#' with the maximum value of r_sim.
#' 
#' @param config a list containing two elements named house and rsim. config$house is a nr x nc matrix storing the occupation information of the house at each grid point; config$rsim is a nrxnc matrix storing the similarity ratio at each grid point. This list may be taken directly from the output of the function initial_config() or the output of this function.
#' @param nr,nc the rectangular grid size parameters.
#' @param t a number between 0 and 1 representing the similarity threshold.
#' @param r a number between 0 and 1 specifying the maximum fraction of satisfied agents to relocate.
#' @return A list containing two elements named house and rsim, storing the updated information of config after the relocation of agents.
relocate <- function(config,nr,nc,t,r) {
  m <- config$house
  r_sim <- config$rsim
  
  # reloc: indices of dissatisfied agents; it's a matrix with two columns
  reloc <- which( (r_sim < t) & (m != "white"), arr.ind=TRUE)
  
  # satisfied: indices of satisfied agents; it's a matrix with two columns
  satisfied <- which( (r_sim >= t) & (m != "white"), arr.ind=TRUE)
  n_satisfied <- nrow(satisfied)
  
  # randomly pick some (up to r*satisfied agents) satisfied agents to relocate
  n_reloc <- floor(n_satisfied*runif(1, 0,r)+0.5)
  if (n_reloc > 0) {
    reloc_satisfied <- satisfied[sample.int(n_satisfied, n_reloc),]
    reloc <- rbind(reloc, reloc_satisfied)
  }
  
  # randomly shuffle the agents to be relocatd
  n_reloc <- nrow(reloc)
  if (n_reloc > 1) {
    reloc <- reloc[sample.int(n_reloc),] 
  }
  
  # recloate agents
  for (k in 1:n_reloc) {
    
    if (n_reloc==0) {
      # no agents to relocate
      break
    }
    
    # (i,j) indices of the kth agent to be relocated
    i <- reloc[k,1]
    j <- reloc[k,2]
    
    # find the indices of all empty locations
    w <- which(m=="white", arr.ind=TRUE)
    nw = nrow(w)
    # Shuffle the order 
    w <- w[sample.int(nw),]
    
    # find an empty location to relocate the agent 
    rsim_new <- rep(NA, nw)
    for (ii in 1:nw) {
      # (i1,j1): index of an empty location
      i1 <- w[ii,1]
      j1 <- w[ii,2]
      
      # temporary relocate to this empty location and then calculate 
      # the similarity ratio there
      m[i1,j1] <- m[i,j]
      m[i,j] <- "white"
      istart <- max(i1-1,1)
      iend <- min(i1+1,nr)
      jstart <- max(j1-1,1)
      jend <- min(j1+1,nc)
      rsim_new[ii] <- compute_rsim_1point(m, i1,j1, istart,iend, jstart,jend)
      if (rsim_new[ii] >= t) {
        # rsim >=t at this location. Relocation sucessful! 
        # Move on to the next agent
        break
      }
      # Not a good location! Move back to the original location and try another empty spot
      m[i,j] <- m[i1,j1]
      m[i1,j1] <- "white"
      
      if (ii==nw) {
        # All the empty locations are bad! Move to the empty location 
        # with the maximum rsim_new
        ii2 <- which.max(rsim_new)
        i1 <- w[ii2,1]
        j1 <- w[ii2,2]
        m[i1,j1] <- m[i,j]
        m[i,j] <- "white"
      }
    }
  }
  
  # update r_sim 
  r_sim <- compute_rsim(m, r_sim, nr, nc)
  
  # output the updated m and r_sim in a list
  list(house=m, rsim=r_sim)
}