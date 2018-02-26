#' Set up the initial configuration by randomly assigning houses to agents.
#' 
#' @param nr,nc integers for setting up a rectangular grid of size nr x nc.
#' @param f_white a number representing the fraction of unoccupied houses.
#' @param g an integer indicating the number of groups.
#' @param fg numeric vector of length g containing the relative ratios of each group; for example fg = c(1,2,1) means that there are equal number of agents in groups 1 and 3, and the number of agents in group 3 is twice of the number in group 1.
#' @param g_color character vector of length g specifying the color representation of each group. For example, g_color=c("red","blue","green") means that group 1 agents are represented by red, group 2 agents by blue and group 3 agents by green. Note that unoccupied houses are represented by the color "white", so "white" should not be used to represent agents of any group.
#' @return A list containing two elements named house and rsim. house is a nr x nc matrix storing the occupation information of the house at each grid point; rsim is a nr x nc matrix storing the similarity ratio at each grid point.
#' @examples 
#' nr <- 50; nc <- 50; g <- 4 
#' fg <- c(1,1,1,1)  # equal number of agents in all 4 groups
#' g_color <- c("red","blue","green","gold")
#' set.seed(8326454)
#' initial <- initial_config(nr,nc,f_white, g,fg,g_color)
initial_config <- function(nr,nc,f_white, g,fg,g_color) {
  N <- nr*nc # total number of houses
  
  # renormalize fg: n_agent_in_group_g/n_agent
  fg <- fg/sum(fg)
  
  # calculate the number of empty houses and number of agents in each group;
  # round numbers to the nearest integer
  n_white <- round(N*f_white,0)
  n_agent <- N - n_white
  ng <- round(n_agent*fg,0)
  
  # recompute n_agent and n_white to account for rounding
  n_agent <- sum(ng)
  n_white <- N - n_agent
  
  # sanity check
  if (n_white < 2) {
    stop("Error in initial_config: too few empty houses! Please change the input parameters.")
  }
  if (any(ng < 2)) {
    print("Number of agents in each group:")
    names(ng) <- g_color
    print(ng)
    stop("Error in initial config: too few agents in at least one group. Please change the input parameters.")
  }
  
  # set up a matrix and randomly assign agents to the houses in the city
  empty <- rep("white",n_white)
  agents <- as.character(unlist(mapply(rep,g_color,ng)))
  m <- matrix(sample(c(empty,agents)), nrow=nr, ncol=nc)
  # Compute the similarity ratio
  r_sim <- matrix(NA, nrow=nr, ncol=nc)
  r_sim <- compute_rsim(m, r_sim, nr, nc)
  
  # output the initial configuration in a list
  list(house=m, rsim=r_sim)
}