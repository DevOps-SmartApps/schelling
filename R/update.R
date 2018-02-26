#' Control the update of configuration and show animation.
#' @param initial the list outputted by the function initial_config().
#' @param n_agent an integer representing the total number of agents.
#' @param nr,nc the rectangular grid size parameters.
#' @param t a number between 0 and 1 representing the similarity threshold.
#' @param r a number between 0 and 1 specifying the maximum fraction of satisfied agents to relocate.
#' @param max_iter an integer specifying the maximum number of iterations (default is 10000). If the parameter r=0, the simulation will stop when a static equilibrium is reached.
#' @param anim_step an integer that that controls the animation: show updated map every anim_step iterations (default is 20000).
#' @param sleep_time parameter that controls the animation: pause the execution for sleep_time seconds (default is 0, meaning no pause). This is useful in cases where the execution is too fast for the plots to update.
#' @examples 
#' nr <- 50; nc <- 50; N <- nr*nc
#' f_white <- 0.1 # 10% of unoccupied houses
#' g <- 4   # 4 groups
#' fg <- c(1,2,3,4)  # number of agents in the 4 groups has ratios 1:2:3:4
#' g_color <- c("red","blue","green","gold")
#' t <- 0.15; r <- 0.05
#' # set up initial configuration
#' set.seed(8326454)
#' initial <- initial_config(nr,nc,f_white, g,fg,g_color)
#' # Run update() for 10 iterations
#' final <- update(initial,n_agent,nr,nc,t,r,10)
update <- function(initial,n_agent,nr,nc,t,r, max_iter=10000L, 
                   anim_step=20000L, sleep_time=0) {
  # calculate the mean similarity ratio
  rsim_avg <- sum(initial$rsim)/n_agent
  
  # set up the grid for plotting
  x <- rep(1:nr,nc)+0.5
  y <- rep(1:nc,each=nr)+0.5
  
  # start simulation
  round_k <- initial
  for (k in 1:max_iter) {
    round_k <- relocate(round_k,nr,nc,t,r)
    
    # append rsim_avg
    rsim_avg <- c(rsim_avg, sum(round_k$rsim)/n_agent)
    
    # If r=0, check if an equilibrium has been reached.
    if (r==0) {
      if (all(round_k$rsim[round_k$house != "white"] >= t)) {
        # No more dissatisfied agents. Stop the simulation.
        break
      }
    }
    
    # animation
    if (k %% anim_step==0) {
      plot(x,y, pch=16, col=as.character(round_k$house),xlab="",ylab="", xaxt='n',yaxt='n',
           xlim=c(2.8,nr-0.8),ylim=c(2.8,nc-0.8),
           main=substitute(paste("Iteration ",kk,": t = ",tt,", ",bar(r)[sim]," = ",rsimAvg),
                           list(tt=t,kk=k, rsimAvg=sprintf("%0.4f",rsim_avg[k+1]))))
      abline(v=1:(nc+1), h=1:(nr+1))
      if (sleep_time > 0) {
        Sys.sleep(sleep_time) 
      }
    }
  }
  
  # output the last configuration in a list
  list(house=round_k$house, rsim=round_k$rsim, rsim_avg=rsim_avg)
}