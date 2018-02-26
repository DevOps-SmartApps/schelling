#' Compute the similarity ratio at each grid point
#'
#' @param m a matrix of size nr x nc storing the occupation information of the house at each grid point.
#' @param r_sim a matrix of size nr x nc storing the old similarity ratios at each grid point.
#' @param nr,nc the rectangular grid size parameters.
#' @export
compute_rsim <- function(m, r_sim, nr, nc) {
  # The max() and min() functions are called to take care of boundary and corner points
  for (i in 1:nr) {
    istart <- max(i-1,1)
    iend <- min(i+1,nr)
    for (j in 1:nc) {
      jstart <- max(j-1,1)
      jend <- min(j+1,nc)
      r_sim[i,j] <- compute_rsim_1point(m,i,j,istart,iend,jstart,jend)
    }
  }
  r_sim
}



#' Compute the similarity ratio at the grid point (i,j)
#'
#' @param m a matrix of size nr x nc storing the occupation information of the house at each grid point.
#' @param i,j integers: the grid point (i,j) at which the similarity ratio is to be computed.
#' @param istart,iend,jstart,jend input parameters that specify the neighbors of (i,j): the neighbors are the grid points at (istart:iend, jstart:jend) excluding (i,j) and locations where m is "white" (empty houses). These parameters are determined by the values of i, j, nr, and nc. They are computed in the functions compute_rsim() and relocate() right before calling this function.
#' @return numeric vector of length 1 returning the similarity ratio at point (i,j).
#' @export
compute_rsim_1point <- function(m, i,j, istart,iend, jstart,jend) {
  if(m[i,j]=="white") {
    # (i,j) is an empty location, return 0
    return(0)
  }
  r_sim <- 1
  # convert m[istart:iend,jstart:jend] from a matrix to a character vector
  # and then remove the empty houses
  neighbors <- as.character(m[istart:iend,jstart:jend])
  neighbors <- neighbors[neighbors != "white"]
  # n_neighbor: Number of neighbors, -1 is inserted to substract off the (i,j) location
  n_neighbor <- length(neighbors)-1
  # n_sim: number of neighbors who are like (i,j), -1 is inserted to substract off (i,j)
  n_sim <- sum(neighbors==m[i,j])-1
  # If there is at least one neighbor, calculate the similarity ratio;
  # otherwise return 1
  if (n_neighbor != 0) {
    r_sim <- n_sim/n_neighbor
  }
  r_sim
}
