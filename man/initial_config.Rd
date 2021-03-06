% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/initial_config.R
\name{initial_config}
\alias{initial_config}
\title{Set up the initial configuration by randomly assigning houses to agents.}
\usage{
initial_config(nr, nc, f_white, g, fg, g_color)
}
\arguments{
\item{nr, nc}{integers for setting up a rectangular grid of size nr x nc.}

\item{f_white}{a number representing the fraction of unoccupied houses.}

\item{g}{an integer indicating the number of groups.}

\item{fg}{numeric vector of length g containing the relative ratios of each group; for example fg = c(1,2,1) means that there are equal number of agents in groups 1 and 3, and the number of agents in group 3 is twice of the number in group 1.}

\item{g_color}{character vector of length g specifying the color representation of each group. For example, g_color=c("red","blue","green") means that group 1 agents are represented by red, group 2 agents by blue and group 3 agents by green. Note that unoccupied houses are represented by the color "white", so "white" should not be used to represent agents of any group.}
}
\value{
A list containing two elements named house and rsim. house is a nr x nc matrix storing the occupation information of the house at each grid point; rsim is a nr x nc matrix storing the similarity ratio at each grid point.
}
\description{
Set up the initial configuration by randomly assigning houses to agents.
}
\examples{
nr <- 50; nc <- 50; g <- 4
fg <- c(1,1,1,1)  # equal number of agents in all 4 groups
g_color <- c("red","blue","green","gold")
f_white <- 0.1 # 10\% of unoccupied houses
set.seed(8326454)
initial <- initial_config(nr,nc,f_white, g,fg,g_color)
# plot the initial configuration
x <- rep(1:nr,nc)+0.5
y <- rep(1:nc,each=nr)+0.5
plot(x,y, pch=16, col=as.character(initial$house),xlab="",ylab="",
     xaxt='n',yaxt='n', xlim=c(2.8,nr-0.8), ylim=c(2.8,nc-0.8),
     main="Initial Configuration")
abline(v=1:(nc+1), h=1:(nr+1))
}
