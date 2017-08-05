# draft
# source("../Rfunctions/Rfunctions.R")

library(ggplot2)

df = data.frame(v1 = runif(100, 0, 1),
                v2 = runif(100, 0, 4))


# params ------------------------------------------------------------------

dimz = lapply(df, range)

# init --------------------------------------------------------------------

cm = list()



# grid --------------------------------------------------------------------
a = list(c(0,1), c(0,4))
rec.grid = function(a,x){
  # a is list of ranges for each variable
  out = seq(a[1], a[length(a)], len = 2^x + 1)
  return(out)
}

in.interval = function(x, y){
  # x data, y = interval
  # return(cbind(x, findInterval(x, y)))
  return(findInterval(x, y))
}

#  ------------------------------------------------------------------------




# algorithm ---------------------------------------------------------------


a = list(c(0,1), c(0,4))
plot(0, xlim = a[[1]], ylim = a[[2]], col = 'white')

cond = T
i = 0
fake = gridz = list()
nsteps = 4

for(i in 1:nsteps){
  # i = i + 1
  
  # grid
  a = lapply(a, rec.grid, x = i)
  
  # count
  
  
  # plot
  abline(v = a[[1]], lty = 2, col = 'grey')
  abline(h = a[[2]], lty = 2, col = 'grey')
  
  for(j in 1:ncol(df)){
    fake[[j]] = in.interval(df[,j], a[[j]])
  }
  gridz[[i]] = do.call(paste0, fake)
  points(df$v1, df$v2, pch = 20, col = gridz[[i]])
  # evaluate condition
  
  readline('fesfes')
}

lapply(gridz, table)

