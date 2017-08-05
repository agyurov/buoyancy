# draft
# source("../Rfunctions/Rfunctions.R")

library(ggplot2)
library(deldir)
df = data.frame(v1 = runif(100, 0, 2),
                v2 = runif(100, 0, 3))

df = iris[,1:4]
resp = iris$Species




# grid --------------------------------------------------------------------

rec.grid = function(a,x){
  # a is list of ranges for each variable
  out = seq(a[1], a[length(a)], len = 2^x + 1)
  return(out)
}

in.interval = function(x, y){
  # x data, y = interval
  # return(cbind(x, findInterval(x, y)))
  return(findInterval(x, y, rightmost.closed = T))
}

#  ------------------------------------------------------------------------




# algorithm ---------------------------------------------------------------


a = lapply(df, range)
cond = T
i = 0
results = result = cm = fake = gridz = list()
nsteps = 2
newdf = df

for(i in 1:nsteps){
  # i = i + 1
  
  # grid
  a = lapply(a, rec.grid, x = i)
  
  # count
  for(j in 1:ncol(df)){
    fake[[j]] = in.interval(df[,j], a[[j]])
  }
  gridz[[i]] = do.call(paste0, fake)
  names(gridz)[i] = paste0('g', 2^i)
  newdf = cbind(df, do.call(paste0, fake))
  names(newdf)[ncol(newdf)] = 'grp'
  cm[[i]] = aggregate(.~grp, newdf, mean)
  
  # plot
  plot(df[,1], df[,2], pch = 20, col = gridz[[i]])
  abline(v = a[[1]], lty = 2, col = 'grey')
  abline(h = a[[2]], lty = 2, col = 'grey')
  points(cm[[i]][,2], cm[[i]][,3], pch = 4, lwd = 2, cex = 2)
  
  # evaluate condition
  
  # readline('fesfes')
}

mass = lapply(gridz, table)
finaldf = cbind(df, do.call(cbind.data.frame, gridz))


for(i in 1:nsteps){
  results[[i]] = cbind(mass[[i]], cm[[i]])
}

result = results[[nsteps]]


# exploration -------------------------------------------------------------

groups = result[result$Freq %in% sort(result$Freq, decreasing = T)[1:3],]
groups
tr = aggregate(. ~ Species, iris, mean)
tr



# predict -----------------------------------------------------------------

my.dist = function(x,y){
  return(sqrt(sum((x-y)^2)))
}

g1 = apply(df, 1, my.dist, y = groups[1,4:7])
g2 = apply(df, 1, my.dist, y = groups[2,4:7])
g3 = apply(df, 1, my.dist, y = groups[3,4:7])

prd = cbind(g1, g2, g3)

prdctn = apply(prd, 1, which.min)

table(prdctn, resp)