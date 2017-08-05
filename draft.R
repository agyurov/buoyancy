# draft
# source("../Rfunctions/Rfunctions.R")

library(ggplot2)
library(deldir)
df = data.frame(v1 = runif(100, 0, 2),
                v2 = runif(100, 0, 3))


# Working examples --------------------------------------------------------

# df = iris[,1:3]
# resp = iris$Species

# df0 = as.data.frame(msleep)
# df0 = na.omit(df0)
# df = df0[,c(6:11)]
# resp = do.call(cbind.data.frame, lapply(df0[,1:5], factor))

df = ToothGrowth[,c(1,3)]
resp = ToothGrowth$supp

nsteps = 2
Ngroups = length(unique(resp))

# functions ---------------------------------------------------------------

my.dist = function(x,y){
  return(sqrt(sum((x-y)^2)))
}

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

# algorithm ---------------------------------------------------------------


a = lapply(df, range)
cond = T
i = 0
results = result = cm = fake = gridz = list()
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
  # plot(df[,1], df[,2], pch = 20, col = gridz[[i]])
  # abline(v = a[[1]], lty = 2, col = 'grey')
  # abline(h = a[[2]], lty = 2, col = 'grey')
  # points(cm[[i]][,2], cm[[i]][,3], pch = 4, lwd = 2, cex = 2)
  
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

groups = result[result$Freq %in% sort(result$Freq, decreasing = T)[1:Ngroups],]
groups

# predict -----------------------------------------------------------------

g = list()
for(i in 1:Ngroups){
  cat(paste0('Run ', i, ' of ', Ngroups, '\n'))
  g[[i]] = apply(df, 1, my.dist, y = groups[i,4:(4+ncol(df)- 1)])
}
names(g) = paste0('g', 1:length(g))

prd = do.call(cbind.data.frame, g)

prdctn = apply(prd, 1, which.min)

cat('\014')
if(is.data.frame(resp)){
  for(i in 1:ncol(resp)){
    print(table(prdctn, resp[,i]))
  }
}
if(!is.data.frame(resp)){
  table(prdctn, resp)
}
