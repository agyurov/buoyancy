# draft
# source("../Rfunctions/Rfunctions.R")
cat('\014')
library(ggplot2)
library(deldir)
df = data.frame(v1 = runif(100, 0, 2),
                v2 = runif(100, 0, 3))


# Working examples --------------------------------------------------------

# df0 =  read.csv('dkpol1.csv')
# df0$textlength = nchar(as.character(df0$text))
# resp = factor(df0$screen_name)
# df0 = df0[df0$screen_name %in% names(sort(c(table(resp)), decreasing = T)[1:8]),]
# resp = factor(df0$screen_name)
# df = df0[,sapply(df0, is.numeric)]


df = iris[,2:3]
resp = iris$Species

# df = ToothGrowth[,c(1,3)]
# resp = ToothGrowth$supp

ncubes = 4
Ngroups = 2 #length(unique(resp))
plt = T
convhul = T

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
i = 1
results = result = cm = fake = gridz = list()
newdf = df

for(i in 1:ncubes){
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
  chdata = split(newdf, newdf$grp) # convex hull data
  # plot
  if(plt == T){
    plot(df[,1], df[,2], pch = 20, col = resp, main = paste0('Grid ', 2^(i*2)),
         panel.first = c(abline(v = a[[1]], lty = 2, col = 'grey'),
                         abline(h = a[[2]], lty = 2, col = 'grey'))) #, col = gridz[[i]]
    
    points(cm[[i]][,2], cm[[i]][,3], pch = 4, lwd = 2, cex = 2)
    if(convhul == T){
      ch = lapply(chdata, function(x) chull(x[,1:2]))
      ch = lapply(ch, function(x) ch = c(x, x[1]))
      for(k in 1:length(chdata)){
        lines(chdata[[k]][ch[[k]],1:2])
      }
    }
  }
  

  # evaluate condition
  
  # readline('fesfes')
}
mass = lapply(gridz, table)
finaldf = cbind(df, do.call(cbind.data.frame, gridz))


for(i in 1:ncubes){
  results[[i]] = cbind(mass = as.numeric(mass[[i]]), cm[[i]])
}

result = results[[ncubes]]


# exploration -------------------------------------------------------------

groups = result[result$mass %in% sort(result$mass, decreasing = T)[1:Ngroups],]

# predict -----------------------------------------------------------------

g = list()
for(i in 1:Ngroups){
  cat(paste0('Run ', i, ' of ', Ngroups, '\n'))
  g[[i]] = apply(df, 1, my.dist, y = groups[i,3:(3+ncol(df)- 1)])
}
names(g) = paste0('g', 1:length(g))

prd = do.call(cbind.data.frame, g)

prdctn = apply(prd, 1, which.min)

# cat('\014')
if(is.data.frame(resp)){
  for(i in 1:ncol(resp)){
    print(table(prdctn, resp[,i]))
  }
}
if(!is.data.frame(resp)){
  tbl = table(prdctn, resp)
}
tbl = rbind(tbl, colSums(tbl))
tbl = cbind(tbl, rowSums(tbl))
cat('\014')
tbl


x = kmeans(iris[,1:4], centers = 3)
table(x$cluster, iris$Species)
