# wrapping cm

what = sample(1:4,4,replace = F)
df = iris[, what]

r = max(dist(df))

midpoint = function(x,y){
  out = (x + y)/2
  return(out)
}

midpoint.mat = function(x,y){
  # x = vector
  # y = matrix
  y = y[-match(x,y)[1],] # remove x itself
  out = apply(y, 1, midpoint, x)
  return(t(out))
}

N = 50

s = sample(1:nrow(df), N, replace = T)

df2 = as.matrix(df[s,])

fku = list()
for(i in 1:nrow(df2)){
  fku[[i]] = midpoint.mat(df2[i,], df2)
}

x = do.call(rbind, fku)
x = x[, -seq(N,N*(N-1),N)]

plot(df[,1], df[,2], pch = 20, col = iris$Species)
for(i in 1:nrow(x)){
  points(x[i,1], x[i,2], pch = 20, cex = .5)
}

library(scatterplot3d)

p1 = scatterplot3d(x[,1:3], pch = 20, cex.symbols = .5)

p1$points3d(iris[,what[1:3]], col = as.numeric(iris$Species) + 1, pch = 20)
