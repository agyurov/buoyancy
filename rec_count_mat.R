x = sets[1:7,1:7]
x[upper.tri(x)] = 0
diag(x) = 1

# default {1,5} {2} {3} {4} {6} {7} 6,1,2,1
x[5,3] = x[6,5] = x[7,3] = x[4,2] = 1 # {1,5,3,7,6} {4,2} # 3,0,0,4
# x[5,3] = x[6,5] = x[7,3] = 1 # {1,5,3,7,6} {2} {4} # 4,3,5,3
# x[5,2] = x[4,3] = 1   # {1,5,2} {3,4} {6} {7} # 5,2,5,2
# x[1,1] = 0


rw = which(x != 0) %% 7
rw[rw == 0] = 7
cl = rep(1:7, colSums(x))
a = cbind(rw, cl)
groups = 7 - qr(x)$rank
y = x
y = y[-which(rowSums(y) + colSums(y) == 0), -which(rowSums(y) + colSums(y)  == 0)]
z = x[-which(rowSums(x) == 0), -which(colSums(x) == 0)]


cat('\014')
x
a
groups
qr(y)$rank
dim(y)
qr(z)$rank



orthog = function(y, x){
  # x matrix, y vector
  out = apply(x, 2, function(x, y) x %*% y, y = y)
  return(out)
}

mat = x
colnames(mat) = NULL

rec.count.mat = function(set = 1, mat){
  olset = c(set, unlist(lapply(set, function(x,y) which(orthog(y[,x], y) != 0), y = mat)))
  olset = unique(olset)
  (what = olset %in% set)
  if(all(what)){
    return(olset)
  }
  set = rec.count.mat(mat, set = olset)
}


lapply(1:7, rec.count.mat, mat = x)

