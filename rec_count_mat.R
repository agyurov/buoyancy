x = sets[1:7,1:7]
x[upper.tri(x)] = 0
diag(x) = 1

# default {1,5} {2} {3} {4} {6} {7} 6,1,2,1
x[5,3] = x[6,5] = x[7,3] = x[4,2] = 1 # {1,5,3,7,6} {4,2} # 3,0,0,4
# x[5,3] = x[6,5] = x[7,3] = 1 # {1,5,3,7,6} {2} {4} # 4,3,5,3
# x[5,2] = x[4,3] = 1   # {1,5,2} {3,4} {6} {7} # 5,2,5,2
# x[1,1] = 0


orthog = function(y, x){
  # x matrix, y vector
  out = apply(x, 2, function(x, y) x %*% y, y = y)
  return(out)
}

rec.count.mat = function(set = 1, mat){
  olset = c(set, unlist(lapply(set, function(x,y) which(orthog(y[,x], y) != 0), y = mat)))
  olset = unique(olset)
  (what = olset %in% set)
  if(all(what)){
    # print.noquote(olset)
    return(sort(olset, decreasing = F))
  }
  set = rec.count.mat(mat, set = olset)
}




b = list()
j = 0
for(i in 1:ncol(x)){
  j = j + 1
  b[[j]] = rec.count.mat(set = i, x)
  b = unique(b)
  b[unlist(lapply(b, is.null))] = NULL
}
names(b) = paste0('clstr', 1:length(b))
b

clusters = function(x){
  # x matrix of pts in range
  b = list()
  j = 0
  for(i in 1:ncol(x)){
    j = j + 1
    b[[j]] = rec.count.mat(set = i, x)
    b = unique(b)
    b[unlist(lapply(b, is.null))] = NULL
  }
  names(b) = paste0('clstr', 1:length(b))
  return(b)
}

cat('\014')
clusters(x)
