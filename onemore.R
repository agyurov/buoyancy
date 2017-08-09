# one more

rm(list = ls())
cat('\014')
# functions ---------------------------------------------------------------

mydist = function(x,y){
  return(sqrt(sum((x-y)^2)))
}

myprint = function(x,y){
  print(paste0(x,  paste0(y, collapse = " ")))
}

overlap = function(x, r, df){
  tmp = apply(df, 1, mydist, y = x)
  # tmp = tmp < r & tmp != 0
  tmp = tmp < r
  return(tmp)
}

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


# data --------------------------------------------------------------------

df = iris[, 1:4]
df = as.matrix(df)
resp = iris$Species

# df = OrchardSprays[,1:3]
# resp = OrchardSprays$treatment



# parameters --------------------------------------------------------------

N = .5 * nrow(df) # number of spheres
r = .1 # initial radius
dr = 1.05 # delta radius
nclusters = NULL
maxrun = 200



# count chain -------------------------------------------------------------

x = sample(1:nrow(df), 10, replace = F) # spheres



run = 0
cond = T
while(cond){
  run = run + 1
  cat('--------------------\n')
  cat(paste0('Run: ', run, '\n'))
  
  # tmp = apply(df, 1, mydist, y = x)
  sets = apply(df[x,], 1, overlap, r = r[length(r)], df = df[x,])
  if(is.list(sets)){
    names(sets) = paste0('pt', x)
  }
  if(is.matrix(sets)){
    colnames(sets) = paste0('pt', x)
    rownames(sets) = paste0('pt', x)
  }

  # resulsts
  r = c(r, r[length(r)] * dr)
  nclusters = c(nclusters, length(clusters(sets))) # old
  
  # rm(sets)
  print(nclusters[length(nclusters)])
  
  # condition
  if(run > maxrun){
    cat(paste0('Maximum iteration ', maxrun, ' reached.\n'))
    cond = F
  }
  if(nclusters[length(nclusters)] == 1){
    cat('Entire data set wrapped. One cluster.\n')
    cond = F
  }
  
  # readline('next:')
}

r = r[-1]

# explore -----------------------------------------------------------------

plot(r, nclusters, type = 'l')
abline(h = 1:N, col = 'grey', lty = 2)
# text(x = mean(par('usr')[1:2]), y = nclusters[length(nclusters)],
#      labels = paste0('Final # clusters ', nclusters[length(nclusters)]), col = 2)
nclusters[length(nclusters)]

