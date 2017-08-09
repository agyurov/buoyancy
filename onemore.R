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

multiplyrows = function(x){
  
}

# data --------------------------------------------------------------------

df = iris[, 1:4]
df = as.matrix(df)
resp = iris$Species

# df = OrchardSprays[,1:3]
# resp = OrchardSprays$treatment



# parameters --------------------------------------------------------------

N = .3 * nrow(df) # number of spheres
r = .05 # initial radius
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
  nclusters = c(nclusters, qr(sets)$rank) # old
  
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
  
  readline('next:')
}

r = r[-1]

# explore -----------------------------------------------------------------

plot(r, nclusters, type = 'l')
abline(h = 1:5, col = 'grey', lty = 2)
text(x = mean(par('usr')[1:2]), y = nclusters[length(nclusters)],
     labels = paste0('Final # clusters ', nclusters[length(nclusters)]), col = 2)
nclusters[length(nclusters)]

