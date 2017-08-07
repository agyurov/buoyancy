# one more

rm(list = ls())

# functions ---------------------------------------------------------------

mydist = function(x,y){
  return(sqrt(sum((x-y)^2)))
}


# data --------------------------------------------------------------------

df = iris[, 1:4]
df = as.matrix(df)
resp = iris$Species

# df = OrchardSprays[,1:3]
# resp = OrchardSprays$treatment

# init --------------------------------------------------------------------




# count chain -------------------------------------------------------------

scanned = sample(1:nrow(df), 1) # available indexes
av = NULL
x = df[scanned,]
cond = T
run = j = 0
chain = rad = list()
rad[[1]] = r = .01

while(cond){
  run = run + 1
  
  # calc distances
  tmp = apply(df[-scanned,], 1, mydist, y = x) 
  
  # which pts are in range
  tmp2 = tmp <= r
  if(any(tmp2)){
    # add those points to the list of available pts to start searching from
    av = unique(c(av, which(tmp2)))
    # pick a new pt from this list
    newpt = sample(av, 1)
    # add this new pt to the list of scanned pts to avoid duplicating
    scanned = c(scanned, newpt)
    # remove the scanned pts from the list of available pts
    av = av[!av %in% scanned]
    # choose new x 
    x = df[newpt,]
  }

  # readline('lol')
  
  # exit if we have scanned everything
  if(length(scanned) >= nrow(df)){
    break
  }
  # increase radius if everything has been scanned
  if(!any(tmp2)){
    
    r = 1.25*r
    if(!is.null(av)){
      
      
    }
    j = j + 1
    chain[[j]] = av
    # names(chain)[j] = paste0("run", j)
    rad[[j]] = r
    # names(rad)[j] = paste0("run", j)
  }

  
}




# explore -----------------------------------------------------------------
cat('\014')
resp[av]
run
length(av)
length(scanned)
r
plot(unlist(rad), unlist(lapply(chain, length)), type = 'l')