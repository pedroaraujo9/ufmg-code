myrunif = function(n, seed = 28) {
  m = (2^31) -1 
  a = 7^5
  x = numeric(length = n)
  x[1] = seed
  sp = numeric(length = n)
  sp[1] = x[1]/m
  for(i in 2:(n)) {
    x[i] = (a*x[i-1]) %% m
    sp[i] = x[i]/m
  }
  
  return(sp)
}