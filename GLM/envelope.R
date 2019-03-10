library(ggplot2)
library(doParallel)
library(foreach)

envelope = function(modelo, parallel = T) {
  h = influence(modelo)$hat

  
  cl = makeCluster(detectCores())
  registerDoParallel(cl)
  
  X = model.matrix(modelo)
  n = nrow(X)
  p = ncol(X)
  td = resid(modelo, type="deviance")/sqrt(1-h)
  e = matrix(0, n, 100)
  
  if(parallel == T) {
    e = foreach(i=1:100, .combine = "cbind") %dopar% {
      dif = runif(n) - fitted(modelo)
      dif[dif >= 0 ] = 0
      dif[dif<0] = 1
      nresp = dif
      fit <- glm(nresp ~ X, family=binomial)
      h = influence(modelo)$hat
      sort(resid(fit,type="deviance")/sqrt(1-h))
      
    }  
  }else{
    e = foreach(i=1:100, .combine = "cbind") %do% {
      dif = runif(n) - fitted(modelo)
      dif[dif >= 0 ] = 0
      dif[dif<0] = 1
      nresp = dif
      fit <- glm(nresp ~ X, family=binomial)
      h = influence(modelo)$hat
      sort(resid(fit,type="deviance")/sqrt(1-h))
      
    }
  }
  
  
  e1 = numeric(n)
  e2 = numeric(n)
  
  for(i in 1:n){
    eo = sort(e[i,])
    e1[i] = (eo[2]+eo[3])/2
    e2[i] = (eo[97]+eo[98])/2}
  
  med = apply(e,1,mean)
  faixa = range(td,e1,e2)
  
  x = qqnorm(td, plot.it = F)$x
  y = qqnorm(td, plot.it = F)$y
  
  e1x = qqnorm(e1, plot.it = F)$x
  e1y = qqnorm(e1, plot.it = F)$y
  
  e2x = qqnorm(e2, plot.it = F)$x
  e2y = qqnorm(e2, plot.it = F)$y
  
  medx = qqnorm(med, plot.it = F)$x
  medy = qqnorm(med, plot.it = F)$y
  
  p1 = ggplot(data.frame(e1x,e1y, e2x,e2y,medx,medy,td,x,y), aes(x=x, y=y)) +
    geom_point()  + 
    geom_line(aes(x=e1x,y=e1y), size=0.5, col='black', show.legend = T) + 
    geom_line(aes(x=e2x,y=e2y), size=0.5, col='black') + 
    geom_line(aes(x=medx,y=medy), size=0.5, col='black', linetype='longdash') +
    coord_equal() + 
    theme_bw() +
    labs(title="Gráfico de envelope", x="Percentis N(0,1)",y="Componente")
  return(p1)
}

#