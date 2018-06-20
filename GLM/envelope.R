library(ggplot2)
library(doParallel)
library(foreach)

envelope = function(modelo) {
  h = influence(modelo)$hat
  ts = resid(modelo, type="pearson")/sqrt(1-h)
  td = resid(modelo, type="deviance")/sqrt(1-h)
  di = (h/(1-h))*(ts^2)
  a = max(td)
  b = min(td)
  
  cl = makeCluster(detectCores())
  registerDoParallel(cl)

  X = model.matrix(modelo)
  n = nrow(X)
  p = ncol(X)
  td = resid(modelo,type="deviance")/sqrt(1-h)
  e = matrix(0,n,100)
  
  e = foreach(i=1:100, .combine = "cbind") %dopar% {
    dif = runif(n) - fitted(fit.model)
    dif[dif >= 0 ] = 0
    dif[dif<0] = 1
    nresp = dif
    fit <- glm(nresp ~ X, family=binomial)
    h = influence(fit)$hat
    sort(resid(fit,type="deviance")/sqrt(1-h))
    
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
    geom_point(alpha=0.4, size = 0.3)  + 
    geom_line(aes(x=e1x,y=e1y), size=1, col='blue', show.legend = T) + 
    geom_line(aes(x=e2x,y=e2y), size=1, col='blue') + 
    geom_line(aes(x=medx,y=medy), size=1, col='grey') +
    theme_bw() +labs(title="Gráfico de envelope", x="Percentis N(0,1)",y="Resíduos")
  return(p1)
}

#