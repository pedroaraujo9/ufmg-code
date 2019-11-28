library(tidyverse)
library(rmutil)
library(mvtnorm)
library(truncnorm)

set.seed(20)
#núemro de observações
n = 400
#covariáveis
x = cbind(
  runif(n, 0, 5),
  runif(n, 0, 3)
)

#categorias
k = 5 
#percentil
p = 0.5
#efeitos
b = cbind(c(1.5, -0.7))

#variável latente
z = numeric(n) 


#transformação
theta = (1-2*p)/(p*(1-p))
tau = sqrt(2/(p*(1-p)))

w = rexp(n)
u = rnorm(n)

z = as.numeric(x%*%b) + theta*w + tau*sqrt(w)*u
hist(z)

#pontos de corte
cutpoints = c(-Inf, 0, 2, 4.5, 6, Inf)
#gerando variável observada
y = cut(z, cutpoints, labels = c(1, 2, 3, 4, 5)) %>% 
  as.character() %>%
  as.numeric()
table(y) %>% prop.table() %>% round(2)

deltas = log(cutpoints[-c(1, length(cutpoints))] %>% diff())
deltas

Fa = function(e) {
  if(e >= Inf) return(1)
  if(e <= -Inf) return(0)
  ifelse(e < 0, p*exp(e*(1-p)), p + (1-p)*(1-exp(-e*p)))
}

Fa = Vectorize(Fa)



logvero = function(deltas, beta) {
  res = numeric(n)
  #beta = cbind(c(param[1], param[2]))
  cuts = c(-Inf, 0, cumsum(exp(deltas)), Inf)

  pred = as.numeric(x%*%beta)
  
  for(i in 1:n) {
    upp = cuts[y[i]+1] - pred[i]
    low = cuts[y[i]] - pred[i]
    res[i] = Fa(upp) - Fa(low)
  }
  
  -sum(log(res))
  
}


optim(par = c(0.1, 0.2, 0.3), logvero, hessian = T, beta = c(0.1, 0.3))


#priors
mub0 = cbind(rep(0, 2))
Sigb0 = diag(rep(1,2))

mudelta0 = cbind(rep(0, length(deltas)))
Sigdelta0 = diag(rep(0.25, length(deltas)))





#inits
iters = 1000
beta = matrix(nrow = iters, ncol = nrow(b)) 
delta = matrix(nrow = iters, ncol = length(deltas))
zi = matrix(nrow = iters, ncol = n)
wi = matrix(nrow = iters, ncol = n)

set.seed(10)
beta[1,] = 0
delta[1,] = runif(length(deltas)) %>% sort()
zi[1, ] = 0
wi[1, ] = rexp(n) 



i = 2
#Sib B
m = matrix(0, ncol(beta), ncol(beta))
for(k in 1:n) {
  m = m + (cbind(x[k,])%*%rbind(x[k,])/(wi[i-1, k]*(tau^2)))
}

Sigb = solve(m + solve(Sigb0))
Sigb

# mu B
s = cbind(rep(0, ncol(beta)))
for(k in 1:n) {
  s = s + cbind(x[k,])*(zi[i-1, k] - theta*wi[i-1,k])/(wi[i-1, k]*(tau^2))
}

s


mub = Sigb%*%(s + solve(Sigb0)%*%mub0)
mub

beta[i,] = rmvnorm(n = 1, as.numeric(mub), Sigb)
beta[i,]

#wi | .

lamb = ((zi[i-1,] - as.numeric(x%*%beta[i,]))/tau)^2
eta = ((theta/tau)^2) + 2


wi[i,] = sapply(lamb, function(x){
  rgig(1, param = c(x, eta, 0.5))
})

#wi[i,] = rgig(1, param = c(lamb[1], eta, 0.5))
#wi[i,] = rginvgauss(n = n, m = sqrt(lamb/eta), 1/lamb, 0.5)

hist(wi[i,])

#delta |.

D = optim(c(0.1, 0.2, 0.3), fn = logvero, hessian = T, beta = beta[i,])$hessian
D = solve(D)
D
t = 3
propd = delta[i-1,] + rmvnorm(1, sigma = t*D)
propd
deltas

f1 = -logvero(propd, beta[i,])
f2 = -logvero(delta[i-1,], beta[i,])

rat = exp(f1 - f2)

p1 = dmvnorm(propd, as.numeric(mudelta0), Sigdelta0)
p2 = dmvnorm(delta[i-1,], as.numeric(mudelta0), Sigdelta0)

alpha = min(c(1, rat*(p1/p2)))
alpha
if(runif(1) < alpha) {
  delta[i,] = propd
}else{
  delta[i,] = delta[i-1,]
}

head(delta,18)


# Z | .
cuts = c(-Inf, 0, cumsum(exp(delta[i,])), Inf)
pred = as.numeric(x%*%beta[i,])
upp = numeric(n)
low = numeric(n)

for(k in 1:n) {
  upp[k] = cuts[y[k]+1]
  low[k] = cuts[y[k]] 
}


zi[i,] = rtruncnorm(n, a = low, b = upp, mean = pred + theta*wi[i,], 
                    sd = sqrt(wi[i,]*tau^2))

hist(zi[i,])
i = i + 1



i = 2
#beta | .
for(i in 2:100) {
  
  m = matrix(0, ncol(beta), ncol(beta))
  for(k in 1:n) {
    m = m + (cbind(x[k,])%*%rbind(x[k,])/(wi[i-1, k]*(tau^2)))
  }
  
  Sigb = solve(m + solve(Sigb0))
  Sigb
  
  # mu B
  s = cbind(rep(0, ncol(beta)))
  for(k in 1:n) {
    s = s + cbind(x[k,])*(zi[i-1, k] - theta*wi[i-1,k])/(wi[i-1, k]*(tau^2))
  }
  
  s
  
  
  mub = Sigb%*%(s + solve(Sigb0)%*%mub0)
  mub
  
  beta[i,] = rmvnorm(n = 1, as.numeric(mub), Sigb)
  beta[i,]
  
  #wi | .
  
  lamb = ((zi[i-1,] - as.numeric(x%*%beta[i,]))/tau)^2
  eta = ((theta/tau)^2) + 2
  
  
  wi[i,] = sapply(lamb, function(x){
    rgig(1, param = c(x, eta, 0.5))
  })
  
  #wi[i,] = rgig(1, param = c(lamb[1], eta, 0.5))
  #wi[i,] = rginvgauss(n = n, m = sqrt(lamb/eta), 1/lamb, 0.5)
  
  hist(wi[i,])
  
  #delta |.
  
  D = optim(c(0.1, 0.2, 0.3), fn = logvero, hessian = T, beta = beta[i,])$hessian
  D = solve(D)
  D
  t = 3
  propd = delta[i-1,] + rmvnorm(1, sigma = t*D)
  propd
  deltas
  
  f1 = -logvero(propd, beta[i,])
  f2 = -logvero(delta[i-1,], beta[i,])
  
  rat = exp(f1 - f2)
  
  p1 = dmvnorm(propd, as.numeric(mudelta0), Sigdelta0)
  p2 = dmvnorm(delta[i-1,], as.numeric(mudelta0), Sigdelta0)
  
  alpha = min(c(1, rat*(p1/p2)))
  alpha
  if(runif(1) < alpha) {
    delta[i,] = propd
  }else{
    delta[i,] = delta[i-1,]
  }
  
  head(delta,18)
  
  
  # Z | .
  cuts = c(-Inf, 0, cumsum(exp(delta[i,])), Inf)
  pred = as.numeric(x%*%beta[i,])
  upp = numeric(n)
  low = numeric(n)
  
  for(k in 1:n) {
    upp[k] = cuts[y[k]+1]
    low[k] = cuts[y[k]] 
  }
  
  
  zi[i,] = rtruncnorm(n, a = low, b = upp, mean = pred + theta*wi[i,], 
                      sd = sqrt(wi[i,]*tau^2))
  
  cat(i, "\r")
  
}





i = 2
#Sib B
m = matrix(0, ncol(beta), ncol(beta))
for(k in 1:n) {
  m = m + (cbind(x[k,])%*%rbind(x[k,])/(wi[i-1, k]*(tau^2)))
}

Sigb = solve(m + solve(Sigb0))
Sigb

# mu B
s = cbind(rep(0, ncol(beta)))
for(k in 1:n) {
  s = s + cbind(x[k,])*(zi[i-1, k] - theta*wi[i-1,k])/(wi[i-1, k]*(tau^2))
}

s


mub = Sigb%*%(s + solve(Sigb0)%*%mub0)
mub

beta[i,] = rmvnorm(n = 1, as.numeric(mub), Sigb)
beta[i,]

#wi | .

lamb = ((zi[i-1,] - as.numeric(x%*%beta[i,]))/tau)^2
eta = ((theta/tau)^2) + 2

wi[i,] = rginvgauss(n = n, m = sqrt(lamb/eta), 1/lamb, 0.5)
 
hist(wi[i,])

#delta |.

D = optim(c(1, 2, 3), fn = logvero, hessian = T, beta = beta[i,])$hessian
D = solve(D)
D
t = 3
propd = delta[i-1,] + t*rmvnorm(1, sigma = D)
propd
deltas

f1 = -logvero(propd, beta[i,])
f2 = -logvero(delta[i-1,], beta[i,])

rat = exp(f1 - f2)

p1 = dmvnorm(propd, as.numeric(mudelta0), Sigdelta0)
p2 = dmvnorm(delta[i-1,], as.numeric(mudelta0), Sigdelta0)

alpha = min(c(1, rat*(p1/p2)))
alpha
if(runif(1) < alpha) {
  delta[i,] = propd
}else{
  delta[i,] = delta[i-1,]
}

head(delta,20)


# Z | .
cuts = c(-Inf, 0, cumsum(exp(delta[i,])), Inf)
pred = as.numeric(x%*%beta[i,])
upp = numeric(n)
low = numeric(n)

for(k in 1:n) {
  upp[k] = cuts[y[k]+1] - pred[k]
  low[k] = cuts[y[k]] - pred[k]
}


zi[i,] = rtruncnorm(n, a = low, b = upp, mean = pred + theta*wi[i,], 
                    sd = sqrt(wi[i,]*tau^2))

hist(zi[i,])
i = i + 1










