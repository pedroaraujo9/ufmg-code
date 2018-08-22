library(magrittr)

#gerando números pseudoaleatórios de uma uniforme (0, 1)
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

#examplo
myrunif(100, seed=20)

#transformação inversa com variável discreta
#gerando de uma distribuição tal discreta tal que:
#P(X=0) = 0.1, P(x=1) = 0.35, P(X=2) = 0.4, P(X=3) = 0.15

rmydistri = function(n) {
  rd = numeric(length = n)
  u = myrunif(n)
  for(i in 1:n) {
    if(u[i] <= 0.1) {
      rd[i] = 0
    }else if(u[i] <= 0.45) {
      rd[i] = 1
    }else if(u[i] <= 0.85) {
      rd[i] = 2
    }else {
      rd[i] = 3
    }
  }
  return(rd)
}
#checando
myrDistri(10000) %>% table() %>% prop.table()

#transformação inversa com variável contínua

#exemplo com uma normal(0,1)

myrnorm = function(n) {
  u = myrunif(n) #u(0,1)
  qnorm(u) #aplicando na inversa
}


rnormtrunc = function(n, mu, sigma, a, b) {
  u = myrunif(n)
  qnorm(u*(pnorm(b, mu, sigma)-pnorm(a, mu, sigma)) + pnorm(a, mu, sigma), mu, sigma)
  
} 

#teste
rnormtrunc(5000, mu = 10, sigma= 1, a = 5, b = 12) %>% hist()

#transformação inversa em uma exponencial
myrexp = function(n, lambda) {
  u = myrunif(n)
  -(log(1-u))/lambda
}


myrexp(n=500, lambda = 2) %>% hist()

#método da transformação == basta usar uma variável aleatória para gerar outra

#cauchy padrão é definida como (x1/(x1+x2)) sendo x1 e x2 normais padrão

myrcauchy = function(n) {
  x1 = rnorm(n)
  x2 = rnorm(n)
  x1/(x1 + x2)
}

myrcauchy(3000) %>% hist()

#aceitação-rejeição

#distribuição alvo -> beta(10, 10) usando u(0,1)
f = function(x){dbeta(x, 2, 2)}
g = function(x){dunif(x, 0, 1)}

alpha = 0.7

y = runif(1000)

u = runif(1000)

amostra = (f(y)/g(y))*alpha

y[u < amostra] %>% hist()


aceptrej = function(n, f, g, Y, alpha, ...) {
  amostra = numeric(length = n)
  
  while(length(amostra[amostra!=0]) <  n){
    y = Y(1)
    u = runif(1)
    amostra = y[u < ((f(y)/g(y))*alpha)]
  }
  amostra
}

#arrumar
aceptrej(n=100, f=dbeta, g=dunif, Y=runif, alpha=0.2, shape1=2, shape2=2)


#numéro esperado de iterações: 1/alpha

##SIR

f = Vectorize(function(x){
  if(x==0) {
    (1/(2*sqrt(2*pi)))
  }else{
    ((1-exp((-x^2)/2))/((x^2)*sqrt(2*pi)))}
}) 

g = function(x){dnorm(x)}

rslash = function(n) {
  x = rnorm(n)
  u = runif(n)
  x/u
}

y = rslash(10000)
y = y[!is.na(y)]


w = (f(y)/g(y))/sum(f(y)/g(y))
y = w[!is.na(w)]
w = w[!is.na(w)]



sample(y, 50, prob = w) %>% hist()










