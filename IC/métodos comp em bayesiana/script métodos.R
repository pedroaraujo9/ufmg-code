set.seed(10)
#gernado dados de uma pois(2), X|mu ~ N(2, 5)
x = rnorm(100, mean=2, sd=sqrt(5))



### Rejeição

#gerando u ~ unif(0,1)
u = runif(1000)


#gernado da priori mu ~ N(0,100)
mu = rnorm(1000, 0, 10)

n = length(x)
sigma2 = 5
f = numeric(1000)
#f(x|lambda)
for(i in 1:1000) f[i] = ((2*pi*sigma2)^(-n/2))*exp(-sum((x-mu[i])^2)/(2*sigma2))
#f(x|mean(x))
g = ((2*pi*sigma2)^(-n/2))*exp(-sum((x-mean(x))^2)/(2*sigma2))
#valores selecionados
selecionados = u < f/g
#taxa de aceitação
sum(selecionados)/1000

#amostra final
amostraFinal = lamb[selecionados]


#### SIR

m = 100000 #tamanho da amostra gerada da posteriori
nn = 1000 #tamanho da amostra final reamostrada
mu = rnorm(m, 0, 10) #gerando mu da priori mu ~ n(0, 100) 
#calculando f(x|mu)
f = numeric(m) 
for(i in 1:1000) f[i] = ((2*pi*sigma2)^(-n/2))*exp(-sum((x-mu[i])^2)/(2*sigma2))
w=f/sum(f) #pesos
amostraFinal = sample(mu, nn, p=w, replace = T) #reamostrando da amostra final

hist(amostraFinal)







#METROPOLIS-HASTINGS

#inicializando cadeia de tamanho 1000
mu = numeric(1000)
mu[1] = 0


for(i in 2:1000) {
  muprop = rnorm(1, mu[i-1], 2.5);muprop
  
  #log do denominador de R
  a=sum(dnorm(x, mean=muprop, sd=sqrt(5), log=T)) + dnorm(muprop, 0, 3, log=T) + dnorm(mu[i-1], muprop, 2.5)
  #log do numerador de R
  b=sum(dnorm(x, mean=mu[i-1], sd=sqrt(5), log=T)) + dnorm(mu[i-1], 0, 3, log=T) + dnorm(muprop, mu[i-1], 2.5)
  
  #calculando R
  p = exp(a-b);p
  u = runif(1)

  #Regra de aceitação
  
  if(u<=p) {
    mu[i] = muprop
  }else{
    mu[i] = mu[i-1]
  }  
}

plot(mu, type='l')

mean(mu)
hist(mu)
acf(mu)



#### exemplo y = b1x + e
#b1 = 10

#e ~ n(0, 7^2)

#gerando dados
n = 40
x = rnorm(n, 30, 5)
y = 10*x + rnorm(n, 0, 7)

library(invgamma)

#número de iterações
iter = 1000

#inicializando a cadeia
b1 = numeric(iter)
sigma2 = numeric(iter)

b1[1] = 0
sigma2[1] = 1

m1 = 0
v1 = 100
a = 10
b = 100


for(i in 2:iter) {
  mb1 = (v1*sum(y*x) + sigma2[i-1]*m1)/(sum(x^2)*v1+sigma2[i-1]);mb1
  vb1 = (sigma2[i-1]*v1)/(sum(x^2)*v1+sigma2[i-1]);vb1
  b1[i] = rnorm(1, mb1, sqrt(vb1));b1[i]
  
  ap = (n/2) + a + 1;ap
  bp = sum((y-b1[i]*x)^2) + b;bp
  sigma2[i] = rinvgamma(1, ap, bp);sigma2[i]
}

















