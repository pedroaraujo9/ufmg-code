#carregando pacotes
library(rjags)
library(coda)

### modelo de regressão simples

#Y = b0 + b1*x + e, e ~ n(0, sigma2)


#simulando dados

#selecionando semente
set.seed(10)

#simulando banco de dados
x1 = rnorm(30, 10, 30)
x2 = rnorm(30, 3, 4)
x3 = rnorm(30, -1, 1)

#modelo teórico y = 4 + 7x1 + 10x2 - 5x3 + e, e ~  n(0, 25)

y = 4 + 7*x1 + 10*x2 - 5*x3 + rnorm(n=30, mean=0, sd=5)

#gráfico de disperção
plot(y~x1)
plot(y~x2)
plot(y~x3)


#banco de dados
X = cbind(x1, x2, x3)


#modelo clássico
modelo = lm(y~X)
summary(modelo)

#intervalo de confiança de 95% para os betas
confint(modelo)


#modelo bayesiano no jags
model_string = "model{
  #verossimilhança
  for(i in 1:n) {
    y[i] ~ dnorm(mu[i], tau)
    
    mu[i] = b0 + inprod(b, x[i,])
  }
  
  #prioris não informativas

  #betas
  b0 ~ dnorm(0, 0.0001)
  for(i in 1:p) {
    b[i] ~ dnorm(0, 0.0001)
  }

  #precisão
  tau ~ dgamma(0.001, 0.001)


  #reparametrizando para o desvio padrão e não a precisão
  sigma = sqrt(1/tau)

}"


#definindo variáveis que serão usadas no modelo
#número de obervações
n = length(y)
p = ncol(X)
#compilando modelo
modelo_jags = jags.model(textConnection(model_string), 
                         #lista com os dados
                         data = list(n=n,
                                     x=X,
                                     p=p,
                                     y=y)) 

#gernaod amostra a posteriori
amostra = coda.samples(modelo_jags,
                      #iterações
                      n.iter=1000, 
                      #número de cadeias
                      n.chains=1, 
                      #lag
                      thin=1,
                      #variáveis que devem ser retornadas para análise
                      variable.names = c("b0", "b", "sigma")
                      )

#estatísticas descritivas
summary(amostra)

#média ergódiga
cumuplot(amostra)

#autocorrelação
autocorr.plot(amostra)

#densidade
densplot(amostra[,'b0'])
densplot(amostra[,'b1'])
densplot(amostra[,'sigma'])

#hpd
HPDinterval(amostra)



















