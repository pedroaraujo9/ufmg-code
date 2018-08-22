library(stringr)
library(magrittr)
library(microbenchmark)
library(purrr)
library(caret)
set.seed(10)

x = seq(-10, 10, 0.001)
x = sample(x,60, replace = T)
y = (0.5 + 1*x + 0.3*(x^2) + 0.04*(x^3)) + rnorm(n=length(x), mean=0, sd=8.5)

#modelo original y = 0.5 + 0.4x + 0.3x^2 + 3x^3
plot(x,y)

#gerando formulas para modelos polinomiais
gen_poly = Vectorize(function(n) {
  
  form = str_c("y ~ ", str_flatten(str_c("I(x^", str_c(1:n, ")")), "+"))
  form
})

#erro quadrátio médio com os dados treinados
eqm = function(modelo) {
  sum(residuals(modelo)^2)/nrow(modelo$model)
}

#AIC 
lapply(gen_poly(1:20), FUN = lm) %>% sapply(FUN=AIC) %>% plot()

#validação cruzada leave-one-out
cross_loo = function(modelo) {
  x = modelo$model[,2]
  y = modelo$model[,1]
  n = length(x)
  eqm = numeric(length = n)
  
  for(i in 1:n) {
    pred = lm(formula(modelo), subset = -c(i)) %>% predict(data.frame(x=x[i]))
    eqm[i] = (y[i] - pred)^2
    
  }
  sum(eqm)/(n-2)
}

lapply(gen_poly(1:10), FUN=lm) %>% sapply(FUN=cross_loo) %>% sqrt()

#particionaldo o banco
treino_indice = sample(1:length(x), 40)
treino = x[treino_indice]
validacao = x[-treino_indice]

cross = function(modelo) {
  set.seed(10)
  treino_indice = sample(1:length(x), 40)
  x = modelo$model[,2][treino_indice]
  validacao = modelo$model[,2][-treino_indice]
  validacao_y = modelo$model[,1][-treino_indice]
  y = modelo$model[,1][treino_indice]
  n = length(validacao)
  
  pred = lm(formula(modelo)) %>% predict(data.frame(x=validacao))
  
  sum((validacao_y - pred)^2)/n
}

lapply(gen_poly(1:10), FUN=lm) %>% sapply(FUN=cross) %>% plot()


#### usando caret
df = data.frame(x,y)
treino = trainControl(method="LOOCV")
model = train(y~x, data=df, trControl = treino, method="lm")
print(model)










