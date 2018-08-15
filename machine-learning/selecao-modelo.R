library(stringr)
library(magrittr)
set.seed(10)

x = c(seq(2, 10, 0.05))
y = (0.5 + 0.4*x + 0.3*(x^2) + 0.9*(x^3)) + rnorm(n=length(x), 0, sd=80)

#modelo original y = 0.5 + 0.4x + 0.3x^2 + 0.9x^3
plot(x,y)

#gerando formulas para modelos polinomiais
gen_poly = Vectorize(function(n) {
  
  form = str_c("y ~ ", str_flatten(str_c("I(x^", str_c(1:n, ")")), "+"))
  form
})

sse = function(modelo) {
  sum(residuals(modelo)^2)
}
#Resíduo 
lapply(gen_poly(1:20), FUN = lm) %>% sapply(FUN=AIC) %>% plot()