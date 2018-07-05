library(magrittr)


#distância eucliadiana 
X = iris[sample(1:nrow(iris), 10), 1:4]
x1 = X[1,] 
x2 = X[2,]


euclidiana = function(xi, xj) {
  return(sqrt(sum((xi-xj)^2)))
}

#distância generalizada
#tem matriz de ponderação 
generalizada = function(xi, xj, A) {
  xi = xi %>% unlist() %>% cbind()
  xj = xj %>% unlist() %>% cbind()
  d = xi - xj
  return(sqrt(as.numeric(t(d)%*%A%*%d)))
}


#distância de minkowsky
minkowsky = function(xi, xj, w, lambda) {
  return(sum(w*abs(xi-xj)^(lambda))^(1/lambda))
}


#testando
euclidiana(x1,x2)
generalizada(x1, x2, solve(cov(X)))
minkowsky(x1, x2, w = sapply(X, FUN=max), lambda = 2)


#matriz de distância (não é eficiente)
D = function(x, metodo, ...) {
  p = ncol(x)
  n = nrow(x)
  d = matrix(0, n, n)
  for(i in 1:n){
    for(j in 1:n)
    d[i, j] = metodo(x[i,], x[j,], ...)
  }
  return(d)
}

dd = D(X, eucliadiana)



#concordância simples

simples = function(xi, xj) {
  return(sum(x1==x2)/length(xi))
}

simples(x1,x2)


#concordância positiva
positiva = function(xi, xj) {
  return(sum(xi==1 & xj == 1)/length(xi))
}

positiva(x1,x2)

#jaccard
jaccard = function(xi, xj) {
  nxi = xi[!(xi == 0 & xj == 0)]
  nxj = xj[!(xi == 0 & xj == 0)]
  return(sum(nxj==1 & nxi==1)/length(nxi))
  
}

jaccard(x1,x2)

#distância euclidiana média

euclidiana_media = function(x1, x2) {
  return(sqrt(sum((x1-x2)^2)/length(x1)))
}


