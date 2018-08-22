#markov chain
P = rbind(c(0.1, 0.7, 0.2),
          c(0.5, 0.5, 0),
          c(0.4, 0.4, 0.2))

lambda = eigen(P)$values %>% diag() #diagonalizavel
A = eigen(P)$vectors

A%*%((lambda)^19)%*%solve(A) #converge



P = rbind(c(0.2, 0.7, 0.1),
          c(0.4, 0.1, 0.5),
          c(0.5, 0.3, 0.2))
rowSums(P)
eigen(P) #não diagonalizável

lambda = eigen(P)$values %>% diag()
A = eigen(P)$vectors

A%*%((lambda)^20)%*%solve(A)



library(markovchain)
weatherStates = c("sunny", "cloudy", "rain")
byRow = TRUE
weatherMatrix = matrix(data = c(0.70, 0.2,0.1,
                                0.3,0.4, 0.3,
                                0.2,0.45,0.35), byrow = byRow, nrow = 3,
                       
                       dimnames = list(weatherStates, weatherStates))

mcWeather = new("markovchain", states = weatherStates, byrow = byRow,
                transitionMatrix = weatherMatrix, name = "Weather")
uo = c(0.3, 0.4, 0.3)

after7Days = uo * (mcWeather^7)


new("markovchain") %>% print()
plot(mcWeather, package="igraph", box.size = 4)


summary(mcWeather)
markovchain::firstPassage(mcWeather, state=1, n = 10)



#####
#amostrar de uma cadeia
rmarkovchain(n = 10, mcWeather)


#gerando cadeias aleatórias

rMarkov = function(ordem) {
  
}




P = rbind(c(0.2,0.3,0,0.1,0.4),
          c(0.1,0.3,0.3,0,0.3),
          c(0,0.3,0.7,0,0),
          c(0.2,0.4,0.1,0,0.3),
          c(0,0.2,0.3,0.3,0.2))

P4 = P%*%P%*%P%*%P

u%*%P4
