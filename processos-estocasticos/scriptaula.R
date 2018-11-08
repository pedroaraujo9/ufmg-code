#instalando pacote
install.packages("markovchain")
#chamando funções do pacote
library(markovchain)

#criando objeto markov chain

#definindo matriz
P = rbind(c(0.4, 0.1, 0.3, 0.2),
          c(0.1, 0.3, 0.4, 0.2), 
          c(0.2, 0.2, 0.3, 0.3),
          c(0.3, 0.1, 0.3, 0.3))

#somando as linhas 
rowSums(P)

#classe do objeto
class(P); P

#label dos estados
estados = as.character(0:3); estados 

#definindo objeto markov chain
Pmc = new("markovchain", states = estados, transitionMatrix = P, byrow=T)
Pmc

#periodo
period(Pmc)

#acessibilidade de um estado i para um estado j
is.accessible(Pmc, from="0", to="1")

#checando irredutibilidade
is.irreducible(Pmc)


#distribuição estacionária
steadyStates(Pmc)

#classes fechadas e de transição
communicatingClasses(Pmc)

#gerando amostras aleatórias
rmarkovchain(n=100, Pmc, t0="0")


#cálculo "manual" da distribuição estacionária
I = diag(rep(1, 4));I

M = (I - P)
M[,4] = 1;M


rbind(c(0,0,0,1))%*%solve(M)


#utilizando autovalores/vetores
eigen(t(P))

v = eigen(t(P))$vectors[,1];v

(1/sum(v))*v

