setwd("c:/users/pedro/desktop/matérias/2018-01/glm/trabalho glm")

library(tidyverse)
library(translateR)
library(stringr)
library(gridExtra)
library(RColorBrewer)


#BANCO DE DADOS
dados = data.table::fread("bank-full.csv", stringsAsFactors = T) %>% as.data.frame()

dim(dados)

dados$ybin = ifelse(dados$y=="yes", 1, 0)

dados = dados %>% 
  rename(idade = age, trabalho = job, estado_civil = marital, educacao = education,
         credito_default = default, saldo = balance,
         casa_emprestimo = housing, emprestimo = loan, 
         contato_tipo = contact, mes = month, dia = day, duracao = duration, 
         chamadas = campaign, pausa = pdays, chamadas_prev = previous, 
         status_prev = poutcome)

#traduzindo variáveis
trans = function(data, colunas) {
  for(col in colunas) {
    x = data[, col] %>% as.character()
    en = unique(x)
    tl = translate(content.vec = en, source.lang = "en", target.lang = "pt", 
                   google.api.key = "")
    
    for(i in seq_along(tl)) {
      x[x==en[i]] = tl[i]
    }
    data[, col] = x
  }
  
  data
}

data_tl = trans(data = dados, 
                colunas = c("estado_civil", "educacao", "credito_default", 
                            "casa_emprestimo", "emprestimo", "contato_tipo", 
                            "mes", "status_prev", "trabalho", "y"))

save(data_tl, file="dados_tl.RData")

load("dados_tl.RData")
data_tl = rename(data_tl, resposta=y)
data_tl$trabalho = ifelse(data_tl$trabalho == "trabalhadores por conta própria", 
                          "autônomos", data_tl$trabalho)

data_tl$mes = ifelse(data_tl$mes == "pode", "maio", data_tl$mes)

ordenar = function(x) {table(x) %>% sort() %>% rev() %>% names()}

#NA's
VIM::aggr(data_tl)

#ANÁLISE DESCRITIVA

do_bar = function(data, var, ordem = NULL) {
  if(is.null(ordem)) {
    data[, var] = factor(data[,var])
  }else{
    data[, var] = factor(data[,var], levels = ordem)
  }
  p1 = ggplot(data, aes_string(x=var)) + 
    geom_bar(aes(y=..prop.., group=1), fill="dodgerblue4", col="white") + 
    theme_minimal()
    
  return(p1)
}

relacione = function(data, var, ordem = NULL) {
  if(is.null(ordem)) { 
    data[, var] = as.factor(data[,var])
    p1 = ggplot(data, aes_string(x="resposta", fill=var)) + 
      geom_bar(position="fill", col="black") + 
      theme_minimal() + 
      scale_fill_brewer(palette = "Blues") 
  }else{
    data[, var] = factor(data[,var], levels = ordem)
    p1 = ggplot(data, aes_string(x="resposta", fill=var)) + 
      geom_bar(position="fill", col="black") + 
      theme_minimal() + 
      scale_fill_brewer(palette = "Blues")
  }

    
  
  return(p1)
}


#distribuição da idade
do_bar(data_tl, "idade") + 
  annotate(geom = "text", x=60, y=0.04, 
           label=paste0("Média: ", mean(data_tl$idade)%>%round(2), "\n", 
                        "Mediana: ", median(data_tl$idade), "\n", 
                        "Desvio padrão: ", sd(data_tl$idade) %>% round(2), "\n")) + 
  theme(axis.text.x = element_text(angle = 90)) 




#distribuição da idade pela resposta
ggplot(data_tl, aes(x=factor(idade), fill=resposta)) + 
  geom_bar(position = "fill", col="black") + 
  labs(title="Distribuição de sucessos por idade", x="idade", y="Frequência relativa") +
  theme_minimal() + 
  scale_fill_brewer(palette = "Blues") +
  theme(axis.text.x = element_text(angle = 90)) #nota-se uma certa relação entre idade e sucesso


#distribuição do trabalho
do_bar(data_tl, "trabalho", ordenar(data_tl$trabalho)) + 
  labs(title="Distribuição das profissões", x="Status", y="Proporção") + 
  theme(axis.text.x = element_text(angle=90))


relacione(data_tl, "trabalho") + 
  labs(title="Distribuição do tipo de trabalho na resposta",
       y="Proporção", x="Resposta") + 
  scale_fill_brewer(palette = "Set3") #parece não haver distinção dentro da resposta

#estado civil
do_bar(data_tl, "estado_civil", ordenar(data_tl$estado_civil)) + 
  labs(title="Distribuição do estado civil" , x="Estado Civil", y="Proporção")

relacione(data_tl, "estado_civil") #solteiros parecem ser mais dispostos a aceitar

#idade/estado civil
ggplot(data_tl, aes(x=factor(idade), fill=estado_civil)) + 
  geom_bar(position = "fill", col="black") + 
  theme_minimal() + 
  scale_fill_brewer(palette = "Blues") +
  theme(axis.text.x = element_text(angle=90))

#educação

do_bar(data_tl, "educacao", c("primário", "secundário","terciário", "desconhecido")) 
relacione(data_tl, "educacao") #parece ter relação

#se tem crédito
do_bar(data_tl, "credito_default")
relacione(data_tl, "credito_default") #sem relação aparente

#saldo

summary(data_tl$saldo)
sd(data_tl$saldo)

data_tl %>% 
  filter(saldo > -2000, saldo < 5000) %>% 
  ggplot(aes(x=saldo, fill=resposta)) + 
  geom_histogram(aes(y=..density..), col="black") + 
  scale_fill_brewer(palette = "Blues") + 
  theme_minimal()


#casa

do_bar(data_tl, "casa_emprestimo")
relacione(data_tl, "casa_emprestimo")


#empréstimo 
do_bar(data_tl, "emprestimo") 
relacione(data_tl, "emprestimo")

#Tipo de contato 
do_bar(data_tl, "contato_tipo", ordem = c("Telefone","celular", "desconhecido"))
relacione(data_tl, "contato_tipo")

#dia do mes
ggplot(data_tl, aes(x=factor(dia), fill=resposta)) + 
  geom_bar(position = "fill", col="black") + 
  scale_fill_brewer(palette = "Blues") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle=90))

#mes 
do_bar(data_tl, "mes", ordem = c("jan", "feb", "mar", "abril", "maio", "junho", 
                                 "julho", "aug", "sep", "oct", "nov", "dec"))
relacione(data_tl, "mes", ordem = c("jan", "feb", "mar", "abril", "maio", "junho", 
                                    "julho", "aug", "sep", "oct", "nov", "dec")) + 
  scale_fill_brewer(palette = "Set3")

#duracao da ligação
summary(data_tl$duracao)

#histograma todas as obs
ggplot(data_tl, aes(x=duracao)) + 
  geom_histogram(col='white')



#histograma < 1500
data_tl %>% 
  filter(duracao < 1500) %>% 
  ggplot(aes(x=duracao, fill=resposta)) + 
  geom_histogram(col='white', aes(y=..density..))


data_tl %>% 
  filter(duracao < 1500) %>% 
  ggplot(aes(x=duracao)) + 
  geom_histogram(col='white', aes(y=..density..)) + 
  facet_grid(~resposta) + 
  theme_minimal()


#chamadas 
table(data_tl$chamadas)

do_bar(data_tl, "chamadas")

#agrupando variáveis
data_tl$chamadas_agr = cut(data_tl$chamadas, 
                           breaks = c(-1, 0, 1, 2, 3, 10,  20, 30, 40, 50, 60), 
                           labels = c("0", "1", "2", "3", "4-10", 
                                      "11-20", "21-30", "31-40", 
                                      "41-50", "51-60"))

#distribuição
relacione(data_tl, "chamadas_agr")

#dias sem ligar 
table(data_tl$pausa) 

data_tl %>% filter(pausa > 0, pausa < 1000) %>% 
  ggplot(aes(x=pausa, fill=resposta)) + 
  geom_histogram(col='black', position = "fill")
  
#campanhas anteriores
do_bar(data_tl, "status_prev")
relacione(data_tl, "status_prev")



modelo_set = data_tl %>% 
  select(-resposta, -chamadas_agr)

modelo_set$mes_num = NA

mes = c("jan", "feb", "mar", "abril", "maio", "junho", 
        "julho", "aug", "sep", "oct", "nov", "dec")

for(i in 1:nrow(modelo_set)) {
  for(j in 1:length(mes)) {
    if(modelo_set$mes[i] == mes[j]) modelo_set$mes_num[i] = j
  }
}


#modelo com mes categórico 
modelo1 = glm(ybin ~ . -mes_num, family=binomial(link="logit"),
              data=modelo_set, na.action = "na.fail")
AIC(modelo1)
#modelo com mes não categórico
modelo2 = glm(ybin ~. -mes, family=binomial(link="logit"),  data=modelo_set)
####modelo com mes categórico é melhor


modelo3 = glm(ybin ~.-idade-mes_num-chamadas_prev-pausa-credito_default-educacao-idade,
              family=binomial(link="logit"),
              data=modelo_set)
AIC(modelo3)

summary(modelo1)
summary(modelo2)
summary(modelo3)


AIC(modelo1)
AIC(modelo2)
AIC(modelo3)


library(MuMIn)

dredge(modelo1)

pchisq(21562, nrow(modelo_set)-(ncol(modelo_set)+1), lower.tail = F)











