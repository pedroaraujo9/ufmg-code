setwd("c:/users/pedro/desktop/matérias/2018-01/glm/trabalho glm")

library(tidyverse)
library(translateR)
library(stringr)
library(gridExtra)


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
    geom_bar(aes(y=..prop.., group=1)) + 
    theme_minimal()
    
  return(p1)
}

relacione = function(data, var) {
  data[, var] = as.factor(data[,var])
  p1 = ggplot(data, aes_string(x="resposta", fill=var)) + 
    geom_bar(position="fill") + 
    theme_minimal()
  
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
  geom_bar(position = "fill") + 
  labs(title="Distribuição de sucessos por idade", x="idade", y="Frequência relativa") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90)) #nota-se uma certa relação entre idade e sucesso


#distribuição do trabalho
do_bar(data_tl, "trabalho", ordenar(data_tl$trabalho)) + 
  labs(title="Distribuição das profissões", x="Status", y="Proporção") + 
  theme(axis.text.x = element_text(angle=90))


relacione(data_tl, "trabalho") + 
  labs(title="Distribuição do tipo de trabalho na resposta",
       y="Proporção", x="Resposta") #parece não haver distinção dentro da resposta

#estado civil
do_bar(data_tl, "estado_civil", ordenar(data_tl$estado_civil)) + 
  labs(title="Distribuição do estado civil" , x="Estado Civil", y="Proporção")

relacione(data_tl, "estado_civil") #solteiros parecem ser mais dispostos a aceitar


ggplot(data_tl, aes(x=factor(idade), fill=estado_civil)) + 
  geom_bar(position = "fill") + 
  theme_minimal() + 
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
  geom_histogram(aes(y=..density..))


#casa

do_bar(data_tl, "casa_emprestimo")
relacione(data_tl, "casa_emprestimo")






















