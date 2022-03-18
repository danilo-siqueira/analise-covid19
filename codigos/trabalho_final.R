#############################################################################
##############                                                 ##############
##############           Trabalho Final IAED - 2021            ##############
##############          Nome:   Danilo Alves Siqueira          ##############
##############                  RA: 201026121                  ##############
##############                                                 ##############
#############################################################################

# Lendo as tabelas de "Casos Geral" e "Mortes":
if(!"readxl" %in% installed.packages()) install.packages("readxl")
if(!"lubridate" %in% installed.packages()) install.packages("lubridate")
if(!"xlsx" %in% installed.packages()) install.packages("xlsx")
if(!"tidyverse" %in% installed.packages()) install.packages("tidyverse")
if(!"RColorBrewer" %in% installed.packages()) install.packages("RColorBrewer")
library(RColorBrewer)
library(readxl)
library(xlsx)
library(tidyverse)
library(lubridate)

tabelaCasosGeral <- read.csv("./dados/covid_19_bauru_casos_geral.csv", header = T, sep=";", encoding = "UTF-8")
tabelaMortes <- read.csv("./dados/covid_19_bauru_mortes.csv", header = T, sep=";", encoding = "UTF-8")

str(tabelaCasosGeral)
str(tabelaMortes)

#############################################################################

#Classificaçao das variáveis:

#Tabela Casos Gerais:
#Todas as variáveis = Quantitativas Discretas

#Tabela Mortes:
#data_boletim = quantitativa discreta
#sexo = qualitativa nominal
#idade = quantitativa discreta
#inicio_sintoma = quantitativa discreta
#tipo_hosp = qualitativa nominal
#comorbidade = qualitativa nominal
#data_obito = quantitativa discreta
#doses_vacina = quantitativa discreta

#############################################################################

#Infectados por idade:
summary(tabelaMortes$idade)
#Tabela de frequência
k <- nclass.Sturges(tabelaMortes$idade)
minIdade <- min(tabelaMortes$idade,na.rm = TRUE)
maxIdade <- max(tabelaMortes$idade, na.rm = TRUE)

intervalos <- seq(minIdade,maxIdade + round(((maxIdade - minIdade)/k)),k)
idade.t <- table(cut(tabelaMortes$idade,breaks = intervalos,right = FALSE))

par(mfrow=c(1,1))
hist(las = 1,tabelaMortes$idade,col = "black",border= "white",breaks= intervalos,
     xlab= "Idade dos Infectados",ylab= "Freq.(Pessoas)",
     main="Figura 1 - Quantidade de Mortes por Idade")

cbind("fr" = addmargins(prop.table(idade.t)))
#Pessoas entre 50-84 anos representam cerca de 70% das mortes e bebês até adultos representam cerca de 13,5%

#############################################################################
#Relaçao óbitos e comorbidades

comorbidades <- strsplit(tabelaMortes$comorbidade, " e ")

vetor_comorbidades <- unlist(comorbidades)
tabela_comorbidades <- table(vetor_comorbidades)
cbind("f" = tabela_comorbidades)

tb_reduzida_comorbidades <- tabela_comorbidades[tabela_comorbidades>=40]
cbind("f" = tb_reduzida_comorbidades)

corDegrade <- colorRampPalette(c("yellow","red"))
corDegrade(7)
barplot(sort(tb_reduzida_comorbidades), ylab = "Nº de mortos", ylim=c(0,400),
        names.arg = c("DNC", "DRC", "DCVC", "OBSD", "Cardiopatia", "Hipert.", "Diabetes"),
        col = corDegrade(7), main = "Figura 2 - Frequência de Comorbidades em Óbitos")

#Das 7 comorbidades que mais aparecem entre óbitos, Diabetes e Hipertensao sao as mais frequentes.

#############################################################################
#Variaçao Periódica de Internaçao

#Transformando data em padrao
data_inicio_sintoma <- strptime(tabelaMortes$inicio_sintoma, "%d/%m/%Y")
class(data_inicio_sintoma)
data_inicio_sintoma

data_do_obito <- strptime(tabelaMortes$data_obito, "%d/%m/%Y")
class(data_do_obito)

#Calculando variaçao periodica de obitos e suas respectivas idades
th <- c()
idade_obitos <- c()
tipo_hosp <- c()
i <- 1
j <- 0
while (i <= nrow(tabelaMortes)) {
  if(!is.na(data_inicio_sintoma[i]) && !is.na(data_do_obito[i])) {
    j = j + 1
    th[j] = ymd_hms(data_do_obito[i], truncated = 3) - ymd_hms(data_inicio_sintoma[i], truncated = 3)
    idade_obitos[j] = tabelaMortes$idade[i]
    tipo_hosp[j] = tabelaMortes$tipo_hosp[i]
  }
  i = i + 1
}
th

# Limpando valores negativos
dias_internado <- th[th >= 0]   #variaçao periodica > 0
dias_internado

idade_positiva_obitos <- idade_obitos[th >= 0]    #idade relativa dos obitos com variaçao periodica > 0
idade_positiva_obitos

internacao <- table(th[th >= 0])    #Tabela de frequência absoluta
cbind("f" = internacao)

cores <- brewer.pal(8, "BrBG")
barplot(internacao,main = "Figura 3 - Óbitos por Variaçao Periódica do Tempo de Hospitalizaçao",
     xlab = "Tempo internado (dias)", ylab = "Óbitos (pessoas)", ylim = c(0,50), col = cores)

# media_dias_internado <- mean(dias_internado)
# summary(dias_internado)
# 
# dp_dias_internado <- sd(dias_internado)

#############################################################################
#Comparando var. periódica entre internaçao pública e privada

th2 <- c()
tipo_hosp <- c()
i <- 1
j <- 0
while (i <= nrow(tabelaMortes)) {
  if(!is.na(data_inicio_sintoma[i]) && !is.na(data_do_obito[i]) && !is.na(tabelaMortes$tipo_hosp)) {
    j = j + 1
    th2[j] = ymd_hms(data_do_obito[i], truncated = 3) - ymd_hms(data_inicio_sintoma[i], truncated = 3)
    tipo_hosp[j] = tabelaMortes$tipo_hosp[i]
  }
  i = i + 1
}

internacao2 <- table(th2[th2 >= 0])    #Tabela de frequência absoluta
cbind("f" = internacao2)

dias_internado_2 <- th2[th2 >= 0]
dias_internado_2

tipo_hosp_tratada <- tipo_hosp[th2 >= 0]
tipo_hosp_tratada

df_hospitalizaco = data.frame(dias = dias_internado_2, hospitalizacao = tipo_hosp_tratada)

dias_pv.t <- table(df_hospitalizaco$dias[df_hospitalizaco$hospitalizacao == "privado"])
cbind("f" = dias_pv.t)

dias_pb.t <- table(df_hospitalizaco$dias[df_hospitalizaco$hospitalizacao == "público"])
cbind("f" = dias_pb.t)

par(
  mfrow=c(1,2),
  mar=c(4,4,1,0)
)

barplot(dias_pb.t, ylim=c(0,30), col=rgb(1,0,0,0.5) ,
     xlab="Tempo internado(dias) - Público" , ylab="Óbitos (pessoas)" , main="" )

barplot(dias_pv.t, ylim=c(0,30), col=rgb(0,0,1,0.5) ,
     xlab="Tempo internado(dias) - Privado" , ylab="Óbitos (pessoas)" , main="")

##############################################################################
############################## FIM DO TRABALHO ###############################
##############################################################################
