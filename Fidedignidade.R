### Fidedignidade ###
# Pacotes
install.packages("psych")
install.packages("lavaan")
install.packages("qgraph")
install.packages("eRm")
install.packages("devtools")
devtools::install_github("simsem/semTools/semTools")
library(psych)
library(lavaan)
library(qgraph)
library(eRm)
library(devtools)
library(semTools)


# Banco DASS-21
dasspoly<-read.csv("https://raw.githubusercontent.com/wagnerLM/netusf/master/dasspoly",sep = ";")
#View(dasspoly)
# Nome resumido dos itens
dasslabels<-scan("https://raw.githubusercontent.com/wagnerLM/netusf/master/dasslabels",what = "character", sep = "\n")
#dasslabels
# Itens completos 
dassnames<-scan("https://raw.githubusercontent.com/wagnerLM/netusf/master/dassnames",what = "character", sep = "\n")
#dassnames

# Grupos de itens por dimensao (estresse, ansiedade, depressao)
# estresse c(1,6,8,11,12,14,18)
# ansiedade c(2,4,7,9,15,19,20)
# depressao c(3,5,10,13,16,17,21)

# Exemplo inicial com a subescala de depressao 
dasspoly_sub<-dasspoly[,c(3,5,10,13,16,17,21)]
dass_efa<-fa(dasspoly_sub,1,cor = "poly")

# Fidedignidade na TCT:

# Teste e reteste 
# testRetest(t1,t2)

# Formas paralelas
# cor(f1,f2)

# Duas metades
splitHalf(dasspoly_sub)

# Consistencia interna
guttman(dasspoly_sub)
alpha(dasspoly_sub,check.keys = T)
# Alpha "ordinal", com correlacoes policoricas ou tetracoricas
alpha((cor_auto(dasspoly_sub)))

# Fidedignidade em medidas congenéricas (psicometria moderna)

# ver: https://github.com/wagnerLM/tutoriais/blob/master/Tutorial_Fidedignidade_Congen%C3%A9rica.pdf
omega(dasspoly_sub,1)
# Omega ordinal, ver alpha ordinal
omega((cor_auto(dasspoly_sub)),1)
?omega

# Exemplo de Fidedignidade Composta (Omega de McDonald)
# e Variancia Media Extraida
dass_model<-'
Dep =~ DASS3 + DASS5 + DASS10 + DASS13 + DASS16 + DASS17 + DASS21
'
dass_cfa<-cfa(dass_model,dasspoly_sub,ordered = colnames(dasspoly_sub))
summary(dass_cfa,standardized=T)
reliability(dass_cfa)
?reliability

# Fidedignidade no contexto da TRI - separacao

dassbin<-read.csv("https://raw.githubusercontent.com/wagnerLM/netusf/master/dassbin",sep = ";")
View(dassbin)
dassbin_sub<-dassbin[,c(3,5,10,13,16,17,21)]
mod_sub<-RM(dassbin_sub)
summary(mod_sub)
par(mfrow=c(1,1))
plotINFO(mod_sub,type="item")
dasslabels[c(3,5,10,13,16,17,21)]
# medidas de ajuste
pres <- person.parameter(mod_sub)
IC(pres)
itemfit(pres)
personfit(pres)
summary(pres)
gof.res <- gofIRT(pres)
summary(gof.res)
gof.res
# separacao
sep_res <- SepRel(pres)
sep_res
summary(sep_res)
# ver:
# https://www.rasch.org/rmt/rmt63i.htm
# https://www.rasch.org/rmt/rmt94n.htm

#### outros exemplos ####
# library(irr)

# Fidedignnidade entre avaliadores
# porcentagem simples de concordancia
?agree()
# correlacao intraclasse para medidas intervalares ou ordinais
?ICC()
# Kappa para dois avaliadores
?cohen.kappa()
# Kappa para mais de dois avaliadores
?kappam.light()
# fidedignidade quando há baixa variância nas respostas
?finn()
