# Teoria de Resposta ao Item com R
# Retomando a analise fatorial
library(psych)
ESV<-read.csv("https://raw.githubusercontent.com/wagnerLM/SBP/master/ESV.csv",sep=";")
View(ESV)
fa_ESV<-fa(ESV[,-c(6,7)],1,scores = "regression")
fa_ESV
ESV$ESV_sum<-rowSums(ESV[,-c(6,7)])
View(ESV)
cor.plot(ESV[,c(1:5,8)])
rownames(fa_ESV$loadings)<-paste(c("ideal","excel","satisf","import","mudaria"))
fa_ESV

ESV$ESV_sum<-rowSums(ESV[,-c(6,7)])
View(ESV)
cor.plot(ESV[,c(1:5,8)])
describe(ESV[,c(1:5)])

# Pacotes TRI
install.packages("mirt")  # multidimensional
install.packages("eRm")   # modelos da familia Rasch
# ativando os pacotes
library(mirt)
library(eRm)

# Exemplo dicotômico
dassbin<-read.csv("https://raw.githubusercontent.com/wagnerLM/netusf/master/dassbin",sep = ";")
View(dassbin)
# Nome resumido dos itens
dasslabels<-scan("https://raw.githubusercontent.com/wagnerLM/netusf/master/dasslabels",what = "character", sep = "\n")
dasslabels
# Itens completos 
dassnames<-scan("https://raw.githubusercontent.com/wagnerLM/netusf/master/dassnames",what = "character", sep = "\n")
dassnames

# Selecionando itens de depressão
dassbin_sub<-dassbin[,c(3,5,10,13,16,17,21)]
describe(dassbin_sub)
dassbin_sub$totaldp<-rowSums(dassbin_sub[,1:7])
cor.plot(dassbin_sub)
fa(dassbin_sub[,-8],1)

# Modelo de 2 parâmetros (2PL)
mod1<-mirt(dassbin_sub[,-8],1,itemtype = "2PL")
coef(mod1)
summary(mod1)
plot(mod1)
plot(mod1, type = 'trace')
plot(mod1, type = 'info')
itemplot(mod1,7)
M2(mod1)

# Modelo de 1 parâmetro (Rasch no mirt)
mod2<-mirt(dassbin_sub[,-8],1,itemtype = "Rasch")
coef(mod2)
summary(mod2)
plot(mod2)
plot(mod2, type = 'trace')
plot(mod2, type = 'info')
M2(mod2)
# Comparando modelos em termos de resíduos
anova(mod2,mod1)

# Modelo de 1 parâmetro (Rasch no eRm)
# Fit do modelo rasch
mod_sub<-RM(dassbin_sub[,-8])
summary(mod_sub)
par(mfrow=c(1,1))
plotINFO(mod_sub,type="item")
dasslabels[c(3,5,10,13,16,17,21)]
#medidas de ajuste
pres <- person.parameter(mod_sub)
IC(pres)
itemfit(pres)
personfit(pres)
summary(pres)
gof.res <- gofIRT(pres)
summary(gof.res)
gof.res
#curvas
plotICC(mod_sub)
plotICC(mod_sub, empICC=list("raw"))
par(mfrow=c(1,1))
plotjointICC(mod_sub, legpos = "left")
plotPImap(mod_sub,sorted=T)
dasslabels

# Retomando o exemplo com a ESV
mod3<-mirt(ESV[,c(1:5)],1,itemtype = "graded")
coef(mod3)
summary(mod3)
plot(mod3)
plot(mod3, type = 'trace')
plot(mod3, type = 'info')
M2(mod3)

theta <- as.vector(fscores(mod3))
# difficulty estimates
b <- coef(mod3, simplify = TRUE)
b_mean<-(rowMeans(b$items[,-1]))*-1
par(mfrow=c(2,1))

hist(theta,xlim = c(-5,5))
hist(b_mean,xlim = c(-5,5))

# Leituras
# Artigo introdutório "http://pepsic.bvsalud.org/pdf/avp/v2n2/v2n2a02.pdf"
# Artigo avançado "https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1745-3992.1997.tb00606.x"
# Pra quem quiser rodar a analise mas nao gosta do R - https://shiny.cs.cas.cz/ShinyItemAnalysis/
