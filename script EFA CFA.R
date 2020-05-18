### EFA e CFA ###
install.packages("psych")
install.packages("lavaan")
install.packages("semPlot")
library(psych)
library(lavaan)
library(semPlot)

# EFA
# banco de dados
ESV<-read.csv("https://raw.githubusercontent.com/wagnerLM/SBP/master/ESV.csv",sep=";")
View(ESV)
# descrever variaveis
hist(ESV[,1],breaks = 7)
shapiro.test(ESV[,1])
describe(ESV[,-c(6,7)])
# correlacoes
cor.plot(ESV[,-c(6,7)],numbers = TRUE,cex = 0.8)
pairs.panels(ESV[,-c(6,7)])
# testar pressupostos
KMO(ESV[,-c(6,7)])  # deve ser acima de 0.6 ou 0.8
cortest.bartlett(ESV[,-c(6,7)]) # deve ser significativo (rejeitar H0)
# identificar fatores a interpretar
# ha dois metodos, paramétrico (simulated) e nao parametrico (resampled)
fa.parallel(ESV[,-c(6,7)],fa="fa")
# fa
fa_ESV<-fa(ESV[,-c(6,7)],1,scores = "regression")
fa_ESV
ESV$ESV_sum<-rowSums(ESV[,-c(6,7)])
View(ESV)
cor.plot(ESV[,c(1:5,8)])
rownames(fa_ESV$loadings)<-paste(c("ideal","excel","satisf","import","mudaria"))
fa_ESV
View(fa_ESV$scores)
fa.diagram(fa_ESV)
colnames(fa_ESV$loadings)<-"SV"
fa.diagram(fa_ESV)
# variaveis ordinais ou dicotomicas
fa(ESV[,-c(6,7)],1,cor="poly",scores = "regression")
# usar os ecores modelados para testar hipóteses
scatter.hist(fa_ESV$scores,ESV$Idade,smooth = F,ellipse = F,ab=T)
t.test(fa_ESV$scores~ESV$Sexo) # 1 = masc, 2 = fem

# CFA
model_ESV<-'
SV =~ ESV1 + ESV2 + ESV3 + ESV4 + ESV5
'
fit_ESV<-cfa(model_ESV,ESV)
summary(fit_ESV)
summary(fit_ESV,fit.measures = TRUE, standardized=TRUE, rsq=TRUE)
ESV_cfascores<-lavPredict(fit_ESV)
scatter.hist(fa_ESV$scores,ESV_cfascores,smooth = F,ellipse = F,ab=T)
# variaveis ordinais ou dicotomicas
fit_ESV2<-cfa(model_ESV,ESV,ordered = colnames(ESV[-c(6,7)]))
summary(fit_ESV2,fit.measures = TRUE, standardized=TRUE, rsq=TRUE)
# representacao no diagrama
semPaths(fit_ESV, what = "std", edge.label.cex = 0.7,
         edge.color = 1, esize = 1, sizeMan = 4.5, asize = 2.5,
         intercepts = FALSE, rotation = 4, thresholdColor = "red",
         mar = c(1, 5, 1.5, 5), fade = FALSE, nCharNodes = 4)

# exemplo EFA e CFA com BFI
View(bfi)
View(bfi.keys)
bfi_labels<-scan("https://raw.githubusercontent.com/wagnerLM/quantia/master/bfilabels",what = "character",sep="\n")
View(bfi_labels)

# EFA
KMO(bfi[,-c(26:28)])
cortest.bartlett(bfi[,-c(26:28)])
fa.parallel(bfi[,-c(26:28)],fa="fa")
# fa.parallel(bfi[,-c(26:28)],cor="poly",fa="fa")  demorou pra caramba
fa_bfi<-fa(bfi[,-c(26:28)],5,cor="poly",scores="regression",rotate = "oblimin")
rownames(fa_bfi$loadings)<-paste(bfi_labels)
fa_bfi
# MR5 = AGREE, MR3 = CONSC, MR1 = EXTR, MR2 = NEUR, MR4 = OPEN
colnames(fa_bfi$loadings)
colnames(fa_bfi$loadings)<-c("Neur","Extr","Consc","Agree","Open")
fa_bfi
fa.diagram(fa_bfi)

# CFA
bfi_model<-'
agree =~ A1 + A2 + A3 + A4 + A5 
conscientious =~ C1 + C2 + C3 + C4 + C5
extraversion =~ E1 + E2 + E3 + E4 + E5 
neuroticism =~ N1 + N2 + N3 + N4 + N5
openness =~ O1 + O2 + O3 + O4 + O5
'
bfi_fit<-cfa(bfi_model,bfi,ordered = colnames(bfi))
summary(bfi_fit,fit.measures = TRUE, standardized=TRUE, rsq=TRUE)
semPaths(bfi_fit, what = "std", edge.label.cex = 0.7,
         edge.color = 1, esize = 1, sizeMan = 4.5, asize = 2.5,
         intercepts = FALSE, rotation = 4, thresholdColor = "red",
         mar = c(1, 5, 1.5, 5), fade = FALSE, nCharNodes = 4)
