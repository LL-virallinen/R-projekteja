library(foreign)
ht1.dat<-read.spss("elinolo2020.sav", to.data.frame=TRUE)
attach(ht1.dat)
# Suluissa olevan 1:n tilalle oma opiskelijanumero
set.seed(520829)
# 800 kokoinen otos
oma.otos1<-ht1.dat[sample(nrow(ht1.dat), 800), ]
attach(oma.otos1)
library(tidyverse)
library(dplyr)
library(ggplot2)

#keskiarvot soluittain
with(oma.otos1,tapply(pala,list(supu,ahtas),mean))

#mediaanit soluittain
with(oma.otos1,tapply(pala,list(supu,ahtas),median))

#keskihajonnat soluittain
with(oma.otos1,tapply(pala,list(supu,ahtas),sd))

#tutkitaan miesten havaintojen normaalisuutta Shapiro-Wilk- testin avulla
raj.ainM <- select(filter(oma.otos1, supu=="mies"),c(supu,ahtas,pala))
with(raj.ainM,tapply(pala,list(ahtas),shapiro.test))
attach(raj.ainM)

#laatikko-jana -kuvio, miehet
boxplot(pala~ahtas)
detach(raj.ainM)

#tutkitaan naisten havaintojen normaalisuutta Shapiro-Wilk -testin avulla
raj.ainN <- select(filter(oma.otos1, supu=="nainen"),c(supu,ahtas,pala))
with(raj.ainN,tapply(pala,list(ahtas),shapiro.test))
attach(raj.ainN)

#laatikko-jana -kuvio, naiset
boxplot(pala~ahtas)
detach(raj.ainN)

#populaatioiden hajontojen yhtäsuuruustestaus Levene-testillä
attach(oma.otos1)
library(car)
leveneTest(pala~ahtas*supu)

#nimetään malli ja suoritetaan kaksisuuntainen varianssianalyysi
fit1<-lm(pala ~ahtas*supu, data=oma.otos1)
malli_lm1<-aov(fit1)
summary(malli_lm1)

#keskiarvojen profiilikuvio
interaction.plot(ahtas ,supu, pala,
                 xlab="Asumisahtaus", ylab="Pinta-ala",
                 trace.label="Sukupuoli", las=1, lwd=2)

#Tukeyn testi
tukey.test1<-TukeyHSD(malli_lm1)
tukey.test1
plot(tukey.test1)


#Regressiomalli

#selittäjien väliset korrelaatiot
cor.test(rkyks, alaika, method="pearson")
cor.test(rkyks, asmenot, method="pearson")
cor.test(alaika, asmenot, method="pearson")

#sirontakuvio
plot(rkyks,pala)
abline(lm(pala~rkyks))

#sirontakuvio
plot(asmenot,pala)
abline(lm(pala~asmenot))

#sirontakuvio
plot(alaika,pala)
abline(lm(pala~alaika))

#korrelaatiokertoimet
cor.test(rkyks, pala,method="pearson")
cor.test(rkyks, pala,method="spearman")
cor.test(asmenot, pala,method="pearson")
cor.test(asmenot, pala,method="spearman")
cor.test(alaika, pala,method="pearson")
cor.test(alaika, pala,method="spearman")

#yhden selittäjän regressiomalli
lm.pala2 <- lm(pala~rkyks)
summary(lm.pala2)

#jaannosdiagnostiikka
hist(resid(lm.pala2))
plot(lm.pala2)

#toistomittaus
library(foreign)
ht2.dat<-read.spss("Toistomittausaineisto2020.sav", to.data.frame=TRUE)
attach(ht2.dat)
# Suluissa olevan 1:n tilalle oma opiskelijanumero
set.seed(520829)
# 500 kokoinen otos
oma.otos2<-ht2.dat[sample(nrow(ht2.dat), 500), ]
attach(oma.otos2)

#rajataan erikseen oleelliset muuttujat ja poistetaan tyhjät havainnot
oma.otos2<-oma.otos2[,c(4,15,21)]
oma.otos2<-na.exclude(oma.otos2)

id<-1:488
oma.otos2<-cbind(oma.otos2,id)

#sukupuolineutraalit tunnusluvut
summary(Functional_M1)
summary(Functional_M2)
sd(oma.otos2$Functional_M1)
sd(oma.otos2$Functional_M2)

#tunnusluvut sukupuolittain
# Functional_M1 keskiarvot soluittain
with(oma.otos2,tapply(Functional_M1,list(D2),mean))

# Functional_M1 mediaanit soluittain
with(oma.otos2,tapply(Functional_M1,list(D2),median))

# Functional_M1 keskihajonnat soluittain
with(oma.otos2,tapply(Functional_M1,list(D2),sd))

# Functional_M2 keskiarvot soluittain
with(oma.otos2,tapply(Functional_M2,list(D2),mean))

# Functional_M2 mediaanit soluittain
with(oma.otos2,tapply(Functional_M2,list(D2),median))

# Functional_M2 keskihajonnat soluittain
with(oma.otos2,tapply(Functional_M2,list(D2),sd))

#Laatikko-jana -kuviot
boxplot(Functional_M1,Functional_M2)
boxplot(Functional_M1~D2)
boxplot(Functional_M2~D2)

#sukupuolineutraalit normaalisuustestit
shapiro.test(Functional_M1)
shapiro.test(Functional_M2)

# Tehdään normaalisuustestit sukupuolen tasoilla
with(oma.otos2,tapply(Functional_M1,list(D2),shapiro.test))
with(oma.otos2,tapply(Functional_M2,list(D2),shapiro.test))

library(tidyr)
dat_long<- oma.otos2 %>%
  gather(key="ohjaus", value="arvo", factor_key = TRUE, -c(id,D2))
fit2<- aov(arvo ~D2 * ohjaus + Error(id/ohjaus), data=dat_long)
summary(fit2)

# Keskiarvojen profiilikuvio
attach(dat_long)
interaction.plot(ohjaus,D2,arvo, data=dat_long)
detach (dat_long)
library(dplyr)

#
Miehet <- select(filter(oma.otos2, D2=="male"),c(D2,Functional_M1,Functional_M2))
Naiset <- select(filter(oma.otos2, D2=="female"),c(D2,Functional_M1,Functional_M2))
MiehetF1<- Miehet$Functional_M1
NaisetF1<- Naiset$Functional_M1
MiehetF2<- Miehet$Functional_M2
NaisetF2<- Naiset$Functional_M2
#t-testit
t.test(MiehetF1,MiehetF2, paired = T)
t.test(NaisetF1,NaisetF2, paired = T)
t.test(Functional_M1,Functional_M2, paired = T)


#kategoristen vastemuuttujien mallitus
library(foreign)
ht3.dat<-read.spss("EK2011.sav", to.data.frame=TRUE)
attach(ht3.dat)
# Suluissa olevan 1:n tilalle oma opiskelijanumero
set.seed(520829)
# 800 kokoinen otos
oma.otos3<-ht3.dat[sample(nrow(ht3.dat), 800), ]
attach(oma.otos3)

table(d2)
table(d32)
table(k23)

mytable <- table(d2,d32,k23)
ftable(mytable)

# loglineaarinen mallitus
library(MASS)
mytable <- xtabs(~d2+d32+k23, data=oma.otos3)

# täydellinen riippumattomuus
loglm(~d2+d32+k23, mytable)

# d2 riippumaton muuttujaparista
loglm(~d2+d32+k23+d32*k23, mytable)

# d32 riippumaton muuttujaparista
loglm(~d2+d32+k23+d2*k23, mytable)

# k23 riippumaton muuttujaparista
loglm(~d2+d32+k23+d2*d32, mytable)

#ehdollinen riippumattomuusmalli: d2 ja k23 riippumattomia
loglm(~d2+d32+k23+d2*d32+d32*k23, mytable)

#ehdollinen riippumattomuusmalli: d2 ja d32 riippumattomia
loglm(~d2+d32+k23+d2*k23+d32*k23, mytable)

#ehdollinen riippumattomuusmalli: d32 ja k23 riippumattomia
loglm(~d2+d32+k23+d2*d32+d32*d2, mytable)

#parittaisten riippuvuuksien malli
loglm(~d2+d32+k23+d2*d32+d32*k23+d2*k23, mytable)

# standardoidut jäännökset
resid(loglm(~d2+d32+k23+d2*k23, mytable))

# mallin jatkotarkastelu
taulu1 <- table(k23,d2)
prop.table(taulu1, 1)
taulu2 <- table(k23,d32)
prop.table(taulu2, 1)

#Kaksiluokkainen selittävä muuttuja
# logistinen binäärinen regressio
logr_d32 <- glm(d32 ~ d2+ika, data=oma.otos3, family=binomial)
logr_d32

# p-arvot
summary(logr_d32)

# Odds Ratiot
exp(cbind(OR=coef(logr_d32), confint(logr_d32)))

# Selitysaste
library(fmsb)
data.nagel<-NagelkerkeR2(logr_d32)
data.nagel


#monimuuttujamenetelmät
library(foreign)
ht4.dat<-read.spss("pankkiotos2020.sav", to.data.frame=TRUE)
attach(ht4.dat)
# Suluissa olevan 1:n tilalle oma opiskelijanumero
set.seed(520829)
# 1600 kokoinen otos
oma.otos4<-ht4.dat[sample(nrow(ht4.dat), 1600), ]
attach(oma.otos4)
oma.otos4<-na.exclude(oma.otos4)

#korrelaatiokertoimet
data.kor2<-cor(oma.otos4, method = "pearson", use = "complete.obs")
View(data.kor2)

#korrelaatiokertoimien p-arvot
library(Hmisc)
res2 <- rcorr(as.matrix(oma.otos4))
View(res2[["P"]])
# korrelaatiokertoimet ja p-arvot rinnakkain
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor =(cormat)[ut],
    p = pmat[ut]
  )
}
res3<-rcorr(as.matrix(oma.otos4))
View(flattenCorrMatrix(res3$r, res3$P))

#korrelaatioiden voimakkuudet kuviona
library(corrplot)
corrplot(data.kor2, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)
#korrelaatiomatriisin sopivuus pääkomponenttianalyysiin
library (psych)
cortest.bartlett(data.kor2, n =1598,diag=TRUE)
KMO(data.kor2)
#pääkomponenttianalyysi
pca1<-prcomp(oma.otos4,center = T,scale =T)
summary(pca1)
#ominaisarvot
(pca1$sdev)^2
#
screeplot(pca1, type="lines") +
  abline(h=1, lty=2)

# Valitaan viisi pääkomponenttia, promax-rotaatio
pca.chosen <- pca1$ rotation [ ,1:5]
pca.promax <- promax (pca.chosen )
pca.promax
# muuttujien selitys päämuuttujilla
p<-ncol(oma.otos4)
n<-nrow(oma.otos4)
e<-eigen(data.kor2)
L<-e$values #placing the eigenvalues in L
Vm<-matrix(0,nrow=p,ncol=p) #creating a p x p matrix with zeroes.
#Vm is an orthogonal matrix since all correlations between variable are 0.
diag(Vm)<-L #putting the eigenvalues in the diagonals
View(Vm) #check-- matrix with eigenvalues on the diagonals
comp.matrix<-e$vectors %*% sqrt(Vm) #sometimes referred to as P matrix
#or eigenvectors x sqrt(Vm): P %*% t(P) is equal to the R matrix.
View(comp.matrix)
#Pääkomponentti 1 selittämä osuus muuttujien vaihtelusta
comp.matrix[,1]^2
#Pääkomponentti 2 selittämä osuus muuttujien vaihtelusta
comp.matrix[,2]^2
#Pääkomponentti 3 selittämä osuus muuttujien vaihtelusta
comp.matrix[,3]^2
#Pääkomponentti 4 selittämä osuus muuttujien vaihtelusta
comp.matrix[,4]^2
#Pääkomponentti 5 selittämä osuus muuttujien vaihtelusta
comp.matrix[,5]^2
# kommunaliteetit
comp.matrix[,1]^2 +comp.matrix[,2]^2 +comp.matrix[,3]^2 +comp.matrix[,4]^2
+comp.matrix[,5]^2
pcapoints5<-pca1$x[,1:5]
pcapoints5<-as.data.frame(pcapoints5)
names(pcapoints5)<-c("korttiaktiivisuus", "rahastot","tiliaktiivisuus", "lainat/luotot", "osakkeet")
#lisätään havaintomatriisiin muuttujiksi nimetyt pääkomponenttipistemäärät
oma.otos45<-cbind(oma.otos4,pcapoints5)
cor(pcapoints5)
# 2 klusteria
set.seed(520829)
km2 = kmeans(pcapoints5, 2, nstart=100)
km2
#3 klusteria
set.seed(520829)
km3 = kmeans(pcapoints5, 3, nstart=100)
km3
#4 klusteria
set.seed(520829)
km4 = kmeans(pcapoints5, 4, nstart=100)
km4
#5 klusteria
set.seed(520829)
km5 = kmeans(pcapoints5, 5, nstart=100)
km5
#klusterien graafinen tarkastelu
library(factoextra)
fviz_cluster(km2, data = oma.otos4,ggtheme = theme_minimal(),
             main = "Clustering Plot")
fviz_cluster(km3, data = oma.otos4,ggtheme = theme_minimal(),
             main = "Clustering Plot")
fviz_cluster(km4, data = oma.otos4,ggtheme = theme_minimal(),
             main = "Clustering Plot")
fviz_cluster(km5, data = oma.otos4,ggtheme = theme_minimal(),
             main = "Clustering Plot")
