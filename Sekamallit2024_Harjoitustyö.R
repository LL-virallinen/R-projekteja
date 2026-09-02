library(MASS)
library(ggplot2)
library(lme4)
library(nlme)
library(dplyr)
library(emmeans)
?Alfalfa
data <- Alfalfa
summary(data)
##vidualisoidaan aineistoa ggplotilla
ggplot(data, aes(x = factor(Date, levels = c("S1", "S20", "O7", "None")), y = Yield)) +
  geom_point(aes(col = Variety)) +
  geom_line(aes(col = Variety)) +
  facet_wrap(. ~ Block) +
  theme_bw() +
  labs(x = "Date", y = "Yield")

#mallissa fit_1 on satunnaisvaikutus jokaiselle pelto-lajike parille
fit_1 <- lmer(Yield ~ 1 + Date + Variety + (1 | Block:Variety), data = data)
pairs(emmeans(fit_1, specs = "Variety"), infer = c(TRUE, FALSE))
#fit_2 sisältää sekä pelto-lajike satunnaismuuttujan, myös kunkin pellon satunnaistekijän
fit_2 <- lmer(Yield ~ 1 + Date + Variety + (1|Block) + (1|Block:Variety), data
                   = data)
anova(fit_1 , fit_2)
#näistä malli 2 on merkitsevästi parempi.
#myös uskottavuusosamäärän ollessa suuri, tuetaan mallin 2 paremmuutta

#suoritetaan ensin alustavaa mallin parametriestimaattien tulkintaa
summary(fit_2)

#Huomataan, että block-variety ryhmien välinen vaihtelu on lähes sama kuin ryhmien sisäinen vaihtelu
#Peltojen välinen vaihtelu on noin kolmasosan ryhmien sisäistä vaihtelua suurempaa
pairs(emmeans(fit_2, specs = "Date"), infer = c(TRUE, FALSE))
pairs(emmeans(fit_2, specs = "Variety"), infer = c(TRUE, FALSE))

#Nyt halutaan arvioida mallin soveltuvuutta sekä varmistaa
#että mallin oletukset täyttyvät diagnostiikan avulla

#tarkastetaan ensin mallin kovarianssirakenne jäännösten ja havaintoindeksin kuvaajasta
fit_resid_2 <- resid(fit_2, scaled = TRUE, type="pearson")
plot(fit_resid_2)
abline(h = 0)
abline(v = seq(0.5, by = 24, length.out = 3), lty = 2) 
#tässä on kukin lajike omassa välissään. Ranger-lajike näyttää poikkeavan pienellä hajonnalla
abline(v = seq(0.5, by = 4, length.out = 20), lty = 2) 
#Kukin pelto-lajike omassa välissään
#tasojen virhevariansseilla näyttää olevan hieman heteroskedastisuutta.

#aloitetaan diagnostiikkakuvat jäännösten hajontakuvioista
#tätä varten luodaan objekti, joka sisältää alkuperäisen aineiston sekä 
#skaalatut jäännökset sekä sovitteet
data_diag <- bind_cols(data,
                       data.frame(resid = resid(fit_2, scaled = TRUE)),
                       data.frame(fitted = fitted(fit_2)))
ggplot(data_diag, aes(x = fitted, y = resid)) +
  geom_point() +
  labs(x = "Fitted value", y = "Scaled residual")
#jäännösten sekä sovitteen hajontakuviosta voidaan kenties havaita lineaarista riippuvuutta
#tämä voi tarkoittaa että malliin voisi sopia lisä selittäviä muuttujia tai 
#nähtyjen tulosten perusteella voidaan epäillä myös Date- muuttujan vaikuttavan lineaarisesti
ggplot(data_diag, aes(x = Variety, y = resid)) +
  geom_boxplot() +
  labs(x = "Variety", y = "Scaled residual")
ggplot(data_diag, aes(x = Date, y = resid)) +
  geom_boxplot() +
  labs(x = "Date", y = "Scaled residual")
#Jäännösten laatikkojanakuviot antavat uskoa että hajonta olisi vakio
#pienenä kysymysmerkkinä date=S20 laatikon tiiviys muutamalla suuresti poikkeavalla jäännöksellä
#jatketaan diagnostiikkaa sillä mitään hälyttävää ei olla havaittu

fitted(fit_2)
resid(fit_2, scaled = TRUE)
plot(ranef(fit_2))
#satunnaisvaikutusten jakauma näyttää hyvin normaaliselta.
#ei voida sulkea pois mahdollisuutta etteikö date-muuttuja voisi olla eksponentiaalinen


#piirretään normaali Q-Q-kuviot jäännösten normaalitarkastelua varten
par(mfrow = c(1, 2))
hist(resid(fit_2, scaled = TRUE), main = "", xlab = "Residuals")
qqnorm(resid(fit_2, scaled = TRUE), main = "")
qqline(resid(fit_2, scaled = TRUE), col = 2, lwd = 2, lty = 2)
#Jäännökset näyttävät hyvin normaalisilta ja tutkimukseen suotuisilta.

#Luodaan kuvat satunnaisvaikutusten ennusteille
r_intercept_1 <- ranef(fit_2)$`Block:Variety`$`(Intercept)`
par(mfrow = c(1, 2))
hist(r_intercept_1, main = "", xlab = "Random effect prediction")
qqnorm(r_intercept_1, main = "")
qqline(r_intercept_1, col = 2, lwd = 2, lty = 2)
#pelto-lajike parien ennusteet näyttävät olevan hyvin suotuisasti normaalisti jakautuneita

r_intercept_2 <- ranef(fit_2)$`Block`$`(Intercept)`
par(mfrow = c(1, 2))
hist(r_intercept_2, main = "", xlab = "Random effect prediction")
qqnorm(r_intercept_2, main = "")
qqline(r_intercept_2, col = 2, lwd = 2, lty = 2)
#Block- tasojen pienen määrän takia ennusteen kuvista ei voida vetää johtopäätöksiä. 
#satunnaistekijän tasot voivat olla normaalisti jakautuneet

plot(r_intercept_1, ylab = "Predicted random intercept")
abline(h = 0)
abline(v = c(15.5, 30.5), col = 8, lty = 2)

#






#### nlme

fit_nlme_1 <- lme(Yield ~ 1 + Date + Variety,
                  data = data, method = "ML",  
                  random = list(Block = pdCompSymm(~ 1 + Variety)))
fit_nlme_2 <- lme(Yield ~ 1 + Date + Variety, data = data, method = "ML", 
                  random = list(Block = pdCompSymm(~ 1 + Variety),
                                weights = varIdent(form = ~ 1 | Variety)))

anova(fit_nlme_1, fit_nlme_2)
#nlme- mallien vertailu anova-funktiolla kertoo että yksinkertaisempi
#malli 2 ole huonompi kuin malli 1. verrataan nyt mallia fit_nlme_2 malliin fit_2
AIC(fit_2, fit_nlme_1)
BIC(fit_2, fit_nlme_1)
#Löydettiin parempi aineistoa kuvaava malli fit_nlme_2 
summary(fit_nlme_1)
intervals(fit_nlme_1)
plot(fit_nlme_1, col = data$Date)
plot(fit_nlme_1, col = data$Variety)

#Lasketaan nyt ryhmäkohtaiset jäännöskeskihajonnat
sapply(1:4, function(j) round(sd(resid(fit_nlme_1, type = "pearson")[data$Date == j]),
                              4))
sapply(1:4, round(sd(resid(fit_nlme_1, type = "pearson")[data$Date])))

resid <- residuals(fit_nlme_1)
fitted <- fitted(fit_nlme_1)
#
qqnorm(resid, main = "Normal Q-Q Plot")
ggplot(data, aes(x = fitted, y = resid)) +
  geom_point() +
  labs(x = "Fitted value", y = "Scaled residual")
ggplot(data, aes(x = Variety, y = resid)) +
  geom_point() +
  labs(x = "Fitted value", y = "Scaled residual")

qqnorm(fit_nlme_1, ~ranef(.))
plot(ranef(fit_nlme_1)[,], col = factor(data$Date[seq(6, by =1, length.out = 6)]))
pairs(emmeans(fit_nlme_1, specs = "Date"), infer = c(TRUE, FALSE))
pairs(emmeans(fit_nlme_1, specs = "Variety"), infer = c(TRUE, FALSE))
