setwd("")

# Models to study the changes in the proportions of each prey group ----
dat <- read.csv("DataDiet.csv")

library(nnet)
library(sjPlot)
library(effects)
library(mclogit)
library(MASS)
library(MuMIn)
library(ordinal)

dat$Presa <- factor(dat$Presa, 
                           levels = c("VESPULA", "VELUTINA", "REPTIL", "AVE"), 
                           labels = c("Common-wasp", "Asian-hornet", "Reptile", "Bird") )
dat$Presa <- relevel(dat$Presa, ref="Common-wasp")

dat$Año <- factor(dat$Año, 
                         levels = c("2018", "2019", "2020", "2021"))
dat$Año <- relevel(dat$Año, ref="2018")

# dat$Pollos <- relevel(dat$Pollos, ref=1)

saturated <- clmm(Presa~Edad*Pollos*Año + (1|ID), data=dat)
a <- clmm(Presa~Edad:Pollos + Año:Pollos + Edad + Año + (1|ID), data=dat)
b <- clmm(Presa~Edad:Pollos + Año:Pollos + Edad + Pollos + (1|ID), data=dat)
c <- clmm(Presa~Edad:Pollos + Año:Pollos + Año + Pollos + (1|ID), data=dat)
d <- clmm(Presa~Edad:Pollos + Edad + Año + Pollos + (1|ID), data=dat)
e <- clmm(Presa~Año:Pollos + Edad + Año + Pollos + (1|ID), data=dat)
AICc(a, b, c, d, e, saturated, best)
best <- clmm(Presa~Edad:Pollos + Año:Pollos + Edad + Pollos + (1|ID), data=dat)

d <- clmm(Presa~Edad:Pollos + Edad + Año + Pollos + (1|ID), data=dat)
a <- clmm(Presa~Edad:Pollos + (1|ID), data=dat)
b <- clmm(Presa~Edad + (1|ID), data=dat)
AICc(best, a, b, d)

a <- clmm(Presa~Edad:Pollos + Edad + Año +  (1|ID), data=dat)
b <- clmm(Presa~Edad:Pollos + Edad + Pollos + (1|ID), data=dat)
c <- clmm(Presa~Edad:Pollos + Año + Pollos + (1|ID), data=dat)
AICc(best, a, b, c, d)
best2 <- clmm(Presa~Edad:Pollos + Año + Pollos + (1|ID), data=dat)

a <- clmm(Presa~Edad:Pollos + Edad + Año +  (1|ID), data=dat)
b <- clmm(Presa~Edad:Pollos + Edad + (1|ID), data=dat)
c <- clmm(Presa~Edad:Pollos + Año +  (1|ID), data=dat)
d <- clmm(Presa~Edad:Pollos + (1|ID), data=dat)
AICc(a, b, c, d, best2)

summary(best2)
# likehood ratio test Chi2
drop1(best2, test="Chisq")

#Less complex models:
c <- clmm(Presa~Edad:Pollos + Año +  (1|ID), data=dat)
a <- clmm(Presa~Edad:Pollos + (1|ID), data=dat)
b <- clmm(Presa~Edad + (1|ID), data=dat)
d <- clmm(Presa~Pollos +  (1|ID), data=dat)
e <- clmm(Presa~Edad+ Pollos + Año +  (1|ID), data=dat)
f <- clmm(Presa~Edad+ Pollos +  (1|ID), data=dat)

null <- clmm(Presa~1 +  (1|ID), data=dat)
AICc(a, b, c, d, e, f, null, saturated)

plot_model(best2, type = "pred", terms = c("Pollos"))

# Models to study the changes in the number of cells of the combs ----

dat <- read.csv("DataCell.csv")

library(tidyverse)
library(lubridate)
library(lme4)
library(nlme)
library(performance)
library(sjPlot)
library(MuMIn)

m1 <- lmer(data = dat, log(MediaCeldi) ~ Edad + (1|ID))

m2 <- lmer(data = dat, log(MediaCeldi) ~ Juliano + (1|ID))

AICc(m1,m2) #No differences between using age or julian date

m00 <- lmer(data = dat, log(MediaCeldi) ~ Juliano * Tamaño * Pollos * Año + (1|ID))
m01 <- lmer(data = dat, log(MediaCeldi) ~ Juliano * Tamaño * Pollos + Año + (1|ID))
AICc(m00,m01) 

# m01 <- lmer(data = dat, log(MediaCeldi) ~ Juliano * Tamaño * Pollos + Año + (1|ID))
m02  <- lmer(data = dat, log(MediaCeldi) ~ Juliano * Tamaño * Pollos + (1|ID))

AIC(m01,m02)

# m02 <- lmer(data = dat, log(MediaCeldi) ~ Juliano * Tamaño * Pollos + (1|ID))
m1 <- lmer(data = dat, log(MediaCeldi) ~ Juliano * Tamaño + Pollos + (1|ID))
AICc(m02,m1)

# m1 <- lmer(data = dat, log(MediaCeldi) ~ Juliano * Tamaño + Pollos + (1|ID))
m2 <- lmer(data = dat, log(MediaCeldi) ~ Juliano + Tamaño * Pollos + (1|ID))
AICc(m1, m2)

# m2 <- lmer(data = dat, log(MediaCeldi) ~ Juliano + Tamaño * Pollos + (1|ID))
m3 <- lmer(data = dat, log(MediaCeldi) ~ Juliano + Tamaño + Pollos + (1|ID))
AICc(m1, m3)

# m3 <- lmer(data = dat, log(MediaCeldi) ~ Juliano + Tamaño + Pollos + (1|ID))
m4 <- lmer(data = dat, log(MediaCeldi) ~ Tamaño * Pollos + (1|ID))
AICc(m3, m4)

# m4 <- lmer(data = dat, log(MediaCeldi) ~ Tamaño * Pollos + (1|ID))
m5 <- lmer(data = dat, log(MediaCeldi) ~ Tamaño + Pollos + (1|ID))
AICc(m4, m5)

# m4 <- lmer(data = dat, log(MediaCeldi) ~ Tamaño * Pollos + (1|ID))
m6 <- lmer(data = dat, log(MediaCeldi) ~ Pollos + (1|ID))
AICc(m4, m6)

# m4 <- lmer(data = dat, log(MediaCeldi) ~ Tamaño * Pollos + (1|ID))
m7 <- lmer(data = dat, log(MediaCeldi) ~ Tamaño + (1|ID))
AICc(m4, m7)

# m4 <- lmer(data = dat, log(MediaCeldi) ~ Tamaño * Pollos + (1|ID))
m8 <- lmer(data = dat, log(MediaCeldi) ~ Tamaño * Pollos + Año + (1|ID)) #Añado el año por si la cosa cambia
AIC(m4, m8)

null <- lmer(data = dat, log(MediaCeldi) ~ 1 + (1|ID))
AICc(m4, null)

#Best model: m4
m4.1 <- lme(data = dat, log(MediaCeldi) ~ Tamaño * Pollos, random = ~ 1|ID)
summary(m4.1)
car::Anova(m4, type = 3)
check_model(m4)

set_theme(base = theme_classic())

plot_model(m4, type = "pred", show.data = T, jitter = 0.6,
           terms = c("Tamaño", "Pollos"), title = "", 
           axis.title = c("Wasp species", "Nº of cells"), 
           legend.title = "Nº of Chicks", axis.lim = c(0,800))

plot_model(m4, type = "eff",
           terms = c("Tamaño", "Pollos"), title = "", 
           axis.title = c("Wasp species", "Nº of cells"), 
           legend.title = "Nº of Chicks")

ggplot()+
  geom_violin(data = TamanosMean, aes(x = Tamaño, y = MediaCeldi, fill = Pollos))+
  geom_boxplot(data = TamanosMean, aes(x = Tamaño, y = MediaCeldi, fill = Pollos), 
               width = 0.9, alpha = 0.45, outlier.fill = "black", outlier.alpha = 0.55,
               outlier.colour = "black", show.legend = F)+
  scale_fill_manual(values = c("white", "darkgrey"))+
  scale_y_continuous(limits = c(0,1000))+
  labs(fill = "Nº of nestlings", x = "Vespid species", y  = "Nº of cells")+
  theme_minimal()

# Models to study the changes in prey delivery ----

dat <- read.csv("DataRate.csv") 

dat <- as_tibble(dat)
dat$Año <- as.factor(dat$Año)
dat$Pollos <- as.factor(dat$Pollos)
dat$Juliano <- as.double(dat$Juliano)

m1 <- glmer(data = dat, Presas ~ scale(Edad) + (1|ID), family = "poisson")
m2 <- glmer(data = dat, Presas ~ scale(Juliano) + (1|ID), family = "poisson")
AIC(m1, m2) #Virtually the same. I´m using age (more intuitive)

complex <- glmer(data = dat, Presas ~ scale(Edad) * Pollos * Año + (1|ID), family = "poisson")
#Not converging

m0 <- glmer(data = dat, Presas ~ scale(Edad) * Año + Pollos + (1|ID), family = "poisson")
m1 <- glmer(data = dat, Presas ~ scale(Edad) * Pollos + Año + (1|ID), family = "poisson")
m1.1 <- glmer(data = dat, Presas ~ Pollos * Año + scale(Edad) + (1|ID), family = "poisson")

AICc(m0,m1, m1.1)

m1 <- glmer(data = dat, Presas ~ scale(Edad) * Pollos + Año + (1|ID), family = "poisson")
m2 <- glmer(data = dat, Presas ~ scale(Edad) * Pollos + (1|ID), family = "poisson")
AICc(m1, m2)

m2 <- glmer(data = dat, Presas ~ scale(Edad) * Pollos + (1|ID), family = "poisson")
m3 <- glmer(data = dat, Presas ~ scale(Edad) : Pollos + (1|ID), family = "poisson")
AICc(m2, m3)

m2 <- glmer(data = dat, Presas ~ scale(Edad) * Pollos + (1|ID), family = "poisson")
m4 <- glmer(data = dat, Presas ~ scale(Edad) + Pollos + (1|ID), family = "poisson")
AICc(m2, m3)

m2 <- glmer(data = dat, Presas ~ scale(Edad) * Pollos + (1|ID), family = "poisson")
m5 <- glmer(data = dat, Presas ~ scale(Edad) + (1|ID), family = "poisson")
AICc(m2, m5)

m2 <- glmer(data = dat, Presas ~ scale(Edad) * Pollos + (1|ID), family = "poisson")
m6 <- glmer(data = dat, Presas ~ scale(Edad) + Pollos + (1|ID), family = "poisson")
m7 <- glmer(data = dat, Presas ~ Pollos + (1|ID), family = "poisson")

AICc(m2, m6, m7) 

#m6 is the best model

null <- glmer(data = dat, Presas ~ 1 + (1|ID), family = "poisson")
AICc(m2, m6, m7, null) 


car::Anova(m6, type = 3)

check_model(m6)

plot_model(m6, type = "pred", terms = c("Edad", "Pollos"), show.data = F,
           jitter = 0.6, title = "", legend.title = "No of Chicks",
           axis.title = c("Age", "No of preys"))

plot_model(m6, type = "pred", terms = c("Edad", "Pollos"), show.data = T,
           jitter = 0.6, title = "", legend.title = "N? of Chicks",
           axis.title = c("Age", "Nº of preys"), axis.lim = c(2,10), )

plot_model(m6, type = "re")

