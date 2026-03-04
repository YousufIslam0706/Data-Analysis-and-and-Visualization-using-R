#Stability Analysis using metan package

####Analyzing multi-environment trials using AMMI
###Load the library
library(metan)
data<-read.csv(file.choose())
head(data)

###joint ANOVA
joint <- anova_joint(data, ENV, GEN, REP, GY, verbose = T)
print(joint$GY$anova)

###AMMI ANOVA analysis
AMMI_model <- performs_ammi(data, ENV, GEN, REP, GY, verbose = FALSE)
AMMI_model



###Biplots
#biplot type 1: GY x PC1
a <- plot_scores(AMMI_model)
plot(a)

#biplot type 2: PC1 x PC2
c <- plot_scores(AMMI_model, type = 2)
plot(c)

####Regression-based stability analysis
reg_model <- ge_reg(data, ENV, GEN, REP, GY)
print(reg_model)
plot(reg_model)




####BLUP Based Stability index

Res_ind <- data %>% gamem_met(ENV, GEN, REP, GY, verbose = FALSE) %>% blup_indexes()

View(Res_ind)
print(Res_ind$GY)


####AMMI-based stability indexes
model <- waas(data,
               env = ENV,
               gen = GEN,
               rep = REP,
               resp = c(GY, DF),
               verbose = FALSE)
model_indexes <- ammi_indexes(model)
print(model_indexes)



####Stability analysis based on Wricke’s model

library(metan)
out <- ecovalence(data,
                  env = ENV,
                  gen = GEN,
                  rep = REP,
                  resp = GY)
print(out)



###The GGE model
gge_model <- gge(data, ENV, GEN, GY)


###Biplot type 2: Mean performance vs. stability
gge_model <- gge(data, ENV, GEN, GY, svp = "genotype")
c <- plot(gge_model, type = 2)
plot(c)


###Biplot type 3: Which-won-where
gge_model <- gge(data, ENV, GEN, GY, svp = "symmetrical")
e <- plot(gge_model, type = 3)
plot(e)


###Biplot type 4: Discriminativeness vs. representativeness
g <- plot(gge_model, type = 4)
plot(g)


###Biplot type 6: Ranking environments
gge_model <- gge(data, ENV, GEN, GY)
k <- plot(gge_model, type = 6)
plot(k)


###Biplot type 8: Ranking genotypes
o <- plot(gge_model, type = 8)
plot(o)





###Multi-trait stability index (MTSI)
library(metan)
data<-read.csv(file.choose())
head(data)

MTSI_index <- data %>%waasb(ENV, GEN, REP, resp = c(DF, DM, PH, PPP, SPP, PL, HSW,GY)) %>% mtsi(verbose = FALSE, index = "waasb")
plot(MTSI_index)



###MGIDI index
library(metan)
data<-read.csv(file.choose())
head(data)

mod <- gamem(data,
             gen = GEN,
             rep = REP,
             resp = everything())

mgidi_index <- mgidi(mod,
                     SI = 15) 
plot(mgidi_index)
