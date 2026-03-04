###Load the library
library("pheatmap")
###Load the data
data<-read.csv(file.choose())
head(data)
str(data)
rownames(data)<-c(data$Name)
head(data)
newdata<-data [ , -1]
head(newdata)
matx <- scale(newdata)
matx
heatmap(matx, scale = "row")
###Draw Heatmap with Row & Column Clusters (Euclidean", "ward.D2", method )
pheatmap(matx,
color=colorRampPalette(c("navy", "white", "red"))(50))
pheatmap(matx,
clustering_distance_rows = "euclidean",
clustering_distance_cols = "euclidean", clustering_method = "ward.D2",
cutree_rows = 5, cutree_cols =4,
legend = T,
fontsize = 11,
color=colorRampPalette(c("navy", "white", "red"))(50))
####Analyzing multi-environment trials using AMMI
###Load the library
library(metan)
####Analyzing multi-environment trials using AMMI
###Load the library
library(metan)
data<-read.csv(file.choose())
head(data)
###joint ANOVA
joint <- anova_joint(data, ENV, GEN, REP, GY, verbose = T)
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
library(metan)
out <- ecovalence(data,
env = ENV,
gen = GEN,
rep = REP,
resp = GY)
print(out)
###The GGE model
gge_model <- gge(data, ENV, GEN, GYY)
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
