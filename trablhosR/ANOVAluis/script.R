# Anova para cavidades em proteínas para diferentes simulações

# Loading data ---------------------------------------------------------

setwd("~/Dropbox/r/ANOVAluis")

.libPaths("~/R/x86_64-pc-linux-gnu-library/3.0")


chainA <-read.delim("anova_Chain_A",header=T)
chainB <-read.delim("anova_Chain_B",header=T)
chainC <-read.delim("anova_Chain_C",header=T)
chainD <-read.delim("anova_Chain_D",header=T)

chainA <-na.omit(chainA)
chainB <-na.omit(chainB)
chainC <-na.omit(chainC)
chainD <-na.omit(chainD)

# Análises de diferença para chainA -------------------------------------

str(chainA)
summary(chainA)
attach(chainA)


cavidade <-c(Controle,X25mg,X200mg)

Controle.f <-rep("Controle",length(Controle))
x25mg.f <-rep("x25mg",length(X25mg))
x200mg.f <-rep("x200mg",length(X200mg))


fatores <-c(Controle.f,x25mg.f,x200mg.f)


data.chainA <-data.frame(cavidade,fatores)

fit_chainA <-aov(cavidade~fatores)
summary(fit_chainA)
fit1_Tukey <- TukeyHSD(fit_chainA)
plot(fit1_Tukey)

comp1 <-pairwise.t.test(cavidade,fatores,p.adj="bonf")

cavidade <-c(Controle,X25mg,X200mg)

Controle.f <-rep("Controle",length(Controle))
x25mg.f <-rep("x25mg",length(X25mg))
x200mg.f <-rep("x200mg",length(X200mg))


fatores <-c(Controle.f,x25mg.f,x200mg.f)


data.chainA <-data.frame(cavidade,fatores)

fit_chainA <-aov(cavidade~fatores)
summary(fit_chainA)
fit1_Tukey <- TukeyHSD(fit_chainA)
plot(fit1_Tukey)
