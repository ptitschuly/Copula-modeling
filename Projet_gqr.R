library("quantmod")
library("VineCopula")
library("tidyverse")
library("tidyr")

getSymbols("^SSMI",from="2010-01-01",to="2019-12-31")
getSymbols("^FCHI",from="2010-01-01",to="2019-12-31")
getSymbols("CHFEUR=X",from="2010-01-01",to="2019-12-31")

SMI = SSMI[, "SSMI.Close"]
SMI$SMI_log_return <- diff(log(SMI))
plot(SMI$SSMI.Close,main="Cours du SMI")
plot(SMI$SMI_log_return,main="rdt du SMI")

CAC = FCHI[, "FCHI.Close"]
CAC$CAC_log_return <- diff(log(CAC))
plot(CAC$FCHI.Close,main="cours du CAC40")
plot(CAC$CAC_log_return,main="rdt du CAC40")

tx_de_change = `CHFEUR=X`[, "CHFEUR=X.Close"]
tx_de_change$var_log_change <- diff(log(tx_de_change))
plot(tx_de_change$CHFEUR.X.Close,main="taux de change")
plot(tx_de_change$var_log_change,main="var du tx de change")



df <- merge(SMI,CAC,by=CAC$index)
df <- merge(df,tx_de_change,by=CAC$index)
df <- na.omit(df)
df$SMI_corrigé <- (df$SMI_log_return+1)/(df$var_log_change+1)-1

plot(df$SMI_log_return)
plot(df$CAC_log_return)
plot(df$SMI_corrigé)

summary(df$CAC_log_return)
summary(df$SMI_corrigé)

plot(x=df$SMI_corrigé,y=df$CAC_log_return,type="p",main="Croisement du rdt euro et non euro")
hist(df$SMI_corrigé,breaks=15,freq= FALSE ,main="histogramme du rdt du cours suisse",xlab="rdt du SMI corrigé du tx de change",ylab="density")
lines(density(df$SMI_corrigé))
hist(df$CAC_log_return,main="histogramme du rdt du CAC40")
lines(density(df$CAC_log_return))


# QQ PLOT ET FONCTION DE REPARTITION 
qqplot(df$SMI_corrigé,)
qqline(df$SMI_corrigé)

# test de normalité (kolmogorov smirnov)
ks.test(df$SMI_corrigé,"pnorm",mean=mean(df$SMI_corrigé),sd=sd(df$SMI_corrigé))
ks.test(df$CAC_log_return,"pnorm",mean=mean(df$CAC_log_return),sd=sd(df$CAC_log_return))

# les tests de corrélation 
rsmi <- df$SMI_corrigé
storage.mode(rsmi) <- "numeric"
rcac <- df$CAC_log_return
storage.mode(rcac) <- "numeric"

cor(rcac,rsmi,method="pearson")
cor(rcac,rsmi,method="kendall")
cor(rcac,rsmi,method="spearman")


library("VineCopula")
u <- pobs(as.matrix(cbind(rcac,rsmi)))[,1]
v <- pobs(as.matrix(cbind(rcac,rsmi)))[,2]

selectedCopula <- BiCopSelect(u,v,familyset=NA)
selectedCopula
# 2 -> student copula (elliptic) of dimension = 2

install.packages("copula")
library("copula")

copule <- tCopula(dim=2)
set.seed(2493)
m <- pobs(as.matrix(cbind(rcac,rsmi)))

cor(m,method = "kendall")
fit <- fitCopula(copule,m,method="ml")
coef(fit)
rho <- coef(fit)[1]
df <- coef(fit)[2]
contour(tCopula(dim=2,rho,df=df),dCopula)
persp(tCopula(dim=2,rho,df=df),dCopula)
w <- rCopula(2500,tCopula(dim=2,rho,df=df))
plot(w[,1],w[,2])

copule <- claytonCopula(dim=2)
m <- pobs(as.matrix(cbind(rcac,rsmi)))
fit <- fitCopula(copule,m,method="ml")
coef(fit)
tau(claytonCopula(param=coef(fit)))

gfc <- gofCopula(claytonCopula(dim=2),m,N=100)
gfc

alpha <- coef(fit)[1]
contour(tCopula(dim=2,alpha=alpha),dCopula)
persp(tCopula(dim=2,rho,df=df),dCopula)
w <- rCopula(2500,tCopula(dim=2,rho,df=df))
plot(w[,1],w[,2])