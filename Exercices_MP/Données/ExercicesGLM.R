### Exercices ###

### GLM GAMMA ###

## a) ##
Bcar <- read.table("BritishCar.csv",header=TRUE,sep=";")

modinv <- glm(AvCost~OwnerAge*(Model+CarAge)+Model:CarAge,family=Gamma,data=Bcar)
summary(modinv)

## b) ##
modinv <- glm(AvCost~OwnerAge+Model+CarAge,family=Gamma,data=Bcar)

anova(modinv2,modinv)

## résidus ##
attach(Bcar)
summary(Bcar)
pears <- (AvCost[-which(is.na(AvCost))]-fitted(modinv))/fitted(modinv)
ansc <- 3*(AvCost[-which(is.na(AvCost))]^(1/3)-fitted(modinv)^(1/3))/fitted(modinv)^(1/3)
dev <-sign(AvCost[-which(is.na(AvCost))]-fitted(modinv))*sqrt(2*(log(fitted(modinv)/AvCost[-which(is.na(AvCost))])+pears))

plot(fitted(modinv),pears,xlab="Valeurs prédites",ylab="Résidus Pearson",pch=16)
plot(fitted(modinv),ansc,xlab="Valeurs prédites",ylab="Résidus Anscombe",pch=16)
plot(fitted(modinv),dev,xlab="Valeurs prédites",ylab="Résidus de déviance",pch=16)



## h) ##
summary(modinv)

11.511/0.1074529

modlog <- glm(AvCost~OwnerAge+Model+CarAge,family=Gamma(link=log),data=Bcar)


modlog2 <- update(modlog,~.-OwnerAge:Model:CarAge)
summary(modlog)

anova(modlog2)

modinvfinal <- glm(AvCost~OwnerAge+Model+CarAge,family=Gamma,data=Bcar)
summary(modinvfinal)

anova(modinvfinal)
qchisq(0.95,3)


#####################
### NHANES STUDY ####

phys <- read.csv("nhanesPhysicalActivity.csv",sep=";",header=TRUE)

phys[1,]

modobese <- glm(OBESE~factor(GENDER)+AGE+factor(MARSTAT)+VIGWRK+MODWRK+WLKBIK+VIGRECEXR+MODRECEXR+SEDMIN,family=binomial,data=phys)

summary(modobese)

attach(phys)
summary(phys)
lowHDL <- HDL<40+GENDER*10
highCHOL <- TCHOL>240

modHDL <- glm(lowHDL~factor(GENDER)+AGE+factor(MARSTAT)+VIGWRK+MODWRK+WLKBIK+VIGRECEXR+MODRECEXR+SEDMIN,family=binomial,data=phys)
summary(modHDL)

modTCHOL <- glm(highCHOL~factor(GENDER)+AGE+factor(MARSTAT)+VIGWRK+MODWRK+WLKBIK+VIGRECEXR+MODRECEXR+SEDMIN,family=binomial,data=phys)
summary(modTCHOL)

DECLINED <-pmin((SYSBP>140)+(DBP>90)+(TCHOL>240),1)

mod.de <- glm(DECLINED~factor(GENDER)+AGE+factor(MARSTAT)+WT+HT+VIGWRK+MODWRK+WLKBIK+VIGRECEXR+MODRECEXR+SEDMIN,family=binomial(link="logit"),data=phys)
summary(mod.de)

anova(mod.de)

mod.de2 <- update(mod.de,~.-MODRECEXR)
summary(mod.de2)
mod.de3 <- update(mod.de2,~.-factor(MARSTAT))
summary(mod.de3)
mod.de4 <- update(mod.de3,~.-WLKBIK)
summary(mod.de4)
mod.de5 <- update(mod.de4,~.-MODWRK)
summary(mod.de5)
mod.de6 <- update(mod.de5,~.-VIGRECEXR)
summary(mod.de6)

add1(mod.de6,mod.de,test="Chisq")

mod.de7 <- update(mod.de6,~.+I(MARSTAT==3))
summary(mod.de7)

smoke <- read.csv("nhanesSMOKING.csv",sep=";",header=TRUE)

smoke[1,]
summary(smoke)
attach(smoke)
highBP2 <- pmin((PEPMNK1R>140)+(PEPMNK5R>90),1)

modBP2 <- glm(highBP2~HSAGEIR+HSSEX+factor(DMARACER)+BMPWTLBS+BMPHTIN+factor(SMOKE),family=binomial,data=smoke)
summary(modBP2)

bmi <- (BMPWTLBS*0.45359237)/(BMPHTIN*0.0254)^2

Declined <- pmin((PEPMNK1R>140)+(PEPMNK5R>90)+(TCP>240),1)

modDecl <- glm(Declined~HSAGEIR+HSSEX+factor(DMARACER)+BMPWTLBS+BMPHTIN+factor(SMOKE),family=binomial,data=smoke)
summary(modDecl)

predict(modDecl,data.frame(HSAGEIR=c(20,40,60),HSSEX=c(1,1,0),DMARACER=c(1,1,1),BMPWTLBS=c(120,150,220),BMPHTIN=c(65.44,62.6,68.2),SMOKE=c(1,2,1)),type="response",se.fit=TRUE)

predict(mod.de6,data.frame(AGE=c(20,40,60),GENDER=c(0,0,1),MARSTAT=c(1,2,3),WT=c(120,150,220),HT=c(65.44,62.6,68.2),VIGWRK=c(1,0,0),SEDMIN=c(100,450,700)),type="response",se.fit=TRUE)

summary(mod.de6)
summary(modDecl)

## Homme de 65 ans, pesant 95 kilos (209 livres), mesurant 175 cm (69 pouces)
95*2.20462262
0.393700787*175
95/1.75^2 # bmi
bla <- as.logical((GENDER==0)*(AGE==45))
mean(WT[bla])
mean(HT[bla])

newdataPHYS <- data.frame(AGE=rep(65,40),GENDER=rep(0,40),WT=rep(95,40),HT=rep(175,40),VIGWRK=c(rep(1,20),rep(0,20)),SEDMIN=rep(seq(100,800,length.out=20),2))

phmod <- predict(mod.de6,newdataPHYS,type="response")

plot(seq(100,800,length.out=20),phmod[1:20],col=2,type="l",ylim=c(0.3,0.5))
points(seq(100,800,length.out=20),phmod[21:40],col=3,type="l")

newdataSMOKE <- data.frame(HSAGEIR=rep(65,9),HSSEX=rep(1,9),BMPWTLBS=rep(209,9),BMPHTIN=rep(69,9),DMARACER=rep(1:3,each=3),SMOKE=rep(1:3,3))

smmod <- predict(modDecl,newdataSMOKE,type="response")

## Femme de 20 ans, pesant 70 kilos (155 livres), mesurant 145 cm (57 pouces)
70*2.20462262
0.393700787*145
70/1.45^2 # bmi
bla <- as.logical((GENDER==1)*(AGE==20))
max(WT[bla])
min(HT[bla])

newdataPHYS <- data.frame(AGE=rep(70,40),GENDER=rep(1,40),WT=rep(70,40),HT=rep(145,40),VIGWRK=c(rep(1,20),rep(0,20)),SEDMIN=rep(seq(100,800,length.out=20),2))

phmod <- predict(mod.de6,newdataPHYS,type="response")

plot(seq(100,800,length.out=20),phmod[1:20],col=2,type="l")
points(seq(100,800,length.out=20),phmod[21:40],col=3,type="l")

newdataSMOKE <- data.frame(HSAGEIR=rep(65,9),HSSEX=rep(0,9),BMPWTLBS=rep(209,9),BMPHTIN=rep(69,9),DMARACER=rep(1:3,each=3),SMOKE=rep(1:3,3))

smmod <- predict(modDecl,newdataSMOKE,type="response")


####### START AGAIN #######

smdat <- read.csv("nhanesSMOKE.csv",sep=";",header=TRUE)
physdat <- read.csv("nhanesPHYS.csv",sep=";",header=TRUE)

## create smDecl and physDecl the response variables ##
smdat[1,]

smDecl <- rep(1,dim(smdat)[1])
smDecl[((smdat[,12]>140)+(smdat[,13]>90)+(smdat[,11]>240))==0] <- 0

physdat[1,]

physDecl <- rep(1,dim(physdat)[1])
physDecl[((physdat[,8]>140)+(physdat[,9]>90)+(physdat[,6]>240))==0] <- 0

modphys <- glm(physDecl~GENDER+AGE+WT+HT+VIGWRK+SEDMIN,family=binomial(link="logit"),data=physdat)
summary(modphys)

modsm <- glm(smDecl~GENDER+AGE+WT+HT+factor(RACE)+factor(SMOKE),family=binomial(link="logit"),data=smdat)
summary(modsm)

newdata <- data.frame(AGE=rep(70,360),GENDER=rep(1,360),WT=rep(70,360),HT=rep(145,360),VIGWRK=rep(c(rep(1,20),rep(0,20)),9),SEDMIN=rep(seq(100,800,length.out=20),18),SMOKE=rep(1:3,each=120),RACE=rep(1:3,120))

newdata[200:210,]

phpred <- predict(modphys,newdata,type="response")
smpred <- predict(modsm,newdata,type="response")

plot(rep(seq(100,800,length.out=20),18),phpred)
plot(rep(seq(100,800,length.out=20),18),smpred)


## create smHBP and physHBP the response variables ##
smdat[1,]

smHBP <- rep(1,dim(smdat)[1])
smHBP[((smdat[,12]>140)+(smdat[,13]>90))==0] <- 0

physdat[1,]

physHBP <- rep(1,dim(physdat)[1])
physHBP[((physdat[,8]>140)+(physdat[,9]>90))==0] <- 0

modphysBP <- glm(physHBP~GENDER+AGE+WT+HT+SEDMIN,family=binomial(link="logit"),data=physdat)
summary(modphysBP)

modsmBP <- glm(smHBP~GENDER+AGE+WT+HT+factor(RACE)+factor(SMOKE),family=binomial(link="logit"),data=smdat)
summary(modsmBP)
anova(modsmBP)
qchisq(0.95,2)

newdata <- data.frame(AGE=rep(70,360),GENDER=rep(1,360),WT=rep(70,360),HT=rep(145,360),VIGWRK=rep(c(rep(1,20),rep(0,20)),9),SEDMIN=rep(seq(100,800,length.out=20),18),SMOKE=rep(1:3,each=120),RACE=rep(1:3,120))

phpred <- predict(modphysBP,newdata,type="response")
smpred <- predict(modsmBP,newdata,type="response")

plot(rep(seq(100,800,length.out=20),18),phpred)
plot(rep(seq(100,800,length.out=20),18),smpred)

## create smHC and physHC the response variables ##
smdat[1,]

smHC <- rep(1,dim(smdat)[1])
smHC[(smdat[,11]>240)==0] <- 0

physdat[1,]

physHC <- rep(1,dim(physdat)[1])
physHC[(physdat[,6]>240)==0] <- 0

modphysC <- glm(physHC~AGE+HT+VIGWRK+VIGRECEXR+factor(MARSTAT),family=binomial(link="logit"),data=physdat)
summary(modphysC)
anova(modphysC)
qchisq(0.95,5)
qchisq(0.95,1)

modsmC <- glm(smHC~GENDER+AGE+WT+HT+factor(SMOKE),family=binomial(link="logit"),data=smdat)
summary(modsmC)
anova(modsmC)
qchisq(0.95,2)

newdata <- data.frame(AGE=rep(70,360),GENDER=rep(1,360),WT=rep(70,360),HT=rep(145,360),VIGWRK=rep(c(rep(1,20),rep(0,20)),9),SEDMIN=rep(seq(100,800,length.out=20),18),SMOKE=rep(1:3,each=120),RACE=rep(1:3,120))

phpred <- predict(modphysC,newdata,type="response")
smpred <- predict(modsmC,newdata,type="response")

plot(rep(seq(100,800,length.out=20),18),phpred)
plot(rep(seq(100,800,length.out=20),18),smpred)


#######################
### Chap 5 ####

library(datasets)

fit1 <- glm(ncases~factor(agegp)*(factor(alcgp)+factor(tobgp))+factor(alcgp):factor(tobgp),family=poisson,data=esoph)
summary(fit1)

anova(fit1)
qchisq(0.95,9) ## rejette alcgp:tobgp
qchisq(0.95,15) ## rejette agegp:tobgp mais conserve agegp:alcgp

fit2 <- glm(ncases~factor(agegp)*factor(alcgp)+factor(tobgp),family=poisson,data=esoph)

summary(fit2)

anova(fit2,fit1)
qchisq(0.95,24)

fit3 <- update(fit1,~.-factor(agegp):factor(tobgp))

anova(fit2,fit3)
qchisq(0.95,9)

fit2 <- glm(ncases~agegp*alcgp+tobgp,family=poisson,data=esoph)
summary(fit2)

levels(factor(agegp))
levels(alcgp)
levels(tobgp)

##############################

sweden <- read.table("TPL Sweden.txt",header=TRUE)

fit0 <- glm(Claims~factor(Kilometres)+factor(Zone)+factor(Bonus)+factor(Make)+offset(log(Insured)),data=sweden,family=poisson)

drop1(fit0)

fit1 <- glm(Claims~factor(Kilometres)*factor(Zone)*factor(Bonus)+factor(Zone)*factor(Make)+factor(Bonus)*factor(Make)+offset(log(Insured)),data=sweden,family=poisson)

anova(fit1)
qchisq(0.95,144) ## drop 3-way interaction
qchisq(0.95,24)

fit2 <- update(fit1,~.-factor(Kilometres):factor(Zone):factor(Bonus))

drop1(fit2,test="Chisq")

fit3 <- update(fit2,~.+factor(Make):factor(Zone):factor(Bonus))

anova(fit3)
qchisq(0.95,288) ## 3-way interaction is significant!!

drop1(fit3)

fit4 <- update(fit3,~.-factor(Kilometres):factor(Bonus))

anova(fit4,fit3)
qchisq(0.95,24)

fit00 <- glm(Claims~factor(Kilometres)*factor(Zone)*factor(Bonus)+factor(Make)+offset(log(Insured)),data=sweden,family=poisson)

anova(fit00)
qchisq(0.99,144)

drop1(fit00,test="Chisq")


########################

canada <- read.table("Canadian Auto Claims 57-58.txt",header=TRUE)

canada[1:3,]

fitsat <- glm(Claims~factor(Merit)*factor(Class)+offset(log(Insured)),family=poisson,data=canada)

summary(fitsat)
anova(fitsat)
qchisq(0.99,12)


###
sex <- rep(0:1,each=6)
Dep <- rep(0:5,2)
y <- c(512,353,120,138,53,22,89,17,202,131,94,24)
no <- c(313,207,205,279,138,351,19,8,391,244,299,317)
nb <- y+no

fitp <- glm(y~factor(sex)*factor(Dep)+offset(log(nb)),family=poisson)
summary(fitp)
anova(fitp)
qchisq(0.95,5) ## reject interaction

fitp2 <- glm(y~factor(sex)+factor(Dep)+offset(log(nb)),family=poisson)
summary(fitp2)

fitpDep <- glm(y~factor(Dep)+offset(log(nb)),family=poisson)
anova(fitpDep,fitp2)
qchisq(0.95,1) ## sex is not significant.

anova(fitpDep,fitp)
qchisq(0.95,6)

summary(fitpDep)

sum((y-fitted(fitpDep))^2/fitted(fitpDep))
pchisq(8.025236,6)

fitpSex <- glm(y~factor(sex)+offset(log(nb)),family=poisson)
summary(fitpSex)

fitbSex <- glm(cbind(y,nb-y)~factor(sex),family=binomial)
summary(fitbSex)


fitb <- glm(cbind(y,nb-y)~factor(sex)*factor(Dep),family=binomial)
summary(fitb)
anova(fitb)

fitb2 <- glm(cbind(y,nb-y)~factor(sex)+factor(Dep),family=binomial)
summary(fitb2)

et <- rep("&",5)
fin <- rep("\\",5)


data.frame(y[2:6], et, no[2:6], et,y[8:12],et,no[8:12],fin)


###############
y <- c( 8,7,6,6,3,4,7,2,3,4,9,9,8,14,8,13,11,5,7,6)
x <- rep(0:1,each=10)

fit1 <- glm(y~x,family=poisson)
summary(fit1)

exp(fit1$coef[2]+c(-1,1)*qnorm(0.975)*summary(fit1)$coefficients[2,2])

log(mean(y[which(x==1)])/mean(y[which(x==0)]))

library(MASS)

fit2 <- glm.nb(y~x)
summary(fit2)

2*pnorm(fit2$coef[2]/summary(fit2)$coefficients[2,2],0,1,lower.tail=FALSE)

fit3 <- glm(y~1,family=poisson)
fit4 <- glm.nb(y~1)

summary(fit3)

summary(fit4)

fit3$coef[1]+summary(fit3)

exp(fit3$coef[1]+c(-1,1)*qnorm(0.975)*summary(fit3)$coefficients[1,2])
exp(fit4$coef[1]+c(-1,1)*qnorm(0.975)*summary(fit4)$coefficients[1,2])


######
skin <- read.table("Skin.txt",header=TRUE)

skin[1,]

mod1 <- glm(cbind(Cases,Population-Cases)~Town+Age,family=binomial,data=skin)
summary(mod1)

predict(mod1,data.frame(Town=c(0,1),Age=rep("45-54",2)),type="response",se.fit=TRUE)

eta <- mod1$coef[1]+mod1$coef[2]+mod1$coef[5]
exp(eta)/(1+exp(eta))

attach(skin)
levels(Age)

mod2 <- glm(Cases~Town+Age+offset(log(Population)),family=poisson,data=skin)
summary(mod2)

predict(mod2,data.frame(Town=c(0,1),Age=rep("45-54",2),Population=rep(1,2)),type="response")


###
age <- c(39,42,20,37,20,21,41,52)
blackout <- c(0,1,0,1,1,0,1,1)
mod1 <- glm(blackout~age,family=binomial)
summary(mod1)

names(summary(mod1))
summary(mod1)$coefficients

#####
age <- c(12,15,42,52,59,73,82,91,96,105,114,120,121,128,130,139,139,157,1,1,2,8,11,18,22,31,37,61,72,81,97,112,118,127,131,140,151,159,177,206)
kyp <- c(rep(1,18),rep(0,22))

moda <- glm(kyp~age,family=binomial)
summary(moda)

f <- function(x,coef) {
    eta <- coef[1]+coef[2]*x+coef[3]*x^2
    exp(eta)/(1+exp(eta))
}

plot(age,kyp,pch=16)
curve(f(x,c(moda$coef,0)),add=TRUE)
curve(f(x,modb$coef),add=TRUE,col=2,lty=2)

modb <- glm(kyp~age+I(age^2),family=binomial)
summary(modb)





gr1 <- which(age<=36)
gr2 <- which(as.logical((age>36)*(age<=62)))
gr3 <- which(as.logical((age>62)*(age<=94)))
gr4 <- which(as.logical((age>94)*(age<=128)))
gr5<- which(as.logical((age>128)*(age<=150)))
gr6 <- which(age>150)


gr.kyp <- c(sum(kyp[gr1])/length(gr1),sum(kyp[gr2])/length(gr2),sum(kyp[gr3])/length(gr3),sum(kyp[gr4])/length(gr4),sum(kyp[gr5])/length(gr5),sum(kyp[gr6])/length(gr6))
gr.age <- c(mean(age[gr1]),mean(age[gr2]),mean(age[gr3]),mean(age[gr4]),mean(age[gr5]),mean(age[gr6]))

plot(gr.age,gr.kyp,pch=16,xlim=c(0,200),ylim=c(0,0.8),xlab="Age moyen dans groupe",ylab="probabilité observée dans groupe")

curve(f(x,c(moda$coef,0)),add=TRUE)
curve(f(x,modb$coef),add=TRUE,col=2,lty=2)

##############
x <- 0:2
Success <- c(1,2,4)
Trials <- rep(4,3)


xv2 <- rep(0:2,each=4)
Successv2 <- c(1,0,0,0,1,1,0,0,1,1,1,1)

M0 <- glm(cbind(Success,Trials-Success)~1,binomial)
summary(M0)
M1 <- glm(cbind(Success,Trials-Success)~x,binomial)
summary(M1)

M0v2 <- glm(Successv2~1,binomial)
summary(M0v2)
M1v2 <- glm(Successv2~xv2,binomial)
summary(M1v2)


sum(Success*M0$coef-Trials*log(1+exp(M0$coef))+log(choose(Trials,Success))) ## L0 v1
sum(Success*(M1$coef[1]+M1$coef[2]*x)-Trials*log(1+exp(M1$coef[1]+M1$coef[2]*x))+log(choose(Trials,Success))) ## L1 v1

sum(Successv2*M0v2$coef-log(1+exp(M0v2$coef))) ## L0 v2
sum(Successv2*(M1v2$coef[1]+M1v2$coef[2]*xv2)-log(1+exp(M1v2$coef[1]+M1v2$coef[2]*xv2))) ## L1 v2

anova(M0,M1)
anova(M0v2,M1v2)
qchisq(0.95,1)
