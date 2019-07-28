library(psycho)
library(tidyverse)
library(BMS)


#for humid v. arid predictors
setwd("/Users/herpworld/Desktop/coexp_scripts/dryad/11_BMA/BMA_arid_humid/")
exp.merge<-read.csv("real_exp_merge.csv", header=T)

#for ecoregion predictors
setwd("/Users/herpworld/Desktop/coexp_scripts/dryad/11_BMA/BMA_ecoregion/")
exp.merge<-read.csv("real_merge.csv", header=T)

#test for correlation in elevation data
#cor.test(exp.merge$elev.mean.stand, exp.merge$elevation.index, method=c("pearson"))
# > p-value = 0.0003705, correlated
#cor.test(exp.merge$elev.mean.stand, exp.merge$elev.range.stand, method=c("pearson"))
# > p-value = 0.08992, not correlated
#cor.test(exp.merge$elevation.index, exp.merge$elev.range.stand, method=c("pearson"))
# > p-value = 1.466e-09, super correlated

##exp
#for humid v. arid predictors
keeps <- c("EXP_rej_stand", "elevation.index", "is_amphibian", "is_diurnal", "gen_time_stand.1", "is_humid_real", "is_arid", "is_arboreal", "SVL")
newest.exp<-exp.merge[keeps]

#for ecoregion predictors
keeps <- c("EXP_rej_stand", "elevation.index", "is_amphibian", "is_diurnal", "is_arboreal", "SVL", 
           "gen_time_stand", "is_aridspinybush", "is_evergreenrainforest", "is_centralhighland", 
           "is_sambirano", "is_drydeciuous")
newest.exp<-exp.merge[keeps]


colnames(newest.exp)
names(newest.exp) <- c("EXP_rej_stand", "elevation dispersion", "taxonomy", "activity pattern", "habitat use", "SVL",
                       "generation time", "arid spiny bush", "evergreen rainforest", "central highland", 
                       "Sambirano rainforest", "dry deciuous")

attitude<-newest.exp
att = bms(attitude, mprior = "uniform", g = "UIP", user.int = F)
a<-coef(att)
write.csv(a, file="exp_coef.csv", row.names=T, quote=T)
b<-coef(att, std.coefs = T, order.by.pip = F, include.constant = T)
write.csv(b, "exp_PIPcoef.csv", row.names=T, quote=T)
c<-summary(att)
write.csv(c, "exp_summary.csv", row.names=T, quote=T)
d<-topmodels.bma(att)[, 1:3]
write.csv(d, "exp_topmodels.csv", row.names=T, quote=T)
#blue is a positive coeff and red is a negative coeff
pdf(file="exp_64models.pdf")
image(att)
dev.off()
#mean no regressors = 1.5247 = result of below
sum(coef(att)[, 1])
#prior is symmetric around K/2=3, simpler models are better
pdf(file="exp_postmodelsizedist.pdf")
dev.off()
plotModelsize(att)
pdf(file="exp_PosteriorModelSizeandProbs.pdf")
att_fixed = bms(attitude, mprior = "fixed", mprior.size = 2, user.int = T)
dev.off()


##pi
#for humid v. arid predictors
keeps <- c("Pi_stand", "elevation.index", "is_amphibian", "is_diurnal", "gen_time_stand.1", "is_humid_real", "is_arid", "is_arboreal", "SVL")
newest.pi<-exp.merge[keeps]
head(newest.pi)
names(newest.pi) <- c("Pi_stand", "elevation dispersion", "taxonomy", "activity pattern", "generation time", "humid ecoregion", "arid ecoregion", "habitat use", "SVL")



#for ecoregion predictors
keeps <- c("Pi_stand", "elevation.index", "is_amphibian", "is_diurnal", "is_arboreal", "SVL", 
           "gen_time_stand", "is_aridspinybush", "is_evergreenrainforest", "is_centralhighland", 
           "is_sambirano", "is_drydeciuous")
newest.pi<-exp.merge[keeps]

colnames(newest.pi)
names(newest.pi) <- c("Pi_stand", "elevation dispersion", "taxonomy", "activity pattern", "habitat use", "SVL",
                       "generation time", "arid spiny bush", "evergreen rainforest", "central highland", 
                       "Sambirano rainforest", "dry deciuous")

attitude<-newest.pi
att = bms(attitude, mprior = "uniform", g = "UIP", user.int = F)
a<-coef(att)
write.csv(a, file="pi_coef.csv", row.names=T, quote=T)
b<-coef(att, std.coefs = T, order.by.pip = F, include.constant = T)
write.csv(b, "pi_PIPcoef.csv", row.names=T, quote=T)
c<-summary(att)
write.csv(c, "pi_summary.csv", row.names=T, quote=T)
d<-topmodels.bma(att)[, 1:3]
write.csv(d, "pi_topmodels.csv", row.names=T, quote=T)
#blue is a positive coeff and red is a negative coeff
pdf(file="pi_64models.pdf")
image(att)
dev.off()
#mean no regressors = 1.5247 = result of below
sum(coef(att)[, 1])
#prior is symmetric around K/2=3, simpler models are better
pdf(file="pi_postmodelsizedist.pdf")
dev.off()
plotModelsize(att)
pdf(file="pi_PosteriorModelSizeandProbs.pdf")
att_fixed = bms(attitude, mprior = "fixed", mprior.size = 2, user.int = T)
dev.off()


##TD
#for humid v. arid predictors
keeps <- c("TD_stand", "elevation.index", "is_amphibian", "is_diurnal", "gen_time_stand.1", "is_humid_real", "is_arid", "is_arboreal", "SVL")
newest.td<-exp.merge[keeps]
head(newest.td)
names(newest.td) <- c("TD_stand", "elevation dispersion", "taxonomy", "activity pattern", "generation time", "humid ecoregion", "arid ecoregion", "habitat use", "SVL")

#for ecoregion predictors
keeps <- c("TD_stand", "elevation.index", "is_amphibian", "is_diurnal", "is_arboreal", "SVL", 
           "gen_time_stand", "is_aridspinybush", "is_evergreenrainforest", "is_centralhighland", 
           "is_sambirano", "is_drydeciuous")
newest.td<-exp.merge[keeps]


names(newest.td) <- c("TD_stand", "elevation dispersion", "taxonomy", "activity pattern", "habitat use", "SVL",
                      "generation time", "arid spiny bush", "evergreen rainforest", "central highland", 
                      "Sambirano rainforest", "dry deciuous")


attitude<-newest.td
att = bms(attitude, mprior = "uniform", g = "UIP", user.int = F)
a<-coef(att)
write.csv(a, file="TD_coef.csv", row.names=T, quote=T)
b<-coef(att, std.coefs = T, order.by.pip = F, include.constant = T)
write.csv(b, "TD_PIPcoef.csv", row.names=T, quote=T)
c<-summary(att)
write.csv(c, "TD_summary.csv", row.names=T, quote=T)
d<-topmodels.bma(att)[, 1:3]
write.csv(d, "TD_topmodels.csv", row.names=T, quote=T)
#blue is a positive coeff and red is a negative coeff
pdf(file="TD_64models.pdf")
image(att)
dev.off()
#mean no regressors = 1.5247 = result of below
sum(coef(att)[, 1])
#prior is symmetric around K/2=3, simpler models are better
pdf(file="TD_postmodelsizedist.pdf")
dev.off()
plotModelsize(att)
pdf(file="TD_PosteriorModelSizeandProbs.pdf")
att_fixed = bms(attitude, mprior = "fixed", mprior.size = 2, user.int = T)
dev.off()


##R2
#for humid v. arid predictors
keeps <- c("R2_stand", "elevation.index", "is_amphibian", "is_diurnal", "gen_time_stand.1", "is_humid_real", "is_arid", "is_arboreal", "SVL")
newest.r2<-exp.merge[keeps]
head(newest.r2)
names(newest.r2) <- c("R2_stand", "elevation dispersion", "taxonomy", "activity pattern", "generation time", "humid ecoregion", "arid ecoregion", "habitat use", "SVL")

#for ecoregion predictors
keeps <- c("R2_stand", "elevation.index", "is_amphibian", "is_diurnal", "is_arboreal", "SVL", 
           "gen_time_stand", "is_aridspinybush", "is_evergreenrainforest", "is_centralhighland", 
           "is_sambirano", "is_drydeciuous")
newest.r2<-exp.merge[keeps]


names(newest.r2) <- c("R2_stand", "elevation dispersion", "taxonomy", "activity pattern", "habitat use", "SVL",
                      "generation time", "arid spiny bush", "evergreen rainforest", "central highland", 
                      "Sambirano rainforest", "dry deciuous")


attitude<-newest.r2
att = bms(attitude, mprior = "uniform", g = "UIP", user.int = F)
a<-coef(att)
write.csv(a, file="R2_coef.csv", row.names=T, quote=T)
b<-coef(att, std.coefs = T, order.by.pip = F, include.constant = T)
write.csv(b, "R2_PIPcoef.csv", row.names=T, quote=T)
c<-summary(att)
write.csv(c, "R2_summary.csv", row.names=T, quote=T)
d<-topmodels.bma(att)[, 1:3]
write.csv(d, "R2_topmodels.csv", row.names=T, quote=T)
#blue is a positive coeff and red is a negative coeff
pdf(file="R2_64models.pdf")
image(att)
dev.off()
#mean no regressors = 1.5247 = result of below
sum(coef(att)[, 1])
#prior is symmetric around K/2=3, simpler models are better
pdf(file="R2_postmodelsizedist.pdf")
dev.off()
plotModelsize(att)
pdf(file="R2_PosteriorModelSizeandProbs.pdf")
att_fixed = bms(attitude, mprior = "fixed", mprior.size = 2, user.int = T)
dev.off()

##H
#for humid v. arid predictors
keeps <- c("h_stand", "elevation.index", "is_amphibian", "is_diurnal", "gen_time_stand.1", "is_humid_real", "is_arid", "is_arboreal", "SVL")
newest.h<-exp.merge[keeps]
head(newest.h)
names(newest.h) <- c("h_stand", "elevation dispersion", "taxonomy", "activity pattern", "generation time", "humid ecoregion", "arid ecoregion", "habitat use", "SVL")

#for ecoregion predictors
keeps <- c("h_stand", "elevation.index", "is_amphibian", "is_diurnal", "is_arboreal", "SVL", 
           "gen_time_stand", "is_aridspinybush", "is_evergreenrainforest", "is_centralhighland", 
           "is_sambirano", "is_drydeciuous")
newest.h<-exp.merge[keeps]


names(newest.h) <- c("h_stand", "elevation dispersion", "taxonomy", "activity pattern", "habitat use", "SVL",
                      "generation time", "arid spiny bush", "evergreen rainforest", "central highland", 
                      "Sambirano rainforest", "dry deciuous")



attitude<-newest.h
att = bms(attitude, mprior = "uniform", g = "UIP", user.int = F)
a<-coef(att)
write.csv(a, file="H_coef.csv", row.names=T, quote=T)
b<-coef(att, std.coefs = T, order.by.pip = F, include.constant = T)
write.csv(b, "H_PIPcoef.csv", row.names=T, quote=T)
c<-summary(att)
write.csv(c, "H_summary.csv", row.names=T, quote=T)
d<-topmodels.bma(att)[, 1:3]
write.csv(d, "H_topmodels.csv", row.names=T, quote=T)
#blue is a positive coeff and red is a negative coeff
pdf(file="H_64models.pdf")
image(att)
dev.off()
#mean no regressors = 1.5247 = result of below
sum(coef(att)[, 1])
#prior is symmetric around K/2=3, simpler models are better
pdf(file="H_postmodelsizedist.pdf")
dev.off()
plotModelsize(att)
pdf(file="H_PosteriorModelSizeandProbs.pdf")
att_fixed = bms(attitude, mprior = "fixed", mprior.size = 2, user.int = T)
dev.off()


#standardized coefficients

#post mean = coefficients averaged over all models (even ones it was ot in)
##(comparatively large coefficient & seems important)
#XXX % of model massincludes  this predictor 
#(PIP - sum of all posterior model proba ilities contianig this variable)

#XXX has an intermediate PIP of 40.6%, while the other
#covariates do not seem to matter much (<20).

#last two columns below 1 indicate in virtually all models that include privileges, its coefficient
#sign is negative

#Thebest model, with 29% posterior model probability,12 is the one that only includes complaints.
#However the second best model includes learning in addition and has a PMP of 16%

#Moreover we see that complaints is included in virtually all model mass, and unanimously with
#a positive coefficient

#In contrast, raises is included very little, and its coefficient sign changes
#according to the model.
