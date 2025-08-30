## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width=6, 
  fig.height=4
)
# Legge denne i YAML på toppen for å skrive ut til tex
#output: 
#  pdf_document: 
#    keep_tex: true
# Original:
#  rmarkdown::html_vignette:
#    toc: true

## ----setup--------------------------------------------------------------------
# Start the HDANOVA R package
library(HDANOVA)

## -----------------------------------------------------------------------------
# Load Candy data
data(candies)

# Fit ASCA model
mod <- hdanova(assessment ~ candy*assessor, data=candies)
summary(mod)

## -----------------------------------------------------------------------------
# Fit ASCA model
mod <- asca(assessment ~ candy*assessor, data=candies)
summary(mod)

## -----------------------------------------------------------------------------
# Fit ASCA model step by step
mod_hd <- hdanova(assessment ~ candy*assessor, data=candies)
mod_asca <- sca(mod_hd)

## -----------------------------------------------------------------------------
# Permutation testing (default = 1000 permutations, if not specified)
mod <- asca(assessment ~ candy*assessor, data=candies, permute=TRUE)
summary(mod)

## -----------------------------------------------------------------------------
permutationplot(mod, factor = "assessor")

## -----------------------------------------------------------------------------
mod <- asca(assessment ~ candy*assessor, data=candies)
mod <- permutation(mod)

## -----------------------------------------------------------------------------
# Fit ASCA model with random assessor
mod.mixed <- asca(assessment ~ candy*r(assessor), data=candies, permute=TRUE)
summary(mod.mixed)

## ----fig.width=4.5, fig.height=7----------------------------------------------
par.old <- par(mfrow=c(2,1), mar=c(4,4,2,1))
loadingplot(mod, scatter=TRUE, labels="names", main="Candy loadings")
scoreplot(mod, main="Candy scores")
par(par.old)

## ----fig.width=4.5, fig.height=7----------------------------------------------
par.old <- par(mfrow=c(2,1), mar=c(4,4,2,1))
loadingplot(mod, factor="assessor", scatter=TRUE, labels="names", main="Assessor loadings")
scoreplot(mod, factor="assessor", main="Assessor scores")
par(par.old)

## ----fig.width=4.5, fig.height=7----------------------------------------------
par.old <- par(mfrow=c(2,1), mar=c(4,4,2,1))
scoreplot(mod, factor="assessor", main="Assessor scores", projections=FALSE)
scoreplot(mod, factor="assessor", main="Assessor scores", spider=TRUE)
par(par.old)

## ----fig.width=4.5, fig.height=7----------------------------------------------
mod <- signflip(mod, factor="candy", comp=1)
par.old <- par(mfrow=c(2,1), mar=c(4,4,2,1))
loadingplot(mod, scatter=TRUE, labels="names", main="Candy loadings")
scoreplot(mod, main="Candy scores")
par(par.old)

## -----------------------------------------------------------------------------
L <- loadings(mod, factor="candy")
head(L)
S <- scores(mod, factor="candy")
head(S)

## -----------------------------------------------------------------------------
biplot(mod, factor="candy", labels="names")

## ----fig.width=4.5, fig.height=7----------------------------------------------
par.old <- par(mfrow=c(2,1), mar=c(4,4,2,1))
scoreplot(mod, ellipsoids="data", factor="candy", main="Data ellipsoids")
scoreplot(mod, ellipsoids="confidence", factor="candy", main="Confidence ellipsoids")
par(par.old)

## ----fig.width=4.5, fig.height=7----------------------------------------------
par.old <- par(mfrow=c(2,1), mar=c(4,4,2,1))
scoreplot(mod.mixed, ellipsoids="data", factor="candy", main="Data ellipsoids")
scoreplot(mod.mixed, ellipsoids="confidence", factor="candy", main="Confidence ellipsoids")
par(par.old)

## -----------------------------------------------------------------------------
# Load Caldana data
data(caldana)

# Combined effects
mod.comb <- asca(compounds ~ time + comb(light + time:light), data=caldana)
summary(mod.comb)

## -----------------------------------------------------------------------------
# Scores plotted as a function of time
par.old <- par(mfrow=c(2,1), mar = c(4,4,1,1))
timeplot(mod.comb, factor="light", time="time", comb=2, comp=1, x_time=TRUE)
timeplot(mod.comb, factor="light", time="time", comb=2, comp=2, x_time=TRUE)
par(par.old)

## -----------------------------------------------------------------------------
caldanaNum <- caldana
caldanaNum$time <- as.numeric(as.character(caldanaNum$time))
mod.num <- asca(compounds ~ time*light, data = caldanaNum)
summary(mod.num)

## -----------------------------------------------------------------------------
# Fit APCA model
modp <- apca(assessment ~ candy*assessor, data=candies)
summary(modp)

## ----fig.width=4.5, fig.height=7----------------------------------------------
par.old <- par(mfrow=c(2,1), mar=c(4,4,2,1))
loadingplot(modp, scatter=TRUE, labels="names", main="Candy loadings")
scoreplot(modp, main="Candy scores")
par(par.old)

## -----------------------------------------------------------------------------
mod.pc <- pcanova(assessment ~ candy * assessor, data = candies, ncomp = 0.9)
print(mod.pc)
summary(mod.pc)

## ----fig.width=4.5, fig.height=7----------------------------------------------
par.old <- par(mfrow=c(2,1), mar=c(4,4,2,1))
loadingplot(mod.pc, scatter=TRUE, labels="names", main="Global loadings")
scoreplot(mod.pc, main="Global scores")
par(par.old)

## -----------------------------------------------------------------------------
# Default MSCA model with a single factor
mod.msca <- msca(assessment ~ candy, data=candies)
summary(mod.msca)

## -----------------------------------------------------------------------------
# Scoreplot for between-individuals factor
scoreplot(mod.msca)

# Scoreplot of within-individuals factor
scoreplot(mod.msca, factor="within")

# .. per factor level
par.old <- par(mfrow=c(3,2), mar=c(4,4,2,1), mgp=c(2,0.7,0))
for(i in 1:length(mod.msca$scores.within))
 scoreplot(mod.msca, factor="within", within_level=i, 
           main=paste0("Level: ", names(mod.msca$scores.within)[i]),
           panel.first=abline(v=0,h=0,col="gray",lty=2))
par(par.old)

## -----------------------------------------------------------------------------
# Default LiMM-PCA model with two factors and interaction, 8 PCA components
mod.reml <- limmpca(assessment ~ candy*r(assessor), data=candies, pca.in=8)
summary(mod.reml)
scoreplot(mod.reml, factor="candy")

## -----------------------------------------------------------------------------
# LiMM-PCA with least squares estimation and 8 PCA components
mod.ls <- limmpca(assessment ~ candy*r(assessor), data=candies, REML=NULL, pca.in=8)
summary(mod.ls)
scoreplot(mod.ls)

## -----------------------------------------------------------------------------
set.seed(123)
# Original simulation
dat <- data.frame(
  feed  = factor(rep(rep(c("low","high"), each=6), 4)),
  breed = factor(rep(c("NRF","Hereford","Angus"), 16)),
  bull  = factor(rep(LETTERS[1:4], each = 12)),
  daughter = factor(c(rep(letters[1:4], 3), rep(letters[5:8], 3), rep(letters[9:12], 3), rep(letters[13:16], 3))),
  age   = round(rnorm(48, mean = 36, sd = 5))
)
dat$yield <- 150*with(dat, 10 + 3 * as.numeric(feed) + as.numeric(breed) + 
                        2 * as.numeric(bull) + 1 * as.numeric(sample(dat$daughter, 48)) + 
                        0.5 * age + rnorm(48, sd = 2))
# Extended to repeated measures
long <- dat[c(1:4,9:12), c("feed", "daughter", "yield")]
long <- rbind(long, long, long)
long$daughter <- factor(long$daughter) # Remove redundant daughters
long$time  <- factor(rep(1:3, each=8))
long$yield <- long$yield + rnorm(24, sd = 100) + rep(c(-200,0,200), each=8)
# Made multiresponse (no added structure, only noise)
long$yield <- I(matrix(rep(long$yield,10),nrow=length(long$yield),ncol=10)) + rnorm(length(long$yield)*10)

## -----------------------------------------------------------------------------
# Least squares mixed model ASCA
mod.rm.asca <- asca(yield ~ r(daughter) + feed*r(time), data = long)
summary(mod.rm.asca)

## -----------------------------------------------------------------------------
# REML mixed model LiMM-PCA
mod.rm.limmpca <- limmpca(yield ~ r(daughter) + feed*r(time), data = long, pca.in=10)
summary(mod.rm.limmpca)

