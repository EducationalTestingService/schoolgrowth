##############################################
##############################################
##Adapted from JR'S 'test schoolgrowth' CODE
##DATE: 8/15/23
##############################################
##############################################
setwd("C:/Users/kcastellano001/OneDrive - Educational Testing Service/GitHub/katcast_schoolgrowth/schoolgrowth/")
devtools::load_all("R")
devtools::document("R")

##or use following after installing from github
##devtools:::install_github("EducationalTestingService/schoolgrowth")
##library(schoolgrowth)
#######################
options(width=150)
sessionInfo()

## load example data, using SGPs
load("test/testdata.Robj")

print(dim(d))
print(head(d))
d$Y <- d$G2
print(summary(d$Y))

#######################
## RANDOM SCHOOL SAMPLING FOR CODE DEVELOPMENT AND TESTING,
## AND RESTRICTING TO 3 years
#######################
set.seed(3033)
d <- subset(d, year %in% 1:3)
s <- sample(sort(unique(d$school)), size=500, replace=FALSE)
d <- subset(d, school %in% s)
rm(s)

#######################
## TESTING
#######################

## basic case
r1 <- schoolgrowth(d, target = c(years="final", subjects="math", grades="all", weights="n"), control=list(quietly=FALSE, mse_blp_chk=TRUE, jackknife=FALSE))

print(names(r1))
print(summary(c(as.matrix(r1$R))))
print(summary(c(as.matrix(r1$G))))
print(r1$Ginfo)
print(r1$modstats)
print(head(g1 <- na.omit(r1$aggregated_growth)))
print(summary(g1$est.direct))
print(summary(g1$mse.direct))
print(summary(g1$est.blp))
print(summary(g1$mse.blp))
print(summary(g1$prmse.direct))
print(tapply(g1$prmse.direct, g1$ntarget <= 100, summary))

## using Oren's RCO method for estimating the var-cov matrix G
r1r <- schoolgrowth(d, target = c(years="final", subjects="math", grades="all", weights="n"), control=list(quietly=FALSE, Gadj_method="rco",mse_blp_chk=TRUE, jackknife=FALSE))

print(names(r1r))
print(summary(c(as.matrix(r1r$R))))
print(summary(c(as.matrix(r1r$G))))
print(r1r$Ginfo)
print(r1r$modstats)
print(head(g1r <- na.omit(r1r$aggregated_growth)))
print(summary(g1r$est.direct))
print(summarry4(g1r$mse.direct))
print(summary(g1r$est.blp))
print(summary(g1r$mse.blp))
print(summary(g1r$prmse.direct))
print(tapply(g1r$prmse.direct, g1r$ntarget <= 100, summary))
plot(as.matrix(r1r$G)~as.matrix(r1$G),pch=19,cex=.5); abline(0,1)
stopifnot(max(abs( g1r$mse.direct - g1$mse.direct)) < 1e-10)
summary(g1r$mse.blp-g1$mse.blp) ##Estimated MSE generally smaller using RCO method


## spectral rather than nearPD adjustments to G and R, with much different eigenvalue tolerances
r1s <- schoolgrowth(d, target = c(years="final", subjects="math", grades="all", weights="n"), control=list(quietly=FALSE, mse_blp_chk=TRUE, jackknife=FALSE, Radj_method="spectral", Gadj_method="spectral", Radj_eig_tol = 1e-2, Gadj_eig_tol = 1e-2))

print(names(r1s))
print(summary(c(as.matrix(r1s$R))))
print(summary(c(as.matrix(r1s$G))))
print(r1s$modstats)
print(head(g1s <- na.omit(r1s$aggregated_growth)))
print(summary(g1s$est.direct))
print(summary(g1s$mse.direct))
print(summary(g1s$est.blp))
print(summary(g1s$mse.blp))
print(summary(g1s$prmse.direct))
print(tapply(g1s$prmse.direct, g1$ntarget <= 100, summary))
print(summary(g1$est.blp - g1s$est.blp))
print(summary(g1$mse.blp - g1s$mse.blp))

## control$pattern_nmin="min"
r1p <- schoolgrowth(d, target = c(years="final", subjects="math", grades="all", weights="n"), control=list(quietly=FALSE, mse_blp_chk=TRUE, jackknife=FALSE, pattern_nmin="min"))

print(names(r1p))
print(summary(c(as.matrix(r1p$R))))
print(summary(c(as.matrix(r1p$G))))
print(r1p$modstats)
print(head(g1p <- na.omit(r1p$aggregated_growth)))
print(summary(g1p$est.direct))
print(summary(g1p$mse.direct))
print(summary(g1p$est.blp))
print(summary(g1p$mse.blp))
print(summary(g1p$prmse.direct))
print(tapply(g1p$prmse.direct, g1p$ntarget <= 100, summary))

## jackknife
r1j <- schoolgrowth(d, target = c(years="final", subjects="math", grades="all", weights="n"), control=list(quietly=FALSE, mse_blp_chk=TRUE, jackknife=TRUE))
stopifnot(identical(r1$R, r1j$R) && identical(r1$G, r1j$G))
print(length(r1j$dsch[[1]]$jack_mu))
print(cbind(unlist(r1$modstats), unlist(r1j$modstats)))
print(head(g1j <- na.omit(r1j$aggregated_growth)))
stopifnot(identical(g1$est.blp, g1j$est.blp))
print(summary(g1j$mse.blp - g1$mse.blp))
v1  <- sapply(r1$dsch, function(x){ x$var_muhat})
v1j <- sapply(r1j$dsch, function(x){ x$var_muhat})
print(summary(v1j - v1))
print(summary(c(r1j$varhat_G)))

## a contrast
r2 <- schoolgrowth(d, target = c(years="final", subjects="math", grades="all", weights="n"),
                   target_contrast = c(years = "2", subjects="math", grades="all", weights="n"),
                   control=list(quietly=FALSE, mse_blp_chk=TRUE, jackknife=FALSE))
print(head(g2 <- na.omit(r2$aggregated_growth)))
print(summary(g2$est.direct))
print(summary(g2$mse.direct))
print(summary(g2$est.blp))
print(summary(g2$mse.blp))
print(summary(g2$prmse.direct))
print(tapply(g2$prmse.direct, g2$ntarget <= 100, summary))

## passing R and G from r1
r3 <- schoolgrowth(d, target = c(years="final", subjects="math", grades="all", weights="n"), control=list(quietly=FALSE, R=r1$R, G=r1$G, mse_blp_chk=TRUE, jackknife=FALSE))
stopifnot( identical(r1$R, r3$R) && identical(r1$G, r3$G) )
g3 <- na.omit(r3$aggregated_growth)
stopifnot(identical(g1,g3))

## setting alpha==0
r4 <- schoolgrowth(d, target = c(years="final", subjects="math", grades="all", weights="n"), control=list(quietly=FALSE, mse_blp_chk=TRUE, jackknife=FALSE, alpha_zero=TRUE))
print(r4$bhat_ols)
S  <- r4$dsch[sapply(r4$dsch, function(x){ !is.null(x$weights) })]
tmp <- sapply(S, function(x){
    w <- x$weights[x$weights[,"obs"]==1L,"blp"]
    stopifnot( (abs(sum(w) - 1) < 1e-10) && all(names(w) == x$tab$block) )
    return( sum(w * x$tab$Y_sb) - x$est$est.blp )
})
stopifnot(max(abs(tmp)) < 1e-10)

## patterns_only==TRUE
r5 <- schoolgrowth(d, target = c(years="final", subjects="math", grades="all", weights="n"), control=list(quietly=FALSE, patterns_only=TRUE))
str(r5)

## a boundary condition with only 3 blocks (note needed to let pattern_nmin be determined)
tmp <- subset(d, subject=="math" & grade=="6")
print(table(tmp$year))
r6 <- schoolgrowth(tmp, target = c(years="final", subjects="math", grades="6", weights="n"), control=list(quietly=FALSE, mse_blp_chk=TRUE, jackknife=FALSE, pattern_nmin="min"))
print(head(r6$aggregated_growth))

####################################
## tests of permutation invariance
## (NOTE: set seeds to use 50 jackknife batches and ensure sample batch assignments)
####################################
set.seed(3015)
r7 <- schoolgrowth(d, target = c(years="final", subjects="math", grades="all", weights="n"), control=list(quietly=FALSE, mse_blp_chk=TRUE, jackknife=TRUE))


## now permute the grade, year, and subject labels
dp <- d

dp$pgrade                       <- ""
dp$pgrade[which(dp$grade=="4")] <- "7"
dp$pgrade[which(dp$grade=="5")] <- "6"
dp$pgrade[which(dp$grade=="6")] <- "4"
dp$pgrade[which(dp$grade=="7")] <- "8"
dp$pgrade[which(dp$grade=="8")] <- "5"
dp$grade <- dp$pgrade; dp$pgrade <- NULL

dp$pyear                      <- ""
dp$pyear[which(dp$year=="1")] <- "2"
dp$pyear[which(dp$year=="2")] <- "3"
dp$pyear[which(dp$year=="3")] <- "1"
dp$year <- dp$pyear; dp$pyear <- NULL

dp$subject <- ifelse(dp$subject=="ELA", "math", "ELA")

## fit model to permuted data, changing target appropriately
set.seed(3015)
r7p <- schoolgrowth(dp, target = c(years="1", subjects="ELA", grades="all", weights="n"), control=list(quietly=FALSE, mse_blp_chk=TRUE, jackknife=TRUE))

## comparisons
stopifnot(max(abs( sort(r7$R@x) - sort(r7p$R@x))) < 1e-10)
stopifnot(max(abs( sort(r7$G@x) - sort(r7p$G@x))) < 1e-10)
stopifnot(max(abs(unlist(r7$modstats) - unlist(r7p$modstats))) < 1e-10)
stopifnot(max(sort(r7$varhat_G) - sort(r7p$varhat_G)) < 1e-8)
a  <- r7$aggregated_growth[,-c(1,2)]
ap <- r7p$aggregated_growth[-c(1,2)]
stopifnot(max(abs(na.omit(unlist(a)) - na.omit(unlist(ap)))) < 1e-10)
