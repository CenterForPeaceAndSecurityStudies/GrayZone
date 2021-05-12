## This file replicates Tables 1 and 2 in the online appendix
## Note that the models in Table 2 were estimated using version
## 1.2-3 of the RSiena package. Using different versions of the
## package may necessitate changes in syntax and may produce
## slightly different point estimates and SEs. For best results,
## use v1.2-3 of the package.

library(reshape2)
library(network)
library(xergm)
library(RSiena)
library(gdata)
library(texreg)

setwd("YourDirectory/IO-replication")

## Grab the full dataset, restricted to post-Cold War period
dat <- read.csv("zz.data", sep=",", header=T)
dat <- dat[ dat$year > 1989, ]

## All countries and years in the sample
names <- sort(unique(dat$ccode1))
yr <- sort(unique(dat$year))

############################
## APPENDIX TABLE 1 - TERGMs
############################

## Import DV matrices as tie creation. TERGM package now allows
## matrices to differ in dimensions across time. Grab each
## annual DCA matrix, coerce to matrix, then convert to a
## network object.
for (i in yr) {
    use <- dat[dat$year==i,c("ccode1","ccode2","DCA")]
    net <- acast(use, ccode1~ccode2, value.var="DCA")
    ## Safety checks
    print(dim(net))
    print(isSymmetric(net))
    ## Set missings to zero, if any
    net[is.na(net)] <- 0
    assign(paste("dca.", i, sep=""), network(net,directed=FALSE))
    rm(net,use)
}; rm(i)
dv.new <- lapply(paste("dca.", yr, sep=""), get)
names(dv.new) <- paste("dca.", yr, sep="")
rm(list=paste("dca.", yr, sep=""))

## Do the same for the "in force" version of the DCA measure
for (i in yr) {
    use <- dat[dat$year==i,c("ccode1","ccode2","DCAactual")]
    net <- acast(use, ccode1~ccode2, value.var="DCAactual")
    ## Safety checks
    print(dim(net))
    print(isSymmetric(net))
    ## Set missings to zero, if any
    net[is.na(net)] <- 0
    assign(paste("dca.IF.", i, sep=""), network(net,directed=FALSE))
    rm(net,use)
}; rm(i)
dv.inforce <- lapply(paste("dca.IF.", yr, sep=""), get)
names(dv.inforce) <- paste("dca.IF.", yr, sep="")
rm(list=paste("dca.IF.", yr, sep=""))

## Save each covariate as a list of matrices. Since monadic
## covariates have already been interacted into "dyadic"
## measures, just treat them here as dyadic.
covs <- c("DCA2paths", "DCAdegree", "dPactNonNATO", "NATO",
          "NATOPfP", "sameSideMIDs", "terrorism", "absIdealDiff",
          "trade", "armsMatch", "bothDemocracy", "GDPcap", "CINC",
          "distance", "colony")

for ( k in covs ) {
    print(k)
    for ( i in yr ) {
        ## Grab the data for this year and covariate
        use <- dat[dat$year==i, c("ccode1","ccode2",k)]
        ## Convert to adjacency matrix form
        net <- acast(use, ccode1~ccode2, value.var=k)
	## Safety checks
        print(dim(net))
        print(isSymmetric(net))
        ## TERGM estimator requires no missing values, so
        ## we must set all missings to zero. This almost certainly
        ## biases estimates for some of the covariates, but it appears
        ## to have little effect on the quantities of interest (i.e.,
        ## estimates for pref attachmet and triadic closure).
        diag(net) <- 0
        net[is.na(net)] <- 0
        assign(paste(k, ".", i, sep=""), net)
        rm(use,net)
    }; rm(i)
    out.list <- lapply(paste(k, ".", yr, sep=""), get)
    names(out.list) <- paste(k, ".", yr, sep="")
    assign(paste("iv.", k, sep=""), out.list)
    rm(out.list, list=paste(k, ".", yr, sep=""))
}; rm(k)

## Set the seed so we can replicate exactly the results from
## the online appendix.
set.seed(10)

## First estimate a TERGM version of the model from the main paper
fit1 <- btergm(dv.new ~ edges + edgecov(iv.DCAdegree) + edgecov(iv.DCA2paths) +
                   edgecov(iv.CINC) + edgecov(iv.GDPcap) + edgecov(iv.armsMatch) +
                   edgecov(iv.sameSideMIDs) + edgecov(iv.terrorism) +
                   edgecov(iv.bothDemocracy) + edgecov(iv.absIdealDiff) +
                   edgecov(iv.trade) + edgecov(iv.NATO) + edgecov(iv.NATOPfP) +
                   edgecov(iv.dPactNonNATO) + edgecov(iv.distance) + edgecov(iv.colony) +
                   memory(type="stability"),
               R = 1000, parallel="multicore", ncpus=2)

## A better approach is to look at DCAs in force
fit2 <- btergm(dv.inforce ~ edges + edgecov(iv.DCAdegree) + edgecov(iv.DCA2paths) +
                   edgecov(iv.CINC) + edgecov(iv.GDPcap) + edgecov(iv.armsMatch) +
                   edgecov(iv.sameSideMIDs) + edgecov(iv.terrorism) +
                   edgecov(iv.bothDemocracy) + edgecov(iv.absIdealDiff) +
                   edgecov(iv.trade) + edgecov(iv.NATO) + edgecov(iv.NATOPfP) +
                   edgecov(iv.dPactNonNATO) + edgecov(iv.distance) + edgecov(iv.colony) +
                   memory(type="stability"),
               R = 1000, parallel="multicore", ncpus=2)

## Now let's try the above with a more appropriate combination of endogenous
## netowrk statistics (gwdegree and gwesp)
fit3 <- btergm(dv.inforce ~ edges + gwdegree(0.5) + gwesp(0.5) +
                   edgecov(iv.CINC) + edgecov(iv.GDPcap) + edgecov(iv.armsMatch) +
                   edgecov(iv.sameSideMIDs) + edgecov(iv.terrorism) +
                   edgecov(iv.bothDemocracy) + edgecov(iv.absIdealDiff) +
                   edgecov(iv.trade) + edgecov(iv.NATO) + edgecov(iv.NATOPfP) +
                   edgecov(iv.dPactNonNATO) + edgecov(iv.distance) + edgecov(iv.colony) +
                   memory(type="stability"),
               R = 1000, parallel="multicore", ncpus=2)

## Put TERGM results in a single table
texreg(list(fit1,fit2,fit3),file="output/TERGMs.tex")



##########################
## TABLE 2 - SAOMS 
##########################

## Need to start over completely here, as RSiena package needs network
## matrices in a different format than TERGM package. Also note that
## we'll ONLY be using the "DCA actual" version here, as SAOMs are not
## designed for event-type data.
keep(dat, names, yr, covs, sure=TRUE)

## Build an empty reference matrix, which includes all actors in the
## entire dataset for the 1980--2010 period.
emat <- matrix(NA, length(names), length(names),
               dimnames=list(names,names))

## Need to use the "in force" version, not the tie creation version,
## for SAOMS.
for ( i in yr ) {
    use <- dat[dat$year==i,c("ccode1","ccode2","DCAactual")]
    names(use) <- c("ccode1","ccode2","DCA")
    net <- acast(use, ccode1~ccode2, value.var="DCA")
    ## Safety checks
    print(dim(net))
    print(isSymmetric(net))
    ## Now embed this year's matrix in the ref matrix
    out <- emat
    out[
        rownames(out) %in% rownames(net),
        colnames(out) %in% colnames(net)] <- net
    ## Should be same number of ties in both objects
    print( (sum(out, na.rm=TRUE)) - sum(net) )
    print(sum(is.na(out)))
    print(identical(rownames(out),rownames(emat)))
    print(identical(colnames(out),colnames(emat)))
    ## DON'T CHANGE MISSINGS! Must leave as is.
    assign(paste("dv.", i, sep=""), out)
    rm(net,use,out)
}; rm(i)
dv.siena <- lapply(paste("dv.", yr, sep=""), get)
names(dv.siena) <- paste("dv.", yr, sep="")
rm(list=paste("dv.", yr, sep=""))

## Now convert all covariates into a series of square matrices
for ( j in covs ) {
    for ( i in yr ) {
        use <- dat[dat$year==i,c("ccode1","ccode2",j)]
        names(use) <- c("ccode1","ccode2",j)
        net <- acast(use, ccode1~ccode2, value.var=j)
        ## Safety checks
        print(dim(net))
        print(isSymmetric(net))
        ## Now embed this year's matrix in the ref matrix
        out <- emat
        out[
            rownames(out) %in% rownames(net),
            colnames(out) %in% colnames(net)] <- net
        ## Should be same number of ties in both objects
        print( (sum(out, na.rm=TRUE)) - sum(net) )
        print(sum(is.na(out)))
        print(identical(rownames(out),rownames(emat)))
        print(identical(colnames(out),colnames(emat)))
        ## DON'T CHANGE MISSINGS! Must leave as is.
        assign(paste(j, ".", i, sep=""), out)
        rm(net,use,out)
    }; rm(i)
    out.list <- lapply(paste(j, ".", yr, sep=""), get)
    names(out.list) <- paste(j, ".", yr, sep="")
    assign(paste("iv.", j, sep=""), out.list)
    rm(out.list, list=paste(j, ".", yr, sep=""))
}

## Convert to Rsiena objects
N <- length(names)
thi <- length(dv.siena)
tlo <- length(dv.siena)-1

## Put into RSiena format. First DV.
assign("dcas.net", sienaNet(array(do.call("c", dv.siena), dim=c(N,N,thi))))

## Then covariates (all dyadic). Save colony and distance for constant covariates.
for ( x in paste("iv.", covs, sep="") ) {
    assign(paste(x, ".net", sep=""), varDyadCovar(array(do.call("c", get(x)), dim=c(N,N,tlo))))
}; rm(x)

## Assemble into RSiena object
netdata <- sienaDataCreate(
    dcas.net,
    iv.DCA2paths.net,
    iv.DCAdegree.net,
    iv.CINC.net,
    iv.GDPcap.net,
    iv.armsMatch.net,
    iv.sameSideMIDs.net,
    iv.terrorism.net,
    iv.bothDemocracy.net,
    iv.absIdealDiff.net,
    iv.trade.net,
    iv.NATO.net,
    iv.NATOPfP.net,
    iv.dPactNonNATO.net,
    iv.distance.net,
    iv.colony.net
)

model <- sienaAlgorithmCreate(useStdInits=F, projname="Appendix.SAOMS",
                              nsub=5, n3=3000, seed=10, maxlike=F,
                              modelType=c(dcas.net=3))
eff <- getEffects(netdata)

## Set effects for first model, using "exogenous" network terms
## Need to do some of this manually, due to bugs in some versions of RSiena
eff[ eff$shortName=="transTriads" & eff$type=="eval", ]$include <- FALSE
d.effs <- paste("iv.", covs, ".net", sep="")
for (x in 1:length(d.effs)) {
    eff <- includeEffects(eff, X, interaction1=d.effs[x], include=T, type="eval")
}; rm(x)

## Estimate model
output1 <- siena07(model, data=netdata, effects=eff, batch=TRUE, verbose=FALSE)

## Set effects for endogenous network terms. First turn off
## exogenous terms. Again, need to do this manually, due to bugs
## in some versions of RSiena.
eff[ eff$effectName=="iv.DCA2paths.net" & eff$shortName=="X" & eff$type=="eval", ]$include <- FALSE
eff[ eff$effectName=="iv.DCAdegree.net" & eff$shortName=="X" & eff$type=="eval", ]$include <- FALSE

## Then include equivalent endogenous terms
eff <- includeEffects(eff, transTies, include=TRUE, type="eval")
eff <- includeEffects(eff, inPopSqrt, include=TRUE, type="eval")

## And re-estimate the model
output2 <- siena07(model, data=netdata, effects=eff, batch=TRUE, verbose=FALSE)

## Put both sets of SAOM results in a single table
texreg(list(output1,output2),file="output/SAOMs.tex",stars=0.05)

print("El fin!")
