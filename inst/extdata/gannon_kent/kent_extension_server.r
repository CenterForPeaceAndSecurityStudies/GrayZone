## Import

atop <- readr::read_csv("atop4_01m.csv")
                     
## drop non-aggression pacts
atop <- dplyr::filter(atop, !(nonagg ==1 & consul == 0 & 
    neutral == 0 & offense == 0 & defense == 0))

## Clean data
bc_ext <- dplyr::tibble(
    "atopid" = atop$atopid, 
    "member" = atop$member, 
    "yrent" = atop$yrent, 
    "yrexit" = atop$yrexit, 
    ## Military action conditional war environment
    "macowe" = with(atop, ifelse(
        defcoadv == 1 | defcoloc == 1 | defcocon == 1| 
        defconum == 1| offcoadv == 1| offcoloc == 1 |
        offcocon == 1 | offconum == 1, 
        1, 0
    )),
    ## Military action conditional on non-compliance
    "maconc" = with(atop, ifelse(
        defcodem == 1 | offcodem == 1,
        1, 0
    )),
    ## Military action conditional on non-provocation
    "maconp" = atop$defconpr,
    ## Non-military action conditional on war environment
    "nmacowe" = with(atop, ifelse(
        neucoadv == 1 | neucoloc == 1 | neucocon == 1 | 
        neuconum == 1 | neucoatt == 1 | concoadv == 1 |
        concoloc == 1 | concocon == 1, 
        1, 0 
    )),
    ## Non-military action required only if requested
    "nmaroir" = atop$concoreq,
    ## Other conditions
    "condother" = atop$olimob,
    ## Renunciation allowed
    "renounceok" = with(atop, ifelse(
        renounce == 1,
        1, 0
    )),
    ## Renunciation prohibited
    "renounceproh" = with(atop, ifelse(
        renounce == 2,
        1, 0 
    )),
    ## Renunciation conditional
    "renouncecond" = with(atop, ifelse(
        renounce == 3,
        1, 0 
    )),
    ## Offense alliance
    "offenseatop" = atop$offense,
    ## Defense alliance
    "defenseatop" = atop$defense,
    ## Neutrality pact
    "neutatop" = atop$neutral,
    ## Consultation
    "consultatop" = atop$consul,
    ## Military contact
    "milcontact" = with(atop, ifelse(
        milcon == 2,
        1, 0
    )),
    ## Common defense pact
    "commondef" = with(atop, ifelse(
        milcon == 3,
        1, 0
    )),
    ## Integrated command
    "intcom" = atop$intcom,
    ## Military aid
    "milaid" = atop$milaid,
    ## Military base
    "base" = atop$base,
    ## Specific contribution
    "contrib" = atop$contrib,
    ## Requires the creation of any organization
    "organ1" = atop$organ1,
    ## Economic aid
    "ecaid" = with(atop, ifelse(
        ecaid > 0, 
        1, 0
    )),
    ## Secret pact
    "secrecy" = atop$pubsecr
)


all_alliances <- dplyr::tibble(
    "atopid" = as.integer(),
    "macowe" = as.integer(),
    "maconc" = as.integer(),
    "maconp" = as.integer(),
    "nmacowe" = as.integer(),
    "nmaroir" = as.integer(),
    "condother" = as.integer(),
    "renounceok" = as.integer(),
    "renounceproh" = as.integer(),
    "renouncecond" = as.integer(),
    "offenseatop" = as.integer(),
    "defenseatop" = as.integer(),
    "neutatop" = as.integer(),
    "consultatop" = as.integer(),
    "milcontact" = as.integer(),
    "commondef" = as.integer(),
    "intcom" = as.integer(),
    "milaid" = as.integer(),
    "base" = as.integer(),
    "contrib" = as.integer(),
    "organ1" = as.integer(),
    "ecaid" = as.integer(),
    "secrecy" = as.integer()
)

## First, list of unique alliance
alliances <- unique(bc_ext$atopid)

for(i in 1:length(alliances)){
    ## subset data on all states in alliance
    temp <- dplyr::filter(bc_ext, atop$atopid == alliances[i])
    all_alliances[i, 1] <- temp$atopid[1]
    all_alliances[i, 2] <- sum(temp$macowe)/nrow(temp)
    all_alliances[i, 3] <- sum(temp$maconc)/nrow(temp)
    all_alliances[i, 4] <- sum(temp$maconp)/nrow(temp)
    all_alliances[i, 5] <- sum(temp$nmacowe)/nrow(temp) 
    all_alliances[i, 6] <- sum(temp$nmaroir)/nrow(temp) 
    all_alliances[i, 7] <- sum(temp$condother)/nrow(temp) 
    all_alliances[i, 8] <- sum(temp$renounceok)/nrow(temp) 
    all_alliances[i, 9] <- sum(temp$renounceproh)/nrow(temp) 
    all_alliances[i, 10] <- sum(temp$renouncecond)/nrow(temp) 
    all_alliances[i, 11] <- sum(temp$offenseatop)/nrow(temp) 
    all_alliances[i, 12] <- sum(temp$defenseatop)/nrow(temp)
    all_alliances[i, 13] <- sum(temp$neutatop)/nrow(temp) 
    all_alliances[i, 14] <- sum(temp$consultatop)/nrow(temp)
    all_alliances[i, 15] <- sum(temp$milcontact)/nrow(temp) 
    all_alliances[i, 16] <- sum(temp$commondef)/nrow(temp)
    all_alliances[i, 17] <- sum(temp$intcom)/nrow(temp)
    all_alliances[i, 18] <- sum(temp$milaid)/nrow(temp) 
    all_alliances[i, 19] <- sum(temp$base)/nrow(temp)
    all_alliances[i, 20] <- sum(temp$contrib)/nrow(temp) 
    all_alliances[i, 21] <- sum(temp$organ1)/nrow(temp)
    all_alliances[i, 22] <- sum(temp$ecaid)/nrow(temp) 
    all_alliances[i, 23] <- sum(temp$secrecy)/nrow(temp) 
}

all_alliances <- tidyr::drop_na(all_alliances)

scope <- MCMCpack::MCMCfactanal(
    ~ macowe + maconc + maconp + nmacowe + nmaroir + 
    condother + renounceok + renounceproh + renouncecond + 
    offenseatop + defenseatop + neutatop + consultatop,
    data = all_alliances,
    factors = 1,
    lambda.constraints = list(compel = list(1, "+")),
    std.mean = TRUE, 
    std.var = TRUE,
    verbose = 100000,
    mcmc = 1000000, 
    burnin = 100000, 
    thin = 1000,
    store.scores = TRUE)

factor.load3 <- scope[, 1:26]
apply(factor.load3, 2, mean)
rbd <- scope[ , -seq(1:26)]
scope.score <- apply(rbd, 2, mean)

depth <- MCMCpack::MCMCfactanal(
    ~ milcontact + commondef + intcom + milaid + base +
    contrib + organ1 + ecaid + secrecy,
    data = all_alliances,
    factors = 1,
    lambda.constraints = list(compel = list(1, "+")),
    std.mean = TRUE, 
    std.var = TRUE,
    verbose = 100000,
    mcmc = 1000000, 
    burnin = 100000, 
    thin = 1000,
    store.scores = TRUE)

factor.load2 <- depth[, 1:18]
apply(factor.load2, 2, mean)
rbd <- depth[,-seq(1:18)]
depth.score <- apply(rbd, 2, mean)

AllianceDataScore <- as.data.frame(cbind(all_alliances$atopid,
    depth.score, scope.score))

AllianceDataScore <- round(AllianceDataScore, digits = 3)

write.csv(AllianceDataScore, file = "AllianceDataScore_Kent.csv")