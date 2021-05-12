/* This script replicates Tables 3-6 in the online appendix */
cd "YourDirectory/IO-replication"

import delimited zz.data, case(preserve) encoding(ISO-8859-1) clear

/* Make undirected and tsset */
keep if ccode1 < ccode2
sort dyadid year
tsset dyadid year, yearly

/* Generate detrended and temporally lagged measures, only for FE models */
foreach var of varlist DCA2paths DCAdegree dPactNonNATO NATO NATOPfP sameSideMIDs terrorism absIdealDiff trade armsMatch bothDemocracy GDPcap CINC {
	quietly areg `var' year, absorb(dyadid)
	predict r_`var', r
	gen lr_`var' = l.r_`var'
	label var lr_`var' "One year lag of `var' residual"
	drop r_`var'
}

/* Detrend the alternative network measures */
foreach var of varlist lnDegree1year ln2paths1year lnDegree4year ln2paths4year lnDegree9year ln2paths9year lnDegreeActual ln2pathsActual lnDegreeGeneral ln2pathsGeneral {
	quietly areg `var' year, absorb(dyadid)
	predict r_`var', r
	gen lr_`var' = l.r_`var'
	label var lr_`var' "One year lag of `var' residual"
	drop r_`var'
}

/* ~~~~~~~~~~ * ~~~~~~~~~~ * ~~~~~~~~~~ * ~~~~~~~~~~ * ~~~~~~~~~~ */
/* TABLE 3 FROM ONLINE APPENDIX                                   */
/* ~~~~~~~~~~ * ~~~~~~~~~~ * ~~~~~~~~~~ * ~~~~~~~~~~ * ~~~~~~~~~~ */

/* Set local macro for control variables */
local fixed "lr_dPactNonNATO lr_NATO lr_NATOPfP lr_armsMatch lr_absIdealDiff lr_trade lr_sameSideMIDs lr_terrorism lr_bothDemocracy lr_CINC lr_GDPcap"

/* Estimate a series of models */
xtlogit DCA lr_ln2paths1year lr_lnDegree1year `fixed' if newDeal==1 & year > 1989, fe /* with FEs, new DCAs only */
xtlogit DCA lr_ln2paths1year lr_lnDegree1year `fixed' if postDeal==1 & year > 1989, fe /* with FEs, subsequent DCAs only */

xtlogit DCA lr_ln2paths4year lr_lnDegree4year `fixed' if newDeal==1 & year > 1989, fe /* with FEs, new DCAs only */
xtlogit DCA lr_ln2paths4year lr_lnDegree4year `fixed' if postDeal==1 & year > 1989, fe /* with FEs, subsequent DCAs only */

xtlogit DCA lr_ln2paths9year lr_lnDegree9year `fixed' if newDeal==1 & year > 1989, fe /* with FEs, new DCAs only */
xtlogit DCA lr_ln2paths9year lr_lnDegree9year `fixed' if postDeal==1 & year > 1989, fe /* with FEs, subsequent DCAs only */

xtlogit DCA lr_ln2pathsActual lr_lnDegreeActual `fixed' if newDeal==1 & year > 1989, fe /* with FEs, new DCAs only */
xtlogit DCA lr_ln2pathsActual lr_lnDegreeActual `fixed' if postDeal==1 & year > 1989, fe /* with FEs, subsequent DCAs only */

xtlogit DCA_General lr_ln2pathsGeneral lr_lnDegreeGeneral `fixed' if NewDeal_Gen==1 & year > 1989, fe /* with FEs, new DCAs only */
xtlogit DCA_General lr_ln2pathsGeneral lr_lnDegreeGeneral `fixed' if PostDeal_Gen==1 & year > 1989, fe /* with FEs, subsequent DCAs only */


/* ~~~~~~~~~~ * ~~~~~~~~~~ * ~~~~~~~~~~ * ~~~~~~~~~~ * ~~~~~~~~~~ */
/* TABLE 4 FROM ONLINE APPENDIX 								  */
/* ~~~~~~~~~~ * ~~~~~~~~~~ * ~~~~~~~~~~ * ~~~~~~~~~~ * ~~~~~~~~~~ */

/* First drop the detrended measures we've used so far */
drop lr_*

/* Set local macro again */
local fixed "lr_dPactNonNATO lr_NATO lr_NATOPfP lr_armsMatch lr_absIdealDiff lr_trade lr_sameSideMIDs lr_terrorism lr_bothDemocracy lr_CINC lr_GDPcap"

/* First approach is to not detrend at all, just lag */
foreach var of varlist DCA2paths DCAdegree dPactNonNATO NATO NATOPfP sameSideMIDs terrorism absIdealDiff trade armsMatch bothDemocracy GDPcap CINC {
	gen lr_`var' = l.`var'
}
xtlogit DCA lr_DCA2paths lr_DCAdegree `fixed' if newDeal==1 & year > 1989, fe /* with FEs, new DCAs only */
xtlogit  DCA lr_DCA2paths lr_DCAdegree `fixed' if postDeal==1 & year > 1989, fe /* with FEs, subsequent DCAs only */
drop lr_*

/* Now the same version used in the main paper (i.e. linear) */
foreach var of varlist DCA2paths DCAdegree dPactNonNATO NATO NATOPfP sameSideMIDs terrorism absIdealDiff trade armsMatch bothDemocracy GDPcap CINC {
	quietly areg `var' year, absorb(dyadid)
	predict r_`var', r
	gen lr_`var' = l.r_`var'
	label var lr_`var' "One year lag of `var' residual"
	drop r_`var'
}
xtlogit DCA lr_DCA2paths lr_DCAdegree `fixed' if newDeal==1 & year > 1989, fe /* with FEs, new DCAs only */
xtlogit  DCA lr_DCA2paths lr_DCAdegree `fixed' if postDeal==1 & year > 1989, fe /* with FEs, subsequent DCAs only */
drop lr_*

/* Do a quadratic time trend */
gen yearSQ = year ^ 2
foreach var of varlist DCA2paths DCAdegree dPactNonNATO NATO NATOPfP sameSideMIDs terrorism absIdealDiff trade armsMatch bothDemocracy GDPcap CINC {
	quietly areg `var' year yearSQ, absorb(dyadid)
	predict r_`var', r
	gen lr_`var' = l.r_`var'
	drop r_`var'
}
xtlogit DCA lr_DCA2paths lr_DCAdegree `fixed' if newDeal==1 & year > 1989, fe /* with FEs, new DCAs only */
xtlogit  DCA lr_DCA2paths lr_DCAdegree `fixed' if postDeal==1 & year > 1989, fe /* with FEs, subsequent DCAs only */
drop yearSQ lr_*

/* A logarithmic time trend */
gen yearLOG = log(year)
foreach var of varlist DCA2paths DCAdegree dPactNonNATO NATO NATOPfP sameSideMIDs terrorism absIdealDiff trade armsMatch bothDemocracy GDPcap CINC {
	quietly areg `var' year yearLOG, absorb(dyadid)
	predict r_`var', r
	gen lr_`var' = l.r_`var'
	drop r_`var'
}
xtlogit DCA lr_DCA2paths lr_DCAdegree `fixed' if newDeal==1 & year > 1989, fe /* with FEs, new DCAs only */
xtlogit  DCA lr_DCA2paths lr_DCAdegree `fixed' if postDeal==1 & year > 1989, fe /* with FEs, subsequent DCAs only */
drop yearLOG lr_*

/* An exponential time trend */
gen yearEXP = exp(year - 1989)
foreach var of varlist DCA2paths DCAdegree dPactNonNATO NATO NATOPfP sameSideMIDs terrorism absIdealDiff trade armsMatch bothDemocracy GDPcap CINC {
	quietly areg `var' year yearEXP, absorb(dyadid)
	predict r_`var', r
	gen lr_`var' = l.r_`var'
	drop r_`var'
}
xtlogit DCA lr_DCA2paths lr_DCAdegree `fixed' if newDeal==1 & year > 1989, fe /* with FEs, new DCAs only */
xtlogit  DCA lr_DCA2paths lr_DCAdegree `fixed' if postDeal==1 & year > 1989, fe /* with FEs, subsequent DCAs only */
drop yearEXP lr_*


/* ~~~~~~~~~~ * ~~~~~~~~~~ * ~~~~~~~~~~ * ~~~~~~~~~~ * ~~~~~~~~~~ */
/* TABLE 5 FROM ONLINE APPENDIX */
/* ~~~~~~~~~~ * ~~~~~~~~~~ * ~~~~~~~~~~ * ~~~~~~~~~~ * ~~~~~~~~~~ */

/* Detrend everything again, including additional control variables for this table */
foreach var of varlist DCA2paths DCA2pathsDum DCAdegree lnDegreeAlt dPactNonNATO NATO NATOPfP sameSideMIDs terrorism absIdealDiff trade armsMatch bothDemocracy GDPcap CINC lnIGOs totalBilatArmsLOG ContArmsMatch {
	quietly areg `var' year, absorb(dyadid)
	predict r_`var', r
	gen lr_`var' = l.r_`var'
	label var lr_`var' "One year lag of `var' residual"
	drop r_`var'
}

/* Set local macro again */
local fixed "lr_dPactNonNATO lr_NATO lr_NATOPfP lr_armsMatch lr_absIdealDiff lr_trade lr_sameSideMIDs lr_terrorism lr_bothDemocracy lr_CINC lr_GDPcap"

/* First with version of degree where triads aren't subtracted */
xtlogit DCA lr_lnDegreeAlt lr_DCA2paths `fixed' if newDeal==1 & year > 1989, fe /* with FEs, new DCAs only */
xtlogit DCA lr_lnDegreeAlt lr_DCA2paths  `fixed' if postDeal==1 & year > 1989, fe /* with FEs, subsequent DCAs only */

/* Now with dummy version of triadic closure */
xtlogit DCA lr_DCA2pathsDum lr_DCAdegree `fixed' if newDeal==1 & year > 1989, fe /* with FEs, new DCAs only */
xtlogit DCA lr_DCA2pathsDum lr_DCAdegree `fixed' if postDeal==1 & year > 1989, fe /* with FEs, subsequent DCAs only */

/* Now arms trade stuff */
local fixed2 "lr_dPactNonNATO lr_NATO lr_NATOPfP lr_absIdealDiff lr_trade lr_sameSideMIDs lr_terrorism lr_bothDemocracy lr_CINC lr_GDPcap"
foreach var of varlist lr_ContArmsMatch lr_totalBilatArmsLOG {
	xtlogit DCA `var' lr_DCA2paths lr_DCAdegree `fixed2' if newDeal==1 & year > 1989, fe /* with FEs, new DCAs only */
	xtlogit DCA `var' lr_DCA2paths lr_DCAdegree `fixed2' if postDeal==1 & year > 1989, fe /* with FEs, subsequent DCAs only */
}

/* Throw IGOs into the mix */
xtlogit DCA lr_lnIGOs lr_DCA2paths lr_DCAdegree `fixed' if newDeal==1 & year > 1989, fe /* with FEs, new DCAs only */
xtlogit DCA lr_lnIGOs lr_DCA2paths lr_DCAdegree `fixed' if postDeal==1 & year > 1989, fe /* with FEs, subsequent DCAs only */


/* ~~~~~~~~~~ * ~~~~~~~~~~ * ~~~~~~~~~~ * ~~~~~~~~~~ * ~~~~~~~~~~ */
/* TABLE 6 FROM ONLINE APPENDIX */
/* ~~~~~~~~~~ * ~~~~~~~~~~ * ~~~~~~~~~~ * ~~~~~~~~~~ * ~~~~~~~~~~ */

/* Detrend some additional measures */
foreach var of varlist lnmilex_mean lnmilex_diff lnmilper_mean lnmilper_diff {
	quietly areg `var' year, absorb(dyadid)
	predict r_`var', r
	gen lr_`var' = l.r_`var'
	label var lr_`var' "One year lag of `var' residual"
	drop r_`var'
}

/* Local macro for controls */
local fixed3 "lr_dPactNonNATO lr_NATO lr_NATOPfP lr_armsMatch lr_absIdealDiff lr_trade lr_sameSideMIDs lr_terrorism lr_bothDemocracy lr_GDPcap"

foreach var of varlist lr_lnmilex_mean lr_lnmilex_diff lr_lnmilper_mean lr_lnmilper_diff {
	xtlogit DCA `var' lr_DCA2paths lr_DCAdegree `fixed3' if newDeal==1 & year > 1989, fe /* with FEs, new DCAs only */
	xtlogit DCA `var' lr_DCA2paths lr_DCAdegree `fixed3' if postDeal==1 & year > 1989, fe /* with FEs, subsequent DCAs only */
}

/* And a "rule of three" version */
xtlogit DCA lr_DCA2paths lr_DCAdegree if newDeal==1 & year > 1989, fe /* with FEs, new DCAs only */
xtlogit DCA lr_DCA2paths lr_DCAdegree if postDeal==1 & year > 1989, fe /* with FEs, subsequent DCAs only */

clear

display "El fin!"

