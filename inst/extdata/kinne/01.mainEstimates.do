cd "YourDirectory/IO-replication"
capture mkdir "output"
import delimited zz.data, case(preserve) encoding(ISO-8859-1) clear

/* Since DCAs are nondirected, dataset must also be nondirected. All dyadic
variables used in the below analyses are symmetrized. Note that some of the
dyadic variables used in the R code are NOT symmetrized. */
keep if ccode1 < ccode2
sort dyadid year
tsset dyadid year, yearly

/* Generate detrended and temporally lagged measures for the FE and pooled models */
foreach var of varlist DCA2paths DCAdegree dPactNonNATO NATO NATOPfP sameSideMIDs terrorism absIdealDiff trade armsMatch bothDemocracy GDPcap CINC {
	quietly areg `var' year, absorb(dyadid)
	predict r_`var', r
	gen lr_`var' = l.r_`var'
	label var lr_`var' "One year lag of `var' residual"
	drop r_`var'
	gen l_`var' = l.`var'
	label var l_`var' "One year lag of `var'"
}

/* Generate additional lagged measures, not detrended, for the pooled models */
foreach var of varlist colony distance ambassador IGOs instIGOs ATOP {
	gen l_`var' = l.`var'
	label var l_`var' "One year lag of `var'"
}

/*
Estimate some models, both pooled and with FEs
*/

/* Specify macros for control variables */
local pooled "l_dPactNonNATO l_NATO l_NATOPfP l_colony l_distance l_armsMatch l_absIdealDiff l_trade l_sameSideMIDs l_terrorism l_bothDemocracy l_CINC l_GDPcap"
local fixed "lr_dPactNonNATO lr_NATO lr_NATOPfP lr_armsMatch lr_absIdealDiff lr_trade lr_sameSideMIDs lr_terrorism lr_bothDemocracy lr_CINC lr_GDPcap"

/* Estimate five models */
eststo clear

eststo: logit DCA l_DCA2paths l_DCAdegree `pooled' if year > 1979, vce(robust) /* pooled for full period */

eststo: logit DCA l_DCA2paths l_DCAdegree `pooled' if (newDeal==1 | noDeals==1) & year > 1989, vce(robust) /* pooled, new DCAs only, post Cold War */
estimates store pool1

eststo: logit DCA l_DCA2paths l_DCAdegree `pooled' if postDeal==1 & year > 1989, vce(robust) /* pooled, subsequent DCAs only */
estimates store pool2

eststo: xtlogit DCA lr_DCA2paths lr_DCAdegree `fixed' if newDeal==1 & year > 1989, fe /* with FEs, new DCAs only */
gen mark_fixed_new=1 if e(sample) /* Mark this sample. We'll use it later. */
estimates store fe1

eststo: xtlogit  DCA lr_DCA2paths lr_DCAdegree `fixed' if postDeal==1 & year > 1989, fe /* with FEs, subsequent DCAs only */
estimates store fe2

esttab using output/estimates.csv, se replace nogaps r2 ar2 pr2 aic bic sca(N_g N_clust ll rmse) plain wide
eststo clear



/*
Estimate marginal effects
*/

/* First compare the post-Cold War pooled model to a 1980s-only model. Drop NATO-PfP
since PfP didn't exist in 1980s. */
local pooled "l_dPactNonNATO l_NATO l_colony l_distance l_armsMatch l_absIdealDiff l_trade l_sameSideMIDs l_terrorism l_bothDemocracy l_CINC l_GDPcap"
logit DCA l_DCA2paths l_DCAdegree `pooled' if (newDeal==1 | noDeals==1) & year > 1979 & year < 1990, vce(robust)
quietly sum l_DCA2paths if e(sample)
local incs1 = (`r(max)' - `r(min)')/20
margins if e(sample), at(l_DCA2paths=(`r(min)'(`incs1')`r(max)')) atmeans predict(pr)
matrix b1 = r(b)'
local nam = "margins_new_2paths"
svmat b1, names("`nam'") /* Grab estimated margins */
matrix b1se = vecdiag(r(V))'
svmat b1se, names("`nam'_SEs")
replace `nam'_SEs = sqrt(`nam'_SEs)

quietly sum l_DCAdegree if e(sample)
local incs1 = (`r(max)' - `r(min)')/20
margins if e(sample), at(l_DCAdegree=(`r(min)'(`incs1')`r(max)')) atmeans predict(pr)
matrix b1 = r(b)'
local nam = "margins_new_degree"
svmat b1, names("`nam'") /* Grab estimated margins */
matrix b1se = vecdiag(r(V))'
svmat b1se, names("`nam'_SEs")
replace `nam'_SEs = sqrt(`nam'_SEs)

/* Then estimate marginal effects for post-1989 period */
estimates restore pool1
quietly sum l_DCA2paths if e(sample)
local incs1 = (`r(max)' - `r(min)')/20
margins if e(sample), at(l_DCA2paths=(`r(min)'(`incs1')`r(max)')) atmeans predict(pr)
matrix b1 = r(b)'
local nam = "margins_new_2paths_1980s"
svmat b1, names("`nam'") /* Grab estimated margins */
matrix b1se = vecdiag(r(V))'
svmat b1se, names("`nam'_SEs")
replace `nam'_SEs = sqrt(`nam'_SEs)

quietly sum l_DCAdegree if e(sample)
local incs1 = (`r(max)' - `r(min)')/20
margins if e(sample), at(l_DCAdegree=(`r(min)'(`incs1')`r(max)')) atmeans predict(pr)
matrix b1 = r(b)'
local nam = "margins_new_degree_1980s"
svmat b1, names("`nam'") /* Grab estimated margins */
matrix b1se = vecdiag(r(V))'
svmat b1se, names("`nam'_SEs")
replace `nam'_SEs = sqrt(`nam'_SEs)

/* Export the marginal predictions */
preserve
keep margins_*
drop if margins_new_2paths1==.
outsheet using output/PredictProbsColdWar.dat, replace
restore

drop margins_*

/* Next do the new ties FE model and derive margins for network effects */
estimates restore fe1
quietly sum lr_DCA2paths if e(sample)
local incs1 = (`r(max)' - `r(min)')/20
margins if e(sample), at(lr_DCA2paths=(`r(min)'(`incs1')`r(max)')) atmeans predict(pu0)
matrix b1 = r(b)'
local nam = "margins_new_2paths"
svmat b1, names("`nam'") /* Grab estimated margins */
matrix b1se = vecdiag(r(V))'
svmat b1se, names("`nam'_SEs")
replace `nam'_SEs = sqrt(`nam'_SEs)

quietly sum lr_DCAdegree if e(sample)
local incs3 = (`r(max)' - `r(min)')/20
margins if e(sample), at(lr_DCAdegree=(`r(min)'(`incs3')`r(max)')) atmeans predict(pu0)
matrix b3 = r(b)'
local nam = "margins_new_degree"
svmat b3, names("`nam'") /* Grab estimated margins */
matrix b3se = vecdiag(r(V))'
svmat b3se, names("`nam'_SEs")
replace `nam'_SEs = sqrt(`nam'_SEs)

/* Then do repeated ties FE model */
estimates restore fe2
quietly sum lr_DCA2paths if e(sample)
local incs2 = (`r(max)' - `r(min)')/20
margins if e(sample), at(lr_DCA2paths=(`r(min)'(`incs2')`r(max)')) atmeans predict(pu0)
matrix b2 = r(b)'
local nam = "margins_old_2paths"
svmat b2, names("`nam'") /* Grab estimated margins */
matrix b2se = vecdiag(r(V))'
svmat b2se, names("`nam'_SEs")
replace `nam'_SEs = sqrt(`nam'_SEs)

quietly sum lr_DCAdegree if e(sample)
local incs4 = (`r(max)' - `r(min)')/20
margins if e(sample), at(lr_DCAdegree=(`r(min)'(`incs4')`r(max)')) atmeans predict(pu0)
matrix b4 = r(b)'
local nam = "margins_old_degree"
svmat b4, names("`nam'") /* Grab estimated margins */
matrix b4se = vecdiag(r(V))'
svmat b4se, names("`nam'_SEs")
replace `nam'_SEs = sqrt(`nam'_SEs)

/* Export the marginal predictions */
preserve
keep margins_*
drop if margins_new_2paths1==.
outsheet using output/PredictProbs.dat, replace
restore

drop margins_*



/*
Do some out of sample predictions
*/

preserve
keep if mark_fixed_new==1 /* Keep only observations from new ties FE model */

/* Repeatedly sample dyads, specify training set, estimate model, then test on validation set */
local fixed "lr_dPactNonNATO lr_NATO lr_NATOPfP lr_armsMatch lr_absIdealDiff lr_trade lr_sameSideMIDs lr_terrorism lr_bothDemocracy lr_CINC lr_GDPcap"
forvalues i = 1(1)10 {
	gen long order = _n
	egen select = tag(dyadid)
	gen rnd = runiform()
	sort select rnd 
	replace select = _n > (_N - 325) /* Keep about half the dyads */
	bysort dyadid (select): replace select = select[_N]
	sort order
	drop order rnd
	/* Estimate model on the selected sample of dyads */
	quietly xtlogit DCA lr_DCA2paths lr_DCAdegree `fixed' if newDeal==1 & year > 1989 & select==1, fe
	/* Predict outcome on the excluded sample */
	predictnl prNets`i' = predict() if select==0, se(prNets`i'_se)
	/* Do the same without network effects */
	quietly xtlogit DCA `fixed' if newDeal==1 & year > 1989 & select==1, fe
	/* And again predict on excluded sample */
	predictnl prNoNets`i' = predict() if select==0, se(prNoNets`i'_se)
	drop select
}

keep ccode1 ccode2 abbrev1 abbrev2 year dyadid DCA prN*
sort dyadid year
export delimited using output/PredictOOS-FE.csv, replace

restore



/*
Testable implications, using a variety of interaction terms
*/

eststo clear
local pooled "l_dPactNonNATO l_NATO l_NATOPfP l_colony l_distance l_armsMatch l_absIdealDiff l_trade l_sameSideMIDs l_terrorism l_bothDemocracy l_CINC l_GDPcap"

/* First IGO stuff */
eststo: logit DCA c.l_DCA2paths##c.l_IGOs l_DCAdegree `pooled' if year > 1989, vce(r)
quietly sum l_DCA2paths if e(sample)
local inc1 = (`r(max)' - `r(min)')/10
local min1 = `r(min)'
local max1 = `r(max)'
quietly centile l_IGOs if e(sample), centile(10 50 90)
margins if e(sample), at(l_DCA2paths=(`min1'(`inc1')`max1') l_IGOs=(`r(c_1)' `r(c_2)' `r(c_3)')) predict(pr)
matrix b1 = r(b)'
local nam = "margins_IIGOS2path"
svmat b1, names("`nam'") /* Grab estimated margins */
matrix b1se = vecdiag(r(V))'
svmat b1se, names("`nam'_SEs")
replace `nam'_SEs = sqrt(`nam'_SEs)

eststo: logit DCA l_DCA2paths c.l_DCAdegree##c.l_IGOs `pooled' if year > 1989, vce(r)
quietly sum l_DCAdegree if e(sample)
local inc1 = (`r(max)' - `r(min)')/10
local min1 = `r(min)'
local max1 = `r(max)'
quietly centile l_IGOs if e(sample), centile(10 50 90)
margins if e(sample), at(l_DCAdegree=(`min1'(`inc1')`max1') l_IGOs=(`r(c_1)' `r(c_2)' `r(c_3)')) predict(pr)
matrix b1 = r(b)'
local nam = "margins_IIGOSdegree"
svmat b1, names("`nam'") /* Grab estimated margins */
matrix b1se = vecdiag(r(V))'
svmat b1se, names("`nam'_SEs")
replace `nam'_SEs = sqrt(`nam'_SEs)

/* Then highly institutionalized IGOs */
eststo: logit DCA c.l_DCA2paths##c.l_instIGOs l_DCAdegree `pooled' if year > 1989, vce(r)
quietly sum l_DCA2paths if e(sample)
local inc1 = (`r(max)' - `r(min)')/10
local min1 = `r(min)'
local max1 = `r(max)'
quietly centile l_instIGOs if e(sample), centile(10 50 90)
margins if e(sample), at(l_DCA2paths=(`min1'(`inc1')`max1') l_instIGOs=(`r(c_1)' `r(c_2)' `r(c_3)')) predict(pr)
matrix b1 = r(b)'
local nam = "margins_IINST2path"
svmat b1, names("`nam'") /* Grab estimated margins */
matrix b1se = vecdiag(r(V))'
svmat b1se, names("`nam'_SEs")
replace `nam'_SEs = sqrt(`nam'_SEs)

eststo: logit DCA l_DCA2paths c.l_DCAdegree##c.l_instIGOs `pooled' if year > 1989, vce(r)
quietly sum l_DCAdegree if e(sample)
local inc1 = (`r(max)' - `r(min)')/10
local min1 = `r(min)'
local max1 = `r(max)'
quietly centile l_instIGOs if e(sample), centile(10 50 90)
margins if e(sample), at(l_DCAdegree=(`min1'(`inc1')`max1') l_instIGOs=(`r(c_1)' `r(c_2)' `r(c_3)')) predict(pr)
matrix b1 = r(b)'
local nam = "margins_IINSTdegree"
svmat b1, names("`nam'") /* Grab estimated margins */
matrix b1se = vecdiag(r(V))'
svmat b1se, names("`nam'_SEs")
replace `nam'_SEs = sqrt(`nam'_SEs)

local pooled "l_dPactNonNATO l_NATO l_NATOPfP l_colony l_distance l_armsMatch l_absIdealDiff l_trade l_sameSideMIDs l_terrorism l_bothDemocracy l_CINC l_GDPcap"

/* Then ambassadors */
eststo: logit DCA c.l_DCA2paths##i.l_ambassador l_DCAdegree `pooled' if year > 1989, vce(r)
quietly sum l_DCA2paths if e(sample)
local inc1 = (`r(max)' - `r(min)')/10
local min1 = `r(min)'
local max1 = `r(max)'
margins if e(sample), at(l_DCA2paths=(`min1'(`inc1')`max1') l_ambassador=(0 1)) predict(pr)
matrix b1 = r(b)'
local nam = "margins_AMBASS2path"
svmat b1, names("`nam'") /* Grab estimated margins */
matrix b1se = vecdiag(r(V))'
svmat b1se, names("`nam'_SEs")
replace `nam'_SEs = sqrt(`nam'_SEs)

eststo: logit DCA l_DCA2paths c.l_DCAdegree##i.l_ambassador `pooled' if year > 1989, vce(r)
quietly sum l_DCAdegree if e(sample)
local inc1 = (`r(max)' - `r(min)')/10
local min1 = `r(min)'
local max1 = `r(max)'
margins if e(sample), at(l_DCAdegree=(`min1'(`inc1')`max1') l_ambassador=(0 1)) predict(pr)
matrix b1 = r(b)'
local nam = "margins_AMBASSdegree"
svmat b1, names("`nam'") /* Grab estimated margins */
matrix b1se = vecdiag(r(V))'
svmat b1se, names("`nam'_SEs")
replace `nam'_SEs = sqrt(`nam'_SEs)

/* Then institutionalized alliances, using ATOP criteria */
eststo: logit DCA c.l_DCA2paths##i.l_ATOP l_DCAdegree l_NATO l_NATOPfP l_colony l_distance l_absIdealDiff l_trade l_sameSideMIDs l_terrorism l_bothDemocracy l_CINC l_GDPcap if year > 1989, vce(r)
quietly sum l_DCA2paths if e(sample)
local inc1 = (`r(max)' - `r(min)')/10
local min1 = `r(min)'
local max1 = `r(max)'
margins if e(sample), at(l_DCA2paths=(`min1'(`inc1')`max1') l_ATOP=(0 1)) predict(pr)
matrix b1 = r(b)'
local nam = "margins_ATOP2path"
svmat b1, names("`nam'") /* Grab estimated margins */
matrix b1se = vecdiag(r(V))'
svmat b1se, names("`nam'_SEs")
replace `nam'_SEs = sqrt(`nam'_SEs)

eststo: logit DCA l_DCA2paths c.l_DCAdegree##i.l_ATOP l_NATO l_NATOPfP l_colony l_distance l_absIdealDiff l_trade l_sameSideMIDs l_terrorism l_bothDemocracy l_CINC l_GDPcap if year > 1989, vce(r)
quietly sum l_DCAdegree if e(sample)
local inc1 = (`r(max)' - `r(min)')/10
local min1 = `r(min)'
local max1 = `r(max)'
margins if e(sample), at(l_DCAdegree=(`min1'(`inc1')`max1') l_ATOP=(0 1)) predict(pr)
matrix b1 = r(b)'
local nam = "margins_ATOPdegree"
svmat b1, names("`nam'") /* Grab estimated margins */
matrix b1se = vecdiag(r(V))'
svmat b1se, names("`nam'_SEs")
replace `nam'_SEs = sqrt(`nam'_SEs)

esttab using output/implications.csv, se replace nogaps r2 ar2 pr2 aic bic sca(N_g N_clust ll rmse) plain wide
eststo clear

preserve
keep margins_*
drop if margins_IIGOS2path1 == .
export delimited using output/MarginsPooledLogit.csv, replace
restore

/* Now go to R file for plots and more post-regression analysis */
