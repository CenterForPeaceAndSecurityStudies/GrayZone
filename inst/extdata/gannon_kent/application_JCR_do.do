clear
use "..../application_JCR_data.dta"

/*MODEL1*/
reg scope affdiv diffcap depth startyear startyear2, robust
/*MODEL2*/
reg scope diffcap depth startyear startyear2 if startyear<1946, robust
/*MODEL3*/	
reg depth affdiverge dem majpow_dum cap scope startyear startyear2, robust

