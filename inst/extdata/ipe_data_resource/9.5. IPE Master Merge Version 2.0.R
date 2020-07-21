########################
#Master Merge for 
#IPE Data Resource
#Version 2.0
#By: Jacob Tucker
#Updated: June 20, 2017 JT
#Updated: July 7, 2017 GX
#Updated: July 7, 2017 SW
#########################
library(foreign)
library(readstata13)

# Disclaimer: #####
# Please set and run your the 'set file paths' file before running this script. This script won't work without it!

# First, get all the RDATA and STATA files from the prepped data folder #####
files.rdata <- list.files(path=preppeddata, pattern="*.RDATA",full.names=T,recursive=FALSE)
files.dta <- list.files(path=preppeddata,pattern="*.dta",full.names=T,recursive=FALSE)

# Check how many files are in the folder. One file is CountryIDsMaster_windows.RDATA #####
length(files.rdata)  # 46 R files
length(files.dta)  #34 dta files

# Now, loop through all of the files and merge them together.#####

# load in first dataset in folder #####
ipe = load(files.rdata[1])

start = 0
# Check to make sure the first file isn't the country ids file #####
if (strsplit(files.rdata[1],"//")[[1]][2]=="CountryIDsMaster_windows.RDATA") {
  #If it is, we'll import the second file as the base
  ipe = load(files.rdata[2])
  load(files.rdata[2])
  start = 3
} else {
  load(files.rdata[1])
  start = 2
}

ipe = get(ipe)

# Start the merge process with the RDATA files #####
for (i in start:length(files.rdata)) {
  # load in files
  newdata = load(files.rdata[i])
  newdata = get(newdata)
  
  
  
  # this prints out the number and the name of the dataset being looped in
  print(i)
  print(files.rdata[i])
  
  
  
  # the actual merge
  ipe = merge(ipe,newdata,all.x=T,all.y=T,by=c("gwno","year"))
  
  # dropping duplicated columns created from the merge process
  ipe$country <- NULL
  ipe$ccode <- NULL
  ipe$ifs <- NULL
  ipe$ifscode <- NULL
  ipe$gwabbrev <- NULL
  
  ipe$country.x <- NULL
  ipe$ccode.x <- NULL
  ipe$ifs.x <- NULL
  ipe$ifscode.x <- NULL
  ipe$gwabbrev.x <- NULL
  
  ipe$country.y <- NULL
  ipe$ccode.y <- NULL
  ipe$ifs.y <- NULL
  ipe$ifscode.y <- NULL
  ipe$gwabbrev.y <- NULL
  
  print(colnames(ipe))
  # browser()
}

# Now merge in the STATA files #####
for (i in 1:length(files.dta)) {
  # this prints out the number and the name of the dataset being looped in
  print(i)
  print(files.dta[i])
  
  # load in the file
  newdata = read.dta13(files.dta[i], nonint.factors =  TRUE, generate.factors =  TRUE)
  # browser()
  
  # merge in the dataset
  ipe = merge(ipe, newdata, all = TRUE, by=c("gwno","year"))
  
  # dropping duplicated columns
  ipe$country <- NULL
  ipe$ccode <- NULL
  ipe$ifs <- NULL
  ipe$ifscode <- NULL
  ipe$gwabbrev <- NULL
  
  ipe$country.x <- NULL
  ipe$ccode.x <- NULL
  ipe$ifs.x <- NULL
  ipe$ifscode.x <- NULL
  ipe$gwabbrev.x <- NULL
  
  ipe$country.y <- NULL
  ipe$ccode.y <- NULL
  ipe$ifs.y <- NULL
  ipe$ifscode.y <- NULL
  ipe$gwabbrev.y <- NULL
}


# Standardize Country Names by GWNO  ####

# load in reference table
load(paste(rawdata, "Standard_gwno_country_ifs.RDATA", sep = ""))

# Create new columns 
ipe$ifs <- NA
ipe$ifscode <- NA
ipe$ccode <- NA
ipe$gwabbrev <- NA

# Change all the country names to standard ones, this will print the before and after of the countrynames
for(m in 1:nrow(ipe)){
  for(n in 1:nrow(gwnoCountry)){
    if(ipe$gwno[m] == gwnoCountry$gwno[n]){
      print(paste("Before: ", ipe$country[m]))
      ipe$country[m] = gwnoCountry$countryName[n]
      ipe$ifs[m] = gwnoCountry$ifs[n]
      ipe$ifscode[m] = gwnoCountry$ifscode[n]
      ipe$ccode[m] = gwnoCountry$ccode[n]
      ipe$gwabbrev[m] = gwnoCountry$gwabbrev[n]
      
      print(paste("After: ", ipe$country[m]))
      print(paste(m, ipe$ifs[m]))
    }
  }
}

# Remove Raw Country Names #####
ipe$countryname_raw_AP <- NULL
ipe$countryname_raw_BH <- NULL
ipe$countryname_raw_BL <- NULL
ipe$countryname_raw_BTI <- NULL
ipe$countryname_raw_BX <- NULL
ipe$countryname_raw_BZ <- NULL
ipe$countryname_raw_CE <- NULL
ipe$countryname_raw_DE <- NULL
ipe$countryname_raw_EF <- NULL
ipe$countryname_raw_ES <- NULL
ipe$countryname_raw_EP <- NULL
ipe$countryname_raw_FHP <- NULL
ipe$countryname_raw_FH.x <- NULL
ipe$countryname_raw_FH.y <- NULL
ipe$countryname_raw_FP <- NULL
ipe$countryname_raw_ILO <- NULL
ipe$countryname_raw_LANG <- NULL
ipe$countryname_raw_LI <- NULL
ipe$countryname_raw_MC <- NULL
ipe$countryname_raw_MEM <- NULL
ipe$countryname_raw_OECD <- NULL
ipe$countryname_raw_OFI <- NULL
ipe$countryname_raw_OFS <- NULL
ipe$countryname_raw_PIS <- NULL
ipe$countryname_raw_PTS <- NULL
ipe$countryname_raw_PV <- NULL
ipe$countryname_raw_PVE <- NULL
ipe$countryname_raw_PW <- NULL
ipe$countryname_raw_SFI <- NULL
ipe$countryname_raw_SGI <- NULL
ipe$countryname_raw_SI <- NULL
ipe$countryname_raw_SVS <- NULL
ipe$countryname_raw_TI <- NULL
ipe$countryname_raw_TR <- NULL
ipe$countryname_raw_UBD <- NULL
ipe$countryname_raw_UNCTAD <- NULL
ipe$countryname_raw_WDI <- NULL
ipe$countryname_raw_WE <- NULL
ipe$countryname_raw_WGI <- NULL
ipe$countryname_raw_PO <- NULL
ipe$countryname_raw_BD <- NULL
ipe$countryname_raw_CR <- NULL
ipe$countryname_raw_CV <- NULL
ipe$countryname_raw_TH <- NULL
ipe$countryname_raw_DPI <- NULL
ipe$countryname_raw_FA <- NULL
ipe$countryname_raw_FS <- NULL
ipe$countryname_raw_FW <- NULL
ipe$countryname_raw_PF <- NULL
ipe$countryname_raw_GE <- NULL
ipe$countryname_raw_HR <- NULL
ipe$countryname_raw_CIM <- NULL
ipe$countryname_raw_JCB <- NULL
ipe$countryname_raw_LX <- NULL
ipe$countryname_raw_LS <- NULL
ipe$countryname_raw_LY <- NULL
ipe$countryname_raw_MU <- NULL
ipe$countryname_raw_MP <- NULL
ipe$countryname_raw_IP <- NULL
ipe$countryname_raw_RCS <- NULL
ipe$countryname_raw_EMS <- NULL
ipe$countryname_raw_ODC <- NULL
ipe$countryname_raw_OPE <- NULL
ipe$countryname_raw_VDEM <- NULL
ipe$countryname_raw_GFD <- NULL
ipe$countryname_raw_WB2010 <- NULL
ipe$countryname_raw_ZH <- NULL
ipe$countryname_raw_WJP <- NULL
ipe$countryname_raw_IDA <- NULL
ipe$ccode_raw_AE <- NULL
ipe$ccode_raw_BH <- NULL
ipe$ccode_raw_BX <- NULL
ipe$ccode_raw_TH <- NULL
ipe$ccode_raw_EP <- NULL
ipe$ccode_raw_FA <- NULL
ipe$ccode_raw_GE <- NULL
ipe$ccode_raw_RCS <- NULL
ipe$ccode_raw_VDEM <- NULL
ipe$ccode_raw_MC <- NULL
ipe$ccodw_raw_LS <- NULL

backup <- ipe

ipe <- backup

# Label all Variables #####

# Start with R variables
library(Hmisc)

label(ipe$top_margintax_EF) = "Top marginal income tax rate [EF]"
label(ipe$top_margin_payrolltax_EF) = "Top marginal income and payroll tax rate [EF]"
label(ipe$propright_EF) = "Protection of property rights [EF]"
label(ipe$impartcourts_EF) = "Impartial courts [EF]"
label(ipe$bizcrimecost_EF) = "Business cost of crime [EF]"
label(ipe$policerely_EF) = "Reliability of police [EF]"

label(ipe$pl_FH) = "Gastil index of political liberties [FH]"
label(ipe$cl_FH) = "Gastil index of civil liberties [FH]"
label(ipe$rulelaw_FH) = "Rule of Law Index [FH]"

label(ipe$ers_TR) <- "Exchange Rate Stability Index [TR]"
label(ipe$mi_TR) <- "Monetary Independence Index [TR]"
label(ipe$ka_open_TR) <- "Financial Openness Index [TR]"

label(ipe$cinc_MC) <- "CINC Score [MC]"
label(ipe$milex_MC) <- "Military Expenditures [MC]"
label(ipe$milper_MC) <- "Military Personnel  [MC]"
label(ipe$irst_MC) <- "Iron and steel production [MC]"
label(ipe$pec_MC) <- "Primary energy consumption [MC]"
label(ipe$tpop_MC) <- "Total Population [MC]"
label(ipe$upop_MC) <- "Urban population [MC]"

label(ipe$milex_SI) <- "Military Expenditures (SIPRI) in millions (2011 USD) [SI]"

label(ipe$cbi_BH) <- "Central bank Indepedence score [BH]"
label(ipe$cbiw_BH) <- "Weighted Central Bank Independence score [BH]"
label(ipe$reform_BH) <- "Reform year (yes/no) [BH]"

label(ipe$fdiflows_UNCTAD) = "FDI Flows (Inward) USD Millions [UNCTAD]"
label(ipe$fdistocks_UNCTAD) = "FDI Stocks (Inward) USD Millions [UNCTAD]"

label(ipe$ind_total_OFS) = "Outward FDI stocks from the US in USD (millions), all industries, BEA [OFS]"

label(ipe$ind_total_OFI) <- "US Outward FDI: Position, All Industries, in mil. USD [OFI]" 
label(ipe$mining_OFI) <- "US Outward FDI: Position, Mining, in mil. USD [OFI]"
label(ipe$util_OFI) <- "US Outward FDI: Position, Utilities, in mil. USD [OFI]"
label(ipe$mf_total_OFI) <- "US Outward FDI: Position, Manufacturing: Total Manufacturing, in mil. USD [OFI]"
label(ipe$mf_food_OFI) <- "US Outward FDI: Position, Manufacturing: Food, in mil. USD [OFI]"
label(ipe$mf_chem_OFI)<- "US Outward FDI: Position, Manufacturing: Chemicals, in mil. USD [OFI]"
label(ipe$mf_pfm_OFI) <- "US Outward FDI: Position, Manufacturing: Primary and fabricated metals, in mil. USD [OFI]"
label(ipe$mf_mc_OFI) <-  "US Outward FDI: Position, Manufacturing: Machinery, in mil. USD [OFI]"
label(ipe$mf_cep_OFI) <-  "US Outward FDI: Position, Computers and electronic products, in mil. USD [OFI]"
label(ipe$mf_eeac_OFI)	<- "US Outward FDI: Position, Electrical equipment, appliances, components, in mil. USD [OFI]"
label(ipe$mf_te_OFI) <- "US Outward FDI: Position, Manufacturing: Transportation Equipment, in mil. USD [OFI]"
label(ipe$mf_other_OFI) <-  "US Outward FDI: Position, Manufacturing: Other Manufacturing, in mil. USD [OFI]"
label(ipe$wt_OFI) <- "US Outward FDI: Position, Wholesale Trade, in mil. USD [OFI]"
label(ipe$info_OFI) <- "US Outward FDI: Position, Information, in mil. USD [OFI]"
label(ipe$dep_ins_OFI) <- "US Outward FDI: Position, Depository Institutions, in mil. USD [OFI]"
label(ipe$fi_OFI) <-  "US Outward FDI: Position, Finance (no depository inst.) and insurance, in mil. USD [OFI]"
label(ipe$psts_OFI) <- "US Outward FDI: Position, Professional, scientific, technical services, in mil. USD [OFI]"
label(ipe$hc_nb_OFI) <- "US Outward FDI: Position, Holding Companies (nonbank), in mil. USD [OFI]"
label(ipe$ind_other_OFI) <- "US Outward FDI: Position, Other Industries, in mil. USD [OFI]"

label(ipe$currency_unit_PW) <- "Currency Unit [PW]"
label(ipe$rgdpe_PW) <- "Expenditure-side real GDP at chained PPPs (in mil. 2011US$) [PW]"
label(ipe$rgdpo_PW) <- "Output-side real GDP at chained PPPs (in mil. 2011US$) [PW]"
label(ipe$pop_PW) <- "Population (in millions) [PW]"
label(ipe$emp_PW) <- "Number of persons engaged (in millions) [PW]"
label(ipe$hc_PW) <- "Average annual hours worked by persons engaged [PW]"
label(ipe$cgdpe_PW) <- "Expenditure-side real GDP at current PPPs (in mil. 2011US$) [PW]"
label(ipe$cgdpo_PW) <- "Output-side real GDP at current PPPs (in mil. 2011US$) [PW]"
label(ipe$ck_PW) <- "Capital stock at current PPPs (in mil. 2011US$) [PW]"
label(ipe$ctfp_PW) <- "TFP level at current PPPs (USA=1) [PW]"
label(ipe$rgdpna_PW) <- "Real GDP at constant 2011 national prices (in mil. 2011US$) [PW]"
label(ipe$rkna_PW) <- "Capital stock at constant 2011 national prices (in mil. 2011US$) [PW]"
label(ipe$rtfpna_PW) <- "TFP at constant national prices (2011=1) [PW]"
label(ipe$rwtfpna_PW) <- "Welfare-relevant TFP at constant national prices (2011=1) [PW]"
label(ipe$labsh_PW) <- "Share of labor compensation in GDP at current national prices [PW]"
label(ipe$xr_PW) <- "Exchange rate, national currency/USD (market+estimated) [PW]"
label(ipe$statcap_PW) <- "Statistical capacity indicator (source: World Bank, developing countries only) [PW]"

label(ipe$p_FHP) = "Printing press freedom [FHP]"
label(ipe$b_FHP) = "Broadcast press freedom [FHP]"
label(ipe$ss_FHP) = "Free status [FHP]"
label(ipe$sr_FHP) = "Numerical score [FHP]"

label(ipe$area_WDI) = "Surface area (sq.km) [WDI]"
label(ipe$area_land_WDI) = "Land area (sq.km) [WDI]"
label(ipe$armed_WDI) = "armed forces personnel, total [WDI]"
label(ipe$deaths_bt_WDI) = "Battle-related deaths(number of people) [WDI]"
label(ipe$debt_cgov_WDI) = "Central government debt. Total (% of GDP) [WDI]"
label(ipe$ex_agri_WDI) = " Agricultural raw materials exports (% of merchandise exports) [WDI]"
label(ipe$ex_food_WDI) = " Food exports (% of merchandise exports) [WDI]"
label(ipe$ex_fuel_WDI) = " Fuel exports (% of merchandise exports) [WDI]"
label(ipe$ex_mfg_WDI) = "Manufactures exports (% of merchandise exports) [WDI]"
label(ipe$ex_oresmet_WDI) = "Ores and metal exports (% of merchandise exports) [WDI]"
label(ipe$exp_WDI) = "Exports of goods and services (% of GDP) [WDI]"
label(ipe$extdebt_WDI) = "External debt stocks (% of GNI) [WDI]"
label(ipe$extdebt_avgint_WDI) = "Average interest on new external debt commitments (%) [WDI]"
label(ipe$extdebt_avgintoff_WDI) = "Average interest on new external debt commitments, official (%) [WDI]"
label(ipe$extdebt_avgintpriv_WDI) = "Average interest on new external debt commitments, private (%) [WDI]"
label(ipe$extdebt_intpaygni_WDI) = "Interest payments on external debt (% of GNI) [WDI]"
label(ipe$extdebt_intpayperex_WDI) = "Interest payments on external debt (% exports and primary income) [WDI]"
label(ipe$extdebt_pv_WDI) = "Present value of external debt (current US$) [WDI]"
label(ipe$extdebt_tot_WDI) = "External debt stocks, total (DOD, current US$) [WDI]"
label(ipe$fdidollars_WDI) = "Foreign direct investment, net inflows (BoP, current US$) [WDI]"
label(ipe$fdi_inper_WDI) = "Foreign direct investment, net inflows (% of GDP) [WDI]"
label(ipe$fdi_outper_WDI) = "Foreign direct investment, net outflows (% of GDP) [WDI]"
label(ipe$gdp_WDI) = "GDP (constant 2010 US$) [WDI]"
label(ipe$growth_WDI) = "GDP growth (annual %) [WDI]"
label(ipe$gdppc_WDI) = "GDP per capita (constant 2010 US$) [WDI]" 
label(ipe$gini_WDI) = "GINI index [WDI]"
label(ipe$gni_WDI) = "GNI (constant 2010 US$) [WDI]"
label(ipe$gnipc_WDI) = "GNI per capita (constant 2010 US$) [WDI]"
label(ipe$inf_mort_WDI) = "Mortality rate, infant (per 1000 live births) [WDI]"
label(ipe$inflation_WDI) = "Inflation, consumer prices (annual%) [WDI]"
label(ipe$insfin_svc_WDI) = "Insurance and financial services (% of commercial service exports) [WDI]"
label(ipe$internet_br_WDI) = "Fixed broadband Internet subscribers (per 100 people) [WDI]"
label(ipe$journal_WDI) = "Scientific and technical journal articles [WDI]"
label(ipe$legal_rt_WDI) = "Strength of legal rights index (0=weak to 12=strong) [WDI]"
label(ipe$lending_intrate_WDI) = "Lending interest rate (%) [WDI]"
label(ipe$life_exp_WDI) = "Life expectancy at birth, total (years) [WDI]"
label(ipe$lit_WDI) = "Literacy rate, adult total (% of people ages 15 and above) [WDI]"
label(ipe$migrant_per_WDI) = "International migrant stock (% of population) [WDI]"
label(ipe$migrant_tot_WDI) = "International migrant stock, total [WDI]"
label(ipe$mobile_WDI) = "Mobile cellular subscriptions (per 100 people) [WDI]"
label(ipe$patapps_WDI) = "Patent applications, residents [WDI]"
label(ipe$pop_WDI) = "Population, total [WDI]"
label(ipe$pop_den_WDI) = "Population density (ppl per sq km of land area) [WDI]"
label(ipe$pop_urb_WDI) = "Urban population (% of total) [WDI]"
label(ipe$workingpop_WDI) = "Population ages 15-64 (% of total)[WDI]"
label(ipe$power_out_WDI) = "Power outages in firms in a typical month (number) [WDI]"
label(ipe$portfolio_WDI) = "Portfolio equity, net inflows (BoP, current US$) [WDI]"
label(ipe$reer_WDI) = "Real effective exchange rate index (2010 = 100) [WDI]"
label(ipe$remittances_WDI) = "Personal remittances, received (% of GDP) [WDI]"
label(ipe$reserves_im_WDI) = "Total reserves in months of imports [WDI]"
label(ipe$reserves_tot_WDI) = "Total reserves (includes gold, current US$) [WDI]"
label(ipe$rir_WDI) = "Real interest rate (%) [WDI]"
label(ipe$risk_prem_WDI) = "Risk premium on lending (lending rate minus treasury bill rate, %) [WDI]"
label(ipe$tax_rev_WDI) = "Tax revenue (% of GDP) [WDI]"
label(ipe$tech_rnd_WDI) = "Technicians in R&D (per million people) [WDI]"
label(ipe$tele_WDI) = "Telephone lines (per 100 people) [WDI]"
label(ipe$trade_WDI) = "Trade (% of GDP) [WDI]"
label(ipe$trade_services_WDI) = "Trade in services (% of GDP) [WDI]"
label(ipe$trdmk_apps_WDI)= "Trademark applications, total [WDI]"
label(ipe$natresource_rents_WDI) = "Total natural resources rents (% of GDP) [WDI]"
label(ipe$govt_consump_WDI)= "General government final consumption expenditure (% of GDP) [WDI]"
label(ipe$GNI_growth_WDI)="GNI per capita growth (annual %) [WDI]"
label(ipe$capform_WDI)= "Gross fixed capital formation (% of GDP) [WDI]"
label(ipe$capformraw_WDI) ="Gross capital formation (constant 2010 US$) [WDI]"
label(ipe$govexp_edu_WDI) = "Government expenditure on education, total (% of GDP) [WDI]"
label(ipe$netoda_WDI)= "Net official development assistance received (current US$) [WDI]"
label(ipe$pop0_14_WDI) = "Population ages 0-14 (% of total) [WDI]"

label(ipe$gdp_WDI_PW) = "GDP (constant 2005 US$) [WDI_PW]"
label(ipe$gdppc_WDI_PW) = "Gross Domestic Product Per Capita [WDI_PW]"
label(ipe$growth_WDI_PW) = "GDP growth (annual %) [WDI_PW]"
label(ipe$pop_WDI_PW) = "Population, total [WDI_PW]"
label(ipe$lngdp_WDI_PW) = "GDP (constant 2005 US$) [WDI_PW] logged"
label(ipe$lngdppc_WDI_PW) = "Gross Domestic Product Per Capita [WDI_PW] logged"
label(ipe$lnpop_WDI_PW) = "Population, total [WDI_PW] logged"

label(ipe$ltint_OECD) = "Long-term interest rate [OECD], percentage"

label(ipe$unitlaborcost_ILO) <- "Mean nominal hourly labour cost per employee (Local Currency) [ILO]"

label(ipe$taxincentsum_LI) <- "Sum of six types of tax incentives [LI]"

label(ipe$rank_BZ) = "Ease of Doing Business Rank [WB_Biz]"
label(ipe$rank_sb_BZ) = "Starting a Business Rank [WB_Biz]"
label(ipe$sb_proc_BZ) = "Starting a business, procedures (number) [WB_Biz]"
label(ipe$sb_time_BZ) = "Starting a business, time (days) [WB_Biz]"
label(ipe$sb_cost_BZ) = "Starting a business, cost (% of income percapita) [WB_Biz]"
label(ipe$sb_paid_BZ) = "Starting a business, paid in min. cap (% of income per capita) [WB_Biz]"
label(ipe$rank_cp_BZ) = "Dealing with Construction Permits Rank [WB_Biz]"
label(ipe$cp_proc_BZ) = "Dealing with Construction Permits, procedures (number) [WB_Biz]"
label(ipe$cp_time_BZ) = "Dealing with Construction Permits, time (days) [WB_Biz]"
label(ipe$cp_cost_BZ) = "Dealing with Construction Permits, cost (% of warehouse value) [WB_Biz]"
label(ipe$rank_ge_BZ) = "Getting Electricity Rank [WB_Biz]"
label(ipe$ge_proc_BZ) = "Getting Electricity, procedures (number) [WB_Biz]"
label(ipe$ge_time_BZ) = "Getting Electricity, time (days) [WB_Biz]"
label(ipe$ge_cost_BZ) = "Getting Electricity, cost (% of income per capita) [WB_Biz]"
label(ipe$rank_rp_BZ) = "Registering Property Rank [WB_Biz]"
label(ipe$rp_proc_BZ) = "Registering Property, procedures (number) [WB_Biz]"
label(ipe$rp_time_BZ) = "Registering Property, time (days) [WB_Biz]"
label(ipe$rp_cost_BZ) = "Registering Property, cost (% of property value) [WB_Biz]"
label(ipe$rank_gc_BZ) = "Getting Credit Rank [WB_Biz]"
label(ipe$gc_legal_BZ) = "Getting Credit, strength of legal rights index (0-12) [WB_Biz]"
label(ipe$gc_legal_old_BZ) = "Getting Credit, strength of legal rights index (0-10) old methodology [WB_Biz]"
label(ipe$gc_info_BZ) = "Getting Credit, depth of credit information index (0-8) [WB_Biz]"
label(ipe$gc_info_old_BZ) = "Getting Credit, depth of credit information index (0-6) old methodology [WB_Biz]"
label(ipe$gc_pub_BZ) = "Getting Credit, public registry coverage (% of adults) [WB_Biz]"
label(ipe$gc_priv_BZ) = "Getting Credit, private bureau coverage (% of adults) [WB_Biz]"
label(ipe$rank_pi_BZ) = "Protecting Minority Investors Rank [WB_Biz]"
label(ipe$pi_disc_BZ) = "Protecting Minority Investors, extent of disclosure index (0-10) [WB_Biz]"
label(ipe$pi_dl_BZ) = "Protecting Minority Investors, extent of director liability index (0-10) [WB_Biz]"
label(ipe$pi_shsu_BZ) = "Protecting Minority Investors, ease of shareholder suits index (0-10) [WB_Biz]"
label(ipe$pi_shsu_old_BZ) = "Protecting Minority Investors, ease of shareholder suits index (0-10) old methodology [WB_Biz]"
label(ipe$pi_ip_BZ) = "Protecting Minority Investors, strength of investor protection index (0-10) [WB_Biz]"
label(ipe$pi_ip_old_BZ) = "Protecting Minority Investors, strength of investor protection index (0-10) old methodology [WB_Biz]"
label(ipe$rank_pt_BZ) = "Paying Taxes Rank [WB_Biz]"
label(ipe$pt_pay_BZ) = "Paying Taxes, payments (number per year) [WB_Biz]"
label(ipe$pt_time_BZ) = "Paying Taxes, time (hours per year) [WB_Biz]"
label(ipe$pt_prof_BZ) = "Paying Taxes, profit tax (%) [WB_Biz]"
label(ipe$pt_lab_BZ) = "Paying Taxes, labor tax and contributions (%) [WB_Biz]"
label(ipe$pt_other_BZ) = "Paying Taxes, other taxes (%) [WB_Biz]"
label(ipe$pt_tot_BZ) = "Paying Taxes, total tax rate (% profit) [WB_Biz]"
label(ipe$rank_tr_BZ) = "Trading Across Borders Rank [WB_Biz]"
label(ipe$tr_exdo_old_BZ) = "Trading Across Borders, documents to export (number) old methodology [WB_Biz]"
label(ipe$tr_exti_old_BZ) = "Trading Across Borders, time to export (days) old methodology [WB_Biz]"
label(ipe$tr_exco_old_BZ) = "Trading Across Borders, cost to export (number) old methodology [WB_Biz]"
label(ipe$rank_tr_BZ) = "Trading Across Borders, cost to export (US$ per container) [WB_Biz]"
label(ipe$tr_imdo_BZ) = "Trading Across Borders, documents to import (number) [WB_Biz]"
label(ipe$tr_imti_old_BZ) = "Trading Across Borders, time to import (days) [WB_Biz]"
label(ipe$tr_imco_old_BZ) = "Trading Across Borders, cost to import (US$ per container) [WB_Biz]"
label(ipe$rank_ec_BZ) = "Enforcing Contracts Rank [WB_Biz]"
label(ipe$ec_time_BZ) = "Enforcing Contracts, time (days) [WB_Biz]"
label(ipe$ec_cost_BZ) = "Enforcing Contracts, cost (% of claim) [WB_Biz]"
label(ipe$rank_ri_BZ) = "Resolving Insolvency Rank [WB_Biz]"
label(ipe$ri_time_BZ) = "Resolving Insolvency, time (years) [WB_Biz]"
label(ipe$ri_cost_BZ) = "Resolving Insolvency, cost (% of estate) [WB_Biz]"
label(ipe$ri_out_BZ) = "Resolving Insolvency, outcome (0 as piecemeal sale and 1 as going concern) [WB_Biz]"
label(ipe$ri_reco_BZ) = "Resolving Insolvency, recovery rate (cents on the dollar) [WB_Biz]"

label(ipe$exselec_DD) = "Mode of executive election [DD]"
label(ipe$legselec_DD) = "Mode of legislative election [DD]"
label(ipe$closed_DD) = "Status of legislature [DD]"
label(ipe$dejure_DD) = "Legal status of parties [DD]"
label(ipe$defacto_DD) = "De facto existence of parties [DD]"
label(ipe$defacto2_DD) = "Existence of parties outside regime front [DD]" 
label(ipe$democracy_DD) = "Dummy variable for democracy [DD]"
label(ipe$regime_DD) = "Regime classification (parliamentary, mil dictatorship, etc.) [DD]"
label(ipe$ttd_DD) = "Dummy for transition to democracy [DD]"
label(ipe$tta_DD) = "Dummy for transition to dictatorship [DD]"

label(ipe$fragment_P4) = "Polity fragmentation [P4]"
label(ipe$democ_P4) = "Institutionalized democracy [P4]"
label(ipe$autoc_P4) = "Institutionalized autocracy [P4]"
label(ipe$polity_P4) = "Combined polity score [P4]"
label(ipe$polity2_P4) = "Revised combined polity score [P4]"
label(ipe$durable_P4) = "Regime durability [P4]"
label(ipe$xconst_P4) = "Executive constraints [P4]"
label(ipe$parreg_P4) = "Regulation of participation [P4]"
label(ipe$parcomp_P4) = "Competitiveness of participation [P4]"
label(ipe$polcomp_P4) = "Political competition [P4]"
label(ipe$change_P4) = "Total change in polity value [P4]"
label(ipe$sf_P4) = "State failure [P4]"
label(ipe$regtrans_P4) = "Regime transition [P4]"

label(ipe$democracy_share_DE) <- "System Level Democratic Share [DE]"
label(ipe$democracy_count_DE) <- "Count of Democracies in the System [DE]"
label(ipe$lndemsys_DE) <- "Count of Democracies in the System (logged) [DE]"

label(ipe$inclusive_IDC) = "Inclusive powersharing [IDC]"
label(ipe$dispersive_IDC) = "Dispersive Powersharing  [IDC]"
label(ipe$constraining_IDC) = "Constraining Powersharing  [IDC]"
label(ipe$inclusive_imp_IDC) = " Inclusive with missing values imputed [IDC]"
label(ipe$dispersive_imp_IDC) = "Dispersive with missing values imputed [IDC]"
label(ipe$constraining_imp_IDC) = "Constraining with missing values imputed [IDC]"
label(ipe$inclusive_nm_IDC) = "Inclusive with missing values replaced as 0 [IDC]"
label(ipe$dispersive_nm_IDC) = " Dispersive with missing values replaced as 0 [IDC]"
label(ipe$constraining_nm_IDC) = "Constraining with missing values replaced as 0 [IDC]"
label(ipe$constsusp_IDC) = "Constitution Suspended  [IDC]"
label(ipe$treaty_IDC) = "Treaty serving in lieu of a constitution [IDC]"
label(ipe$martiallaw_binary_IDC) = "Martial Law [IDC]"
label(ipe$gcman_IDC) = "Grand Coalition (mandated) [IDC]"
label(ipe$gcimp_IDC) = "Grand Coalition (implemented) [IDC]"
label(ipe$unity_IDC) = "National Unity Government [IDC]"
label(ipe$gcseats1_IDC) = "Grand Coalition by Seats (Two largest parties in government) [IDC]"
label(ipe$gcseats2_IDC) = "Grand Coalition by Seats (Excess Party in Coalition) [IDC]"
label(ipe$gcseats3_IDC) = "Grand Coalition by Seats (Either gscseats1 or gcseats2 = 1) [IDC]"
label(ipe$gcnew_IDC) = "gcman or unity = 1 [IDC]"
label(ipe$partynoethnic_IDC) = "Ethnic Party Ban [IDC]"
label(ipe$mveto_IDC) = "Mutual Veto [IDC]"
label(ipe$resman_IDC) = "Reserved Executive Positions (Mandated) [IDC]"
label(ipe$resimp_IDC) = "Reserved Executive Positions (Implemented) [IDC]"
label(ipe$resseats_IDC) = "Reserved Seats (Mandated) [IDC]"
label(ipe$resseats2_IDC) = "Reserved Seats Binary (Mandated) [IDC]"
label(ipe$resseatsimp_IDC) = "Reserved Seats (Implemented) [IDC]"
label(ipe$relestablish_IDC) = "State Establishment of Religion [IDC]"
label(ipe$relestablish_binary_IDC) = "State Establisment of Religion [IDC]"
label(ipe$relrestrict_IDC) = "State Restriction of (minority) religions [IDC]"
label(ipe$relconstp_IDC) = "Religion Protected (Practice) [IDC]"
label(ipe$relconstd_IDC) = "Religion Protected (Discrimination) [IDC]"
label(ipe$stconst_IDC) = "Regional Constituencies in the Upper House [IDC]"
label(ipe$state_IDC) = "State/Provincial Governments Locally Elected [IDC]"
label(ipe$state_dummy1_IDC) = "state_dummy1_IDC [IDC]"
label(ipe$state_dummy2_IDC) = "state_dummy2_IDC [IDC]"
label(ipe$muni_IDC) = "Municipal Governments Locally Elected [IDC]"
label(ipe$subtax_IDC) = "Sub-National Tax Authority [IDC]"
label(ipe$subed_IDC) = "Subnational Education Authority [IDC]"
label(ipe$subpolice_IDC) = "Subnational Police Authority [IDC]"
label(ipe$auton_IDC)= "Asymmetric Federalism [IDC]"
label(ipe$milleg_IDC) = "Military Legislator Ban [IDC]"
label(ipe$miman_IDC) = "Inclusive Military [IDC]"
label(ipe$mfound_IDC) = "Foundational Military [IDC]"
label(ipe$milparty_IDC) = "Constitutional Provision Against Military Party Membership [IDC]"
label(ipe$milparty2_IDC) = "Mandatory Party Membership for Military [IDC]"
label(ipe$jtenure_IDC) = "Judicial Tenure [IDC]"
label(ipe$jtenure_dummy1_IDC) = "jtenure_dummy1_IDC [IDC]"
label(ipe$jtenure_dummy2_IDC) = "jtenure_dummy2_IDC [IDC]"
label(ipe$jcause_IDC) = "Judges can be removed without cause [IDC]"
label(ipe$japptbr_IDC) = "Judicial Appointment Authority Divided [IDC]"
label(ipe$jconst_IDC) = "Judicial Constitution [IDC]"
label(ipe$jrevman_IDC) = "Judicial Review [IDC]"
label(ipe$violation_IDC) = "Violation of Mandated Powersharing [IDC]"
label(ipe$pr_IDC) = "Proportional Representation [IDC]"

label(ipe$VA_EST_WGI) <- "Voice and accountability, estimate  [WB WGI]"
label(ipe$PV_EST_WGI) <- "Political stability absence of violence, estimate [WB WGI]"
label(ipe$GE_EST_WGI) <- "Government effectiveness, estimate [WB WGI]"
label(ipe$GQ_EST_WGI) <- "Regulatory quality, estimate [WB WGI]"
label(ipe$RL_EST_WGI) <- "Rule of law, estimate [WB WGI]"
label(ipe$CC_EST_WGI) <- "Control of corruption, estimate [WB WGI]"

label(ipe$sfi_SFI) <- "State fragility index [SFI]"
label(ipe$effect_SFI) <- "Effectiveness score [SFI]"
label(ipe$legit_SFI) <- "Legitimacy score [SFI]"
label(ipe$seceff_SFI) <- "Security effectiveness score [SFI]"
label(ipe$secleg_SFI) <- "Security legitimacy score [SFI]"
label(ipe$poleff_SFI) <- "Political effectiveness score [SFI]"
label(ipe$polleg_SFI) <- "Political legitimacy score [SFI]"
label(ipe$ecoeff_SFI) <- "Economic effectiveness score [SFI]]"
label(ipe$ecoleg_SFI) <- "Economic legitimacy score [SFI]"
label(ipe$soceff_SFI) <- "Social effectiveness score [SFI]"
label(ipe$socleg_SFI) <- "Social legitimacy score [SFI]"

label(ipe$corrupprev_SGI) <- "Rule of Law: Corruption Prevention [SGI]" 

label(ipe$civil_cf_WJP) <- "Civil conflict is effectively limited [WJP]" 
label(ipe$cj_crpt_WJP) <- "Civil justice is free of corruption [WJP]" 
label(ipe$cj_delay_WJP) <- "Civil justice is not subject to unreasonable delays [WJP]" 
label(ipe$adr_acces_WJP) <- "ADRs are accessible, impartial, and effective [WJP]" 
label(ipe$cj_disc_WJP) <- "Civil justice is free of discrimination [WJP]" 
label(ipe$cj_enfor_WJP) <- "Civil justice is effectively enforced [WJP]" 
label(ipe$crime_ctrl_WJP) <- "Crime is effectively controlled [WJP]" 
label(ipe$due_admin_WJP) <- "Due process is respected in administrative proceedings [WJP]" 
label(ipe$exec_crpt_WJP) <- "Executive branch officials do not use public office for private gain [WJP]" 
label(ipe$expr_comp_WJP) <- "The Government does not expropriate without adequate compensation [WJP]" 
label(ipe$gov_reg_WJP) <- "Government regulations are applied and enforced without improper influence [WJP]" 
label(ipe$judic_crpt_WJP) <- "Judicial branch officials do not use public office for private gain [WJP]" 
label(ipe$legis_crpt_WJP) <- "Legislative branch officials do not use public office for private gain [WJP]" 
label(ipe$milt_crpt_WJP) <- "Police and Military officials do not use public office for private gain [WJP]" 
label(ipe$ppl_cj_WJP) <- "People have access to affordable civil justice [WJP]" 

label(ipe$Rank_FP) <- "Reporters without Borders Press Freedom Annual Ranking [FP]"
label(ipe$Score_FP) <- "Reporters with Borders Press Freedom Score [FP]"

label(ipe$ti_cpi_TI) <- "Corruption perceptions index [TI]" 

label(ipe$totint_PVE) <- "Sum of interstate MEPV magnitude scores for neighboring states [PVE]" 
label(ipe$totciv_PVE) <- "Sum of civil and ethnic MEPVs in neighboring states [PVE]" 
label(ipe$totalac_PVE) <- "Sum of all MEPVs in neighboring states [PVE]" 
label(ipe$nint_PVE) <- "Number of bordering states with international MEPVs [PVE]" 
label(ipe$nciv_PVE) <- "Number of bordering states with civil or ethnic MEPVs [PVE]" 
label(ipe$nac_PVE) <- "Number of bordering states with any MEPVs [PVE]" 
label(ipe$region_PVE) <- "Region [PVE]" 
label(ipe$nregion_PVE) <- "Number of states in region [PVE]" 
label(ipe$regint_PVE) <- "Sum of interstate MEPV magnitude scores in region [PVE]" 
label(ipe$regciv_PVE) <- " Sum of civil and ethnic MEPVs in region [PVE]" 
label(ipe$regac_PVE) <- "Sum of all MEPVs in region [PVE]" 
label(ipe$nrint_PVE) <- "Number of states in region with international MEPVs [PVE]" 
label(ipe$nrciv_PVE) <- "Number of states in region with civil or ethnic MEPVs [PVE]" 
label(ipe$nrac_PVE) <- "Number of states in region with any MEPVs [PVE]" 

label(ipe$politterr_a_PTS) <- "Political Terror Scale based on Amnesty International [PTS]" 
label(ipe$politterr_HRW_PTS) <- "Political Terror Scale based on HRW [PTS]" 
label(ipe$politterr_s_PTS) <- "Political Terror Scale based on the US State Department [PTS]" 

label(ipe$sociaviol_SVS) <- "societal violence scale (1-5 with 5 being highest) [SVS]"

label(ipe$id_PO) <- "Conflict identifier"
label(ipe$sidea_PO) <- "Identifies the country of SideA. Always thegovernment side in internal conflict"
label(ipe$sideb_PO) <- "Country name or opposition actor"
label(ipe$sideb2nd_PO) <-  "Name of state(s) supporting side B with troops"
label(ipe$sidea2nd_PO) <- " Name of state(s) supporting side A with troops"
label(ipe$incomp_PO) <- "Dyad incompatibility"
label(ipe$terr_PO) <- "Name of territory"
label(ipe$intensity_PO) <- "Intensity level (minor armed conflicts and wars)"
label(ipe$cumintensity_PO) <- "cumulative intensity"
label(ipe$type_PO) <- "Conflict type"
label(ipe$region_PO) <- "Region of location"

label(ipe$BdLow_UBD) = "Low estimate of annual battle fatalities [UBD]"
label(ipe$BdHigh_UBD) = "High estimate of annual battle fatalities [UBD]"
label(ipe$BdBest_UBD) = "Best estimate of annual battle fatalities [UBD]"

label(ipe$onset2_AO) <- "2 years between onset variables coded [AO]"
label(ipe$onset3_AO) <- "3 years between onset variables coded [AO]"
label(ipe$onset4_AO) <- "4 years between onset variables coded [AO]"
label(ipe$onset5_AO) <- "5 years between onset variables coded [AO]"
label(ipe$onset6_AO) <- "6 years between onset variables coded [AO]"
label(ipe$onset7_AO) <- "7 years between onset variables coded [AO]"
label(ipe$onset8_AO) <- "8 years between onset variables coded [AO]"
label(ipe$onset9_AO) <- "9 years between onset variables coded [AO]"

label(ipe$tgpm_WE) <- "Transparency in government policy making [WEF]"

label(ipe$EUmem_MEM) <- "binary, 1 if member, 0 if not [EU]"
label(ipe$EUwhen_MEM) <- "year of accession [EU]"
label(ipe$WTOwhen_MEM) <- " year of accession [WTO]"
label(ipe$WTOmem_MEM) <- "binary, 1 if member, 0 if not [WTO]"
label(ipe$IMFwhen_MEM) <- " year of accession [IMF]"
label(ipe$IMFmem_MEM) <- "binary, 1 if member, 0 if not [IMF]"
label(ipe$NATOwhen_MEM) <- " year of accession [NATO]"
label(ipe$NATOmem_MEM) <- "binary, 1 if member, 0 if not [NATO]"
label(ipe$OECDwhen_MEM) <- "year of accession [OECD]"
label(ipe$OECDmem_MEM) <- "binary, 1 if member, 0 if not [OECD]"

label(ipe$math_avg_PIS) = "Average mathematics scores [PIS]"
label(ipe$reading_avg_PIS) = "Average reading scores [PIS]"
label(ipe$science_avg_PIS) = "Average science scores [PIS]"

label(ipe$ptratio_seced_ES) <- "Pupil-teacher ratio in secondary education (headcount basis) [ES]"

label(ipe$intind_PV) <- "War of Independence, Magnitude [PV]" 
label(ipe$intviol_PV) <- "International Violence, Magnitude [PV]" 
label(ipe$intwar_PV) <- "International Warfare, Magnitude [PV]" 
label(ipe$civviol_PV) <- "Civil Violence, Magnitude [PV]" 
label(ipe$civwar_PV) <- "Civil War, Magnitude [PV]" 
label(ipe$ethviol_PV) <- "Ethnic violence, Magnitude [PV]" 
label(ipe$ethwar_PV) <- "Ethnic War, Magnitude [PV]" 
label(ipe$inttot_PV) <- "Total interstate MEPVS, Magnitude [PV]" 
label(ipe$civtot_PV) <- "Total civil and ethnic MEPVS, Magnitude [PV]" 
label(ipe$actotal_PV) <- "Total MEPVS, Magnitude [PV]" 
label(ipe$nborder_PV) <- "Number of states sharing a border [PV]" 

label(ipe$lang1_LANG) <- "Language 1 [LANG]"
label(ipe$lang2_LANG) <- "Language 2 [LANG]"

label(ipe$english_off_CE) <- " Official Language of the Country is English [CE]"
label(ipe$dist_US_CE) <- "Distance from US (km between most populous cities) [CE]"

# DTA files

label(ipe$foreignbkct_CV) = "Count of foreign banks open in country [CV]"

label(ipe$cim_CIM) = "Contract intensive money [IMF_IFS]"
label(ipe$cim_adj_CIM) = "Contract intensive money adjusted [IMF_IFS]"

label(ipe$under_BCG) = "Whether a country was is under an IMF agreement [BCG]"
label(ipe$undertype_BCG) = "Type of IMF agreement country is under [BCG]"

label(ipe$pcredb_FS) = "Private credit by deposit money banks (% GDP) [FS]"
label(ipe$pcredbo_FS) = "Private credit by money banks and other financial institutions (% GDP) [FS]"

label(ipe$class_5way_LY) = "5-way classification [LY&S]"
label(ipe$class_3way_LY) = "3-way classification [LY&S]"

label(ipe$baselinemwp_LX) = "News coverage (total articles) [GKJ]"
label(ipe$investmentmwp_LX) = "Investment Articles in LexisNexis (keyword)[GKJ]"
label(ipe$subjectmwp_LX) = "Potentially Business Relevant Articles in LexisNexis (subjects) [GKJ]"
label(ipe$ln_baselinemwp_LX) ="Total Articles in LexisNexis (logged) [GKJ]"
label(ipe$ln_investmentmwp_LX) = "Investment Articles in LexisNexis (keyword) (logged)[GKJ]"
label(ipe$ln_subjectmwp_LX) = "Potentially Business Relevant Articles in LexisNexis (subjects) (logged)"

label(ipe$news_WB) = "Daily newspapers (per 1,000 people) [WB2010]"

label(ipe$reportcount_IB) = "Number of Unique I/B/E/S Analyst Reports [IB]"
label(ipe$firmcount_IB) ="Number of Unique Firms Covered in I/B/E/S Analyst Reports [IB]"
label(ipe$analystcount_IB) = "Number of Unique I/B/E/S Analysts [IB]"
label(ipe$lnreportcount_IB) = "Number of Unique I/B/E/S Analyst Reports (logged) [IB]"
label(ipe$lnfirmcount_IB) = "Number of Unique Firms Covered in I/B/E/S Analyst Reports (logged) [IB]"              
label(ipe$lnanalystcount_IB) = "Number of Unique I/B/E/S Analysts (logged) [IB]"     

label(ipe$surges_FW) = "Capital Surge Episodes [FW]"
label(ipe$stops_FW) = "Capital Stop Episodes [FW]"
label(ipe$retrench_FW) = "Capital Retrenchment Episodes [FW]"
label(ipe$flight_FW) = "Capital Flight Episodes [FW]"         

label(ipe$net_ppe_OPE) = "US Outward FDI flows: Net Property, Plant, and Equipment in mil. USD [OPE]"

label(ipe$taxhavwhen_TH) = "Year estimate of when the state became a tax haven [DOT]"
label(ipe$taxhav_TH) = "Binary, 1 if considered a tax haven, 0 if not [DOT]"
label(ipe$taxhavens_extrapolated_TH) = "Binary, 1 if considered a tax haven, 0 if not. Extrapolated from taxhav [DOT]"

label(ipe$combinedoil_AE) = "Oil Exports (% of GDP) (Ashford) [AE]"

label(ipe$frmlacct_GFD) = "Account at a formal financial institution (% age 15+) [GFD]"
label(ipe$bk_acct_GFD) =  "Bank accounts per 1,000 adults [GFD]"
label(ipe$bk_branch_GFD) =  "Bank branches per 100,000 adults [GFD]"
label(ipe$finconst_GFD) =  "Firms identifying access to finance as a major constraint (%) [GFD]"
label(ipe$firms_bf_GFD) =  "Firms using banks to finance investments (%) [GFD]"
label(ipe$firms_bw_GFD) = "Firms using banks to finance working capital (%) [GFD]"
label(ipe$firms_loc_GFD) =  "Firms with a bank loan or line of credit (%) [GFD]"
label(ipe$firms_chksv_GFD) =  "Firms with a checking or savings account (%) [GFD]"
label(ipe$loancollat_GFD) =  "Value of collateral needed for a loan (% of the loan amount) [GFD]"
label(ipe$wc_bfin_GFD) =  "Working capital financed by banks (%) [GFD]"
label(ipe$fd_gdp_GFD) =  "Financial system deposits to GDP (%) [GFD]"
label(ipe$dc_priv_GFD) =  "Domestic credit to private sector (% of GDP) [GFD]"
label(ipe$bkconc_GFD) =  "Bank concentration (%) [GFD]"
label(ipe$bkcrisis_GFD) =  "Banking crisis dummy (1=banking crisis, 0=none) [GFD]"
label(ipe$depos_gdp_GFD) =  "Bank deposits to GDP (%) [GFD]"
label(ipe$forbnkast_GFD) =  "Foreign bank assets among total bank assets (%) [GFD]"
label(ipe$forbnk_GFD) =  "Foreign banks among total banks (%) [GFD]"
label(ipe$h_stat_GFD) =  "H-statistic for efficiency measures [GFD]"
label(ipe$nr_loan_GFD) =  "Loans from nonresident banks (amounts outstanding) to GDP (%) [GFD]"
label(ipe$fb_cons_GFD) = "Consolidated foreign claims of BIS reporting banks to GDP (%) [GFD]"
label(ipe$gpd_liab_GFD) =  "Gross portfolio debt liabilities to GDP (%) [GFD]"
label(ipe$gpe_liab_GFD) =  "Gross portfolio equity liabilities to GDP (%) [GFD]"
label(ipe$marketcap_GFD) =  "Stock market capitalization to GDP (%) [GFD]"
label(ipe$compratio_GFD) =  "Number of listed companies per 1,000,000 people [GFD]"
label(ipe$stockvol_GFD) =  "Stock price volatility [GFD]"

label(ipe$MFNtariff_MFN) = "MFN Applied Tariff Rate (unweighted %) [MFN]"
label(ipe$iMFN_tariff_MFN) = "MFN Applied Tariff Rate Interpolated [MFN]"

label(ipe$v2x_polyarchy_VDEM) = "Electoral democracy index [VDEM]"
label(ipe$v2x_api_VDEM) = "Additive polyarchy index [VDEM]"
label(ipe$v2x_mpi_VDEM) = "Multiplicative polyarchy index [VDEM]"
label(ipe$v2x_EDcomp_thick_VDEM) = "Electoral component index [VDEM]"
label(ipe$v2x_libdem_VDEM) = "Liberal democracy index [VDEM]"
label(ipe$v2x_liberal_VDEM) = "Liberal component index [VDEM]"
label(ipe$v2x_partipdem_VDEM) = "Participatory democracy index [VDEM]"
label(ipe$v2x_partip_VDEM) = "Participatory component index [VDEM]"
label(ipe$v2x_delibdem_VDEM) = "Deliberative democracy index [VDEM]"
label(ipe$v2xdl_delib_VDEM) = "Deliberative component index [VDEM]"
label(ipe$v2x_egaldem_VDEM) = "Egalitarian democracy index [VDEM]"
label(ipe$v2x_egal_VDEM) = "Egalitarian component index [VDEM]"
label(ipe$v2x_frassoc_thick_VDEM) = "Freedom of association (thick) index [VDEM]"
label(ipe$v2x_freexp_thick_VDEM) = "Expanded freedom of expression index [VDEM]"
label(ipe$v2x_freexp_VDEM) = "Freedom of expression index [VDEM]"
label(ipe$v2xme_altinf_VDEM) = "Alternative sources of information index [VDEM]"
label(ipe$v2x_suffr_VDEM) ="Share of population with suffrage [VDEM]"
label(ipe$v2xel_frefair_VDEM) = "Clean elections index [VDEM]"
label(ipe$v2x_accex_VDEM) = "Elected executive index [VDEM]"
label(ipe$v2xcl_rol_VDEM) = "Equality before the law and individual liberty index [VDEM]"
label(ipe$v2x_jucon_VDEM) = "Judicial constraints on the executive index [VDEM]"
label(ipe$v2xlg_legcon_VDEM) = "Legislative constraints on the executive index [VDEM]"
label(ipe$v2x_cspart_VDEM) = "Civil society participation index [VDEM]"
label(ipe$v2xdd_dd_VDEM) = "Direct popular vote index [VDEM]"
label(ipe$v2xel_locelec_VDEM) = "Local government index [VDEM]"
label(ipe$v2xel_regelec_VDEM) = "Regional government index [VDEM]"
label(ipe$v2xeg_eqprotec_VDEM) = "Equal protection index [VDEM]"
label(ipe$v2xeg_eqdr_VDEM) = "Equal distribution of resources index [VDEM]"
label(ipe$v2xcs_ccsi_VDEM) = "Core civil society index [VDEM]"
label(ipe$v2xps_party_VDEM) = "Party system institutionalization index [VDEM]"
label(ipe$v2x_gender_VDEM) = "Women political empowerment index [VDEM]"
label(ipe$v2x_gencl_VDEM) = "Women civil liberties index [VDEM]"
label(ipe$v2x_gencs_VDEM) = "Women civil society participation index [VDEM]"
label(ipe$v2x_genpp_VDEM) = "Women political participation index [VDEM]"
label(ipe$v2x_elecreg_VDEM) = "Electoral regime index [VDEM]"
label(ipe$v2xex_elecreg_VDEM) = "Executive electoral regime index [VDEM]"
label(ipe$v2xlg_elecreg_VDEM) = "Legislative electoral regime index [VDEM]"
label(ipe$v2xel_elecparl_VDEM) = "Legislative or constituent assembly election [VDEM]"
label(ipe$v2xlg_leginter_VDEM) = "Legislature closed down or aborted [VDEM]"
label(ipe$v2xel_elecpres_VDEM) = "Presidential election [VDEM]"
label(ipe$v2x_hosinter_VDEM) = "Chief executive no longer elected [VDEM]"
label(ipe$v2x_corr_VDEM) = "Political corruption [VDEM]"
label(ipe$v2x_pubcorr_VDEM) = "Public sector corruption index [VDEM]"
label(ipe$v2x_execorr_VDEM) = "Executive corruption index [VDEM]"
label(ipe$v2x_lgdivparctrl_VDEM) = "Divided party control of legislature [VDEM]"
label(ipe$v2x_feduni_VDEM) = "Division of power index [VDEM]"
label(ipe$v2x_civlib_VDEM) = "Civil liberties index [VDEM]"
label(ipe$v2x_clphy_VDEM) = "Physical violence index [VDEM]"
label(ipe$v2x_clpol_VDEM) = "Political liberties index [VDEM]"
label(ipe$v2x_clpriv_VDEM) = "Private liberties index [VDEM]"
label(ipe$v2elsrgel_VDEM) = "Regional Government Elected [VDEM]"
label(ipe$v2elreggov_VDEM) = "Regional Government Exists [VDEM]"
label(ipe$v2cltrnslw_VDEM) = "Transparent laws with predictable enforcement [VDEM]"
label(ipe$v2juhcind_VDEM)= "High court independence [VDEM]"
label(ipe$v2juhccomp_VDEM) = "Government compliance with high court [VDEM]"
label(ipe$v2jureview_VDEM) = "Judicial review [VDEM]"

label(ipe$system_DPI) = "Political System [DPI]"
label(ipe$yrsoffc_DPI) = "Chief Executive Years in Office [DPI]"
label(ipe$finittrm_DPI) = "Finite Term in Office [DPI]"
label(ipe$yrcurnt_DPI) = "Years Left in Current Term [DPI]"             
label(ipe$multpl_DPI) = "Can Chief Executive Serve Multiple Terms [DPI]"
label(ipe$military_DPI) = "Is Chief Executive a Military Officer? [DPI]"                
label(ipe$defmin_DPI) = "Is Defense Minister a Military Officer? [DPI]"               
label(ipe$percent1_DPI) = "President Percentage of Votes, first round [DPI]"            
label(ipe$percentl_DPI) = "President Percentage of Votes, last round [DPI]"              
label(ipe$prtyin_DPI) = "Party of Chief Executive Length of Time in Office [DPI]"            
label(ipe$execme_DPI)  = "Name of Executive Party [DPI]"          
label(ipe$execrlc_DPI) = "Chief Executive Party Orientation [DPI]"              
label(ipe$execnat_DPI) = "Chief Executive Party: Nationalist [DPI]"
label(ipe$execrurl_DPI) = "Chief Executive Party: Rural [DPI]"             
label(ipe$execreg_DPI) = "Chief Executive Party: Regional [DPI]"           
label(ipe$execrel_DPI) = "Chief Executive Party: Religious [DPI]"         
label(ipe$execage_DPI) = "Age of Chief Executive Party [DPI]"
label(ipe$allhouse_DPI)  = "Does Party of Executive Control All Houses? [DPI]"          
label(ipe$nonchief_DPI) = "Party affiliation of Non-Chief Exec in Systems with Pres & PM [DPI]"            
label(ipe$totalseats_DPI) = "Total Seats in Legislature [DPI]"              
label(ipe$gov1me_DPI) = "Name of Largest Government Party [DPI]"            
label(ipe$gov1seat_DPI) = "Number of Seats of Largest Government Party [DPI]"           
label(ipe$gov1vote_DPI) = "Vote Share of Largest Government Party [DPI]"
label(ipe$gov1rlc_DPI) = "Largest Government Party Orientation [DPI]"
label(ipe$gov1nat_DPI) = "Largest Government Party: Nationalist [DPI]"
label(ipe$gov1rurl_DPI) = "Largest Government Party: Rural [DPI]"
label(ipe$gov1reg_DPI) = "Largest Government Party: Regional [DPI]"
label(ipe$gov1rel_DPI) = "Largest Government Party: Religious [DPI]"
label(ipe$gov1age_DPI) = " Age of Largest Government Party [DPI]"
label(ipe$gov2me_DPI) = "Name of 2nd Largest Government Party [DPI]"
label(ipe$gov2seat_DPI) = "Number of Seats of 2nd Largest Government Party [DPI]"
label(ipe$gov2vote_DPI) = "Vote Share of 2nd Largest Government Party [DPI]"
label(ipe$gov2rlc_DPI) =  "2nd Largest Government Party Orientation [DPI]"
label(ipe$gov2nat_DPI) =  "2nd Largest Government Party: Nationalist [DPI]"
label(ipe$gov2rurl_DPI) =  "2nd Largest Government Party: Rural [DPI]"
label(ipe$gov2reg_DPI) =  "2nd Largest Government Party: Regional [DPI]"
label(ipe$gov2rel_DPI) =  "2nd Largest Government Party: Religious [DPI]"
label(ipe$gov2age_DPI) =   "Age of 2nd Largest Government Party [DPI]"
label(ipe$gov3me_DPI) =   "Name of 3rd Largest Government Party [DPI]"
label(ipe$gov3seat_DPI) =  "Number of Seats of 3rd Largest Government Party [DPI]"
label(ipe$gov3vote_DPI) =   "Vote Share of 3rd Largest Government Party [DPI]"
label(ipe$gov3rlc_DPI) =  "3rd Largest Government Party Orientation [DPI]"
label(ipe$gov3nat_DPI) =  "3rd Largest Government Party: Nationalist [DPI]"
label(ipe$gov3rurl_DPI) =  "3rd Largest Government Party: Rural [DPI]"
label(ipe$gov3reg_DPI) = "3rd Largest Government Party: Regional [DPI]"
label(ipe$gov3rel_DPI) = "3rd Largest Government Party: Religious [DPI]"
label(ipe$gov3age_DPI) = "Age of 3rd Largest Government Party [DPI]"
label(ipe$govoth_DPI) =  "Number of Other Government Parties [DPI]"
label(ipe$govothst_DPI) =  "Number of Seats of Other Government Parties [DPI]"
label(ipe$govothvt_DPI) =  "Vote Share of Other Government Parties [DPI]"
label(ipe$opp1me_DPI) =   "Name of Largest Opposition Party [DPI]"
label(ipe$opp1seat_DPI) =   "Number of Seats of Largest Opposition Party [DPI]"
label(ipe$opp1vote_DPI) =  "Vote Share of Largest Opposition Party [DPI]"
label(ipe$opp1rlc_DPI) =  "Largest Opposition Party Orientation [DPI]"
label(ipe$opp1nat_DPI) = "Largest Opposition Party: Nationalist [DPI]"
label(ipe$opp1rurl_DPI) = "Largest Opposition Party: Rural [DPI]"
label(ipe$opp1reg_DPI) =  "Largest Opposition Party: Regional [DPI]"
label(ipe$opp1rel_DPI) =  "Largest Opposition Party: Religious [DPI]"
label(ipe$opp1age_DPI) = "Age of Largest Opposition Party [DPI]"
label(ipe$opp2me_DPI) =  " Name of 2nd Largest Opposition Party [DPI]"
label(ipe$opp2seat_DPI) =  "Number of Seats of 2nd Largest Opposition Party [DPI]"
label(ipe$opp2vote_DPI) =  "Vote Share of 2nd Largest Opposition Party [DPI]"
label(ipe$opp3me_DPI) =  "Name of 3rd Largest Opposition Party [DPI]"
label(ipe$opp3seat_DPI) =  "Number of Seats of 3rd Largest Opposition Party [DPI]"
label(ipe$opp3vote_DPI) =  "Vote Share of 3rd Largest Opposition Party [DPI]"
label(ipe$oppoth_DPI) =  "Number of Other Opposition Parties [DPI]"
label(ipe$oppothst_DPI) = "Number of Seats of Other Opposition Parties [DPI]"
label(ipe$oppothvt_DPI) =  "Number of Votes of Other Opposition Parties [DPI]"
label(ipe$ulprty_DPI) = "Number of Non-Aligned Parties [DPI]"
label(ipe$numul_DPI) = "Number of Seats of Non-Aligned Parties [DPI]"     
label(ipe$ulvote_DPI) = "Vote Share of Non-Aligned Parties [DPI]"  
label(ipe$oppmajh_DPI) = "Does One Opposition Party have a Majority in the House? [DPI]"   
label(ipe$oppmajs_DPI) = "Does One Opposition Party have a Majority in the Senate? [DPI]"
label(ipe$dateleg_DPI) = "Month Legislative Elections Held [DPI]"            
label(ipe$dateexec_DPI) = "Month Presidential Elections Held [DPI]"          
label(ipe$legelec_DPI) = "Legislative Election Held [DPI]"       
label(ipe$exelec_DPI) = "Presidential Election Held [DPI]"    
label(ipe$liec_DPI) = "Legislative Electoral Competitiveness [DPI]"            
label(ipe$eiec_DPI) = "Executive Electoral Competitiveness [DPI]"
label(ipe$mdmh_DPI) = "Mean District Magnitude House [DPI]"
label(ipe$mdms_DPI) = "Mean District Magnitude Senate [DPI]"
label(ipe$ssh_DPI) = "Number of Seats in Senate/Total Seats in Both Houses [DPI]"
label(ipe$plurality_DPI) = "Plurality [DPI]"
label(ipe$pr_DPI) = "Proportional Representation [DPI]"              
label(ipe$housesys_DPI) = "Electoral Rule House [DPI]"               
label(ipe$sensys_DPI) = "Electoral Rule Senate [DPI]"
label(ipe$thresh_DPI) = "Vote Threshold [DPI]"  
label(ipe$dhondt_DPI) = "D'Hondt System [DPI]"
label(ipe$cl_DPI) = "Closed List [DPI]"                  
label(ipe$select_DPI) = "Candidate Selection [DPI]"
label(ipe$fraud_DPI) = "Vote Fraud [DPI]"
label(ipe$auton_DPI) = "Autonomous Regions [DPI]"                  
label(ipe$muni_DPI) = "Municipal Government [DPI]"               
label(ipe$state_DPI) = "State Government [DPI]" 
label(ipe$author_DPI) = "State Government Authority over Taxing, Spending, or Legislating [DPI]"
label(ipe$stconst_DPI) = "Are the Constituencies of the Senators the States/Provinces? [DPI]"                
label(ipe$numgov_DPI) = "Number of Government Seats [DPI]"             
label(ipe$numvote_DPI) = "Vote Share of Government Parties [DPI]"    
label(ipe$numopp_DPI) = "Number of Opposition Seats [DPI]"
label(ipe$oppvote_DPI) = "Vote Share of Opposition Parties [DPI]"    
label(ipe$maj_DPI) = "Margin of Majority [DPI]"
label(ipe$partyage_DPI) = "Average Age of Parties [DPI]"   
label(ipe$herfgov_DPI) = "Herfindahl Index of Government Parties [DPI]" 
label(ipe$herfopp_DPI) = "Herfindahl Index of Opposition Parties [DPI]"
label(ipe$herftot_DPI) = "Herfindahl Index Total [DPI]"
label(ipe$frac_DPI) = "Fractionalization Index [DPI]"
label(ipe$oppfrac_DPI) = "Opposition Fractionalization Index [DPI]"
label(ipe$govfrac_DPI) = "Government Fractionalization Index [DPI]"
label(ipe$tensys_strict_DPI) = "tensys_strict [DPI]"
label(ipe$tensys_DPI) = "System Tenure [DPI]"
label(ipe$checks_lax_DPI) = "checks_lax [DPI]"
label(ipe$checks_DPI) = "Checks and Balances [DPI]"
label(ipe$stabs_strict_DPI) = "Stability (Threshold: LIEC = 6) [DPI]"
label(ipe$stabs_DPI) = "Stability [DPI]"        
label(ipe$stabns_strict_DPI) = "Stability, single chamber (Threshold: LIEC = 6) [DPI]" 
label(ipe$stabns_DPI) = "Stability, single chamber [DPI]"     
label(ipe$tenlong_strict_DPI) = "Longest Tenure of a Veto Player (Threshold: LIEC = 6) [DPI]"
label(ipe$tenlong_DPI) = "Longest Tenure of a Veto Player [DPI]"     
label(ipe$tenshort_strict_DPI) = "Shortest Tenure of a Veto Player (Threshold: LIEC = 6) [DPI]" 
label(ipe$tenshort_DPI) = "Shortest Tenure of a Veto Player [DPI]"    
label(ipe$polariz_DPI) = "Polarization [DPI]"

label(ipe$spell_GE) = "Time invariant count of number of years in power [Geddes]"
label(ipe$duration_GE) = "Time at risk of failure at time t [Geddes]" 
label(ipe$fail_GE) = "Binary variable indicating regime failure (fail==1) [Geddes]"       
label(ipe$fail_subsregime_GE) = "Regime failure, transition to [Geddes]"
label(ipe$fail_type_GE) = "Regime failure, type (howend) [Geddes]"       
label(ipe$fail_violent_GE) = "Regime failure, violence [Geddes]"
label(ipe$regimetype_GE) = "Regime type, including hybrids (10) [Geddes]"             
label(ipe$party_GE) = "Party regime (including hybrids, oligarchy, and Iran) [Geddes]"            
label(ipe$personal_GE) = "Personalist regime [Geddes]"
label(ipe$military_GE) = "Military regime (including milpersonal and indirect military) [Geddes]"            
label(ipe$monarch_GE) = "Monarchy [Geddes]"

label(ipe$democracy_BX) = "Democracy [Boix]"
label(ipe$sovereign_BX) = "Sovereign Dummy [Boix]"
label(ipe$democracy_trans_BX) = "Democratic Transition [Boix]"
label(ipe$democracy_breakdowns_BX) = "Democratic Breakdowns [Boix]"
label(ipe$democracy_duration_BX) = "Democratic Duration [Boix]"
label(ipe$democracy_omitteddata_BX) = "Democracy Omitted Data [Boix]"

label(ipe$lji_LS) = "Latent Judicial Independence Measure [LS]"

label(ipe$index_labor1_JCB) = "Level of worker protection through labor and employment laws. 0.77-2.31. [JCB]"
label(ipe$pright_JCB) = "Index of political rights in 1996 (1-7). [JCB]"
label(ipe$avelf_JCB) = "Avg. val. of five indices of ethnolinguistic fractionalization (0-5). [JCB]"
label(ipe$legor_uk_JCB) = "Origin of Company Law/Commercial Code is the English Common Law. [JCB]"
label(ipe$legor_fr_JCB) = "Origin of Company Law/Commercial Code is the French Commercial Code. [JCB]"
label(ipe$legor_so_JCB) = "Origin of Company Law/Commercial Code is the German Commercial Code. [JCB]"
label(ipe$legor_ge_JCB) = "Origin of Company Law/Commercial Code is the Scandinavian Commercial Code. [JCB]"
label(ipe$legor_sc_JCB) = "Origin of Company Law/Commercial Code is Socialist/Communist laws. [JCB]"
label(ipe$lat_abst_JCB) = "Absolute val. of the latitude of the country, values scaled 0-1. [JCB]"
label(ipe$f_prop97_JCB) = "Rating of property rights in each country in 1997 (1-5). [JCB]"
label(ipe$caselaw_JCB) = "Dummy variable. 1 if judicial decisions are a source of law, 0 otherwise. [JCB]"
label(ipe$sc_ten_JCB) = "Tenure of Supreme Court judges (0-2). [JCB]"
label(ipe$ac_ten_JCB) = "Tenure of highest ranked judges ruling on administrative cases. 0-2. [JCB]"
label(ipe$jud_rev_JCB) = "Extent, judges have power to review the constitutionality of laws. 0-2. [JCB]"
label(ipe$P_dem94_JCB) = "Democracy score in 1994 (0-10). Lower score ,less democracy. [JCB]"
label(ipe$lgdp_98_JCB) = "Logarithm of GDP/cap. in 1998 (USD). 4.5-10.5 in the sample. [JCB]"
label(ipe$rigid_JCB) = "How hard it is to change the constitution in a given country. (1-4). [JCB]"
label(ipe$humana_9_JCB) = "Based on three major UN treaties. Higher scores, better human rights. [JCB]"
label(ipe$proc_99_JCB) = "Number of procedures for a start-up to obtain a legal status (2-19). [JCB]"
label(ipe$gb_per_JCB) = "Share of assets of top 10 banks owned by the government in 1995. [JCB]"
label(ipe$new_constitutional_JCB) = "Sum of: judiciary review ind., rigidity of const. ind. [JCB]"
label(ipe$new_independence_JCB) = "Sum of: tenure of Sup. Ct. judges, admin. Ct. judges, case law ipe$. [JCB]"

label(ipe$prty_rights_BTI) = "Extent of government ensuring private property rights and regulations [BTI]"
label(ipe$edu_rd_BTI) = "Successfulness of education policies and government support for R & D [BTI]"

label(ipe$pr_rbgovn_IDA) = "Measure of Property Rights and Rule Based Governance [IDA]"

label(ipe$physint_CR) = "physical integrity rights index [CIRI]"
label(ipe$disap_CR) = "disappearnace [CIRI]" 
label(ipe$kill_CR) = "extrajudicial killing [CIRI]"
label(ipe$polpris_CR) = "political imprisonment [CIRI]"
label(ipe$tort_CR) = "torture [CIRI]"
label(ipe$assn_CR) = "freedom of assembly and association [CIRI]"
label(ipe$formov_CR) = "freedom of foreign movement [CIRI]"
label(ipe$dommov_CR) = "freedom of domestic movement [CIRI]"
label(ipe$old_move_CR) = "freedom of movement before 2007 [CIRI]"     
label(ipe$speech_CR) = "freedom of speech [CIRI]"
label(ipe$elecsd_CR) = "electoral self-determination [CIRI]"
label(ipe$old_relfre_CR) = "freedom of religion before 2007 [CIRI]"
label(ipe$new_relfre_CR) = "freedom of religion from 2007 [CIRI]"  
label(ipe$worker_CR) = "worker's rights [CIRI]"    
label(ipe$wecon_CR) = "women's economic rights [CIRI]"
label(ipe$wopol_CR) = "women's political rights [CIRI]"
label(ipe$wosoc_CR) = "women's social rights [CIRI]"   
label(ipe$injud_CR) = "independence of the judiciary [CIRI]"    

label(ipe$amnesty_FA) = "5 category [FA]"
label(ipe$state_FA) = "5 category [FA]"
label(ipe$hathaway_FA) = "5 category, ordered variable for torture [FA]"
label(ipe$itt_FA) = "6 category, ordered variable for torture [FA]"
label(ipe$genocide_FA) = "a binary variable for genocide [FA]"
label(ipe$ciri_FA) = "CIRI country identifier [FA]"
label(ipe$disap_FA) = "3 category, ordered variable for disappearances [FA]"
label(ipe$kill_FA) = "3 category, ordered variable from extra-judical killing [FA]"
label(ipe$polpris_FA) = "3 category, ordered variable for political imprisonment [FA]"
label(ipe$tort_FA) = "3 category, ordered variable for torture [FA]"
label(ipe$rummel_FA) = "a binary variable for politicide/genocide [FA]"
label(ipe$massive_repression_FA) = "a binary variable for massive repressive events [FA]"
label(ipe$executions_FA) = "a binary variable on executions [FA]"
label(ipe$killing_FA) = "binary version of one sided violence count data [FA]"
label(ipe$additive_FA) = "the CIRI Physint scale [FA]"
label(ipe$latentmean_FA) = "the posterior mean of the latent variable (i.e. human rights protection) [FA]"
label(ipe$latentsd_FA) = "the standard deviation of the posterior estimates for human rights protection [FA]"

label(ipe$laborrights_MU) = "Mosley-Uno measure of collective labor rights [MU]"

label(ipe$transparencyindex_HR) = "Transparency Index [HRV]"
label(ipe$transparencyindexub_HR) = "The estimated upper bound of the HRV index for each country and year [HRV]"
label(ipe$transparencyindexlb_HR) = "The estimated lower bound of the HRV index for each country and year [HRV]"
label(ipe$transparencyindexsd_HR) = "The standard deviation of 'HRV index' for each country and year [HRV]"  
label(ipe$transdiff_HR) = "The estimated one year change in HRV index for each country and year [HRV]"    
label(ipe$transdifflb_HR) = "The estimated lower bound of a one year change in the HRV index [HRV]"    
label(ipe$transdiffub_HR) = "The estimated upper bound of a one year change in the HRV index [HRV]"

label(ipe$MID_phat_MP) = "MID Propensity [NOR]"

label(ipe$peacekeep_PF) = "Whether the state hosted a UN Peacekeeping mission that year [PF]"

label(ipe$bdeadlow_BD) = "Low estimate of annual battle fatalities [BD]"
label(ipe$bdeadhigh_BD) = "High estimate of annual battle fatalities [BD]"
label(ipe$bdeadbest_BD) = "Best estimate of annual battle fatalities [BD]"

label(ipe$ipp_index_IP) = "International patent protection index [Park]"

label(ipe$ippstrength_ZH) = "IPP, 1 for weak 2 for strong [Zhao]"

label(ipe$bitstodate_BIT) = "BITs signed to date [BIT]"
label(ipe$lnbitstodate_BIT) = "BITs signed to date (logged) [BIT]"             

label(ipe$lh_1519_BL) = "Percentage of Tertiary Ages 15 to 19 [BL]"        
label(ipe$lh_2024_BL) = "Percentage of Tertiary Ages 20 to 24 [BL]"    
label(ipe$lh_25999_BL) = "Percentage of Tertiary Ages 25 and up [BL]"    
label(ipe$lh_F_1519_BL) = "Percentage of Tertiary Females Ages 15 to 19 [BL]"
label(ipe$lh_F_2024_BL) = "Percentage of Tertiary Females Ages 20 to 24 [BL]"     
label(ipe$lh_F_25999_BL) = "Percentage of Tertiary Females Ages 25 and up [BL]" 
label(ipe$lh_F_long_BL) = "Percentage of tertiary, female population aged 15-64 [BL]" 
label(ipe$lh_long_BL) = "Percentage of tertiary, population aged 15-64 [BL]"     
label(ipe$lpc_1519_BL) = "Percentage of Primary Complete Ages 15 to 19 [BL]"  
label(ipe$lpc_25999_BL) = "Percentage of Primary Complete Ages 25 and up [BL]"
label(ipe$lpc_F_1519_BL) = "Percentage of Primary Complete Females Ages 15 to 19 [BL]"
label(ipe$lpc_F_25999_BL) = "Percentage of Primary Complete Females Ages 25 and up [BL]"
label(ipe$ls_1519_BL) = "Percentage of Secondary Ages 15 to 19 [BL]"
label(ipe$ls_25999_BL) = "Percentage of Secondary Ages 25 and up [BL]"
label(ipe$ls_F_1519_BL) = "Percentage of Secondary Females Ages 15 to 19 [BL]"
label(ipe$ls_F_25999_BL) = "Percentage of Secondary Females Ages 25 and up [BL]"
label(ipe$ls_F_long_BL) = "Percentage of secondary, female population aged 15-64 [BL]"
label(ipe$ls_long_BL) = "Percentage of secondary, population aged 15-64 [BL]"
label(ipe$lsc_1519_BL) = "Percentage of Secondary Complete Ages 15 to 19 [BL]"
label(ipe$lsc_2024_BL) = "Percentage of Secondary Complete Ages 20 to 24 [BL]"
label(ipe$lsc_25999_BL) = "Percentage of Secondary Complete Ages 25 and up [BL]"
label(ipe$lsc_F_1519_BL) = "Percentage of Secondary Complete Females Ages 15 to 19 [BL]"
label(ipe$lsc_F_2024_BL) = "Percentage of Secondary Complete Females Ages 20 to 24 [BL]"
label(ipe$lsc_F_25999_BL) = "Percentage of Secondary Complete Females Ages 25 and up [BL]"
label(ipe$lu_1519_BL) = "Percentage of No Schooling Ages 15 to 19 [BL]"
label(ipe$lu_25999_BL) = "Percentage of No Schooling Ages 25 and up [BL]"
label(ipe$lu_F_1519_BL) = "Percentage of No Schooling Females Ages 15 to 19 [BL]"
label(ipe$lu_F_25999_BL) = "Percentage of No Schooling Females Ages 25 and up [BL]"
label(ipe$lu_F_long_BL) = "Percentage of no schooling, female population aged 15-64 [BL]"
label(ipe$lu_long_BL) = "Percentage of no schooling, population aged 15-64 [BL]"
label(ipe$pri_F_long_BL) = "Primary adjusted enrollment ratio (%) for female population [BL]"
label(ipe$pri_long_BL) = "Primary adjusted enrollment ratio (%) total population [BL]"
label(ipe$sec_F_long_BL) = "Secondary adjusted enrollment ratio (%) for female population [BL]"
label(ipe$sec_long_BL) = "Secondary adjusted enrollment ratio (%) total population [BL]"
label(ipe$syr_F_long_BL) = "Secondary years of schooling, female population ages 15-64 [BL]"
label(ipe$syr_long_BL) = "Secondary years of schooling, population ages 15-64 [BL]"
label(ipe$tyr_F_long_BL) = "Total years of schooling, female population ages 15-64 [BL]"
label(ipe$tyr_long_BL) = "Total years of schooling, population ages 15-64 [BL]"
label(ipe$yr_sch_1519_BL) = "Years of Schooling Ages 15 to 19 [BL]"
label(ipe$yr_sch_25999_BL) = "Years of Schooling Ages 25 and up [BL]"
label(ipe$yr_sch_F_1519_BL) = "Years of Schooling Females Ages 15 to 19 [BL]"
label(ipe$yr_sch_F_25999_BL) =  "Years of Schooling Females Ages 25 and up [BL]"
label(ipe$yr_sch_sec_1519_BL) = "Years of Secondary Schooling Ages 15 to 19 [BL]"
label(ipe$yr_sch_sec_25999_BL) =  "Years of Secondary Schooling Ages 25 and up [BL]"
label(ipe$yr_sch_sec_F_1519_BL) = "Years of Secondary Schooling Females Ages 15 to 19 [BL]"
label(ipe$yr_sch_sec_F_25999_BL) = "Years of Secondary Schooling Females Ages 25 and up [BL]"

label(ipe$egip_groups_count_EP) = "Number of EGIP groups [EP]"
label(ipe$excl_groups_count_EP) = "Number of MEG groups [EP]"
label(ipe$regaut_groups_count_EP) = "Number of groups with regional autonomy [EP]"
label(ipe$regaut_excl_groups_count_EP) = "Number of MEG groups with regional autonomy [EP]"
label(ipe$regaut_egip_groups_count_EP) = "Number of EGIP groups with regional autonomy [EP]"
label(ipe$rlvt_groups_count_EP) = "Number of relevant groups [EP]"
label(ipe$actv_groups_count_EP) = "Number of active groups [EP]"
label(ipe$lpop_EP) = "Sum of ethnically relevant population as fraction of total population [EP]"
label(ipe$egippop_EP) = "Sum of the population of all EGIP groups as fraction of total population [EP]"
label(ipe$legippop_EP) = "EGIP population as a fraction of ethnically relevant population [EP]"
label(ipe$exclpop_EP) = "Sum of the population of all MEG groups as fraction of total population [EP]"
label(ipe$lexclpop_EP) = "MEG population as a fraction of ethnically relevant population [EP]"
label(ipe$discrimpop_EP) = "Sum of discriminated population as fraction of total population [EP]"
label(ipe$ldiscrimpop_EP) = "Sum discriminated population as fraction of ethnically relevant population [EP]"
label(ipe$maxexclpop_EP) = "Size of largest MEG group as fraction of total population [EP]"
label(ipe$lmaxexclpop_EP) = "Size of largest MEG group as fraction of ethnically relevant population [EP]"
label(ipe$regautpop_EP) = "Sum of population with regional autonomy as fraction of total population [EP]"
label(ipe$regautexclpop_EP) = "Sum pop. with regional autonomy and excluded MEG as fraction of total pop. [EP]"
label(ipe$regautegippop_EP) = "Sum pop. with regional autonomy and included EGIP as fraction of total pop. [EP]"
label(ipe$cntr_relevance_EP) = "Is ethnicity relevant at least once in sample period (0=No, 1=Yes) [EP]"

label(ipe$burg_count_ODC) = "Burglary breaking and entering at the national level, number of police recorded offenses [ODC]"
label(ipe$burg_rate_ODC) = "Burglary breaking and entering at the national level, number of police recorded offenses per 10,000 of population [ODC]"
label(ipe$homicide_count_ODC) = "Intentional homicides [ODC]"
label(ipe$homicide_rate_ODC) = "Intentional homicides per 10,000 of population [ODC]"
label(ipe$housebrk_count_ODC) = "Housebreaking at the national level, number of police recorded offences [ODC]"
label(ipe$housebrk_rate_ODC) = "Housebreaking at the national level, number of police recorded offences per 10,000 of population [ODC]"
label(ipe$mvtheft_count_ODC) = "Motor Vehicle Theft at the national level, number of police-recorded offences [ODC]"
label(ipe$mvtheft_rate_ODC) = "Motor Vehicle Theft at the national level, number of police-recorded offences per 10,000 of population [ODC]"
label(ipe$robbery_count_ODC) = "Robbery at the national level, number of police-recorded offences [ODC]"
label(ipe$robbery_rate_ODC) = "Robbery at the national level, number of police-recorded offences per 10,000 of population [ODC]"
label(ipe$hommicidempc_count_ODC) = "Intentional homicide count in the most populated city [ODC]"
label(ipe$hommicidempc_rate_ODC) = "Intentional homicide rate per 10,000 of population in the most populated city [ODC]"
label(ipe$mpcity_ODC) = "The most populated city in each country [ODC]"
label(ipe$theft_count_ODC) = "Theft at the national level, number of police-recorded offences [ODC]"
label(ipe$theft_rate_ODC) = "Theft at the national level, number of police-recorded offences per 10,000 of population [ODC]"
label(ipe$theftcars_count_ODC) = "Theft of Private Cars, number of police recorded offences [ODC]"
label(ipe$theftcars_rate_ODC) = " Theft of Private Cars, police recorded offences per 10,000 of population [ODC]"

label(ipe$indep_RCS) = "State is independent on Dec. 31 of observed year [RCS]"
label(ipe$relmajid_RCS) = "ID code of religion of majority of population [RCS]"
label(ipe$chrpop_RCS) = "Population of Christians [RCS]"
label(ipe$chrpct_RCS) = "Percentage of Christians [RCS]"
label(ipe$jewpop_RCS) = "Population of Jews [RCS]"          
label(ipe$jewpct_RCS) = "Percentage of Jews [RCS]"             
label(ipe$muspop_RCS) = "Population of Muslims [RCS]"
label(ipe$muspct_RCS) = "Percentage of Muslims [RCS]"
label(ipe$hinpop_RCS) = " Population of Hindus (Insufficient data or numbers to subdivide) [RCS]"
label(ipe$hinpct_RCS) = "Percentage of Hindus [RCS]"  
label(ipe$budpop_RCS) = "Population of Buddhists (all branches and schools of Buddhism combined) [RCS]" 
label(ipe$budpct_RCS) = "Percentage of Buddhists [RCS]"
label(ipe$eacomppp_RCS) = "Population of East Asian Religious Complex [RCS]"
label(ipe$eacomppt_RCS) = "Percentage of East Asian Religious Complex [RCS]"
label(ipe$shipop_RCS) = "Population of Shintoists (insufficient data or numbers to subdivided) [RCS]"
label(ipe$shipct_RCS) = "Percentage of Shintoists [RCS]"
label(ipe$cnfpop_RCS) = "Population of Confucianists (insufficient data or numbers to subdivide) [RCS]" 
label(ipe$cnfpct_RCS) = "Percentage of Confucianists [RCS]"
label(ipe$taopop_RCS) = "Population of Taoists (insufficient data or numbers to subdivide) [RCS]" 
label(ipe$taopct_RCS) = "Percentage of Taoists [RCS]"
label(ipe$indpop_RCS) = "Population of Indigenous Religionists [RCS]"
label(ipe$indpct_RCS) = "Percentage of Indigenous Religionists [RCS]"
label(ipe$notrelpp_RCS) = "Population of Not Religious [RCS]"
label(ipe$notrelpt_RCS) = "Percentage of Not Religious [RCS]"
label(ipe$unkpop_RCS) = "Population of Unknown [RCS]"
label(ipe$unkpct_RCS) = "Percentage of Unknown [RCS]" 

label(ipe$EmigrantStock_EMS) = "Total emmigrant stock from [UN]"

label(ipe$lnislands_AP) = "Number of islands (ppl > 100000) logged [Archipelagos]"
label(ipe$archipelago_AP) = "At least two islands"
label(ipe$comments_AP) = "List of the islands in each country"
label(ipe$numislands_AP) = "Number of Islands (ppl > 100000)"

# Change the order of all columns so that country, year, gwno shows up at the front #####
allColnames <- colnames(ipe)
length(allColnames) #834
mainVector <- c("country", "year","gwno", "ccode", "ifscode", "ifs", "gwabbrev")
leftCol <- allColnames[!allColnames %in% mainVector]
newCol <- c(mainVector, leftCol)
ipe <- ipe[,newCol]

# Convert year from character to numeric #####
ipe$year <- as.numeric(as.character(ipe$year))
ipe$ccode <- as.numeric(as.character(ipe$ccode))
ipe$ifscode <- as.numeric(as.character(ipe$ifscode))

# Order the IPE by gwno and year #####
ipe = ipe[order(ipe$gwno, ipe$year),]




# Save an R version #####
save(ipe,file=paste(preppeddata,"../Graham_Tucker_IPE_v2_0.RDATA",sep=""))

# To save the dta version, we must convert to a Stata format #####
for (colname in names(ipe)) {
  if (is.character(ipe[[colname]])) {
    ipe[[colname]] <- as.factor(ipe[[colname]])
  }
}

# Save a dta version #####
write.dta(ipe, "/Users/user/Google Drive/Master IPE Data/Graham_Tucker_IPE_v2_0_NoLabel.dta")

# Creating a COW version #####
cow <- ipe

# Remove NA's #####
cow<- cow[!(is.na(cow$ccode)),]
sum(is.na(cow$ccode))

# Check for duplicates #####
duplicates <- data.frame(table(cow$ccode, cow$year))
print(duplicates[duplicates$Freq > 1,])

# Handle Yugoslavia (gwno separates yugoslavia by year, but COW does not) #####
cow<- cow [!(cow$gwno== 340 & cow$year>1970 & cow$year<2007),]
cow <- cow[!(cow$gwno==345 & cow$year > 2006),]
cow <- cow[!(cow$gwno==345 & cow$year > 1917 &  cow$year <= 1970),]

# Order by ccode and year #####
cow = cow[order(cow$ccode, cow$year),]

# Save an R version #####
save(cow,file=paste(preppeddata,"../Graham_Tucker_IPE_v2_0_COW.RDATA",sep=""))

# To save the dta version, we must convert to a Stata format #####
for (colname in names(cow)) {
  if (is.character(cow[[colname]])) {
    cow[[colname]] <- as.factor(cow[[colname]])
  }
}

# Save a dta Version #####
write.dta(cow, "/Users/user/Google Drive/Master IPE Data/Graham_Tucker_IPE_v2_0_NoLabel_COW.dta")
