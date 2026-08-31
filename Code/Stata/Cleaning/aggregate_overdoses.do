


******First Read and Create all Variables 



forvalues x=2003/2023 {

use "E:\NCHS\death\mortality_`x'", clear
*use "E:\NCHS\death\mortality_2008", clear

rename county_fips county_occ

statastates, abbreviation(state_occ)
gen fipscode=county_occ+state_fips*1000

generate time = ym(year, month)
format time %tm

capture drop _merge

merge m:1 fipscode using "E:\University of Oregon Dropbox\Ben Hansen\hansenmooreolney\importing_paper\3_data_background\urbanicity\NCHS_metro_county.dta"

drop _merge 

gen total_death = 1

merge m:1 fipscode time using  "E:\NCHS\Cleaning NCHS\county_fips_`x'.dta"

tab _merge 

keep if year==`x'

tab year

tab _merge 

 
replace total_death=0 if total_death==. 


gen overdose = 0

foreach code in X40 X41 X42 X43 X44 X60 X61 X62 X63 X64 X85 Y10 Y11 Y12 Y13 Y14 {
    replace overdose = 1 if ucod == "`code'"
}


/*** Overdoses
Definition of overdoses:
- X40-X44, X60-64, X85, or Y10-Y14

- versions of opioids with and without T40.6
*/
gen opioid=0

gen deaths= overdose
***********************************************************
*** poisonings - all
label variable deaths "poisonings all"
rename deaths od_all


************************************************************
*** poisonings - opioids T40.0 T40.1 T40.2 T40.3 T40.4 T40.6
gen deaths=0
forvalues i = 1/20 {
    replace deaths = 1 if inlist(enicon_`i', "T400", "T401", "T402", "T403", "T404", "T406")
}


replace deaths=0 if overdose==0


label variable deaths "poisonings with T40.0, T40.1, T40.2, T40.3, T40.4 or T40.6"
rename deaths od_opioids


**************************
*** poisonings - heroin T40.1
gen deaths=0

forvalues i = 1/20 {
    replace deaths = 1 if enicon_`i' == "T401"
}

replace deaths=0 if overdose==0


label variable deaths "poisonings with T40.1"
rename deaths od_heroin


**************************
*** poisonings - oxy T40.2

gen deaths=0

forvalues i = 1/20 {
    replace deaths = 1 if enicon_`i' == "T402"
}

replace deaths=0 if overdose==0

label variable deaths "poisonings with T40.2"
rename deaths od_oxy


**************************
*** poisonings - methadone T40.3

gen deaths=0

forvalues i = 1/20 {
    replace deaths = 1 if enicon_`i' == "T403"
}

replace deaths=0 if overdose==0

label variable deaths "poisonings with T40.3"
rename deaths od_methadone


*******************************
*** poisonings - fentanyl T40.4

gen deaths=0

forvalues i = 1/20 {
    replace deaths = 1 if enicon_`i' == "T404"
}

replace deaths=0 if overdose==0

label variable deaths "poisonings with T40.4"
rename deaths od_fentanyl


*******************************
*** poisonings - methamphetamine/other psychostimulants T43.6

gen deaths=0

forvalues i = 1/20 {
    replace deaths = 1 if enicon_`i' == "T436"
}

replace deaths=0 if overdose==0

label variable deaths "poisonings with T43.6"
rename deaths od_meth


******************************************
*** poisonings - opioids without heroin T40.1

gen deaths=0

forvalues i = 1/20 {
       replace deaths = 1 if inlist(enicon_`i', "T400", "T402", "T403", "T404", "T406")
}

replace deaths=0 if overdose==0

label variable deaths "poisonings with T40.0, T40.2, T40.3, T40.4 or T40.6"
rename deaths od_opioids_wo_heroin


******************************************
*** poisonings - opioids without oxy T40.2

gen deaths=0

forvalues i = 1/20 {
       replace deaths = 1 if inlist(enicon_`i', "T400", "T401", "T403", "T404", "T406")
}

replace deaths=0 if overdose==0

label variable deaths "poisonings with T40.0, T40.1, T40.3, T40.4 or T40.6"
rename deaths od_opioids_wo_oxy


******************************************
*** poisonings - opioids without oxy T40.3
gen deaths=0

forvalues i = 1/20 {
       replace deaths = 1 if inlist(enicon_`i', "T400", "T401", "T402", "T404", "T406")
}

replace deaths=0 if overdose==0


label variable deaths "poisonings with T40.0, T40.1, T40.2, T40.4 or T40.6"
rename deaths od_opioids_wo_methadone


***********************************************
*** poisonings - opioids without fentanyl T40.4

gen deaths=0

forvalues i = 1/20 {
       replace deaths = 1 if inlist(enicon_`i', "T400", "T401", "T402", "T403", "T406")
}

replace deaths=0 if overdose==0


label variable deaths "poisonings with T40.0, T40.1, T40.2, T40.3 or T40.6"
rename deaths od_opioids_wo_fentanyl


******************************************************
*** poisonings - opioids T40.0 T40.1 T40.2 T40.3 T40.4

gen deaths=0

forvalues i = 1/20 {
       replace deaths = 1 if inlist(enicon_`i', "T400", "T401", "T402", "T403", "T404")
}

replace deaths=0 if overdose==0



label variable deaths "poisonings with T40.0, T40.1, T40.2, T40.3 or T40.4 - conservative opioids defn."
rename deaths od_opioids_wo_t406


******************************************************
*** Creating overdoses only involving one type of drug
gen od_t400=0
gen od_t406=0
gen other_t36_t50=0
forvalues i = 1/20 {
    replace od_t400 = 1 if enicon_`i' == "T400"
    replace od_t406 = 1 if enicon_`i' == "T406"
    replace other_t36_t50 = 1 if substr(enicon_`i',1,1)=="T" ///
        & inrange(real(substr(enicon_`i',2,3)),360,509) ///
        & enicon_`i' != "T436"
}
replace od_t400=0 if overdose==0
replace od_t406=0 if overdose==0
replace other_t36_t50=0 if overdose==0

gen od_oxy_only = od_oxy == 1 & od_heroin == 0 & od_methadone == 0 & ///
    od_fentanyl == 0 & od_t400 == 0 & od_t406 == 0
gen od_heroin_only = od_heroin == 1 & od_oxy == 0 & od_methadone == 0 & ///
    od_fentanyl == 0 & od_t400 == 0 & od_t406 == 0
gen od_methadone_only = od_methadone == 1 & od_oxy == 0 & od_heroin == 0 & ///
    od_fentanyl == 0 & od_t400 == 0 & od_t406 == 0
gen od_fentanyl_only = od_fentanyl == 1 & od_oxy == 0 & od_heroin == 0 & ///
    od_methadone == 0 & od_t400 == 0 & od_t406 == 0
gen od_t406_only = od_t406 == 1 & od_oxy == 0 & od_heroin == 0 & ///
    od_methadone == 0 & od_fentanyl == 0 & od_t400 == 0
gen od_meth_only = od_meth == 1 & other_t36_t50 == 0

local grp oxy heroin methadone fentanyl t406
foreach var of local grp {
    label variable od_`var'_only "poisonings where `var' is only opioid mentioned"
}
label variable od_meth_only "poisonings where T43.6 is the only T36-T50 drug mention"
summ




********************************************************************************
********************************************************************************
********************************************************************************

/*** Other causes of death

All deaths
- Non-overdose all deaths - drop X40-X44, X60-64, X85, or Y10-Y14
Cancer C00-C97
Lung cancer C33-C34
Major cardiovascular disease I00-I78
Heart disease I00-I09, I11, I13, I20-I51
Ischemic heart disease I20-I25
Heart attack I21-I22
Stroke I60-I69
COPD J40-J47
Cirrhosis K70, K73-K74
Alcoholic liver disease K70
Accidents V01-X59 Y85-Y86
- Non-overdose accidents - drop X40-X44, X60-64, X85, or Y10-Y14
Motor vehicle accidents  
Suicide U03 X60-X84 Y87.0
Gun-related suicide X72-X74
Non-gun suicide U03 X60-X71 X75-X84 Y87.0
- Non-overdose suicide - drop X60-64
Non-gun, non-drug suicide U03 X65-X71 X75-X84 Y87.0
- Non-overdose non-gun suicide - drop X60-64
Homicide U01-U02 X85-Y09 Y87.1
Gun-related homicide U01.4, X93-X95
Non-gun homicide U01.0-U01.3 U01.5-U01.9 U02 X85-X92 X96-Y09 Y87.1

*/

* All deaths
gen deaths=(total_death==1)
label variable deaths "all causes of death"
rename deaths allcauses


* Cancer C00-C97

gen deaths = (substr(ucod,1,1)=="C")
replace deaths=0 if inlist(substr(ucod,1,3),"C98","C99")
label variable deaths "all cancers"
rename deaths cancer 


* Lung cancer C33-C34

gen deaths = inlist(substr(ucod,1,3),"C33","C34")
label variable deaths "lung cancer"
rename deaths lungcancer


* Major cardiovascular disease I00-I78

    * flag cardiovascular (I00–I78)
    gen deaths = 0
    replace deaths = 1 if substr(ucod,1,1)=="I" ///
        & inrange(real(substr(ucod,2,2)),0,78)


label variable deaths "major cardiovascular disease"
rename deaths cardiovascular


* Heart disease I00-I09, I11, I13, I20-I51
gen deaths=0

replace deaths=1 if substr(ucod,1,1)=="I" & inrange(real(substr(ucod,2,2)),0,9)
replace deaths = 1 if substr(ucod,1,3)=="I11"
replace deaths = 1 if substr(ucod,1,3)=="I13"
replace deaths = 1 if substr(ucod,1,1)=="I" & inrange(real(substr(ucod,2,2)),20,51)

label variable deaths "heart disease"

rename deaths heartdisease


* Ischemic heart disease I20-I25
gen deaths=0
label variable deaths "coronary heart disease"
replace deaths = 1 if substr(ucod,1,1)=="I" ///
        & inrange(real(substr(ucod,2,2)),20,25)
rename deaths coronaryheartdisease

* Heart attack I21-I22
gen deaths=0 
replace deaths = 1 if substr(ucod,1,1)=="I" ///
        & inrange(real(substr(ucod,2,2)),21,22)

label variable deaths "heart attack"
rename deaths heartattack


* Stroke I60-I69
gen deaths=0
replace deaths = 1 if substr(ucod,1,1)=="I" ///
        & inrange(real(substr(ucod,2,2)),60,69)

label variable deaths "stroke"
rename deaths stroke


* COPD J40-J47
gen deaths=0
replace deaths = 1 if substr(ucod,1,1)=="J" ///
        & inrange(real(substr(ucod,2,2)),40,47)
label variable deaths "COPD"
rename deaths copd


* Cirrhosis K70, K73-K74
gen deaths=0
replace deaths = 1 if substr(ucod,1,1)=="K" ///
        & inrange(real(substr(ucod,2,2)),73,74)
replace deaths = 1 if substr(ucod,1,3)=="K70"

label variable deaths "cirrhosis"
rename deaths cirrhosis


* Alcoholic liver disease K70
gen deaths=0
replace deaths = 1 if substr(ucod,1,3)=="K70"
label variable deaths "alcohol cirrhosis"
rename deaths alccirrhosis


* Accidents V01-X59 Y85-Y86
gen deaths=0 
* V01–X59
replace deaths = 1 if substr(ucod,1,1)=="V" & inrange(real(substr(ucod,2,2)), 1, 99)
replace deaths = 1 if inlist(substr(ucod,1,1),"W","X") ///
    & inrange(real(substr(ucod,2,2)),1,59)

* Y85–Y86 (late effects of accidents)
replace deaths = 1 if substr(ucod,1,1)=="Y" ///
    & inrange(real(substr(ucod,2,2)),85,86)

label variable deaths "all accidents"
rename deaths accidents



* Motor vehicle accidents  V02-V89

gen deaths=0
label variable deaths "Motor vehicle accidents"

replace deaths = 1 if substr(ucod,1,1)=="V" & inrange(real(substr(ucod,2,2)), 2, 89)

rename deaths mvaccidents



* Suicide U03 X60-X84 Y87.0
gen deaths=0 
label variable deaths "suicide"


* X60–X84
replace deaths = 1 if substr(ucod,1,1)=="X" & inrange(real(substr(ucod,2,2)),60,84)

* Y87.0
replace deaths = 1 if substr(ucod,1,3)=="Y87" & substr(ucod,4,1)=="0"

* U03 (terrorism-related)
replace deaths = 1 if substr(ucod,1,3)=="U03"

rename deaths suicide


* Non-drug suicide U03 X65-X84 Y87.0 (excluding X60-X64)

gen deaths=0 
label variable deaths "non-drug suicide"


* X60–X84
 replace deaths = 1 if substr(ucod,1,1)=="X" & inrange(real(substr(ucod,2,2)),65,84)

* Y87.0
replace deaths = 1 if substr(ucod,1,3)=="Y87" & substr(ucod,4,1)=="0"

* U03 (terrorism-related)
replace deaths = 1 if substr(ucod,1,3)=="U03"

label variable deaths "non-drug suicide"
rename deaths nondrugsuicide




* Gun-related suicide X72-X74
gen deaths=0 
label variable deaths "gun-related suicide"

* X72–X74
 replace deaths = 1 if substr(ucod,1,1)=="X" & inrange(real(substr(ucod,2,2)),72,74)

rename deaths gunsuicide

* Non-gun suicide U03 X60-X71 X75-X84 Y87.0

gen deaths=suicide-gunsuicide

label variable deaths "non-gun suicide"
rename deaths nongunsuicide


* Non-drug non-gun suicide U03 X65-X71 X75-X84 Y87.0 (excluding X60-X64)

* X60–X84
gen deaths=0
 replace deaths = 1 if substr(ucod,1,1)=="X" & inrange(real(substr(ucod,2,2)),65,71)
 replace deaths = 1 if substr(ucod,1,1)=="X" & inrange(real(substr(ucod,2,2)),75,84)

* Y87.0
replace deaths = 1 if substr(ucod,1,3)=="Y87" & substr(ucod,4,1)=="0"

* U03 (terrorism-related)
replace deaths = 1 if substr(ucod,1,3)=="U03"

label variable deaths "non-gun-non-drug-suicide"
rename deaths nongunsuicide_nondrug


* Homicide U01-U02 X85-X99 -Y09 Y87.1
gen deaths=0 
 replace deaths = 1 if substr(ucod,1,1)=="U" & inrange(real(substr(ucod,2,2)),1,2)
* X85–Y09
replace deaths = 1 if substr(ucod,1,1)=="X" & inrange(real(substr(ucod,2,2)),85,99)
replace deaths = 1 if substr(ucod,1,1)=="Y" & inrange(real(substr(ucod,2,2)),0,9)

* Y87.1 (sequelae of assault)
replace deaths = 1 if substr(ucod,1,3)=="Y87" & substr(ucod,4,1)=="1"
* U01–U02 (terrorism—homicide)
replace deaths = 1 if substr(ucod,1,1)=="U" & inrange(real(substr(ucod,2,2)),1,2)

label variable deaths "homicide"
rename deaths homicide


gen deaths=0 
* Gun-related homicide U01.4, X93-X95
replace deaths = 1 if inlist(substr(ucod,1,3),"X93","X94","X95")

* Assault by firearm
replace deaths = 1 if inlist(substr(ucod,1,3),"X93","X94","X95")

* Terrorism involving firearms (counts as homicide)
replace deaths = 1 if substr(ucod,1,3)=="U01" & substr(ucod,4,1)=="4"
label variable deaths "gun homicide"
rename deaths gunhomicide



* Non-gun homicide U01.0-U01.3 U01.5-U01.9 U02 X85-X92 X96-Y09 Y87.1

gen deaths=homicide-gunhomicide
label variable deaths "non-gun homicide"
rename deaths nongunhomicide

rename county_occ county_fips


replace year  = year(dofm(time)) if year==. 
replace month = month(dofm(time)) if month==. 



*gen year = yofd(dofm(time))
*

replace state_fips=floor(fipscode/1000) if state_fips==. 
replace county_fips=fipscode-state_fips*1000 if county_fips==. 



collapse (sum) od_all od_opioids od_oxy od_heroin od_methadone od_fentanyl od_meth od_opioids_wo_heroin ///
od_opioids_wo_oxy od_opioids_wo_fentanyl od_opioids_wo_methadone od_opioids_wo_t406 od_*_only  ///
allcauses cancer lungcancer cardiovascular heartdisease coronaryheartdisease  heartattack stroke ///
copd cirrhosis alccirrhosis accidents mvaccidents suicide gunsuicide nongunsuicide nondrugsuicide ///
nongunsuicide_nondrug homicide gunhomicide nongunhomicide, by(state_fips county_fips fipscode month year time) 

save "E:\NCHS\death\county_deaths_`x'", replace

di `x'
}



****append aggregated datasets and label variables according to Tim's preferences****

use "E:\NCHS\death\county_deaths_2003", clear
forvalues x=2004/2023{
append using "E:\NCHS\death\county_deaths_`x'.dta"
}

local grp oxy heroin methadone fentanyl t406



format time %tm
tsset fipscode time
tsfill, full 

replace state_fips=floor(fipscode/1000) if state_fips==. 
replace county_fips=fipscode-state_fips*1000 if county_fips==. 
replace year=year(dofm(time)) if year==.
replace month=month(dofm(time)) if month==.


drop if state_fips==. 
drop if county_fips==. 


foreach x in od_all od_opioids od_oxy od_heroin od_methadone od_fentanyl od_meth od_opioids_wo_heroin ///
od_opioids_wo_oxy od_opioids_wo_fentanyl od_opioids_wo_methadone od_opioids_wo_t406 ///
od_oxy_only od_heroin_only od_methadone_only od_fentanyl_only od_t406_only od_meth_only ///
 allcauses cancer lungcancer cardiovascular heartdisease coronaryheartdisease  heartattack stroke ///
copd cirrhosis alccirrhosis accidents mvaccidents suicide gunsuicide nongunsuicide nondrugsuicide ///
nongunsuicide_nondrug homicide gunhomicide nongunhomicide {

    capture confirm variable `x'
    if !_rc replace `x' = 0 if missing(`x')


}



tsset fipscode time

tsfill, full 
replace state_fips=floor(fipscode/1000) if state_fips==. 
replace county_fips=fipscode-state_fips*1000 if county_fips==. 
replace year=year(dofm(time)) if year==.
replace month=month(dofm(time)) if month==.
*label od_all od_opioids

label variable od_all "poisonings all"
label variable od_opioids  "poisonings with T40.0, T40.1, T40.2, T40.3, T40.4 or T40.6"
label variable od_oxy "poisonings with T40.2"
label variable od_heroin "poisonings with T40.1"
label variable od_methadone "poisonings with T40.3"
label variable od_fentanyl  "poisonings with T40.4"
label variable od_meth "poisonings with T43.6"
label variable od_opioids_wo_heroin "poisonings with T40.0, T40.2, T40.3, T40.4 or T40.6"
label variable od_opioids_wo_oxy "poisonings with T40.0, T40.1, T40.3, T40.4 or T40.6"
label variable od_opioids_wo_fentanyl "poisonings with T40.0, T40.1, T40.2, T40.3 or T40.6"
label variable od_opioids_wo_methadone "poisonings with T40.0, T40.1, T40.2, T40.4 or T40.6"
label variable od_opioids_wo_t406 "poisonings with T40.0, T40.1, T40.2, T40.3 or T40.4 - conservative opioids defn."

foreach var of local grp {
    capture confirm variable od_`var'_only
    if !_rc {
        label variable od_`var'_only "poisonings where `var' is only opioid mentioned"
    }
}
label variable od_meth_only "poisonings where T43.6 is the only T36-T50 drug mention"

label variable allcauses "all causes"
label variable cancer "cancer"
label variable lungcancer "lung cancer"
label variable cardiovascular "cardiovascular"
label variable heartdisease "heartdisease"
label variable coronaryheartdisease "coronary heart disease"
label variable heartattack "heart attack"
label variable stroke "stroke"
label variable copd "COPD" 
label variable cirrhosis "cirrhosis"
label variable alccirrhosis "alcoholic cirrhosis"
label variable accidents "all accidents"
label variable mvaccidents "Motor vehicle accidents"
label variable suicide "suicide" 
label variable nondrugsuicide "non-drug suicide"
label variable gunsuicide "gun-related suicide"
label variable nongunsuicide "nongunsuicide"
label variable nongunsuicide_nondrug  "non-gun suicide - non-drug"
label variable homicide "homicide"
label variable gunhomicide "gun homicide"
label variable nongunhomicide "non-gun homicide"



save "E:\NCHS\death\county_deaths_full", replace

gen frac_gun=gunhomicide/homicide
gen frac_fent=od_fentanyl/od_all

xtreg gunhomicide frac_fent i.year i.month, i(fipscode) fe
xtreg nongunhomicide frac_fent i.year i.month, i(fipscode)  fe
xtreg gunhomicide frac_fent i.year i.month, i(fipscode) fe 

xtpoisson gunhomicide frac_fent i.year, fe exposure(allcauses)
xtpoisson nongunhomicide frac_fent i.year, fe exposure(allcauses)


save "E:\NCHS\death\county_deaths_full", replace

export delimited using "E:\University of Oregon Dropbox\Ben Hansen\OregonDrugs\Data\NCHSRestricted\Overdoses_20182023.csv", replace


***Optional***Aggregate state level data into national data for gut check**** 

/*

collapse (sum) od_all od_opioids od_oxy od_heroin od_methadone od_fentanyl od_meth od_opioids_wo_heroin ///
od_opioids_wo_oxy od_opioids_wo_fentanyl od_opioids_wo_methadone od_opioids_wo_t406 od_*_only  ///
allcauses cancer lungcancer cardiovascular heartdisease coronaryheartdisease  heartattack stroke ///
copd cirrhosis alccirrhosis accidents mvaccidents suicide gunsuicide nongunsuicide nondrugsuicide ///
nongunsuicide_nondrug homicide gunhomicide nongunhomicide, by(year) 

label variable od_all "poisonings all"
label variable od_opioids  "poisonings with T40.0, T40.1, T40.2, T40.3, T40.4 or T40.6"
label variable od_oxy "poisonings with T40.2"
label variable od_heroin "poisonings with T40.1"
label variable od_methadone "poisonings with T40.3"
label variable od_fentanyl  "poisonings with T40.4"
label variable od_meth "poisonings with T43.6"
label variable od_opioids_wo_heroin "poisonings with T40.0, T40.2, T40.3, T40.4 or T40.6"
label variable od_opioids_wo_oxy "poisonings with T40.0, T40.1, T40.3, T40.4 or T40.6"
label variable od_opioids_wo_fentanyl "poisonings with T40.0, T40.1, T40.2, T40.3 or T40.6"
label variable od_opioids_wo_methadone "poisonings with T40.0, T40.1, T40.2, T40.4 or T40.6"
label variable od_opioids_wo_t406 "poisonings with T40.0, T40.1, T40.2, T40.3 or T40.4 - conservative opioids defn."

foreach var of local grp {
    capture confirm variable od_`var'_only
    if !_rc {
        label variable od_`var'_only "poisonings where `var' is only opioid mentioned"
    }
}
label variable od_meth_only "poisonings where T43.6 is the only T36-T50 drug mention"




label variable allcauses "all causes"
label variable cancer "cancer"
label variable lungcancer "lung cancer"
label variable cardiovascular "cardiovascular"
label variable heartdisease "heartdisease"
label variable coronaryheartdisease "coronary heart disease"
label variable heartattack "heart attack"
label variable stroke "stroke"
label variable copd "COPD" 
label variable cirrhosis "cirrhosis"
label variable alccirrhosis "alcoholic cirrhosis"
label variable accidents "all accidents"
label variable mvaccidents "Motor vehicle accidents"
label variable suicide "suicide" 
label variable nondrugsuicide "non-drug suicide"
label variable gunsuicide "gun-related suicide"
label variable nongunsuicide "nongunsuicide"
label variable nongunsuicide_nondrug  "non-gun suicide - non-drug"
label variable homicide "homicide"
label variable gunhomicide "gun homicide"
label variable nongunhomicide "non-gun homicide"

format year %ty

/*

*if needed imputations of 0s but doesn't appear needed at all because we include all deaths, definitely needed for county level stuff*** 

bysort state_fips: egen mode_state=mode(state_occ)
bysort state_fips: egen mode_name=mode(state_name)
bysort time: egen mode_year=mode(year)
bysort time: egen mode_month=mode(month)

replace state_occ=mode_state if state_occ==""
replace state_name=mode_name if state_name==""



rename state_fips state_fips
rename state_name state_name 
save "E:\NCHS\death\state_imp20182023", replace 
 
 



*/

