/*===== Run SC Estimate: Restrict Sample to Late Arrivers========*/

cap cd "D:/Dropbox (University of Oregon)/OregonDrugs" 
cap cd "C:/Users/CHEPS Laptop 1/\University of Oregon Dropbox/Kyutaro Matsuzawa/OregonDrugs"
cap cd "/Users/kmatsuzawa2/Google Drive/My Drive/OregonDrugs/"

*********** Prep Data ***********

//Get Structural Break Data
import delimited "fentanyl_oregon_replication_dataverse/data/changepoint_state_amoc_nflis_fentanyl_percent", clear
statastates, ab(st) nogen
keep state_fips changepoint date
ren date date_break
tempfile break
save `break'

// Import NVSS data
import delimited "Data/Cleaned/NVSS_Mortality_MainRegReady.csv", clear
merge m:1 state_fips using `break'

// Get controls
gen controls = 1 if state_fips != 41 & state_fips!= 53

// Convert into rate per 100,000
replace spencer = spencer/pop * 100000
replace joshi = joshi/pop * 100000
xtset state_fips time

*local controlvar = "fent_pc"
*local controlvar = "fent_pct"

********************** Each Definition ****************************

*********** Run Main Synth ************
foreach breakpoint of numlist 31 33 {
foreach y of varlist spencer joshi {

	foreach trunit of numlist 41 53 {
	local startmo = 2
	if `trunit'==53 local startmo=3
	preserve
		// Exclude the other treated state
		keep if controls==1 | state_fips==`trunit' 
		keep if changepoint >= `breakpoint'

		// Get Locals Ready
		local trtime = `startmo' + (2021-2018)*12
		local match = ""
		foreach i of numlist 1(1)`trtime' {
			if `i' < `trtime' local match = "`match' `y'(`i')"  
		}
			
		// Run Everything!
		synth `y' `match' , trunit(`trunit') trperiod(`trtime') keep("Estimate/SC_Main/Robustness/Synth_`y'_`trunit'_Breakpoint_`breakpoint'", replace)
	restore
	}
}
}



******** Run Placebos **********
foreach breakpoint of numlist 31 33 {
foreach y of varlist spencer joshi {
foreach trunit of numlist 41 53 {
local startmo = 2
if `trunit'==53 local startmo=3
preserve
	
	// Exclude the other treated state
	keep if controls==1 
	keep if changepoint >= `breakpoint'

	// Get Locals Ready
	local trtime = `startmo' + (2021-2018)*12
	local match = ""
	foreach i of numlist 1(1)`trtime' {
		if `i' < `trtime' local match = "`match' `y'(`i')"  
	}

	// Run Everything!
	replace controls = controls*state_fips
	levelsof controls, local(controlstates)
	foreach i of local controlstates {
	tempfile placebo`i'
	qui synth `y' `match' , trunit(`i') trperiod(`trtime') keep(`placebo`i'')
	}

	clear 
	gen unit = .
	foreach i of local controlstates {
	append using `placebo`i''
	replace unit = `i' if unit==.
	}
	gen trtime = `trtime'
	export delimited "Estimate/SC_Main/Robustness/Placebos_`y'_`trunit'_Breakpoint_`breakpoint'.csv", replace
restore
}
}
}
