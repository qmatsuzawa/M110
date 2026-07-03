/*========= Estimate SC Robustness: Estimate Placebo West Coast =============*/
cap cd "D:/Dropbox (University of Oregon)/OregonDrugs" 

cap cd "C:/Users/CHEPS Laptop 1/University of Oregon Dropbox/Kyutaro Matsuzawa/OregonDrugs"

*********** Prep Data ***********
// Get Data
import delimited "Data/Cleaned/NVSS_Mortality_MainRegReady.csv", clear
xtset state_fips time

// Get controls
gen controls = 1 if state_fips != 41 & state_fips!= 53

// Convert into rate per 100,000
replace spencer = spencer/pop * 100000
replace joshi = joshi/pop * 100000

*********** Run Main Synth ************

foreach y of varlist joshi spencer {
foreach startmo of numlist 1 2 3 {
preserve
	
	// Exclude the other treated state
	keep if controls==1 

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
	export delimited "Estimate/SC_Main/Robustness/Placebos_`y'_West_start`startmo'.csv", replace
restore
}
}
