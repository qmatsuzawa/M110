/*========= Estimate SC Robustness: Dropping One State =============*/
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

// Get Control States to Loopover
replace controls = controls*state_fips
levelsof controls, local(controlstates)
	
// Tempfile to load in
tempfile maindta
save `maindta'


*********** Run Main Synth ************

foreach y of varlist joshi spencer {
foreach trunit of numlist 41 53 {
foreach startmo of numlist 1 2 {
if `trunit'==53 local startmo = `startmo'+1 // 1 month lag for WA

	// Get Locals Ready
	local trtime = `startmo' + (2021-2018)*12
	local match = ""
	foreach i of numlist 1(1)`trtime' {
		if `i' < `trtime' local match = "`match' `y'(`i')"  
	}
		
	// Loop over dropping one state at a time and recording ATT
	foreach i of local controlstates {
	
		use `maindta', clear
		// Exclude the other treated state
		keep if controls!=. | state_fips==`trunit' 
		drop if state_fips==`i'
		
		// Run Everything!
		tempfile placebo`i'
		qui synth `y' `match' , trunit(`trunit') trperiod(`trtime') keep(`placebo`i'')
	}

	clear 
	gen unit = .
	foreach i of local controlstates {
		append using `placebo`i''
		replace unit = `i' if unit==.
	}
	gen trtime = `trtime'
	export delimited "Estimate/SC_Main/Robustness/Dropone_`y'_`trunit'_start`startmo'.csv", replace
}
}
}
