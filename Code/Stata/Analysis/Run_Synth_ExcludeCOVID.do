/*======= Run SC Estimate: No COVID-19 ========*/

cap cd "D:/Dropbox (University of Oregon)/OregonDrugs" 

cap cd "C:/Users/CHEPS Laptop 1/\University of Oregon Dropbox/Kyutaro Matsuzawa/OregonDrugs"

*********** Prep Data ***********

// Get Data
import delimited "Data/Cleaned/NVSS_Mortality_MainRegReady.csv", clear
xtset state_fips time

// Get controls
gen controls = 1 if state_fips != 41 & state_fips!= 53

// Convert into rate per 100,000
replace spencer = spencer/pop * 100000
replace joshi = joshi/pop * 100000


********************** Each Definition ****************************
foreach y of varlist spencer joshi {

*********** Prep Data ***********

	*********** Run Main Synth ************
	foreach trunit of numlist 41 53 {
	local startmo = 2
	if `trunit'==53 local startmo=3
	preserve
		// Exclude the other treated state
		keep if controls==1 | state_fips==`trunit'

		// Get Locals Ready
		local trtime = `startmo' + (2021-2018)*12
		local match = "" 
		foreach i of numlist 1(1)24 {
			local match = "`match' `y'(`i')"  
		}


		// Run Everything!
		synth `y' `match' , trunit(`trunit') trperiod(`trtime') keep("Estimate/SC_Main/Robustness/Synth_`y'_`trunit'_Control_COVID", replace)
	restore
	}


	******** Run Placebos **********
	foreach trunit of numlist 41 53 {
	local startmo = 2
	if `trunit'==53 local startmo=3
	preserve
		
		// Exclude the other treated state
		keep if controls==1 

		// Get Locals Ready
		local trtime = `startmo' + (2021-2018)*12
		local match = "" 
		foreach i of numlist 1(6)24 {
			local match = "`match' `y'(`i')"  
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
		export delimited "Estimate/SC_Main/Robustness/Placebos_`y'_`trunit'_COVID.csv", replace
	restore
	}
}
