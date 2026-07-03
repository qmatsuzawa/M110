/*========= Run SC Estimate: UCR Drug Arrest ============*/
cap cd "D:/Dropbox (University of Oregon)/OregonDrugs" 

cap cd "/Users/kmatsuzawa2/Library/CloudStorage/GoogleDrive-kmatsuzawa2@sdsu.edu/My Drive/OregonDrugs"

// Get Data
import delimited "Data/Cleaned/Crime/UCR_Total_Drug.csv", clear
gen time = monthno + (year-2018)*12 
xtset state_fips time

drop if state_fips==11 // Drop DC (since agency is zero pop agency)
keep if year<=2023 // Focus on our sample period (pre-recrim)

// Get controls
gen controls = 1 if state_fips != 41 & state_fips!= 53

// Get outcome
foreach y of varlist drug_all {
	replace `y'=`y'/pop*100000
}

*********** Run Main Synth ************
foreach y of varlist drug_all {
foreach trunit of numlist 41 53 {
	

	if `trunit'==41 local treatmo "1 2"
	if `trunit'==53 local treatmo "2 3"
	foreach startmo of local treatmo {
	preserve
		// Exclude the other treated state
		keep if controls==1 | state_fips==`trunit'

		// Get Locals Ready
		local trtime = `startmo' + (2021-2018)*12
		local match = ""
		foreach i of numlist 1(1)`trtime' {
			if `i' < `trtime' local match = "`match' `y'(`i')"  
		}

		// Run Everything!
		synth `y' `match' , trunit(`trunit') trperiod(`trtime') keep("Estimate/SC_Main/Crime/Synth_`y'_`trunit'_start`startmo'", replace)
	restore
	}


	foreach startmo of local treatmo {
	preserve
		
		// Exclude the other treated state
		keep if controls==1 | state_fips==`trunit'

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
		export delimited "Estimate/SC_Main/Crime/Placebos_`y'_`trunit'_start`startmo'.csv", replace
	restore
	}
}
}
