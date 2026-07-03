/*====== Run SC Estimate for NFILS Data =========*/

cap cd "D:/Dropbox (University of Oregon)/OregonDrugs" 

cap cd "C:/Users/CHEPS Laptop 1/University of Oregon Dropbox/Kyutaro Matsuzawa/OregonDrugs"


// Load NFILS 
import delimited "fentanyl_oregon_replication_dataverse/data//nflis_state_half_panel_2025.csv", clear

// Prep Data
statastates, ab(st)

// Create Important Variables
gen fent_pct = fent_count / alldrugs_seizure_count * 100
replace fent_all_opioid_share = fent_all_opioid_share * 100
gen fent_pc =  pc_fent
gen nonfent_pc = (alldrugs_seizure_count - fent_count) /population * 100000
gen opioid_pc = (opioid_seizure_count - fent_count) /population * 100000
gen drug_pc =  alldrugs_seizure_count /population * 100000

keep if year>=2011 & year<=2023
gen time = (year-2011)*2 + half
gen controls = 1 if state_fips != 41 & state_fips!= 53 

xtset state_fips time

foreach y of varlist fent_pct fent_pc drug_pc nonfent_pc fent_all_opioid_share opioid_pc {
	// Get controls


	*********** Run Main Synth ************
	foreach trunit of numlist 41 53 {
	preserve
		// Exclude the other treated state
		keep if controls==1 | state_fips==`trunit'

		// Get Locals Ready
		local trtime = 21
		local match = ""
		foreach i of numlist 1(1)`trtime' {
			if `i' < `trtime' local match = "`match' `y'(`i')"  
		}

		// Run Everything!
		synth `y' `match' , trunit(`trunit') trperiod(`trtime')  keep("Estimate/Synth/NFIL/Synth_`y'_`trunit'", replace)
		restore
	}

	foreach trunit of numlist 41 53 {
	preserve
		
		// Exclude the other treated state
		keep if controls==1 | state_fips==`trunit'

		// Get Locals Ready
		local trtime = 21
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
		export delimited "Estimate/Synth/NFIL/Placebos_`y'_`trunit'.csv", replace
	restore
	}


}
