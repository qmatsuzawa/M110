/*========== Estimate SC Estimate Controlling for/Matching on Fentanyl =============*/

cap cd "D:/Dropbox (University of Oregon)/OregonDrugs" 

cap cd "C:/Users/CHEPS Laptop 1/\University of Oregon Dropbox/Kyutaro Matsuzawa/OregonDrugs"

cap cd "/Users/kmatsuzawa2/Library/CloudStorage/GoogleDrive-kmatsuzawa2@sdsu.edu/My Drive/OregonDrugs"

*********** Prep Data ***********

//Get NFILS Data
import delimited "Data/Raw/NFLIS/nflis_state_half_panel_2025.csv", clear
destring zoorob_ratio, replace force
destring fent_all_opioid_share, replace force
ren fips state_fips
tempfile nfils
save `nfils'


// Import NVSS data
import delimited "Data/Cleaned/NVSS_Mortality_MainRegReady.csv", clear
gen half = 1
replace half = 2 if  monthdth > 6
merge m:1 state_fips year half using `nfils'
xtset state_fips time

// Get controls
gen controls = 1 if state_fips != 41 & state_fips!= 53

// Convert into rate per 100,000
replace spencer = spencer/pop * 100000
replace joshi = joshi/pop * 100000

// Create New Control Variable
gen fent_pc = pc_fent
gen fent_pct = zoorob_ratio
gen fent_ratio = fent_all_opioid_share

*local controlvar = "fent_pc"
*local controlvar = "fent_pct"

********************** Each Definition ****************************

local contlist = "fent_pc fent_pct fent_ratio"
foreach controlvar of local contlist {
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
		local match = "`controlvar'(1) `controlvar'(7) `controlvar'(13) `controlvar'(19) `controlvar'(25) `controlvar'(31) `controlvar'(37) `controlvar'(43) `controlvar'(49) `controlvar'(55)" 
		foreach i of numlist 1(6)37 {
			local match = "`match' `y'(`i')"  
		}

		// Run Everything!
		synth `y' `match' , trunit(`trunit') trperiod(`trtime') keep("Estimate/SC_Main/Robustness/Synth_`y'_`trunit'_Control_`controlvar'", replace)
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
		local match = "`controlvar'(1) `controlvar'(7) `controlvar'(13) `controlvar'(19) `controlvar'(25) `controlvar'(31) `controlvar'(37) `controlvar'(43) `controlvar'(49) `controlvar'(55)" 
		foreach i of numlist 1(6)37 {
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
		export delimited "Estimate/SC_Main/Robustness/Placebos_`y'_`trunit'_Control_`controlvar'.csv", replace
	restore
	}
	}
}
