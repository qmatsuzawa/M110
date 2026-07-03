/*========= Estimate SC Robustness: Various Random Imputation =============*/

cap cd "D:/Dropbox (University of Oregon)/OregonDrugs" 

cap cd "C:/Users/CHEPS Laptop 1/Dropbox (University of Oregon)/OregonDrugs"

local seed =12345


*********** Prep Data ***********
// Get Data
import delimited "Data/Cleaned/NVSS_Mortality_MainRegReady.csv", clear
xtset state_fips time

// Get controls
gen controls = 1 if state_fips != 41 & state_fips!= 53

// Convert into rate per 100,000
tempfile data
save `data'

********************** Each Definition ****************************
foreach y of varlist spencer joshi {

*********** Prep Data ***********
	// Get Data
	use `data', clear
	gen od_rate = `y'/pop * 100000

	*********** Run Main Synth ************
	foreach trunit of numlist 41 53 {
		local startmonth = "1 2"
		if `trunit'==53 local startmonth = "2 3"
		foreach startmo of local startmonth  {
			preserve
			
			// Exclude the other treated state
			keep if controls==1 | state_fips==`trunit'

			// Get Locals Ready
			local trtime = `startmo' + (2021-2018)*12
			local match = ""
			foreach i of numlist 1(1)`trtime' {
				if `i' < `trtime' local match = "`match' od_rate(`i')"  
			}
			
			// Loop over 
			foreach i of numlist 1(1)1000 {
				// Impute Missing Data
				local seedno = `i' * `seed'
				set seed `seedno'
				cap drop tempvar 
				gen tempvar = runiformint(1,9)
				qui replace od_rate = tempvar/pop*100000 if `y'<=9
				tempfile placebo`i'
				qui synth od_rate `match' , trunit(`trunit') trperiod(`trtime') keep(`placebo`i'')
				di `i'
			}

			clear 
			gen unit = .
			foreach i of numlist 1(1)1000 {
			append using `placebo`i''
			qui replace unit = `i' if unit==.
			}
			gen trtime = `trtime'
			export delimited "Estimate/SC_Main/Robustness/Robust_Imputation_`y'_`trunit'_start`startmo'.csv", replace
		restore
		}
	}
}

