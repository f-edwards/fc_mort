clear
clear mata
clear matrix
set more off
global datadir "[data folder location]"
cd "$datadir"

***********************
** RECODE AND RENAME **
***********************

import delimited using "mort_fc_20251028.csv", clear

* string to numeric *

foreach var in cm_mort mort_rt {
	replace `var'="" if `var'=="NA"
	destring `var', replace
}

encode mort_data_multi_source, gen(mort_data_multi_source2) lab(TEMP)
label drop TEMP
drop mort_data_multi_source
replace mort_data_multi_source2=mort_data_multi_source2-1
rename mort_data_multi_source2 mort_data_multi_source

* state identifiers *

rename state state_code
ssc install statastates, replace
statastates, abbrev(state_code)
encode state_name, gen(id)
sort state_fips year

* drop DC *

drop if state_code=="DC"

* economize variable names *

rename pop_child pop		// child population size
rename cm_mort y			// child maltreatment mortality count
rename fc_entered x			// foster care entry count
rename mort_rt yrt			// child maltreatment mortality rate
rename fc_entered_rt xrt	// foster care entry rate
rename pop_eduless9 z1		// % less than 9th grade education
rename pop_unemp_rt z2		// % unemployed
rename pop_poverty_rt z3	// % in poverty

* labels *

label var id "ID"
label var year "Year"
label var state_fips "State FIPS"
label var state_code "State Postal Code"
label var state_name "State Name"
label var pop "Child Pop Size"
label var y "Maltr Mort Count"
label var x "Fost Care Entry Count"
label var yrt "Maltr Mort Rate"
label var xrt "Fost Care Entry Rate"
label var z1 "% LT 9th Grade"
label var z2 "% Unemployed"
label var z3 "% in Poverty"
label var mort_data_multi_source "=1 Multi Source"

* save outfile *

global vars "id year state_fips state_code state_name pop y x yrt xrt z* mort_data_multi_source"
order $vars
keep $vars

compress
save "foster-care.dta", replace

*************************
** MULTIPLE IMPUTATION **
*************************

use "foster-care.dta", clear

* 20 imputations *

mi set flong
mi register regular id year xrt z1 z2 z3 state_fips state_code state_name
mi register imputed yrt
mi describe

mi impute chained (pmm, knn(5)) yrt = xrt z1 z2 z3 i.id i.year, add(20) rseed(20240306)

* save outfile *

compress
save "foster-care-imputed-20251206.dta", replace
export delimited "foster-care-imputed-20251206.csv", replace

******************************
** ANALYSIS OF IMPUTED DATA **
******************************

use "foster-care-imputed-20251206.dta", clear

* prep MI analysis *

mi xtset id year, yearly

mi passive: gen lnxrt=ln(xrt)
mi passive: gen lnz1=ln(z1)
mi passive: gen lnz2=ln(z2)
mi passive: gen lnz3=ln(z3)

* article table: benchmark twfe results *

mi estimate, cmdok dots: reghdfe yrt xrt z?, a(id year) vce(cluster id)

local i=1
local list " "foster care entry" "LT 9th grade" "unemployed" "in poverty" "
foreach x in xrt z1 z2 z3 {
	local var : word `i' of `list'
	local b=r(table)[1,`i']
	local se=r(table)[2,`i']
	local ll=r(table)[5,`i']
	local ul=r(table)[6,`i']
	di "`var' : " _col(25) %8.2f `b' %8.2f `se' %8.2f `ll' %8.2f `ul'
	local i=`i'+1
}

* supplement 1, etable 2: sensitivity analyses *

mi estimate, cmdok dots: reghdfe yrt xrt z?, a(id year) vce(cluster id)

mi estimate, cmdok dots: prais yrt i.id i.year xrt z?, rho(dw) vce(robust)

qui: gen _ID=state_fips
qui: mi xtset _ID year, yearly
qui: mi merge m:1 _ID using tl_2022_us_state.dta
qui: mi xeq 1: spmatrix create contiguity W if year==2020, first replace
xi: mi estimate, cmdok dots: spxtregress yrt i.year xrt z?, fe errorlag(W) force

mi estimate, cmdok dots: ppmlhdfe yrt lnxrt lnz?, a(id year) vce(cluster id)

* supplement 1, etable 3: fixed effect specifications *

local fe_spec " "" "a(id)" "a(year)" "a(id year)" "
local version " "no FEs" "unit FEs only" "year FEs only" "unit + year FEs" "
forvalues i=1/4 {
	local fe : word `i' of `fe_spec'
	local mod : word `i' of `version'
	qui: mi estimate, cmdok dots: reghdfe yrt xrt, `fe' vce(cluster id)
	local b=r(table)[1,1]
	local se=r(table)[2,1]
	local p=r(table)[4,1]
	di "`mod' : " _col(20) %8.2f `b' %8.2f `se' %10.4f `p'
	local j=`j'+1
}

* supplement 1, etable 4: data quality indicator *

mi estimate, cmdok dots: reghdfe yrt xrt z? i.mort_data_multi_source, a(id year) vce(cluster id)
