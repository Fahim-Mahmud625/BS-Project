*------------------------------*
* Survey setup
*------------------------------*
svyset v001 [pweight=wt], strata(v023)

*------------------------------*
* 0) Outcome check
*------------------------------*
drop if missing(unmet_need)

* Univariate Analysis

svy: tab unmet_need, count cell format(%4.1f)

svy: tab att_cat, count cell format(%4.1f)
svy: tab aut_cat, count cell format(%4.1f)
svy: tab dec_cat, count cell format(%4.1f)

svy: tab age, count cell format(%4.1f)
svy: tab child_cat, count cell format(%4.1f)
svy: tab working_stat, count cell format(%4.1f)
svy: tab wealth_index, count cell format(%4.1f)
svy: tab religion, count cell format(%4.1f)

svy: tab sex_hh, count cell format(%4.1f)
svy: tab dist_facility, count cell format(%4.1f)
svy: tab place_of_res, count cell format(%4.1f)
svy: tab division, count cell format(%4.1f)
svy: tab comm_edu, count cell format(%4.1f)
svy: tab comm_wealth, count cell format(%4.1f)
svy: tab comm_fp_expo, count cell format(%4.1f)