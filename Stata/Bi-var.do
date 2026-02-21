/*****************************************************************
 Purpose:
   1) Bivariate analysis (row % + Pearson chi-square)
   2) Survey-weighted logistic regression (Models I–IV)
   3) Multilevel logistic regression (Null + I–IV)
   4) Multicollinearity check using VIF (Model IV)
*****************************************************************/

*------------------------------*
* Survey setup
*------------------------------*
svyset v001 [pweight=wt], strata(v023)

*------------------------------*
* 0) Outcome check
*------------------------------*
drop if missing(unmet_need)

*==================================================*
* 1) BIVARIATE ANALYSIS
*==================================================*

*------------------------------*
* 1.1 SWPER EMPOWERMENT DOMAINS
*------------------------------*
* (Side note: Always start with main exposure variables)
svy: tab att_cat unmet_need, row percent count format(%4.1f)
svy: tab aut_cat unmet_need, row percent count format(%4.1f)
svy: tab dec_cat unmet_need, row percent count format(%4.1f)


*------------------------------*
* 1.2 INDIVIDUAL CHARACTERISTICS
*------------------------------*
* (Side note: Life-course → Socioeconomic → Behavioural → Partner)
svy: tab age unmet_need, row percent count format(%4.1f)
svy: tab child_cat unmet_need, row percent count format(%4.1f)

svy: tab working_stat unmet_need, row percent count format (%4.1f)
svy: tab wealth_index unmet_need, row percent count format(%4.1f)
svy: tab religion unmet_need, row percent count format(%4.1f)



*------------------------------*
* 1.3 HOUSEHOLD & COMMUNITY
*------------------------------*
* (Side note: Move from micro → macro environment)
svy: tab sex_hh unmet_need, row percent count format(%4.1f)
svy: tab dist_facility unmet_need, row percent count format(%4.1f)
svy: tab place_of_res unmet_need, row percent count format(%4.1f)
svy: tab division unmet_need, row percent count format(%4.1f)

svy: tab comm_edu unmet_need, row percent count format(%4.1f)
svy: tab comm_wealth unmet_need, row percent count format(%4.1f)
svy: tab comm_fp_expo unmet_need, row percent count format(%4.1f)