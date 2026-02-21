*------------------------------------------------------------*
* 0) Survey design
*------------------------------------------------------------*
svyset v001 [pweight=wt], strata(v023)

drop if missing(unmet_need)

*------------------------------------------------------------*
* 1) Model I: Empowerment only (serially ordered)
*------------------------------------------------------------*
svy: logit unmet_need ///
    i.att_cat ///
    i.aut_cat ///
    i.dec_cat, or allbase


*------------------------------------------------------------*
* 2) Model II: Individual + partner (serially ordered)
*------------------------------------------------------------*
svy: logit unmet_need ///
	i.att_cat ///
    i.aut_cat ///
    i.dec_cat ///
    i.age ///
	i.working_stat ///
    i.wealth_index ///
    i.child_cat ///
    i.religion, or allbase


*------------------------------------------------------------*
* 3) Model III: Household + community + region (serially ordered)
*------------------------------------------------------------*
svy: logit unmet_need ///
    i.att_cat ///
    i.aut_cat ///
    i.dec_cat ///
    i.sex_hh ///
    i.dist_facility ///
    i.place_of_res ///
    i.comm_edu ///
    i.comm_wealth ///
    i.comm_fp_expo ///
    i.division, or allbase


*------------------------------------------------------------*
* 4) Model IV: Full model (main results) (serially ordered)
*------------------------------------------------------------*
svy: logit unmet_need ///
    i.att_cat ///
    i.aut_cat ///
    i.dec_cat ///
    i.age ///
	i.working_stat ///
    i.wealth_index ///
    i.child_cat ///
    i.religion ///
    i.sex_hh ///
    i.dist_facility ///
    i.place_of_res ///
    i.comm_edu ///
    i.comm_wealth ///
    i.comm_fp_expo ///
    i.division , or allbase

estat gof

	
*------------------------------------------------------------*
* 4) Model IV: Full model (main results) (serially ordered)
*------------------------------------------------------------*
melogit unmet_need ///
    i.att_cat ///
    i.aut_cat ///
    i.dec_cat ///
    i.age ///
    i.wealth_index ///
	i.working_stat ///
    i.child_cat ///
    i.religion ///
    i.sex_hh ///
    i.dist_facility ///
    i.place_of_res ///
    i.comm_edu ///
    i.comm_wealth ///
    i.comm_fp_expo ///
    i.division [pw=wt] || v021:, or allbase
	
estat icc
	