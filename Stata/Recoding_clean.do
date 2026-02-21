/*****************************************************************
 Recoding + community variables (clean + reproducible)

 EXPECTS:
   - You have already run SWPER_Global_clean.do (or equivalent)
   - Variables att_cat aut_cat dec_cat exist

 OUTPUT:
   - Creates analysis covariates + community-level variables
   - Creates unmet_need from v626a
   - Generates weight wt = v005/1,000,000
   - Keeps final analysis variables
*****************************************************************/

version 17.0
set more off

*------------------------------*
* 0) Required variables check
*------------------------------*
local reqvars att_cat aut_cat dec_cat v012 v106 v201 v190 v511 v626a v005 v025 v151 v130 v021 v023 caseid v716 v701 v730 v467d v384a v384b v384c v384d v384e v384f v384g v384h v501
foreach v of local reqvars {
    capture confirm variable `v'
    if _rc {
        di as error "Required variable `v' not found. Stop."
        exit 198
    }
}

* Restrict to partnered women (SWPER target population)
keep if inlist(v501,1)

*------------------------------*
* 1) Age group
*------------------------------*

* 15-24 -> Young Adults
* 25-34 -> Prime Reproductive Age
* 35 - 49 -> Later Reproductive Age
			
recode v012 (15/24 = 1 "15-24") (25/34 = 2 "25 - 34") (35/49 = 3 "35-49"), gen(age)

*------------------------------*
* 2) Education (level)
*------------------------------*
recode v106 (0=0 "No education") (1=1 "Primary") (2=2 "Secondary") (3=3 "Higher"), gen(educ_cat)

*------------------------------*
* 3) Occupation (from v716) – handle special codes safely
*------------------------------*
replace v716 = . if inlist(v716,99998,99999)

recode v716 ///
    (0 61 62 = 1 "Not working") ///
    (11 12 13 14 15 = 2 "Agriculture") ///
    (16 21 22 23 31 41 51 52 96 = 3 "Non-agriculture"), gen(occ_cat)


recode occ_cat (1 = 1 "NW") (2/3 = 2 "W"), gen(working_stat) 	
	
*------------------------------*
* 4) Parity
*------------------------------*
recode v201 (0=0 "No child") (1=1 "1 child") (2=2 "2 children") (3=3 "3 children") (4/max=4 "4+ children"), gen(child_cat2)

recode child_cat2 (0=0 "No child") (1=1 "1 child") (2=2 "2 children") (3/max =3 "3+ children") , gen(child_cat)

*------------------------------*
* 5) Wealth index (DHS quintiles)
*------------------------------*
*clonevar wealth_index = v190

recode v190 (1/2 = 1 "poor") (3 = 2 "middle") (4/5 = 3 "rich"), gen(wealth_index)

*------------------------------*
* 6) Early marriage (from age at first cohabitation)
*------------------------------*
* v511 is valid for partnered women in BDHS 2022; still guard against implausible codes
replace v511 = . if v511>=97
recode v511 (min/17=1 "Yes") (18/max=0 "No"), gen(early_marriage)
label var early_marriage "Married/cohabited before age 18"

*------------------------------*
* 7) FP media exposure (any source)
*------------------------------*
gen byte fp_exposed = .

replace fp_exposed = 1 if ///
    v384a==1 | v384b==1 | v384c==1 | v384d==1 | ///
    v384e==1 | v384f==1 | v384g==1 | v384h==1

replace fp_exposed = 0 if ///
    v384a==0 & v384b==0 & v384c==0 & v384d==0 & ///
    v384e==0 & v384f==0 & v384g==0 & v384h==0

label define yn01 0 "No" 1 "Yes", replace
label values fp_exposed yn01
label var fp_exposed "Exposed to FP message (any media)"


*------------------------------*
* 8) Partner age (stable categories; avoid tiny reference groups)
*------------------------------*
replace v730 = . if v730>=97
recode v730 (min/29=1 "Below 30") (30/39=2 "30–39") (40/max=3 "40+"), gen(partner_age_cat)

*------------------------------*
* 9) Partner education (level) – do NOT drop; treat DK as missing
*------------------------------*
replace v701 = . if v701==8
clonevar partner_edu_cat = v701

*------------------------------*
* 10) Cluster ID
*------------------------------*
clonevar clust = v021

*------------------------------*
* 11) Community variables (recommended: define high/low by median across clusters)
*     Note: these are *contextual* measures; dichotomization is for interpretability.
*------------------------------*

* 11a) Community education: mean years of schooling (v133 is years; if absent use v106)
* If v133 exists, prefer it
capture confirm variable v133
if !_rc {
    bys clust: egen c_mean_edu = mean(v133)
}
else {
    bys clust: egen c_mean_edu = mean(v106)
}

preserve
    keep clust c_mean_edu
    duplicates drop
    quietly summarize c_mean_edu, detail
    local med_edu = r(p50)
restore

gen byte comm_edu = (c_mean_edu >= `med_edu')
label values comm_edu yn01
label var comm_edu "Community education high (>= median cluster mean)"

* 11b) Community wealth: mean v190 by cluster (>= median)
by clust: egen c_mean_wealth = mean(v190)

preserve
    keep clust c_mean_wealth
    duplicates drop
    quietly summarize c_mean_wealth, detail
    local med_w = r(p50)
restore

gen byte comm_wealth = (c_mean_wealth >= `med_w')
label values comm_wealth yn01
label var comm_wealth "Community wealth high (>= median cluster mean)"

* 11c) Community FP exposure: proportion exposed (>= median)
by clust: egen c_prop_fp = mean(fp_exposed)

preserve
    keep clust c_prop_fp
    duplicates drop
    quietly summarize c_prop_fp, detail
    local med_fp = r(p50)
restore

gen byte comm_fp_expo = (c_prop_fp >= `med_fp')
label values comm_fp_expo yn01
label var comm_fp_expo "Community FP exposure high (>= median)"

* 11d) Distance to facility (v467d: 1 big problem, 2 not a big problem)
clonevar dist_facility = v467d
label define dist 1 "big problem" 2 "not a big problem", replace
label values dist_facility dist
label var dist_facility "Distance to health facility problem"

gen byte bigprob = (dist_facility==1)
by clust: egen c_prop_bigprob = mean(bigprob)

preserve
    keep clust c_prop_bigprob
    duplicates drop
    quietly summarize c_prop_bigprob, detail
    local med_dist = r(p50)
restore

gen byte comm_dist_facility = (c_prop_bigprob >= `med_dist')
label values comm_dist_facility yn01
label var comm_dist_facility "Community distance barrier high (>= median)"

*------------------------------*
* 12) Living with partner (if needed)
*------------------------------*
* v504: currently living with husband/partner
capture confirm variable v504
if !_rc {
    gen byte living_partner = (v504==1)
    label values living_partner yn01
    label var living_partner "Currently living with partner"
}

*------------------------------*
* 13) Unmet need (v626a)
* Codes in BDHS 2022 married women sample observed: 1,2 unmet need; 3,4 using; 7 no unmet
*------------------------------*
drop if inlist(v626a,8,9,0)

gen byte unmet_need = .
replace unmet_need = 1 if inlist(v626a,1,2)
replace unmet_need = 0 if inlist(v626a,3,4,7)
label values unmet_need yn01
label var unmet_need "Unmet need for family planning (v626a)"

*------------------------------*
* 14) Sampling weight
*------------------------------*
gen double wt = v005/1000000
label var wt "Women sample weight (v005/1,000,000)"

*------------------------------*
* 15) Residence, sex of HH head, religion (binary for analysis)
*------------------------------*
recode v025 (1=1 "Urban") (2=2 "Rural"), gen(place_of_res)
label var place_of_res "Place of residence"

recode v151 (1=1 "Male") (2=2 "Female"), gen(sex_hh)
label var sex_hh "Sex of household head"

recode v130 (1=1 "Muslim") (2/max=2 "Non Muslim"), gen(religion)
label var religion "Religion (Muslim vs Non Muslim)"

recode v632 (1 = 1 "respondent") (2 = 2 "partner") (3 = 3 "joint") (4/6 = 4 "Other"), gen(dec_for_contrap)
label var dec_for_contrap "Decision for Contraception Use"

*------------------------------*
* 16) Define final analytic sample explicitly (recommended)
*------------------------------*
drop if missing(att_cat, aut_cat, dec_cat)
drop if missing(unmet_need)

* Experienced Child Death

sum v206 v207
codebook v206 v207

gen child_death_exp = (v206 + v207 > 0)

label variable child_death_exp "Ever experienced child death"
label define yesno 0 "No" 1 "Yes"
label values child_death_exp yesno

tab child_death_exp

clonevar division = v024

*------------------------------*
* 17) Final keep
*------------------------------*
keep ///
    att_cat aut_cat dec_cat ///
    unmet_need wt ///
    age educ_cat occ_cat working_stat child_cat wealth_index ///
    early_marriage fp_exposed ///
    partner_age_cat partner_edu_cat ///
    sex_hh living_partner dist_facility place_of_res division ///
    comm_edu comm_wealth comm_fp_expo comm_dist_facility ///
    religion dec_for_contrap child_death_exp ///
    v021 v023 v024 caseid v001 v626a

compress
