/*************************************************************************************************
 SWPER Global (clean + reproducible)
 - Applies the official SWPER-Global scoring equations (Ewerling et al.)
 - Restricts to women's questionnaire records with DV/decision items present (sqtype==1)\n - Restricts to women currently married/in union (v501 in {1,2}) to match SWPER definition
 - Robust logging + basic checks

 Tested for BDHS 2022 IR (Stata 17).
*************************************************************************************************/

version 17.0
set more off

capture log close
log using "SWPER.log", replace

*------------------------------*
* 0) Basic checks
*------------------------------*
local reqvars v744a v744b v744c v744d v744e v157 v133 v212 v511 v743a v743b v743d v715 v730 v501 v012 sqtype caseid
foreach v of local reqvars {
    capture confirm variable `v'
    if _rc {
        di as error "Required variable `v' not found. Stop."
        exit 198
    }
}

*------------------------------*
* 1) Keep relevant records
*------------------------------*
keep if sqtype==1
keep if inlist(v501,1)   // married/in union

gen byte union = 1
label var union "Currently married or in union"

*------------------------------*
* 2) Recode missings (DHS)
*------------------------------*
* DV/decision/read items often use 9 as missing
mvdecode v744a v744b v744c v744d v744e v743a v743b v743d v157, mv(9)
* Years of schooling / age items often use 99 as missing
mvdecode v133 v715 v730, mv(99)

*------------------------------*
* 3) Beating NOT justified (1=not justified; -1=justified; 0=dk)
* DHS coding typically: 0 no, 1 yes, 8 dk
*------------------------------*
recode v744a (0=1) (1=-1) (8=0) , gen(beat1)
recode v744b (0=1) (1=-1) (8=0) , gen(beat2)
recode v744c (0=1) (1=-1) (8=0) , gen(beat3)
recode v744d (0=1) (1=-1) (8=0) , gen(beat4)
recode v744e (0=1) (1=-1) (8=0) , gen(beat5)
label define beat -1 "Yes (justified)" 0 "Don't know" 1 "No (not justified)", replace
label values beat1 beat
label values beat2 beat
label values beat3 beat
label values beat4 beat
label values beat5 beat

*------------------------------*
* 4) Decision-making (1 = respondent alone or jointly; -1 = husband/partner/other alone)
* BDHS 2022 uses: 1 respondent alone, 2 respondent+husband, 4 husband alone, 5 someone else, 6 other
*------------------------------*
recode v743a (4/7=-1) (2/3=1) (8/max=.) , gen(decide1a)
recode v743b (4/7=-1) (2/3=1) (8/max=.) , gen(decide2a)
recode v743d (4/7=-1) (2/3=1) (8/max=.) , gen(decide3a)
label define decide1 -1 "Husband/partner or other alone" 1 "Respondent alone or jointly", replace
label values decide1a decide1
label values decide2a decide1
label values decide3a decide1

*------------------------------*
* 5) Woman's education (years) + reading exposure
*------------------------------*
recode v133 (98=.) , gen(educ)
recode v157 (3=2) , gen(read)   // collapse 'almost every day' into 'at least weekly'

*------------------------------*
* 6) Age at first birth: hotdeck imputation for women without children
* Requires: ssc install hotdeck
*------------------------------*
clonevar age1cohab = v511
recode age1cohab (33/max=33), gen(age1)

capture which hotdeck
if _rc {
    di as error "Command 'hotdeck' not found. Run: ssc install hotdeck"
    exit 199
}

hotdeck v212, store by(age1) keep(caseid) imp(1)

sort caseid
preserve
    use "imp1.dta", clear
    rename v212 v212_i
    keep caseid v212_i
    tempfile imp
    save `imp', replace
restore

capture drop _merge
merge 1:1 caseid using `imp'

* If merge failed for any reason, stop (should be 3 for all)
assert _merge==3
drop _merge

clonevar age1birth = v212_i

*------------------------------*
* 7) Husband variables + differences
*------------------------------*
recode v715 (98=.) , gen(husb_educ)
recode v730 (98=.) , gen(husb_age)

gen educ_diff = educ - husb_educ
gen age_diff  = v012 - husb_age

*------------------------------*
* 8) SWPER global continuous scores
*------------------------------*
gen swpatt = ((-1.202)+(0.508*beat1)+(0.508*beat2)+(0.526*beat3)+(0.538*beat4)+(0.588*beat5)+(0.083*read)+(0.016*educ)+(-0.006*age1birth)+(-0.010*age1cohab)+(0.001*age_diff)+(0.002*educ_diff)+(0.001*decide1a)+(-0.017*decide2a)+(-0.002*decide3a))/1.811

gen swpsoc = ((-5.661)+(-0.012*beat1)+(-0.026*beat2)+(0.001*beat3)+(0.001*beat4)+(-0.015*beat5)+(0.422*read)+(0.081*educ)+(0.133*age1birth)+(0.139*age1cohab)+(0.031*age_diff)+(0.054*educ_diff)+(-0.004*decide1a)+(-0.022*decide2a)+(-0.034*decide3a))/1.526

gen swpdec = ((0.168)+(-0.003*beat1)+(-0.040*beat2)+(0.007*beat3)+(0.028*beat4)+(-0.020*beat5)+(0.121*read)+(0.022*educ)+(-0.012*age1birth)+(-0.016*age1cohab)+(0.013*age_diff)+(0.001*educ_diff)+(0.599*decide1a)+(0.601*decide2a)+(0.619*decide3a))/1.502

label var swpatt "SWPER global score - attitude to violence domain"
label var swpsoc "SWPER global score - social independence domain"
label var swpdec "SWPER global score - decision-making domain"

*------------------------------*
* 9) SWPER 3-category groups (official cut-points)
*------------------------------*
recode swpatt (min/-0.700001=1) (-0.7/0.400001=2) (0.4/max=3), gen(swpatt3gr)
recode swpsoc (min/-0.559=1) (-0.558999/0.293=2) (0.293001/max=3), gen(swpsoc3gr)
recode swpdec (min/-1.000001=1) (-1.0/0.600001=2) (0.6/max=3), gen(swpdec3gr)

label define swp3gr 1 "low empowerment" 2 "medium empowerment" 3 "high empowerment", replace
label values swpatt3gr swp3gr
label values swpsoc3gr swp3gr
label values swpdec3gr swp3gr

label var swpatt3gr "SWPER global 3 groups - attitude to violence domain"
label var swpsoc3gr "SWPER global 3 groups - social independence domain"
label var swpdec3gr "SWPER global 3 groups - decision-making domain"

* Convenience names (match your analysis)
clonevar att_score = swpatt
clonevar aut_score = swpsoc
clonevar dec_score = swpdec

clonevar att_cat = swpatt3gr
clonevar aut_cat = swpsoc3gr
clonevar dec_cat = swpdec3gr

log close
