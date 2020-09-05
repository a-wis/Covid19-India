* This code processes the individual-level data from the NFHS to be used in the model
* Modelling COVID-19 With More Disaggregation and Less Nomothetic Parameterisation: UK and India Examples (2020)
* By Wendy Olsen (corresponding author), Manasi Bera, Jihye Kim, Arkadiusz Wiśniowski, and Purva Yadav
* 

use "IACR74FL.DTA", clear

sum v001 v002 v003 mv003  v005 mv005 v024 mv024 sdistri smdistri v013 mv013 v025 mv025 s116 sm118 v463a v463b v463e v463x  s707 mv463z sm606 v168 mv168 s723a sm622a s723b sm622b
keep v001 v002 v003 v005 v024 sdistri v013 v025 s116 v463z  v168 s723a  s723b sb18 v437 v445 s723e s723d v190
rename v001 cluster
rename v002 hhno
rename v003 id
rename v005 weight
rename v024 state
rename sdistri district
rename v013 age_category4
rename v025 urbanrural
rename s116 scheduledcaste
rename v463z smoke_non 
rename v168 awayfromhome_12mths
rename s723a  diabetes
rename s723b  asthma
rename sb18 bp_diagnosed
rename v437 kilograms_women
rename v445 bmi_women
rename s723e cancer
rename s723d heart
rename v190 wealth
gen female=1
save "NFHS\couple_female.DTA", replace

use "D:\NFHS\IACR74DT Couple\IACR74FL.DTA", clear
keep v001 v002 mv003 mv005 mv024 smdistri mv013 mv025 sm118 mv463z mv168 sm622a sm622b smb18 sm622e sm622d mv190 
rename v001 cluster
rename v002 hhno
rename mv003 id
rename mv005 weight
rename mv024 state
rename smdistri district
rename mv013 age_category4
rename mv025 urbanrural
rename sm118 scheduledcaste
rename mv463z smoke_non
rename mv168 awayfromhome_12mths
rename sm622a diabetes
rename sm622b asthma
rename sm622e cancer
rename sm622d heart
rename smb18 bp_diagnosed
rename mv190 wealth
gen female=0
save "NFHS\couple_male.DTA", replace

use "NFHS\couple_female.DTA", clear
*Merge with men's file
merge 1:m cluster hhno id using "NFHS\couple_male.DTA"
sort cluster hhno id
duplicates report
duplicates drop

recode bp_diagnosed 8=0
recode diabetes 8=0
recode asthma 8=0
recode heart 8=0
recode cancer 8=0
sem (bp_diagnosed diabetes asthma heart cancer <- F1), nocapslatent latent(F1) standardized
*screeplot
estat eqgof
estat gof, stats(all)
predict f1, latent(F1)
xtile health = f1, nq(5)
recode health 1=0 2=1 3=2 4=3 5=4
save "NFHS\couple.DTA", replace

 
 *Clean household members data(for weight) 
use "NFHS\IAPR74DT household member\IAPR74FL.DTA", clear
gen bmi=ha40
replace bmi=hb40 if bmi==.
rename hv001	cluster
rename hv002	hhno
rename hvidx	id
rename hv005	weight
rename hv024	state
rename shdistri 	district
rename hv101  	hhrelation
rename hv104 	sex
rename hv105	age
rename shb18  	bp_diagnosed
rename shb19  	bp_medicine
rename hc73 	bmi_children
rename hb2	kilograms_men
rename hb3   	centimeters_men
rename hb40 	bmi_men
rename hv025	urbanrural
rename sh36	scheduledcaste

keep cluster hhno id kilograms_men bmi_men
keep if kilograms_men!=.
save "NFHS\members.18Jun.DTA", replace
use "NFHS\members.18Jun.DTA", clear

*match for bmi_males
use "NFHS\couple.DTA", replace
drop _merge
merge 1:m cluster hhno id using "NFHS\members.18Jun.DTA"
sort cluster hhno id
drop if _merge==2
gen kilograms=kilograms_women if female==1
replace kilograms=kilograms_men if kilograms==. & female==0
gen bmi=bmi_women if female==1
replace bmi=bmi_men if bmi==. & female==0
save "NFHS\couple.DTA", replace

  
