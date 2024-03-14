**** ECHILD Course Day 2: Developing e-Cohorts: Stata script **** 

**** HOUSEKEEPING ****
clear
capture log close
set varabbrev off
set more off, perm
frames reset

* Change your working directory to where you wish to save your work 
cd "C:\Users\rmjlkml\OneDrive - University College London\# temp" // rename this file path
* instead of downloading 

* Log your output 
log using "C:\Users\rmjlkml\OneDrive - University College London\# temp\ECHILD_course_$S_DATE.log", replace //rename this log file if you wish

* Load data - here we will use data frames to keep the datasets in memory
* if not using frames, you can just read in the hes dataset (lines 22-26) and move on to question 1
frame create hes
frame change hes 
* two options to load in the data:
* option 1: save the data to your working directory (outside of STATA)
* option 2: download the data directly
copy https://rdr.ucl.ac.uk/ndownloader/files/44972194 hes.csv // delete if using option 1
insheet using "HES.csv", comma names 

frame create ks1
frame change ks1
* two options to load in the data:
* option 1: save the data to your working directory (outside of STATA)
* option 2: download the data directly
copy https://rdr.ucl.ac.uk/ndownloader/files/44972182 ks1.csv // delete if using option 1
insheet using "ks1.csv", comma names 

frame create spine
frame change spine
* two options to load in the data:
* option 1: save the data to your working directory (outside of STATA)
* option 2: download the data directly
copy https://rdr.ucl.ac.uk/ndownloader/files//44972191 spine.csv // delete if using option 1
insheet using "spine.csv", comma names 

**** Part 1 ****

* We are defining a birth cohort using HES data, so we'll focus there first.
frame change hes // this opens the hes frame back up, the other frames are still in memory

* 1) What birth years have we provided you?
* To answer, we can define the calendar year of birth from epistart.
codebook epistart //check the format of epistart

* we need to convert epistart to Stata date format: let's do this for all dates
foreach var of varlist epistart epiend admistart disdate {
	gen d_`var'=date(`var', "YMD")	
	format d_`var' %d
	}

* create a year variable from d_epistart, and tabulate
gen year=year(d_epistart)
tab year

* 2) What is the start age for people in this dataset?
tab startage,m

* 3) What epitype did we provide you? What does this mean?
tab epitype,m

* 4) What admimeth did we provide you? What does this mean?
tab admimeth,m

* 5) What ICD10 code did we provide you in DIAG_01? What does this mean?
tab diag_01,m

* 6) How many unique individuals are there in the dataset?
codebook tokenid

* 7) Identify  which variable represents the region of residence.


* 8) Calculate how many individuals' region of residence at birth is Scotland or Wales.
tab resgor
count if inlist(resgor,"S","W")

* 9) How many individuals are left once you only keep those who resided in English regions at birth?
drop if inlist(resgor,"S","W")
codebook tokenid // or you could type "count" now you know that each row is one individual


**** Part 2 ****

* 1) What ICD10 codes have we included in DIAG_02?
tab diag_02,m

* 2) Using the ICD-10 browser, what do these codes represent?
* in Stata we can also use the icd10 command, e.g. 
icd10 lookup A75

* 3) How many patients have a major congenital anomaly code?
gen mca=inlist(diag_02,"Q24","Q35","Q37","Q43")
tab mca

* Now we can drop them
drop if mca==1
codebook tokenid // or you could type "count" now you know that each row is one individual

/* if you are not using frames then save this dataset here 
save "C:\Users\rmjlkml\OneDrive - University College London\# temp\HES_no_MCAs.dta", replace
*/

* 4) How many patients have a linked record in the spine?

* First, let's ensure there is no duplication of tokenid in the spine (which
* would represent multiple matches in real ECHILD) as these will cause duplication
* in our merged data.table.
frame change spine // we created this frame above, alternatively you can type "insheet using "spine.csv", comma names" here
codebook tokenid
duplicates report tokenid // alternative method to check duplicates

* We can link to NPD via the spine using the frlink function
frame change hes
frlink 1:1 tokenid, frame(spine) // this tells us the number of obs that don't match
gen	link=spine!=. //creates a variable indicating linkage
tab link

/* if not using frames, open and save the datasets and use the merge command
clear
insheet using "spine.csv", comma names 
save "C:\Users\rmjlkml\OneDrive - University College London\# temp\spine.dta", replace
use "C:\Users\rmjlkml\OneDrive - University College London\# temp\HES_no_MCAs.dta", clear
merge 1:1 tokenid using "C:\Users\rmjlkml\OneDrive - University College London\# temp\spine.dta", keep(master match)
gen link=_merge==3 //create a variable indicating linkage
tab link
drop _merge
*/


**** Part 3 ****
* 1) Are there differences in linkage rates between HES and NPD based on sex,
* deprivation and ethnicity? Once you have answered this question, you can drop
* the students without a link.

* Let's first explore our sex, deprivation and ethnicity variables.
* Remember to consult the data dictionary for meaning.
tab sex link, row
tab ethnos link, row
tab imd04_decile link, row

* Now we have analysed linkage rates, We can drop those who did not link and 
* focus on our research question.
drop if link==0

* 2) What is the relationship between gestational age and KS1 results
* (unadjusted and adjusted for sex, deprivation and ethnicity)?

* To start this, we need to link the KS1 data, using the PupilMatchingReference
* that we merged in earlier.
frget pupilmatchingrefanonymous, from(spine) // this takes PMR from the linked frame
frlink 1:1 pupilmatchingrefanonymous, frame(ks1) // linking the ks1 dataset
frget ks1_math, from(ks1) // this takes ks1_math scores from the linked frame

/* if not using frames we need to save, open and merge datasets
save "C:\Users\rmjlkml\OneDrive - University College London\# temp\hes_spine_combined.dta", replace // save current dataset
clear
insheet using "ks1.csv", comma names // open ks1 dataset
save  "C:\Users\rmjlkml\OneDrive - University College London\# temp\ks1.dta", replace  //save ks1 dataset in STATA format
use "C:\Users\rmjlkml\OneDrive - University College London\# temp\hes_spine_combined.dta", clear
capture drop _merge
merge 1:1 pupilmatchingrefanonymous using "C:\Users\rmjlkml\OneDrive - University College London\# temp\ks1.dta", keep(match) // merge
*/

* summarise ks1
sum ks1_math,detail
histogram ks1_math

*destring the gestational age variable
codebook gestat 
tab gestat  
encode gestat,gen(gestat_n) label(gestat_l) //turn into a numeric variable and create label variable

*view new variable
tab gestat_n
tab gestat_n,nolab 

*changing <28 to be the first group
replace gestat_n=0 if gestat=="less_than_28" // puts <28 as earliest number
label define gestat_l 0 "less_than_28", modify //  adds the label to already existing labels
label values gestat_n gestat_l

* Univariable analysis
mean ks1_math, over(gestat_n)
regress ks1_math ib11.gestat_n // logistic regression treating gestational age as cateogrical
// ib11 specifies category 11 (i.e. GA = 40) as the baseline

* Linear regression adjusted for covariates
codebook sex imd04_decile ethnos // imd and ehthnos are string variables
foreach var of varlist imd04_decile ethnos{
	encode `var',gen(`var'_n) //turn into a numeric variable
	}

regress ks1_math ib11.gestat_n sex i.imd04_decile_n i.ethnos_n