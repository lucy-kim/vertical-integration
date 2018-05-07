*tag hospital that participated in BPCI using files from https://innovation.cms.gov/initiatives/Bundled-Payments/Archived-Materials.html

cd /ifs/home/kimk13/VI/data/Medicare/BPCI

loc y 2013
loc q 3
import excel bpci-analyticfile-q`q'`y'.xlsx, firstrow clear

*keep only Medicare providers
keep if MedicareProvi~r=="Yes"
drop MedicareProvi~r

capture rename ParticipantMo~l Model
capture rename ParticipantAddressLine1
foreach v in "Model" "AddressLine1" "Name" "City" "State" "CCN" {
  capture rename Participant`v' `v'
}
drop if CCN=="blank" | CCN=="None Given" | CCN=="" | CCN=="Blank"
drop *Address*
bys CCN: gen a = _N
tab a
*80% hospitals have 48 bundles
drop a

*restrict to short-term general hosps
replace CCN = subinstr(CCN, "-", "",.)
gen l = length(CCN)
tab CCN if l!=6
*list *Name* CCN if l!=6

*drop if CCN has characters
gen x = real(CCN)
drop if x==.
drop x l
destring CCN, gen(provid)
gen n = substr(CCN,-4,4)
destring n, replace
keep if n >= 1 & n <=879
drop n

*also tag a hospital if has any bundle
gen bpci_any = 1

capture destring DiscountRate, replace ig("%")

*Phase I participants transition to Phase II
capture keep if EpisodeStatus=="Phase II"

*create hospital-model level data
*don't even differentiate by Episode Length (30/60/90 days)
capture collapse (max) bpci_* (mean) DiscountRate, by(CCN provid)

keep CCN provid bpci_any
duplicates tag CCN, gen(dup)
assert dup==0
drop dup

gen qtr = `q'
gen yr = `y'

compress
save bpci_q`q'`y', replace
tempfile bpci_q`q'`y'
save `bpci_q`q'`y''


forval y = 2014/2016 {
  forval q = 1/4 {
    import excel bpci-analyticfile-q`q'`y'.xlsx, firstrow clear
    capture rename ParticipantMo~l Model
    capture rename ParticipantAddressLine1
    foreach v in "Model" "AddressLine1" "Name" "City" "State" "CCN" {
      capture rename Participant`v' `v'
    }
    drop if CCN=="blank" | CCN=="None Given" | CCN=="" | CCN=="Blank"
    drop *Address*
    bys CCN: gen a = _N
    tab a
    *80% hospitals have 48 bundles
    drop a

    *restrict to short-term general hosps
    replace CCN = subinstr(CCN, "-", "",.)
    gen l = length(CCN)
    tab CCN if l!=6
    *list *Name* CCN if l!=6

    *drop if CCN has characters
    gen x = real(CCN)
    drop if x==.
    drop x l
    destring CCN, gen(provid)
    gen n = substr(CCN,-4,4)
    destring n, replace
    keep if n >= 1 & n <=879
    drop n

    *simplying everything, mark if bundle = AMI, HF, PN, HK
    capture rename EpisodeName BundledPaymentEpisodes
    loc bAMI regexm(BundledPaymentEpisodes, "Acute myocardial infarction") | BundledPaymentEpisodes=="All DRGs" | regexm(BundledPaymentEpisodes, "Acute Myocardial Infarction")
    loc bHK regexm(BundledPaymentEpisodes, "Major joint replacement of the lower") | BundledPaymentEpisodes=="All DRGs" | regexm(BundledPaymentEpisodes, "Double joint replacement of the lower")
    loc bPN regexm(BundledPaymentEpisodes, "Simple pneumonia and respiratory") | BundledPaymentEpisodes=="All DRGs"
    loc bHF regexm(BundledPaymentEpisodes, "Congestive heart failure") | BundledPaymentEpisodes=="All DRGs"
    foreach c in "AMI" "HF" "PN" "HK" {
      count if `b`c''
      capture drop bpci_`c'
      gen bpci_`c' = `b`c''
    }

    *also tag a hospital if has any bundle
    gen bpci_any = 1

    capture destring DiscountRate, replace ig("%")

    *Phase I participants transition to Phase II
    keep if EpisodeStatus=="Phase II"

    *create hospital-model level data
    *don't even differentiate by Episode Length (30/60/90 days)
    collapse (max) bpci_* (mean) DiscountRate, by(CCN provid)

    gen qtr = `q'
    gen yr = `y'

    compress
    save bpci_q`q'`y', replace
    tempfile bpci_q`q'`y'
    save `bpci_q`q'`y''
  }
}


forval y = 2017/2017 {
  forval q = 1/2 {
    import excel bpci-analyticfile-q`q'`y'.xlsx, firstrow clear
    capture rename ParticipantMo~l Model
    capture rename ParticipantAddressLine1
    foreach v in "Model" "AddressLine1" "Name" "City" "State" "CCN" {
      capture rename Participant`v' `v'
    }
    drop if CCN=="blank" | CCN=="None Given" | CCN=="" | CCN=="Blank"
    drop *Address*
    bys CCN: gen a = _N
    tab a
    *80% hospitals have 48 bundles
    drop a

    *restrict to short-term general hosps
    replace CCN = subinstr(CCN, "-", "",.)
    gen l = length(CCN)
    tab CCN if l!=6
    *list *Name* CCN if l!=6

    *drop if CCN has characters
    gen x = real(CCN)
    drop if x==.
    drop x l
    destring CCN, gen(provid)
    gen n = substr(CCN,-4,4)
    destring n, replace
    keep if n >= 1 & n <=879
    drop n

    *simplying everything, mark if bundle = AMI, HF, PN, HK
    capture rename EpisodeName BundledPaymentEpisodes
    loc bAMI regexm(BundledPaymentEpisodes, "Acute myocardial infarction") | BundledPaymentEpisodes=="All DRGs" | regexm(BundledPaymentEpisodes, "Acute Myocardial Infarction")
    loc bHK regexm(BundledPaymentEpisodes, "Major joint replacement of the lower") | BundledPaymentEpisodes=="All DRGs" | regexm(BundledPaymentEpisodes, "Double joint replacement of the lower")
    loc bPN regexm(BundledPaymentEpisodes, "Simple pneumonia and respiratory") | BundledPaymentEpisodes=="All DRGs"
    loc bHF regexm(BundledPaymentEpisodes, "Congestive heart failure") | BundledPaymentEpisodes=="All DRGs"
    foreach c in "AMI" "HF" "PN" "HK" {
      count if `b`c''
      capture drop bpci_`c'
      gen bpci_`c' = `b`c''
    }

    *also tag a hospital if has any bundle
    gen bpci_any = 1

    capture destring DiscountRate, replace ig("%")

    *Phase I participants transition to Phase II
    keep if EpisodeStatus=="Phase II"

    *create hospital-model level data
    *don't even differentiate by Episode Length (30/60/90 days)
    collapse (max) bpci_* (mean) DiscountRate, by(CCN provid)

    gen qtr = `q'
    gen yr = `y'

    compress
    save bpci_q`q'`y', replace
    tempfile bpci_q`q'`y'
    save `bpci_q`q'`y''
  }
}


/*
forval y = 2014/2016 {
  forval q = 1/4 {
    import excel bpci-analyticfile-q`q'`y'.xlsx, firstrow clear
    capture drop P Q

    di "Q`q' Year `y'-----------------------------------"
    *keep only Medicare providers
    capture keep if MedicareProvi=="Yes"
    capture drop MedicareProvi

    capture rename ParticipantMo~l Model
    capture rename ParticipantAddressLine1
    foreach v in "Model" "AddressLine1" "Name" "City" "State" "CCN" {
      capture rename Participant`v' `v'
    }

    *restrict to short-term general/specialty hospitals by looking at the laast 4 digits
    *0001-0879 (https://www.cms.gov/Regulations-and-Guidance/Guidance/Transmittals/downloads/R29SOMA.pdf)
    drop if CCN=="blank" | CCN=="None Given" | CCN==""
    gen l = length(CCN)
    tab CCN if l!=6
    destring CCN, gen(provid) ig("-", "A", "B", "MA", "KA", ",")
    drop l
    gen l = length(CCN)

    gen n = substr(CCN,-4,4)
    destring n, replace
    keep if n >= 1 & n <=879

    drop Addressline2 n

    capture destring DiscountRate, replace ig("%")

    gen qtr = `q'
    gen yr = `y'

    tempfile f`q'`y'
    save `f`q'`y''
  }
}

forval y = 2013/2013 {
  forval q = 4/4 {
    import excel bpci-analyticfile-q`q'`y'.xlsx, firstrow clear
    capture drop P Q

    *keep only Medicare providers
    keep if MedicareProvi~r=="Yes"
    drop MedicareProvi~r

    *restrict to short-term general/specialty hospitals by looking at the laast 4 digits
    *0001-0879 (https://www.cms.gov/Regulations-and-Guidance/Guidance/Transmittals/downloads/R29SOMA.pdf)
    replace CCN = "" if CCN=="blank"
    gen n = substr(CCN,3,4)
    destring n, replace
    keep if n >= 1 & n <=879

    drop Addressline2 n

    gen qtr = `q'
    gen yr = `y'

    tempfile f`q'`y'
    save `f`q'`y''
  }
}

forval y = 2017/2017 {
  forval q = 1/2 {
    import excel bpci-analyticfile-q`q'`y'.xlsx, firstrow clear
    capture drop P Q

    *keep only Medicare providers
    keep if MedicareProvi~r=="Yes"
    drop MedicareProvi~r

    *restrict to short-term general/specialty hospitals by looking at the laast 4 digits
    *0001-0879 (https://www.cms.gov/Regulations-and-Guidance/Guidance/Transmittals/downloads/R29SOMA.pdf)
    replace CCN = "" if CCN=="blank"
    gen n = substr(CCN,3,4)
    destring n, replace
    keep if n >= 1 & n <=879

    drop Addressline2 n

    gen qtr = `q'
    gen yr = `y'

    tempfile f`q'`y'
    save `f`q'`y''
  }
} */

clear
forval y = 2013/2017 {
  forval q = 1/4 {
    capture append using `bpci_q`q'`y''
  }
}

compress
save hosp_bpci_participant, replace
