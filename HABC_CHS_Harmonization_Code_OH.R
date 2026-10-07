#Data Harmonization
# Runs individual summary statistics for each variable to double check everything
#  is what it says it is. 

# Harmonizes Health ABC and CHS baseline data sets according to coding decision across
#  Health ABC, CHS, and ARIC. Saves harmonized data set as [dataset]_TEST.

# Joins the harmonized data sets to HABC_CHS_TEST. Runs joint summary statistics
#  for each variable 

install.packages("dplyr")
library("dplyr")

#Read in CHS Data
CHS <- read.csv("C:/Users/olharper/OneDrive - Wake Forest 
                Baptist Health/OpenLong/baseline.CHS.csv")

#Read in HABC Data
HABC <- read.csv("C:/Users/olharper/OneDrive - Wake Forest 
                 Baptist Health/OpenLong/baseline.HABC.csv")
##---------------------------------------------------------------------
#Individual Summary Stats for RACE
table(HABC$RACE)
table(CHS$RACE)
##_____________________________________________________________________
#Summary Stats for GENDER
table(HABC$GENDER)
table(CHS$GENDER)

#Change CHS
CHS = CHS %>% 
  mutate(
    GENDER_FINAL = case_when(
      GENDER == 1 ~ 1,
      GENDER == 0 ~ 2
    )
  )
##----------------------------------------------------------------
#Summary Stats for CV1AGE
summary(HABC$CV1AGE)
summary(CHS$CV1AGE)
#should have changed HABC. Will do it with ARIC harmonization
#Change HABC
HABC = HABC %>% 
  mutate(
    CV1AGE_FINAL = case_when(
      CV1AGE == 65 | CV1AGE == 66 ~ 1,
      CV1AGE == 67 | CV1AGE == 68 ~ 2,
      CV1AGE == 69 | CV1AGE == 70 ~ 3,
      CV1AGE == 71 | CV1AGE == 72 ~ 4,
      CV1AGE == 73 | CV1AGE == 74 ~ 5,
      CV1AGE == 75 | CV1AGE == 76 ~ 6,
      CV1AGE == 77 | CV1AGE == 78 ~ 7,
      CV1AGE == 79 | CV1AGE == 80 ~ 8,
      CV1AGE == 81 | CV1AGE == 82 ~ 9,
      CV1AGE == 83 | CV1AGE == 84 ~ 10,
      CV1AGE == 85 | CV1AGE == 86 ~ 11,
      CV1AGE >= 87 & CV1AGE <= 89 ~ 12,
      CV1AGE > 90 ~ 13
    )
  )
summary(HABC$CV1AGE_FINAL)
##----------------------------------------------------------------
#Summary Stats for TSMARSTA
table(HABC$TSMARSTA)
table(CHS$TSMARSTA)

#Change HABC
HABC = HABC %>% 
  mutate(
    TSMARSTA_FINAL = case_when(
      TSMARSTA == 0 ~ 5,
      TSMARSTA == 1 ~ 1,
      TSMARSTA == 2 ~ 2,
      TSMARSTA == 3 ~ 3,
      TSMARSTA == 4 ~ 3
    )
  )
table(HABC$TSMARSTA_FINAL)
table(CHS$TSMARSTA)
##---------------------------------------------------------------
#Summary Stats for FPHSTAT
table(HABC$FPHSTAT)
table(CHS$FPHSTAT)

##--------------------------------------------------------------
#Summary Stats for HQSSOPIH
summary(HABC$HQSSOPIH)
table(CHS$HQSSOPIH)

#Change HABC
HABC = HABC %>% 
  mutate(
    HQSSOPIH_FINAL = case_when(
      HQSSOPIH == 0 ~ 0,
      HQSSOPIH != 0 ~ 1
    )
  )

#Change CHS
CHS = CHS %>% 
  mutate(
    HQSSOPIH_FINAL = case_when(
      HQSSOPIH == 0 ~ 0, 
      HQSSOPIH == 1 | 4 | 5 ~ 1,
      HQSSOPIH == 9 ~ NA
    )
  )

table(HABC$HQSSOPIH_FINAL)
table(CHS$HQSSOPIH_FINAL)

##----------------------------------------------------------------------
#Summary Stats for Y1PCHD1
table(HABC$Y1PCHD1)
table(CHS$Y1PCHD1)

#Change HABC
HABC = HABC %>% 
  mutate(
    Y1PCHD1_FINAL = case_when(
      Y1PCHD1 == 0 ~ 0,
      Y1PCHD1 == 1 ~ 1,
      Y1PCHD1 == 2 ~ 1
    )
  )

table(HABC$Y1PCHD1_FINAL)
##---------------------------------------------------------------------
#Summary Stats for Y1PCHF
table(HABC$Y1PCHF)
table(CHS$Y1PCHF)

#Change HABC
HABC = HABC %>% 
  mutate(
    Y1PCHF_FINAL = case_when(
      Y1PCHF == 0 ~ 0,
      Y1PCHF == 1 ~ 1,
      Y1PCHF == 2 ~ 1
    )
  )

table(HABC$Y1PCHF_FINAL)
##---------------------------------------------------------------------
#Summary Stats for Y1PCBVD
table(HABC$Y1PCBVD)
table(CHS$Y1PCBVD)

#Change HABC
HABC = HABC %>% 
  mutate(
    Y1PCBVD_FINAL = case_when(
      Y1PCBVD == 0 ~ 0,
      Y1PCBVD == 1 ~ 1,
      Y1PCBVD == 2 ~ 1
    )
  )

table(HABC$Y1PCBVD_FINAL)
##--------------------------------------------------------------------
#Individual Summary Stats for Y1ADAEPI
#TBD (Need cutoffs from ARIC)
##--------------------------------------------------------------------
#Individual Summary Stats for Y1POAKN
#TBD (Waiting on ARIC?)
##--------------------------------------------------------------------
#Summary Stats for Y1PHBP1
table(HABC$Y1PHBP1)
table(CHS$Y1PHBP1)

#Change HABC
HABC = HABC %>% 
  mutate(
    Y1PHBP1_FINAL = case_when(
      Y1PHBP1 == 0 ~ 0,
      Y1PHBP1 == 1 ~ 1,
      Y1PHBP1 == 2 ~ 1
    )
  )

table(HABC$Y1PHBP1_FINAL)
##---------------------------------------------------------------------
#Summary Stats for Y1PHBP2
table(HABC$Y1PHBP2)
table(CHS$Y1PHBP2)
#TBD
##-------------------------------------------------------------------
#Summary Stats for Y1PDEPR1
table(HABC$Y1PDEPR1)
table(CHS$Y1PDEPR1)
HABC = HABC %>% 
  mutate(
    Y1PDEPR1_FINAL = case_when(
      Y1PDEPR1 == 0 ~ 0,
      Y1PDEPR1 == 1 ~ 1,
      Y1PDEPR1 == 2 ~ 1,
      Y1PDEPR1 == 3 ~ 1
    )
  )
table(HABC$Y1PDEPR1_FINAL)
##-------------------------------------------------------------------
#Summary Stats for Y1PDEPR2
table(HABC$Y1PDEPR2)
table(CHS$Y1PDEPR2)

##-------------------------------------------------------------------
#Summary Stats for Y1PPULCD
table(HABC$Y1PPULCD)
table(CHS$Y1PPULCD)
HABC = HABC %>% 
  mutate(
    Y1PPULCD_FINAL = case_when(
      Y1PPULCD == 0 ~ 0,
      Y1PPULCD == 1 ~ 1,
      Y1PPULCD == 2 ~ 1,
      Y1PPULCD == 3 ~ 1
    )
  )
table(HABC$Y1PPULCD_FINAL)
##-------------------------------------------------------------------
#Summary Stats for Y1PCANCR
table(HABC$Y1PCANCR)
table(CHS$Y1PCANCR)

#Change HABC
HABC = HABC %>% 
  mutate(
    Y1PCANCR_FINAL = case_when(
      Y1PCANCR == 0 ~ 0,
      Y1PCANCR == 1 ~ 1,
      Y1PCANCR == 2 ~ 1
    )
  )
table(HABC$Y1PCANCR_FINAL)
##-------------------------------------------------------------------
#Individual Summary Stats for CANANYI
#TBD (DOesn't exist in CHS)
##-------------------------------------------------------------------
#Individual Summary Stats for CHDMI
#TBD (Question on Prevalence and Incidence)
##-------------------------------------------------------------------
#Individual Summary Stats for CHDI
#TBD (Question on prevalence and incidence)
##-------------------------------------------------------------------
#Individual Summary Stats for STROKEI
#TBD (Question on prevalence and incidence)
##-------------------------------------------------------------------
#Individual Summary Stats for CVDI
#TBD (Doesn't exist in CHS)
##-------------------------------------------------------------------
#Individual Summary Stats for MMMSCORE
summary(HABC$MMMSCORE)
summary(CHS$MMMSCORE)
##-------------------------------------------------------------------
#Individual Summary Stats for EDUC
table(HABC$EDUC)
table(CHS$EDUC)

#Change CHS
CHS = CHS %>% 
  mutate(
    EDUC_FINAL = case_when(
      EDUC == 1 ~ 1,
      EDUC == 2 ~ 1,
      EDUC == 3 ~ 2,
      EDUC == 4 ~ 3,
      EDUC == 5 ~ 3,
      EDUC == 6 ~ 3
    )
  )
table(CHS$EDUC_FINAL)
##-------------------------------------------------------------------
#Individual Summary Stats for FAMINC
table(HABC$FAMINC)
table(CHS$FAMINC)

#Change HABC
HABC = HABC %>% 
  mutate(
    FAMINC_FINAL = case_when(
      FAMINC == 1 ~ 1,
      FAMINC == 2 ~ 1,
      FAMINC == 3 ~ 2,
      FAMINC == 4 ~ 3
    )
  )

#Change CHS
CHS = CHS %>% 
  mutate(
    FAMINC_FINAL = case_when(
      FAMINC == 1 ~ 1,
      FAMINC == 2 ~ 1,
      FAMINC == 3 ~ 1,
      FAMINC == 4 ~ 1,
      FAMINC == 5 ~ 1,
      FAMINC == 6 ~ 2,
      FAMINC == 7 ~ 2,
      FAMINC == 8 ~ 3
    )
  )
table(HABC$FAMINC_FINAL)
table(CHS$FAMINC_FINAL)
##-------------------------------------------------------------------
#Individual Summary Stats for Y1UWPACE
#TBD (Doesn't exist in CHS)
##-------------------------------------------------------------------
#Individual Summary Stats for HAKCAL
summary(HABC$HAKCAL)
summary(CHS$HAKCAL)
##------------------------------------------------------------------
#Individual Summary Stats for HACAT
table(HABC$HACAT)
table(CHS$HACAT)
##------------------------------------------------------------------
#Individual Summary Stats for WKAINDEX
#TBD (Can't find in CHS)
##------------------------------------------------------------------
#Individual Summary Stats for PACKYR1
summary(HABC$PACKYR1)
summary(CHS$PACKYR1)
##------------------------------------------------------------------
#Indiviual Summary Stats for DRINKER1
table(HABC$DRINKER1)
table(CHS$DRINKER1)

#Change HABC
HABC = HABC %>% 
  mutate(
    DRINKER1_FINAL = case_when(
      DRINKER1 == 0 ~ 0,
      DRINKER1 == 1 ~ 1,
      DRINKER1 == 2 ~ 1
    )
  )
table(HABC$DRINKER1_FINAL)
##----------------------------------------------------------------
#Individual Summary Stats for CURDRNK1
#TBD (waiting on ARIC)
##-----------------------------------------------------------------
#Individual Summary Stats for Y1KP12MO
table(HABC$Y1KP12MO)
table(CHS$Y1KP12MO)
##-----------------------------------------------------------------
#Individual Summary Stats for CES_DBASELINE
summary(HABC$CES_DBASELINE)
summary(CHS$CES_DBASELINE)
##-----------------------------------------------------------------
#Individual Summary Stats for CHR5PACE_BASELINE
#TBD (waiting on ARIC)


##----------------------------------------------------------------
#Creating new CHS data frame
CHS_TEST = CHS %>% 
  select(
    HABCID, RACE, GENDER_FINAL, CV1AGE, TSMARSTA, FPHSTAT,
    HQSSOPIH_FINAL, Y1PCHD1, Y1PCHF, Y1PCBVD, Y1ADAEPI, 
    Y1POAKN, Y1PHBP1, Y1PHBP2, Y1PDEPR1, Y1PDEPR2, Y1PPULCD, Y1PCANCR, 
    CHDMI, CHDI, STROKEI, MMMSCORE, EDUC_FINAL, FAMINC_FINAL,
    HAKCAL, HACAT, PACKYR1, DRINKER1, CURDRNK1, 
    Y1KP12MO, CES_DBASELINE, CHR5PACE_BASELINE) %>% 
  mutate(
    SITE = NA,
    CANANYI = NA,
    Y1UWPACE = NA,
    CVDI = NA,
    WKAINDEX = NA,
    GROUP = "CHS") %>% 
  rename(
    CV1AGE_FINAL = CV1AGE,
    TSMARSTA_FINAL = TSMARSTA,
    Y1PCHD1_FINAL = Y1PCHD1,
    Y1PCHF_FINAL = Y1PCHF,
    Y1PCBVD_FINAL = Y1PCBVD,
    Y1PHBP1_FINAL = Y1PHBP1,
    Y1PDEPR1_FINAL = Y1PDEPR1,
    Y1PPULCD_FINAL = Y1PPULCD,
    Y1PCANCR_FINAL = Y1PCANCR,
    DRINKER1_FINAL = DRINKER1
  )

#Creating a new HABC data frame
HABC_TEST = HABC %>% 
  select(
    HABCID, RACE, GENDER, SITE, CV1AGE_FINAL, TSMARSTA_FINAL, FPHSTAT,
    HQSSOPIH_FINAL, Y1PCHD1_FINAL, Y1PCHF_FINAL, Y1PCBVD_FINAL, Y1ADAEPI,
    Y1POAKN, Y1PHBP1_FINAL, Y1PHBP2, Y1PDEPR1_FINAL, Y1PDEPR2, Y1PPULCD_FINAL,
    Y1PCANCR_FINAL, CANANYI, CHDMI, CHDI, STROKEI, CVDI, MMMSCORE, EDUC,
    FAMINC_FINAL, Y1UWPACE, HAKCAL, HACAT, WKAINDEX, PACKYR1, DRINKER1_FINAL,
    CURDRNK1, Y1KP12MO, CES_DBASELINE, CHR5PACE_BASELINE
  ) %>% 
  mutate(
    GROUP = "HABC"
  ) %>% 
  rename(
    GENDER_FINAL = GENDER,
    EDUC_FINAL = EDUC,
  )

##-------------------------------------------------------------------------
#Joint Summary Statistics
HABC_CHS_TEST <- bind_rows(HABC_TEST, CHS_TEST)

#Summary Statistics 
table1::table1(~ factor(RACE) + factor(GENDER_FINAL) + factor(SITE) + 
                 factor(CV1AGE_FINAL) + factor(TSMARSTA_FINAL) + factor(FPHSTAT) + 
                 factor(HQSSOPIH_FINAL) + factor(Y1PCHD1_FINAL) + factor(Y1PCHF_FINAL) + 
                 factor(Y1PCBVD_FINAL) + factor(Y1ADAEPI) + factor(Y1POAKN) + 
                 factor(Y1PHBP1_FINAL) + factor(Y1PHBP2) + factor(Y1PDEPR1_FINAL) + 
                 factor(Y1PDEPR2) + factor(Y1PPULCD_FINAL) + factor(Y1PCANCR_FINAL) +
                 factor(CANANYI) + factor(CHDMI) + factor(CHDI) + factor(STROKEI) +
                 factor(CVDI) + MMMSCORE + factor(EDUC_FINAL) + factor(FAMINC_FINAL) +
                 Y1UWPACE + HAKCAL + factor(HACAT) + factor(WKAINDEX) + PACKYR1 +
                 factor(DRINKER1_FINAL) + factor(CURDRNK1) + factor(Y1KP12MO) +
                 CES_DBASELINE + CHR5PACE_BASELINE| GROUP, 
               data = HABC_CHS_TEST )

#RACE
# HABC_CHS_TEST %>% 
#   group_by(GROUP, RACE) %>% 
#   summarise(
#     #n_distinct(RACE, na.rm = TRUE)
#     count = sum(!is.na(RACE))
#   )


#MMMSCORE
# HABC_CHS_TEST %>% 
#   group_by(GROUP) %>% 
#   summarize(
#     mean = mean(MMMSCORE, na.rm = TRUE),
#     var = var(MMMSCORE, na.rm = TRUE),
#     sd = sd(MMMSCORE, na.rm = TRUE),
#     median = median(MMMSCORE, na.rm = TRUE),
#     min = min(MMMSCORE, na.rm = TRUE),
#     max = max(MMMSCORE, na.rm = TRUE),
#     IQR = IQR(MMMSCORE, na.rm = TRUE)
# 
#   )

