# Project Name: Independent Research: Fertility Preferences #
# Section code: Exploratory Data Analysis

# Author/Maintainer of the code: Angga Bagus Bismoko
# ORCID: https://orcid.org/0000-0001-8716-7492
# Institution: Research Center for Population, National Research & Innovation
#              Agency Republic of Indonesia
# Email1: angg028@brin.go.id

# Start Date: Jan, 27th 2024
# End Date: Jul, 28th 2024

rm(list=ls())

# Set working directory for this project
setwd("D:/The BRIN A New Chapter/Pusat Riset Kependudukan - BRIN/Research Group - Family Dynamics/Proyek Pribadi/2. Famili Size Preferences/Fertility Preferences_Project")

# Install necessary packages with dependencies
install.packages(c("tidyverse", "haven", "openxlsx", "stargazer", "plm", "fixest", "forcats", "labelled"), dependencies = TRUE)

# Load required packages
packages <- c("tidyverse", "haven", "openxlsx", "stargazer", "plm", "fixest", "forcats", "labelled")
lapply(packages, library, character.only = TRUE)

### ---- Import Datasets ---- ###

# Import PTRACK and HTRACK datasets
ptrack <- haven::read_dta("ptrack.dta")
htrack <- haven::read_dta("htrack.dta")

# Import data from IFLS 2 (1997)
# Book K
bk_ar12 <- haven::read_dta("bk_ar1_w2.dta")
bk_sc2 <- haven::read_dta("bk_sc_w2.dta")

# Book 1
b1_ks02 <- haven::read_dta("b1_ks0_w2.dta")
b1_ks12 <- haven::read_dta("b1_ks1_w2.dta")
b1_ks22 <- haven::read_dta("b1_ks2_w2.dta")
b1_ks32 <- haven::read_dta("b1_ks3_w2.dta")

# Book 2
b2_hr12 <- haven::read_dta("b2_hr1_w2.dta")
b2_kr2 <- haven::read_dta("b2_kr_w2.dta")

# Book 3A
b3a_dl12 <- haven::read_dta("b3a_dl1_w2.dta")
b3a_pk22 <- haven::read_dta("b3a_pk2_w2.dta")
b3a_pk32 <- haven::read_dta("b3a_pk3_w2.dta")
b3a_tk12 <- haven::read_dta("b3a_tk1_w2.dta")

# Book 4
b4_cov2 <- haven::read_dta("b4_cov_w2.dta")
b4_br2 <- haven::read_dta("b4_br_w2.dta")
b4_cx22 <- haven::read_dta("b4_cx2_w2.dta")
b4_kw12 <- haven::read_dta("b4_kw1_w2.dta")
b4_kw22 <- haven::read_dta("b4_kw2_w2.dta")

# Import data from IFLS 3 (2000)
# Book K
bk_ar13 <- haven::read_dta("bk_ar1_w3.dta")
bk_sc3 <- haven::read_dta("bk_sc_w3.dta")

# Book 1
b1_ks03 <- haven::read_dta("b1_ks0_w3.dta")
b1_ks13 <- haven::read_dta("b1_ks1_w3.dta")
b1_ks23 <- haven::read_dta("b1_ks2_w3.dta")
b1_ks33 <- haven::read_dta("b1_ks3_w3.dta")

# Book 2
b2_hr13 <- haven::read_dta("b2_hr1_w3.dta")
b2_kr3 <- haven::read_dta("b2_kr_w3.dta")

# Book 3A
b3a_dl13 <- haven::read_dta("b3a_dl1_w3.dta")
b3a_pk23 <- haven::read_dta("b3a_pk2_w3.dta")
b3a_pk33 <- haven::read_dta("b3a_pk3_w3.dta")
b3a_tk13 <- haven::read_dta("b3a_tk1_w3.dta")

# Book 4
b4_cov3 <- haven::read_dta("b4_cov_w3.dta")
b4_br3 <- haven::read_dta("b4_br_w3.dta")
b4_cx23 <- haven::read_dta("b4_cx2_w3.dta")
b4_kw23 <- haven::read_dta("b4_kw2_w3.dta")
b4_kw33 <- haven::read_dta("b4_kw3_w3.dta")

# Import data from IFLS 4 (2007)
# Book K
bk_ar14 <- haven::read_dta("bk_ar1_w4.dta")
bk_sc4 <- haven::read_dta("bk_sc_w4.dta")

# Book 1
b1_ks04 <- haven::read_dta("b1_ks0_w4.dta")
b1_ks14 <- haven::read_dta("b1_ks1_w4.dta")
b1_ks24 <- haven::read_dta("b1_ks2_w4.dta")
b1_ks34 <- haven::read_dta("b1_ks3_w4.dta")

# Book 2
b2_hr14 <- haven::read_dta("b2_hr1_w4.dta")
b2_kr4 <- haven::read_dta("b2_kr_w4.dta")

# Book 3A
b3a_dl14 <- haven::read_dta("b3a_dl1_w4.dta")
b3a_pk24 <- haven::read_dta("b3a_pk2_w4.dta")
b3a_pk34 <- haven::read_dta("b3a_pk3_w4.dta")
b3a_tk14 <- haven::read_dta("b3a_tk1_w4.dta")

# Book 4
b4_cov4 <- haven::read_dta("b4_cov_w4.dta")
b4_br4 <- haven::read_dta("b4_br_w4.dta")
b4_cx24 <- haven::read_dta("b4_cx2_w4.dta")
b4_kw24 <- haven::read_dta("b4_kw2_w4.dta")
b4_kw34 <- haven::read_dta("b4_kw3_w4.dta")

# Import data from IFLS 5 (2014)
# Book K
bk_ar15 <- haven::read_dta("bk_ar1_w5.dta")
bk_sc5 <- haven::read_dta("bk_sc1_w5.dta")

# Book 1
b1_ks05 <- haven::read_dta("b1_ks0_w5.dta")
b1_ks15 <- haven::read_dta("b1_ks1_w5.dta")
b1_ks25 <- haven::read_dta("b1_ks2_w5.dta")
b1_ks35 <- haven::read_dta("b1_ks3_w5.dta")

# Book 2
b2_hr15 <- haven::read_dta("b2_hr1_w5.dta")
b2_kr5 <- haven::read_dta("b2_kr_w5.dta")

# Book 3A
b3a_dl15 <- haven::read_dta("b3a_dl1_w5.dta")
b3a_pk25 <- haven::read_dta("b3a_pk2_w5.dta")
b3a_pk35 <- haven::read_dta("b3a_pk3_w5.dta")
b3a_tk15 <- haven::read_dta("b3a_tk1_w5.dta")

# Book 4
b4_cov5 <- haven::read_dta("b4_cov_w5.dta")
b4_br5 <- haven::read_dta("b4_br_w5.dta")
b4_cx25 <- haven::read_dta("b4_cx2_w5.dta")
b4_kw25 <- haven::read_dta("b4_kw2_w5.dta")
b4_kw35 <- haven::read_dta("b4_kw3_w5.dta")

#### END ####

### ---- Clean Dataset ---- ####
# Panel respondent : PANEL_it
panel_w2 <- b4_cov2 %>%
  select(pidlink, hhid97, panel) %>%
  rename(hhid = hhid97) %>%
  filter(panel == 1)

panel_w3 <- b4_cov3 %>% 
  select(pidlink, hhid00, panel) %>% 
  rename(hhid = hhid00) %>% 
  filter(panel == 1)

panel_w4 <- b4_cov4 %>% 
  select(pidlink, hhid07, panel) %>% 
  rename(hhid = hhid07) %>% 
  filter(panel == 1)

panel_w5 <- b4_cov5 %>% 
  select(pidlink, hhid14, panel) %>% 
  rename(hhid = hhid14) %>% 
  filter(panel == 1)

# Age variable: AGE_it
age_w2 <- ptrack %>% 
  select(pidlink, hhid97, age_97) %>% 
  rename(age = age_97,
         hhid = hhid97)

age_w3 <- ptrack %>% 
  select(pidlink, hhid00, age_00) %>% 
  rename(age = age_00,
         hhid = hhid00)

age_w4 <- ptrack %>% 
  select(pidlink, hhid07, age_07) %>% 
  rename(age = age_07,
         hhid = hhid07)

age_w5 <- ptrack %>% 
  select(pidlink, hhid14, age_14) %>% 
  rename(age = age_14,
         hhid = hhid14)

# Respondent live in HH variable: LIVEIN_it
livein_w2 <- ptrack %>% 
  select(pidlink, hhid97, ar01a_97) %>% 
  rename(alive = ar01a_97,
         hhid = hhid97)

livein_w3 <- ptrack %>% 
  select(pidlink, hhid00, ar01a_00) %>% 
  rename(alive = ar01a_00,
         hhid = hhid00)

livein_w4 <- ptrack %>% 
  select(pidlink, hhid07, ar01a_07) %>% 
  rename(alive = ar01a_07,
         hhid = hhid07)

livein_w5 <- ptrack %>% 
  select(pidlink, hhid14, ar01a_14) %>% 
  rename(alive = ar01a_14,
         hhid = hhid14)

# Sex variable: SEX_it
sex_w2 <- ptrack %>% 
  select(pidlink, sex) 

sex_w3 <- ptrack %>% 
  select(pidlink, sex) 

sex_w4 <- ptrack %>% 
  select(pidlink, sex) 

sex_w5 <- ptrack %>% 
  select(pidlink, sex) 

# Birth year variable: YOB_it
yob_w2 <- ptrack %>% 
  select(pidlink, bth_year) %>% 
  rename(yob = bth_year)

yob_w3 <- ptrack %>% 
  select(pidlink, bth_year) %>% 
  rename(yob = bth_year)

yob_w4 <- ptrack %>% 
  select(pidlink, bth_year) %>% 
  rename(yob = bth_year)

yob_w5 <- ptrack %>% 
  select(pidlink, bth_year) %>% 
  rename(yob = bth_year)

# Ethnic group variable: ETHNIC_it
ethnic_w5 <- bk_ar15 %>% 
  select(pidlink, ar15d) %>% 
  rename(ethnicity = ar15d)

ethnic_w2 <- ethnic_w5
ethnic_w3 <- ethnic_w5
ethnic_w4 <- ethnic_w5

# Religion variable: RELIGION_it
religion_w2 <- bk_ar12 %>% 
  select(pidlink, ar15) %>% 
  rename(religion = ar15)

religion_w3 <- bk_ar13 %>% 
  select(pidlink, ar15) %>% 
  rename(religion = ar15)

religion_w4 <- bk_ar14 %>% 
  select(pidlink, ar15) %>% 
  rename(religion = ar15)

religion_w5 <- bk_ar15 %>% 
  select(pidlink, ar15) %>% 
  rename(religion = ar15)

# Married Status and related variable MARSTATE_it
marstat_w2 <- b4_kw12 %>% 
  select(pidlink, kw02) %>% 
  rename(marstat = kw02)

marstat_w3 <- b4_cov3 %>% 
  select(pidlink, marstat) 

marstat_w4 <- b4_cov4 %>% 
  select(pidlink, marstat) 

marstat_w5 <- b4_cov5 %>% 
  select(pidlink, marstat) 

timemar_w2 <- b4_kw12 %>% 
  select(pidlink, kw03) %>% 
  rename(timemar = kw03)

timemar_w3 <- b4_kw23 %>% 
  select(pidlink, kw03)%>% 
  rename(timemar = kw03)

timemar_w4 <- b4_kw24 %>% 
  select(pidlink, kw03)%>% 
  rename(timemar = kw03)

timemar_w5 <- b4_kw25 %>% 
  select(pidlink, kw03)%>% 
  rename(timemar = kw03)

# Married related variables Married_it
married_w2 <- b4_kw22 %>% 
  select(pidlink, kw10yr, kw11, kwn) %>% 
  filter(kwn == 1) %>% 
  rename(ymar = kw10yr, agemar = kw11, marriagenum = kwn)

married_w3 <- b4_kw33 %>% 
  select(pidlink, kw10yr, kw11, kwn) %>% 
  filter(kwn == 1) %>% 
  rename(ymar = kw10yr, agemar = kw11, marriagenum = kwn)

married_w4 <- b4_kw24 %>% 
  select(pidlink, kw10yr, kw11, kwn) %>% 
  filter(kwn == 1) %>% 
  rename(ymar = kw10yr, agemar = kw11, marriagenum = kwn)

married_w5 <- b4_kw35 %>% 
  select(pidlink, kw10yr, kw11, kwn_num) %>% 
  filter(kwn_num == 1) %>% 
  rename(ymar = kw10yr, agemar = kw11, marriagenum = kwn_num)

# Variable related to having another child 
addchild_w2 <- b4_kw12 %>% 
  select(pidlink, hhid97, kw23a, kw23d, kw24, kw25, kw26, kw27a, kw27b) %>% 
  rename(hhid = hhid97, inc = kw23a, menstruate = kw23d, able = kw24, wishmorechild = kw25, numaddchild = kw26, numaddson = kw27a, numadddaugh = kw27b) %>% 
  mutate(menopause = ifelse(menstruate == 3, 1, 0))

addchild_w3 <- b4_kw23 %>% 
  select(pidlink, hhid00, kw23a, kw23d, kw24, kw25, kw26, kw27a, kw27b) %>% 
  rename(hhid = hhid00, inc = kw23a, menstruate = kw23d, able = kw24, wishmorechild = kw25, numaddchild = kw26, numaddson = kw27a, numadddaugh = kw27b) %>% 
  mutate(menopause = ifelse(menstruate == 3, 1, 0))

addchild_w4 <- b4_kw34 %>% 
  select(pidlink, hhid07, kw23a, kw23d, kw24a, kw25, kw26, kw27a, kw27b) %>% 
  rename(hhid = hhid07, inc = kw23a, menstruate = kw23d, able = kw24a, wishmorechild = kw25, numaddchild = kw26, numaddson = kw27a, numadddaugh = kw27b) %>% 
  mutate(menopause = ifelse(menstruate == 3, 1, 0))

addchild_w5 <- b4_kw25 %>% 
  select(pidlink, hhid14, kw23a, kw23d, kw24a, kw25, kw26, kw27a, kw27b) %>% 
  rename(hhid = hhid14, inc = kw23a, menstruate = kw23d, able = kw24a, wishmorechild = kw25, numaddchild = kw26, numaddson = kw27a, numadddaugh = kw27b) %>% 
  mutate(menopause = ifelse(menstruate == 3, 1, 0))

# Variable related to birth 
birth_w2 <- b4_br2 %>%
  select(pidlink, br01, br02, br03, br04, br05, br06, br07, br08, br09, br10, br15, br16) %>% 
  rename(givebirth = br01, livewchild = br02, numson = br03, numdaughter = br04, childliveelsewhere = br05, sonliveelsewhere = br06, daughterliveelsewhere = br07, expchilddead = br08, numchild = br15) %>% 
  mutate(badbirthexp = ifelse(br16 != 0, 1, 0),
         mixgender = ifelse((numson != 0 | numdaughter != 0) & numchild > 1, 1, 0))

birth_w3 <- b4_br3 %>%
  select(pidlink, br01, br02, br03, br04, br05, br06, br07, br08, br09, br10, br15, br16) %>% 
  rename(givebirth = br01, livewchild = br02, numson = br03, numdaughter = br04, childliveelsewhere = br05, sonliveelsewhere = br06, daughterliveelsewhere = br07, expchilddead = br08, numchild = br15) %>% 
  mutate(badbirthexp = ifelse(br16 != 0, 1, 0),
         mixgender = ifelse((numson != 0 | numdaughter != 0) & numchild > 1, 1, 0))

birth_w4 <- b4_br4 %>%
  select(pidlink, br01, br02, br03, br04, br05, br06, br07, br08, br09, br10, br15, br16) %>% 
  rename(givebirth = br01, livewchild = br02, numson = br03, numdaughter = br04, childliveelsewhere = br05, sonliveelsewhere = br06, daughterliveelsewhere = br07, expchilddead = br08, numchild = br15) %>% 
  mutate(badbirthexp = ifelse(br16 != 0, 1, 0),
         mixgender = ifelse((numson != 0 | numdaughter != 0) & numchild > 1, 1, 0))

birth_w5 <- b4_br5 %>%
  select(pidlink, br01, br02, br03, br04, br05, br06, br07, br08, br09, br10, br15, br16) %>% 
  rename(givebirth = br01, livewchild = br02, numson = br03, numdaughter = br04, childliveelsewhere = br05, sonliveelsewhere = br06, daughterliveelsewhere = br07, expchilddead = br08, numchild = br15) %>% 
  mutate(badbirthexp = ifelse(br16 != 0, 1, 0),
         mixgender = ifelse((numson != 0 | numdaughter != 0) & numchild > 1, 1, 0))

# Living arrangement 
livearr_w2 <- b4_kw12 %>% 
  select(pidlink, kw14g) %>% 
  mutate(parliv = ifelse(grepl("B", kw14g), 1, 0),
         parinlawliv = ifelse(grepl("C", kw14g), 1, 0),
         sibliv = ifelse(grepl("D|E", kw14g), 1, 0),
         sibinlawliv = ifelse(grepl("F|G", kw14g), 1, 0))

livearr_w3 <- b4_kw23 %>% 
  select(pidlink, kw14g) %>% 
  mutate(parliv = ifelse(grepl("B", kw14g), 1, 0),
         parinlawliv = ifelse(grepl("C", kw14g), 1, 0),
         sibliv = ifelse(grepl("D|E", kw14g), 1, 0),
         sibinlawliv = ifelse(grepl("F|G", kw14g), 1, 0))

livearr_w4 <- b4_kw24 %>% 
  select(pidlink, kw14g) %>% 
  mutate(parliv = ifelse(grepl("B", kw14g), 1, 0),
         parinlawliv = ifelse(grepl("C", kw14g), 1, 0),
         sibliv = ifelse(grepl("D|E", kw14g), 1, 0),
         sibinlawliv = ifelse(grepl("F|G", kw14g), 1, 0))

livearr_w5 <- b4_kw25 %>% 
  select(pidlink, kw14g) %>% 
  mutate(parliv = ifelse(grepl("B", kw14g), 1, 0),
         parinlawliv = ifelse(grepl("C", kw14g), 1, 0),
         sibliv = ifelse(grepl("D|E", kw14g), 1, 0),
         sibinlawliv = ifelse(grepl("F|G", kw14g), 1, 0))

# Variable related to contraception use
contracept_w2 <- b4_cx22 %>% 
  select(pidlink, cx20, cx26, cx27) %>% 
  rename(contracept = cx20, reasonnouse = cx26, futureusecontracept = cx27)

contracept_w3 <- b4_cx23 %>% 
  select(pidlink, cx20, cx26, cx27) %>% 
  rename(contracept = cx20, reasonnouse = cx26, futureusecontracept = cx27)

contracept_w4 <- b4_cx24 %>% 
  select(pidlink, cx20, cx26, cx27) %>% 
  rename(contracept = cx20, reasonnouse = cx26, futureusecontracept = cx27)

contracept_w5 <- b4_cx25 %>% 
  select(pidlink, cx20, cx26, cx27) %>% 
  rename(contracept = cx20, reasonnouse = cx26, futureusecontracept = cx27)

# Variables related to household decision making
demake_w2 <- b3a_pk22 %>% 
  select(pidlink, hhid97, pk2type, pk18) %>% 
  mutate(demake = ifelse(grepl("^A$", pk18), 1, 0),
         pardemake = ifelse(grepl("^[E|F]$", pk18), 1, 0),
         parinlawdemake = ifelse(grepl("^[G|H]$", pk18), 1, 0),
         joinpar = ifelse(grepl("[EF]", pk18), 1, 0),
         joinilw = ifelse(grepl("[GH]", pk18), 1, 0))

demake_w2_summary <- demake_w2 %>%
  group_by(pidlink) %>%
  summarize(
    total_demake = sum(demake),
    ratio_demake = total_demake/17,
    total_pardemake = sum(pardemake),
    ratio_pardemake = total_pardemake/17,
    total_parinlawdemake = sum(parinlawdemake),
    ratio_parinlawdemake = total_parinlawdemake/17,
    total_joinpar = sum(joinpar),
    ratio_joinpar = total_joinpar/17,
    total_joinilw = sum(joinilw),
    ratio_joinilw = total_joinilw/17)

demake_w3 <- b3a_pk23 %>% 
  select(pidlink, hhid00, pk2type, pk18) %>% 
  mutate(demake = ifelse(grepl("^A$", pk18), 1, 0),
         pardemake = ifelse(grepl("^[E|F]$", pk18), 1, 0),
         parinlawdemake = ifelse(grepl("^[G|H]$", pk18), 1, 0),
         joinpar = ifelse(grepl("[EF]", pk18), 1, 0),
         joinilw = ifelse(grepl("[GH]", pk18), 1, 0))

demake_w3_summary <- demake_w3 %>%
  group_by(pidlink) %>%
  summarize(
    total_demake = sum(demake),
    ratio_demake = total_demake/18,
    total_pardemake = sum(pardemake),
    ratio_pardemake = total_pardemake/18,
    total_parinlawdemake = sum(parinlawdemake),
    ratio_parinlawdemake = total_parinlawdemake/18,
    total_joinpar = sum(joinpar),
    ratio_joinpar = total_joinpar/18,
    total_joinilw = sum(joinilw),
    ratio_joinilw = total_joinilw/18)

demake_w4 <- b3a_pk24 %>% 
  select(pidlink, hhid07, pk2type, pk18) %>% 
  mutate(demake = ifelse(grepl("^A$", pk18), 1, 0),
         pardemake = ifelse(grepl("^[E|F]$", pk18), 1, 0),
         parinlawdemake = ifelse(grepl("^[G|H]$", pk18), 1, 0),
         joinpar = ifelse(grepl("[EF]", pk18), 1, 0),
         joinilw = ifelse(grepl("[GH]", pk18), 1, 0))

demake_w4_summary <- demake_w4 %>%
  group_by(pidlink) %>%
  summarize(
    total_demake = sum(demake),
    ratio_demake = total_demake/18,
    total_pardemake = sum(pardemake),
    ratio_pardemake = total_pardemake/18,
    total_parinlawdemake = sum(parinlawdemake),
    ratio_parinlawdemake = total_parinlawdemake/18,
    total_joinpar = sum(joinpar),
    ratio_joinpar = total_joinpar/18,
    total_joinilw = sum(joinilw),
    ratio_joinilw = total_joinilw/18)

demake_w5 <- b3a_pk25 %>% 
  select(pidlink, hhid14, pk2type, pk18) %>% 
  mutate(demake = ifelse(grepl("^A$", pk18), 1, 0),
         pardemake = ifelse(grepl("^[E|F]$", pk18), 1, 0),
         parinlawdemake = ifelse(grepl("^[G|H]$", pk18), 1, 0),
         joinpar = ifelse(grepl("[EF]", pk18), 1, 0),
         joinilw = ifelse(grepl("[GH]", pk18), 1, 0))

demake_w5_summary <- demake_w5 %>%
  group_by(pidlink) %>%
  summarize(
    total_demake = sum(demake),
    ratio_demake = total_demake/18,
    total_pardemake = sum(pardemake),
    ratio_pardemake = total_pardemake/18,
    total_parinlawdemake = sum(parinlawdemake),
    ratio_parinlawdemake = total_parinlawdemake/18,
    total_joinpar = sum(joinpar),
    ratio_joinpar = total_joinpar/18,
    total_joinilw = sum(joinilw),
    ratio_joinilw = total_joinilw/18)

# All variables related education
education_w2 <- b3a_dl12 %>%
  select(pidlink, dl04, dl06, dl07, dl07a) %>%
  rename(pidlink = pidlink, attended_school = dl04, highest_education = dl06,
         highest_grade=dl07, currently_attending_school = dl07a) %>%
  mutate(attended_school = as.factor(ifelse(attended_school == 8, NA,
                                            ifelse(attended_school == 1, "yes",
                                                   ifelse(attended_school == 3, "no",
                                                          attended_school)))),
         highest_education = as.factor(ifelse(highest_education == 2 | highest_education == 72,
                                              "Elementary",
                                              ifelse(highest_education == 3 |
                                                       highest_education == 4 |
                                                       highest_education == 73,
                                                     "Junior High",
                                                     ifelse(highest_education == 5 |
                                                              highest_education == 6 |
                                                              highest_education == 74,
                                                            "Senior High",
                                                            ifelse(highest_education == 60 |
                                                                     highest_education == 61 |
                                                                     highest_education == 62 |
                                                                     highest_education == 63 |
                                                                     highest_education == 13,
                                                                   "University", NA))))),#"other" = NA
         highest_grade = ifelse(highest_grade == 98, NA,
                                ifelse(highest_grade == 99, NA, highest_grade)),
         currently_attending_school = as.factor(ifelse(currently_attending_school == 1, "yes",
                                                       ifelse(currently_attending_school == 3, "no", NA))))

## Create variable that includes years of education (highest_education.highest_grade) as a numeric variable
# People who started an education level, but dropped out within the first year, are coded with half a year of schooling
education_w2 <- education_w2 %>%
  mutate(years_of_education_factor = as.factor(str_c(highest_education, ".", highest_grade)),
         years_of_education = as.numeric(ifelse(attended_school == "no", 0,
                                                ifelse(years_of_education_factor == "Elementary.0", 0.5,
                                                       ifelse(years_of_education_factor == "Elementary.1", 1,
                                                              ifelse(years_of_education_factor == "Elementary.2", 2,
                                                                     ifelse(years_of_education_factor == "Elementary.3", 3,
                                                                            ifelse(years_of_education_factor == "Elementary.4", 4,
                                                                                   ifelse(years_of_education_factor == "Elementary.5", 5,
                                                                                          ifelse(years_of_education_factor == "Elementary.6", NA,
                                                                                                 ifelse(years_of_education_factor == "Elementary.7", 6,
                                                                                                        ifelse(years_of_education_factor == "Junior High.0", 6.5,
                                                                                                               ifelse(years_of_education_factor == "Junior High.1", 7,
                                                                                                                      ifelse(years_of_education_factor == "Junior High.2", 8,
                                                                                                                             ifelse(years_of_education_factor == "Junior High.3", NA,
                                                                                                                                    ifelse(years_of_education_factor == "Junior High.7", 9,
                                                                                                                                           ifelse(years_of_education_factor == "Senior High.0", 9.5,
                                                                                                                                                  ifelse(years_of_education_factor == "Senior High.1", 10,
                                                                                                                                                         ifelse(years_of_education_factor == "Senior High.2", 11,
                                                                                                                                                                ifelse(years_of_education_factor == "Senior High.3", NA,
                                                                                                                                                                       ifelse(years_of_education_factor == "Senior High.7", 12,
                                                                                                                                                                              ifelse(years_of_education_factor == "University.0", 12.5,
                                                                                                                                                                                     ifelse(years_of_education_factor == "University.1", 13,
                                                                                                                                                                                            ifelse(years_of_education_factor == "University.2", 14,
                                                                                                                                                                                                   ifelse(years_of_education_factor == "University.3", 15,
                                                                                                                                                                                                          ifelse(years_of_education_factor == "University.4", 16,
                                                                                                                                                                                                                 ifelse(years_of_education_factor == "University.5", 17,
                                                                                                                                                                                                                        ifelse(years_of_education_factor == "University.6", NA,
                                                                                                                                                                                                                               ifelse(years_of_education_factor == "University.7", 18,
                                                                                                                                                                                                                                      NA)))))))))))))))))))))))))))))

education_w3 <- b3a_dl13 %>%
  select(pidlink, dl04, dl06, dl07, dl07a) %>%
  rename(pidlink = pidlink, attended_school = dl04, highest_education = dl06,
         highest_grade=dl07, currently_attending_school = dl07a) %>%
  mutate(attended_school = as.factor(ifelse(attended_school == 8, NA,
                                            ifelse(attended_school == 1, "yes",
                                                   ifelse(attended_school == 3, "no",
                                                          attended_school)))),
         highest_education = as.factor(ifelse(highest_education == 2 | highest_education == 72,
                                              "Elementary",
                                              ifelse(highest_education == 3 |
                                                       highest_education == 4 |
                                                       highest_education == 73,
                                                     "Junior High",
                                                     ifelse(highest_education == 5 |
                                                              highest_education == 6 |
                                                              highest_education == 74,
                                                            "Senior High",
                                                            ifelse(highest_education == 60 |
                                                                     highest_education == 61 |
                                                                     highest_education == 62 |
                                                                     highest_education == 63 |
                                                                     highest_education == 13,
                                                                   "University", NA))))),#"other" = NA
         highest_grade = ifelse(highest_grade == 98, NA,
                                ifelse(highest_grade == 99, NA, highest_grade)),
         currently_attending_school = as.factor(ifelse(currently_attending_school == 1, "yes",
                                                       ifelse(currently_attending_school == 3, "no", NA))))

## Create variable that includes years of education (highest_education.highest_grade) as a numeric variable
# People who started an education level, but dropped out within the first year, are coded with half a year of schooling
education_w3 <- education_w3 %>%
  mutate(years_of_education_factor = as.factor(str_c(highest_education, ".", highest_grade)),
         years_of_education = as.numeric(ifelse(attended_school == "no", 0,
                                                ifelse(years_of_education_factor == "Elementary.0", 0.5,
                                                       ifelse(years_of_education_factor == "Elementary.1", 1,
                                                              ifelse(years_of_education_factor == "Elementary.2", 2,
                                                                     ifelse(years_of_education_factor == "Elementary.3", 3,
                                                                            ifelse(years_of_education_factor == "Elementary.4", 4,
                                                                                   ifelse(years_of_education_factor == "Elementary.5", 5,
                                                                                          ifelse(years_of_education_factor == "Elementary.6", NA,
                                                                                                 ifelse(years_of_education_factor == "Elementary.7", 6,
                                                                                                        ifelse(years_of_education_factor == "Junior High.0", 6.5,
                                                                                                               ifelse(years_of_education_factor == "Junior High.1", 7,
                                                                                                                      ifelse(years_of_education_factor == "Junior High.2", 8,
                                                                                                                             ifelse(years_of_education_factor == "Junior High.3", NA,
                                                                                                                                    ifelse(years_of_education_factor == "Junior High.7", 9,
                                                                                                                                           ifelse(years_of_education_factor == "Senior High.0", 9.5,
                                                                                                                                                  ifelse(years_of_education_factor == "Senior High.1", 10,
                                                                                                                                                         ifelse(years_of_education_factor == "Senior High.2", 11,
                                                                                                                                                                ifelse(years_of_education_factor == "Senior High.3", NA,
                                                                                                                                                                       ifelse(years_of_education_factor == "Senior High.7", 12,
                                                                                                                                                                              ifelse(years_of_education_factor == "University.0", 12.5,
                                                                                                                                                                                     ifelse(years_of_education_factor == "University.1", 13,
                                                                                                                                                                                            ifelse(years_of_education_factor == "University.2", 14,
                                                                                                                                                                                                   ifelse(years_of_education_factor == "University.3", 15,
                                                                                                                                                                                                          ifelse(years_of_education_factor == "University.4", 16,
                                                                                                                                                                                                                 ifelse(years_of_education_factor == "University.5", 17,
                                                                                                                                                                                                                        ifelse(years_of_education_factor == "University.6", NA,
                                                                                                                                                                                                                               ifelse(years_of_education_factor == "University.7", 18,
                                                                                                                                                                                                                                      NA)))))))))))))))))))))))))))))

education_w4 <- b3a_dl14 %>%
  select(pidlink, dl04, dl06, dl07, dl07a) %>%
  rename(pidlink = pidlink, attended_school = dl04, highest_education = dl06,
         highest_grade=dl07, currently_attending_school = dl07a) %>%
  mutate(attended_school = as.factor(ifelse(attended_school == 8, NA,
                                            ifelse(attended_school == 1, "yes",
                                                   ifelse(attended_school == 3, "no",
                                                          attended_school)))),
         highest_education = as.factor(ifelse(highest_education == 2 | highest_education == 72,
                                              "Elementary",
                                              ifelse(highest_education == 3 |
                                                       highest_education == 4 |
                                                       highest_education == 73,
                                                     "Junior High",
                                                     ifelse(highest_education == 5 |
                                                              highest_education == 6 |
                                                              highest_education == 74,
                                                            "Senior High",
                                                            ifelse(highest_education == 60 |
                                                                     highest_education == 61 |
                                                                     highest_education == 62 |
                                                                     highest_education == 63 |
                                                                     highest_education == 13,
                                                                   "University", NA))))),#"other" = NA
         highest_grade = ifelse(highest_grade == 98, NA,
                                ifelse(highest_grade == 99, NA, highest_grade)),
         currently_attending_school = as.factor(ifelse(currently_attending_school == 1, "yes",
                                                       ifelse(currently_attending_school == 3, "no", NA))))

## Create variable that includes years of education (highest_education.highest_grade) as a numeric variable
# People who started an education level, but dropped out within the first year, are coded with half a year of schooling
education_w4 <- education_w4 %>%
  mutate(years_of_education_factor = as.factor(str_c(highest_education, ".", highest_grade)),
         years_of_education = as.numeric(ifelse(attended_school == "no", 0,
                                                ifelse(years_of_education_factor == "Elementary.0", 0.5,
                                                       ifelse(years_of_education_factor == "Elementary.1", 1,
                                                              ifelse(years_of_education_factor == "Elementary.2", 2,
                                                                     ifelse(years_of_education_factor == "Elementary.3", 3,
                                                                            ifelse(years_of_education_factor == "Elementary.4", 4,
                                                                                   ifelse(years_of_education_factor == "Elementary.5", 5,
                                                                                          ifelse(years_of_education_factor == "Elementary.6", NA,
                                                                                                 ifelse(years_of_education_factor == "Elementary.7", 6,
                                                                                                        ifelse(years_of_education_factor == "Junior High.0", 6.5,
                                                                                                               ifelse(years_of_education_factor == "Junior High.1", 7,
                                                                                                                      ifelse(years_of_education_factor == "Junior High.2", 8,
                                                                                                                             ifelse(years_of_education_factor == "Junior High.3", NA,
                                                                                                                                    ifelse(years_of_education_factor == "Junior High.7", 9,
                                                                                                                                           ifelse(years_of_education_factor == "Senior High.0", 9.5,
                                                                                                                                                  ifelse(years_of_education_factor == "Senior High.1", 10,
                                                                                                                                                         ifelse(years_of_education_factor == "Senior High.2", 11,
                                                                                                                                                                ifelse(years_of_education_factor == "Senior High.3", NA,
                                                                                                                                                                       ifelse(years_of_education_factor == "Senior High.7", 12,
                                                                                                                                                                              ifelse(years_of_education_factor == "University.0", 12.5,
                                                                                                                                                                                     ifelse(years_of_education_factor == "University.1", 13,
                                                                                                                                                                                            ifelse(years_of_education_factor == "University.2", 14,
                                                                                                                                                                                                   ifelse(years_of_education_factor == "University.3", 15,
                                                                                                                                                                                                          ifelse(years_of_education_factor == "University.4", 16,
                                                                                                                                                                                                                 ifelse(years_of_education_factor == "University.5", 17,
                                                                                                                                                                                                                        ifelse(years_of_education_factor == "University.6", NA,
                                                                                                                                                                                                                               ifelse(years_of_education_factor == "University.7", 18,
                                                                                                                                                                                                                                      NA)))))))))))))))))))))))))))))

education_w5 <- b3a_dl15 %>%
  select(pidlink, dl04, dl06, dl07, dl07a) %>%
  rename(pidlink = pidlink, attended_school = dl04, highest_education = dl06,
         highest_grade=dl07, currently_attending_school = dl07a) %>%
  mutate(attended_school = as.factor(ifelse(attended_school == 8, NA,
                                            ifelse(attended_school == 1, "yes",
                                                   ifelse(attended_school == 3, "no",
                                                          attended_school)))),
         highest_education = as.factor(ifelse(highest_education == 2 | highest_education == 72,
                                              "Elementary",
                                              ifelse(highest_education == 3 |
                                                       highest_education == 4 |
                                                       highest_education == 73,
                                                     "Junior High",
                                                     ifelse(highest_education == 5 |
                                                              highest_education == 6 |
                                                              highest_education == 74,
                                                            "Senior High",
                                                            ifelse(highest_education == 60 |
                                                                     highest_education == 61 |
                                                                     highest_education == 62 |
                                                                     highest_education == 63 |
                                                                     highest_education == 13,
                                                                   "University", NA))))),#"other" = NA
         highest_grade = ifelse(highest_grade == 98, NA,
                                ifelse(highest_grade == 99, NA, highest_grade)),
         currently_attending_school = as.factor(ifelse(currently_attending_school == 1, "yes",
                                                       ifelse(currently_attending_school == 3, "no", NA))))

## Create variable that includes years of education (highest_education.highest_grade) as a numeric variable
# People who started an education level, but dropped out within the first year, are coded with half a year of schooling
education_w5 <- education_w5 %>%
  mutate(years_of_education_factor = as.factor(str_c(highest_education, ".", highest_grade)),
         years_of_education = as.numeric(ifelse(attended_school == "no", 0,
                                                ifelse(years_of_education_factor == "Elementary.0", 0.5,
                                                       ifelse(years_of_education_factor == "Elementary.1", 1,
                                                              ifelse(years_of_education_factor == "Elementary.2", 2,
                                                                     ifelse(years_of_education_factor == "Elementary.3", 3,
                                                                            ifelse(years_of_education_factor == "Elementary.4", 4,
                                                                                   ifelse(years_of_education_factor == "Elementary.5", 5,
                                                                                          ifelse(years_of_education_factor == "Elementary.6", NA,
                                                                                                 ifelse(years_of_education_factor == "Elementary.7", 6,
                                                                                                        ifelse(years_of_education_factor == "Junior High.0", 6.5,
                                                                                                               ifelse(years_of_education_factor == "Junior High.1", 7,
                                                                                                                      ifelse(years_of_education_factor == "Junior High.2", 8,
                                                                                                                             ifelse(years_of_education_factor == "Junior High.3", NA,
                                                                                                                                    ifelse(years_of_education_factor == "Junior High.7", 9,
                                                                                                                                           ifelse(years_of_education_factor == "Senior High.0", 9.5,
                                                                                                                                                  ifelse(years_of_education_factor == "Senior High.1", 10,
                                                                                                                                                         ifelse(years_of_education_factor == "Senior High.2", 11,
                                                                                                                                                                ifelse(years_of_education_factor == "Senior High.3", NA,
                                                                                                                                                                       ifelse(years_of_education_factor == "Senior High.7", 12,
                                                                                                                                                                              ifelse(years_of_education_factor == "University.0", 12.5,
                                                                                                                                                                                     ifelse(years_of_education_factor == "University.1", 13,
                                                                                                                                                                                            ifelse(years_of_education_factor == "University.2", 14,
                                                                                                                                                                                                   ifelse(years_of_education_factor == "University.3", 15,
                                                                                                                                                                                                          ifelse(years_of_education_factor == "University.4", 16,
                                                                                                                                                                                                                 ifelse(years_of_education_factor == "University.5", 17,
                                                                                                                                                                                                                        ifelse(years_of_education_factor == "University.6", NA,
                                                                                                                                                                                                                               ifelse(years_of_education_factor == "University.7", 18,
                                                                                                                                                                                                                                      NA)))))))))))))))))))))))))))))


# Create parent pidlink to create birth order related variable
# IFLS 2
parent_pidlink2 <- bk_ar12 %>%
  left_join(
    select(., hhid97, pid97, pidlink) %>% rename(ar10 = pid97, father_pidlink = pidlink),
    by = c("hhid97", "ar10")
  ) %>%
  left_join(
    select(., hhid97, pid97, pidlink) %>% rename(ar11 = pid97, mother_pidlink = pidlink),
    by = c("hhid97", "ar11")
  )

# Generate observations_w5 and rename variables
observations_w2 <- parent_pidlink2 %>% 
  select(
    hhid = hhid97, pid = pid97, pidlink, father_pidlink, mother_pidlink,
    relation_to_HH_head = ar02b, fatherID = ar10, motherID = ar11,
    sex = ar07, age = ar09, status = ar01a, ar08mth, ar08yr)

# Variables to check and process
variables_to_check <- c("age", "ar08yr")
# Define a function to process a variable
process_variable <- function(variable) {
  cat("Processing variable:", variable, "\n")
  print(table(observations_w2[[variable]]))
  observations_w2 <- observations_w2[!(observations_w2[[variable]] %in% c(998, 999, 9998, 99, 98)), ]
  print(table(observations_w2[[variable]]))
  return(observations_w2)
}

# Apply the function to each variable using purrr's map function
observations_w2 <- reduce(map(variables_to_check, process_variable), ~ .x)
# Check for duplicated observations based on 'hhid' and 'pidlink' variables
duplicated_rows2 <- observations_w2[duplicated(observations_w2[c('hhid', 'pidlink')]), ]
# Tabulate the duplicated observations
table(duplicated_rows2$status)
# Remove duplicats because some observations are mentioned more than once, (e.g. duplication (value 6))
observations_w2 <- observations_w2 %>% distinct(pidlink, .keep_all = TRUE)

## Create date of birth
#Set all variables missing that have not been reported:
observations_w2 <- parent_pidlink2 %>% 
  mutate(
    ar08mth = ifelse(ar08mth > 12 | is.nan(ar08mth), NA, ar08mth),
    ar08yr = ifelse(ar08yr > 1998 | is.nan(ar08yr), NA, ar08yr)
  )
# Create birthdate & mother birthdate
birth_order_w2 <- observations_w2 %>%
  mutate(
    birthdate = str_c(ar08yr, "/", ar08mth),
    mother_birthdate = str_c(mother_pidlink, "-", birthdate)
  ) 

birth_order_w2 <- birth_order_w2 %>% 
  group_by(mother_pidlink) %>%
  mutate(
    any_multiple_birthdate = if_else(any(duplicated(birthdate) & !is.na(ar08yr)), 1, 0),
    twins_dummy = if_else(duplicated(birthdate) & !is.na(mother_pidlink), 1, 0)
  )

# Tabulate the multiple birthdate and twins dummy
table(birth_order_w2$any_multiple_birthdate)
table(birth_order_w2$twins_dummy)

# Create Naive birthorder & Sibling counts
birth_order_w2 <- birth_order_w2 %>% 
  group_by(mother_pidlink) %>% 
  mutate(
    naive_birthorder = if_else(!is.na(mother_pidlink), min_rank(ar08yr), NA_integer_),
    naive_sibling_count = if_else(!is.na(mother_pidlink), n(), NA_integer_)
  ) %>% 
  ungroup()

birth_order_w2 <- birth_order_w2 %>%
  group_by(mother_pidlink) %>%
  mutate(
    birth_order_category = ifelse(
      is.na(naive_birthorder) | is.na(naive_sibling_count), NA,
      ifelse(naive_birthorder == 1 & naive_sibling_count > 1, "oldest",
             ifelse(naive_birthorder == max(naive_birthorder, na.rm = TRUE) & naive_sibling_count > 1, "youngest",
                    ifelse(naive_birthorder == 1 & naive_sibling_count == 1, "only child", "middle child")))
    )
  ) %>%
  ungroup()
# Select specific variables needed
birth_order_w2 <- birth_order_w2 %>% 
  select(hhid97, pidlink, mother_pidlink, naive_birthorder, naive_sibling_count, birth_order_category) %>% 
  rename(hhid = hhid97)

# IFLS 3 
parent_pidlink3 <- bk_ar13 %>%
  left_join(
    select(., hhid00, pid00, pidlink) %>% rename(ar10 = pid00, father_pidlink = pidlink),
    by = c("hhid00", "ar10")
  ) %>%
  left_join(
    select(., hhid00, pid00, pidlink) %>% rename(ar11 = pid00, mother_pidlink = pidlink),
    by = c("hhid00", "ar11")
  )

# Generate observations_w3 and rename variables
observations_w3 <- parent_pidlink3 %>% 
  select(
    hhid = hhid00, pid = pid00, pidlink, father_pidlink, mother_pidlink,
    relation_to_HH_head = ar02b, fatherID = ar10, motherID = ar11,
    sex = ar07, age = ar09, status = ar01a, ar08mth, ar08yr)

# Variables to check and process
variables_to_check <- c("age", "ar08yr")

# Define a function to process a variable
process_variable <- function(variable) {
  cat("Processing variable:", variable, "\n")
  print(table(observations_w3[[variable]]))
  observations_w3 <- observations_w3[!(observations_w3[[variable]] %in% c(998, 999, 9998, 99, 98)), ]
  print(table(observations_w3[[variable]]))
  return(observations_w3)
}

# Apply the function to each variable using purrr's map function
observations_w3 <- reduce(map(variables_to_check, process_variable), ~ .x)
# Check for duplicated observations based on 'hhid' and 'pidlink' variables
duplicated_rows3 <- observations_w3[duplicated(observations_w3[c('hhid', 'pidlink')]), ]
# Tabulate the duplicated observations
table(duplicated_rows3$status)
# Remove duplicats because some observations are mentioned more than once, (e.g. duplication (value 6))
observations_w3 <- observations_w3 %>% distinct(pidlink, .keep_all = TRUE)

## Create date of birth
#Set all variables missing that have not been reported:
observations_w3 <- parent_pidlink3 %>% 
  mutate(
    ar08mth = ifelse(ar08mth > 12 | is.nan(ar08mth), NA, ar08mth),
    ar08yr = ifelse(ar08yr > 2001 | is.nan(ar08yr), NA, ar08yr)
  )
# Create birthdate & mother birthdate
birth_order_w3 <- observations_w3 %>%
  mutate(
    birthdate = str_c(ar08yr, "/", ar08mth),
    mother_birthdate = str_c(mother_pidlink, "-", birthdate)
  ) 

birth_order_w3 <- birth_order_w3 %>% 
  group_by(mother_pidlink) %>%
  mutate(
    any_multiple_birthdate = if_else(any(duplicated(birthdate) & !is.na(ar08yr)), 1, 0),
    twins_dummy = if_else(duplicated(birthdate) & !is.na(mother_pidlink), 1, 0)
  )

# Tabulate the multiple birthdate and twins dummy
table(birth_order_w3$any_multiple_birthdate)
table(birth_order_w3$twins_dummy)

# Create Naive birthorder & Sibling counts
birth_order_w3 <- birth_order_w3 %>% 
  group_by(mother_pidlink) %>% 
  mutate(
    naive_birthorder = if_else(!is.na(mother_pidlink), min_rank(ar08yr), NA_integer_),
    naive_sibling_count = if_else(!is.na(mother_pidlink), n(), NA_integer_)
  ) %>% 
  ungroup()

birth_order_w3 <- birth_order_w3 %>%
  group_by(mother_pidlink) %>%
  mutate(
    birth_order_category = ifelse(
      is.na(naive_birthorder) | is.na(naive_sibling_count), NA,
      ifelse(naive_birthorder == 1 & naive_sibling_count > 1, "oldest",
             ifelse(naive_birthorder == max(naive_birthorder, na.rm = TRUE) & naive_sibling_count > 1, "youngest",
                    ifelse(naive_birthorder == 1 & naive_sibling_count == 1, "only child", "middle child")))
    )
  ) %>%
  ungroup()
# Select specific variables needed
birth_order_w3 <- birth_order_w3 %>% 
  select(hhid00, pidlink, mother_pidlink, naive_birthorder, naive_sibling_count, birth_order_category) %>% 
  rename(hhid = hhid00)

# IFLS 4
parent_pidlink4 <- bk_ar14 %>%
  left_join(
    select(., hhid07, pid07, pidlink) %>% rename(ar10 = pid07, father_pidlink = pidlink),
    by = c("hhid07", "ar10")
  ) %>%
  left_join(
    select(., hhid07, pid07, pidlink) %>% rename(ar11 = pid07, mother_pidlink = pidlink),
    by = c("hhid07", "ar11")
  )

# Generate observations_w4 and rename variables
observations_w4 <- parent_pidlink4 %>% 
  select(
    hhid = hhid07, pid = pid07, pidlink, father_pidlink, mother_pidlink,
    relation_to_HH_head = ar02b, fatherID = ar10, motherID = ar11,
    sex = ar07, age = ar09, status = ar01a, ar08mth, ar08yr)

# Variables to check and process
variables_to_check <- c("age", "ar08yr")

# Define a function to process a variable
process_variable <- function(variable) {
  cat("Processing variable:", variable, "\n")
  print(table(observations_w4[[variable]]))
  observations_w4 <- observations_w4[!(observations_w4[[variable]] %in% c(998, 999, 9998, 99, 98)), ]
  print(table(observations_w4[[variable]]))
  return(observations_w4)
}

# Apply the function to each variable using purrr's map function
observations_w4 <- reduce(map(variables_to_check, process_variable), ~ .x)
# Check for duplicated observations based on 'hhid' and 'pidlink' variables
duplicated_rows4 <- observations_w4[duplicated(observations_w4[c('hhid', 'pidlink')]), ]
# Tabulate the duplicated observations
table(duplicated_rows4$status)
# Remove duplicats because some observations are mentioned more than once, (e.g. duplication (value 6))
observations_w4 <- observations_w4 %>% distinct(pidlink, .keep_all = TRUE)

## Create date of birth
#Set all variables missing that have not been reported:
observations_w4 <- parent_pidlink4 %>% 
  mutate(
    ar08mth = ifelse(ar08mth > 12 | is.nan(ar08mth), NA, ar08mth),
    ar08yr = ifelse(ar08yr > 2008 | is.nan(ar08yr), NA, ar08yr)
  )
# Create birthdate & mother birthdate
birth_order_w4 <- observations_w4 %>%
  mutate(
    birthdate = str_c(ar08yr, "/", ar08mth),
    mother_birthdate = str_c(mother_pidlink, "-", birthdate)
  ) 

birth_order_w4 <- birth_order_w4 %>% 
  group_by(mother_pidlink) %>%
  mutate(
    any_multiple_birthdate = if_else(any(duplicated(birthdate) & !is.na(ar08yr)), 1, 0),
    twins_dummy = if_else(duplicated(birthdate) & !is.na(mother_pidlink), 1, 0)
  )

# Tabulate the multiple birthdate and twins dummy
table(birth_order_w4$any_multiple_birthdate)
table(birth_order_w4$twins_dummy)

# Create Naive birthorder & Sibling counts
birth_order_w4 <- birth_order_w4 %>% 
  group_by(mother_pidlink) %>% 
  mutate(
    naive_birthorder = if_else(!is.na(mother_pidlink), min_rank(ar08yr), NA_integer_),
    naive_sibling_count = if_else(!is.na(mother_pidlink), n(), NA_integer_)
  ) %>% 
  ungroup()

birth_order_w4 <- birth_order_w4 %>%
  group_by(mother_pidlink) %>%
  mutate(
    birth_order_category = ifelse(
      is.na(naive_birthorder) | is.na(naive_sibling_count), NA,
      ifelse(naive_birthorder == 1 & naive_sibling_count > 1, "oldest",
             ifelse(naive_birthorder == max(naive_birthorder, na.rm = TRUE) & naive_sibling_count > 1, "youngest",
                    ifelse(naive_birthorder == 1 & naive_sibling_count == 1, "only child", "middle child")))
    )
  ) %>%
  ungroup()
# Select specific variables needed
birth_order_w4 <- birth_order_w4 %>% 
  select(hhid07, pidlink, mother_pidlink, naive_birthorder, naive_sibling_count, birth_order_category) %>% 
  rename(hhid = hhid07)

# IFLS 5
parent_pidlink5 <- bk_ar15 %>%
  left_join(
    select(., hhid14, pid14, pidlink) %>% rename(ar10 = pid14, father_pidlink = pidlink),
    by = c("hhid14", "ar10")
  ) %>%
  left_join(
    select(., hhid14, pid14, pidlink) %>% rename(ar11 = pid14, mother_pidlink = pidlink),
    by = c("hhid14", "ar11")
  )

# Generate observations_w5 and rename variables
observations_w5 <- parent_pidlink5 %>% 
  select(
    hhid = hhid14, pid = pid14, pidlink, father_pidlink, mother_pidlink,
    relation_to_HH_head = ar02b, fatherID = ar10, motherID = ar11,
    sex = ar07, age = ar09, status = ar01a, ar08mth, ar08yr)

# Variables to check and process
variables_to_check <- c("age", "ar08yr")

# Define a function to process a variable
process_variable <- function(variable) {
  cat("Processing variable:", variable, "\n")
  print(table(observations_w5[[variable]]))
  observations_w5 <- observations_w5[!(observations_w5[[variable]] %in% c(998, 999, 9998, 99, 98)), ]
  print(table(observations_w5[[variable]]))
  return(observations_w5)
}

# Apply the function to each variable using purrr's map function
observations_w5 <- reduce(map(variables_to_check, process_variable), ~ .x)
# Check for duplicated observations based on 'hhid' and 'pidlink' variables
duplicated_rows5 <- observations_w5[duplicated(observations_w5[c('hhid', 'pidlink')]), ]
# Tabulate the duplicated observations
table(duplicated_rows5$status)
# Remove duplicats because some observations are mentioned more than once, (e.g. duplication (value 6))
observations_w5 <- observations_w5 %>% distinct(pidlink, .keep_all = TRUE)

## Create date of birth
#Set all variables missing that have not been reported:
observations_w5 <- parent_pidlink5 %>% 
  mutate(
    ar08mth = ifelse(ar08mth > 12 | is.nan(ar08mth), NA, ar08mth),
    ar08yr = ifelse(ar08yr > 2015 | is.nan(ar08yr), NA, ar08yr)
  )
# Create birthdate & mother birthdate
birth_order_w5 <- observations_w5 %>%
  mutate(
    birthdate = str_c(ar08yr, "/", ar08mth),
    mother_birthdate = str_c(mother_pidlink, "-", birthdate)
  ) 

birth_order_w5 <- birth_order_w5 %>% 
  group_by(mother_pidlink) %>%
  mutate(
    any_multiple_birthdate = if_else(any(duplicated(birthdate) & !is.na(ar08yr)), 1, 0),
    twins_dummy = if_else(duplicated(birthdate) & !is.na(mother_pidlink), 1, 0)
  )

# Tabulate the multiple birthdate and twins dummy
table(birth_order_w5$any_multiple_birthdate)
table(birth_order_w5$twins_dummy)

# Create Naive birthorder & Sibling counts
birth_order_w5 <- birth_order_w5 %>% 
  group_by(mother_pidlink) %>% 
  mutate(
    naive_birthorder = if_else(!is.na(mother_pidlink), min_rank(ar08yr), NA_integer_),
    naive_sibling_count = if_else(!is.na(mother_pidlink), n(), NA_integer_)
  ) %>% 
  ungroup()

birth_order_w5 <- birth_order_w5 %>%
  group_by(mother_pidlink) %>%
  mutate(
    birth_order_category = ifelse(
      is.na(naive_birthorder) | is.na(naive_sibling_count), NA,
      ifelse(naive_birthorder == 1 & naive_sibling_count > 1, "oldest",
             ifelse(naive_birthorder == max(naive_birthorder, na.rm = TRUE) & naive_sibling_count > 1, "youngest",
                    ifelse(naive_birthorder == 1 & naive_sibling_count == 1, "only child", "middle child")))
    )
  ) %>%
  ungroup()
# Select specific variables needed
birth_order_w5 <- birth_order_w5 %>% 
  select(hhid14, pidlink, mother_pidlink, naive_birthorder, naive_sibling_count, birth_order_category) %>% 
  rename(hhid = hhid14)

# Main activity of during past week
main_activity_w2 <- b3a_tk12 %>% 
  select(pidlink, tk01) %>% 
  rename(main_activity = tk01)

main_activity_w3 <- b3a_tk13 %>% 
  select(pidlink, tk01) %>% 
  rename(main_activity = tk01)

main_activity_w4 <- b3a_tk14 %>% 
  select(pidlink, tk01) %>% 
  rename(main_activity = tk01)

main_activity_w5 <- b3a_tk15 %>% 
  select(pidlink, tk01) %>% 
  rename(main_activity = tk01)

## household_exp_w ## 
## It is expected to accommodate variable: food expenditure and nonfood expenditure, household expenditure & per capita expenditure
# household_exp_w2
# Generating household monthly food expenditure 
# Food expenditure data is available in "b1_ks12.dta"
# The food data is captured in "ks1type" and "ks02"
b1_ks12 <- b1_ks12 %>% 
  select(hhid97, ks1type, ks02) %>% 
  rename(hhid = hhid97)

# The data is currently structured in a long format. It needs to get restructured in a wide format.
food_exp_w2 <- b1_ks12 %>% 
  pivot_wider(names_from = ks1type, values_from = ks02)

# Calculate household food expenditure 
food_exp_w2$food <- (rowSums(food_exp_w2[,-1]))*52/12 #calculating monthly expenditure
food_exp_w2 <- food_exp_w2[,c("hhid","food")]

# Generating household monthly non-food expenditure
# Non-food expenditure data is available in "b1_ks04", "b1_ks24", "b1_ks34", and "b2_kr4"
b1_ks22 <- b1_ks22 %>% 
  select(hhid97, ks2type, ks06) %>% 
  rename(hhid = hhid97)
# The data is currently structured in a long format. It needs to get restructured in a wide format.
nonfood_exp1_w2 <- b1_ks22 %>% 
  pivot_wider(names_from = ks2type, values_from = ks06)
# Now that the data is already in a wide format. It needs to get calculated in monthly non-food total expenditure.
nonfood_exp1_w2$nonfood1 <- (rowSums(nonfood_exp1_w2[,2:10])) #calculating monthly non-food expenditure
nonfood_exp1_w2 <- nonfood_exp1_w2[,c("hhid","nonfood1")]

b1_ks32 <- b1_ks32 %>% 
  select(hhid97, ks3type, ks08) %>% 
  rename(hhid = hhid97)
# The data is currently structured in a long format. It needs to get restructured in a wide format.
nonfood_exp2_w2 <- b1_ks32 %>% 
  pivot_wider(names_from = ks3type, values_from = ks08)
# Now that the data is already in a wide format. It needs to get calculated in monthly non-food total expenditure.
nonfood_exp2_w2$nonfood2 <- (rowSums(nonfood_exp2_w2[,-1]))/12 #calculating monthly non-food expenditure
nonfood_exp2_w2 <- nonfood_exp2_w2[,c("hhid","nonfood2")]

# Non-food (Housing Rent)
#Note: The non-food data (housing rent) is captured in "kr04a" -- to simplify the data frame, the "b2_kr" variable will be restructured to only reflect household ID ("hhid97"), monthly/annual rent indicator ("kr04"), and housing rent expenditure ("kr04a")
rent_exp_w2 <- b2_kr2 %>% 
  select(hhid97, kr04) %>% 
  rename(hhid = hhid97)

rent_exp_w2$kr04[is.na(rent_exp_w2$kr04)] <- 0 #fill in the NAs with 0
rent_exp_w2 <- rent_exp_w2 %>% 
  select(hhid, kr04) %>% 
  rename(rent_exp = kr04)

# Non-food (Education-related expenditure)
#Education-related expenditure (annual) is captured in "ks10aa", "ks11aa", "ks12aa"
#The data is already in a wide format. 
edu_exp_w2 <- b1_ks02 %>% 
  select(hhid97, ks10aa, ks11aa, ks12aa) %>% 
  rename(hhid = hhid97)
#Since it is an annual expenditure, the data needs to be standardized into monthly education expenditure
edu_exp_w2$edu_exp <- (rowSums(edu_exp_w2[,-1]))/12
edu_exp_w2 <- edu_exp_w2[,c("hhid","edu_exp")]

## Aggregating the non-food expenditures
# Non-food = nonfood_exp1_w +nonfood_exp2_w + rent_exp_w + edu_exp_w
# Merging nonfood_exp1_w and nonfood_exp2_w
nonfood_exp_w2 <- merge(nonfood_exp1_w2, nonfood_exp2_w2, by="hhid")
# Merging nonfood_exp and rent_exp_w
nonfood_rent_w2 <- merge(nonfood_exp_w2,rent_exp_w2, by="hhid")
# Merging nonfood_rent_w and edu_exp_w
nonfood_totexp_w2 <- merge(nonfood_rent_w2, edu_exp_w2, by="hhid")
# Calculating total non food expenditure
attach(nonfood_totexp_w2)
nonfood_totexp_w2$sum_nonfood_exp <- nonfood1 + nonfood2 + rent_exp + edu_exp
## Aggregating the non-food and food expenditures 
household_exp_w2 <- merge(nonfood_totexp_w2, food_exp_w2, by="hhid")
household_exp_w2 <- household_exp_w2 %>%
  mutate(sum_exp = sum_nonfood_exp + food)

## Generating household size  
#Household size data are available in"bk_ar14"                     
#ar01a is a question on whether the listed household member is still a part of the same household
#Only include answers ar01a=1, ar01a=2, and ar01a=5, and ar01a=11 -- ar01a=0 is a code for "has died" and ar01a=3 is a code for "no longer part of the same household"                  
household_size <- bk_ar12[bk_ar12$ar01a == 1 | bk_ar12$ar01a == 2 | bk_ar12$ar01a == 5 | bk_ar12$ar01a == 11,]
household_size_w2 <- data.frame(count(household_size,hhid97))
household_size_w2 <- household_size_w2 %>% rename(hhid = hhid97)
## Merging household expenditure and household size 
household_exp_w2 <- merge(household_exp_w2, household_size_w2, by="hhid")
household_exp_w2 <- household_exp_w2 %>% 
  mutate(pce = sum_exp/n)
# Transform per capita expenditure into log natural
household_exp_w2 <- household_exp_w2 %>% 
  mutate(lnpce = log(pce))
# Divide data into Expenditure Groups 1 to 10 by sum of expenditure
expenditure_group_breaks <- quantile(household_exp_w2$sum_exp, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
expenditure_group_labels <- paste("Expenditure Group", 1:10)

household_exp_w2$expenditure_category <- cut(household_exp_w2$sum_exp, breaks = expenditure_group_breaks, labels = expenditure_group_labels, include.lowest = TRUE)

# The food data is captured in "ks1type" and "ks02"
b1_ks13 <- b1_ks13 %>% 
  select(hhid00, ks1type, ks02) %>% 
  rename(hhid = hhid00)

# The data is currently structured in a long format. It needs to get restructured in a wide format.
food_exp_w3 <- b1_ks13 %>% 
  pivot_wider(names_from = ks1type, values_from = ks02)

# Calculate household food expenditure 
food_exp_w3$food <- (rowSums(food_exp_w3[,-1]))*52/12 #calculating monthly expenditure
food_exp_w3 <- food_exp_w3[,c("hhid","food")]

# Generating household monthly non-food expenditure
# Non-food expenditure data is available in "b1_ks04", "b1_ks24", "b1_ks34", and "b2_kr4"
b1_ks23 <- b1_ks23 %>% 
  select(hhid00, ks2type, ks06) %>% 
  rename(hhid = hhid00)
# The data is currently structured in a long format. It needs to get restructured in a wide format.
nonfood_exp1_w3 <- b1_ks23 %>% 
  pivot_wider(names_from = ks2type, values_from = ks06)
# Now that the data is already in a wide format. It needs to get calculated in monthly non-food total expenditure.
nonfood_exp1_w3$nonfood1 <- (rowSums(nonfood_exp1_w3[,2:10])) #calculating monthly non-food expenditure
nonfood_exp1_w3 <- nonfood_exp1_w3[,c("hhid","nonfood1")]

b1_ks33 <- b1_ks33 %>% 
  select(hhid00, ks3type, ks08) %>% 
  rename(hhid = hhid00)
# The data is currently structured in a long format. It needs to get restructured in a wide format.
nonfood_exp2_w3 <- b1_ks33 %>% 
  pivot_wider(names_from = ks3type, values_from = ks08)
# Now that the data is already in a wide format. It needs to get calculated in monthly non-food total expenditure.
nonfood_exp2_w3$nonfood2 <- (rowSums(nonfood_exp2_w3[,-1]))/12 #calculating monthly non-food expenditure
nonfood_exp2_w3 <- nonfood_exp2_w3[,c("hhid","nonfood2")]

# Non-food (Housing Rent)
#Note: The non-food data (housing rent) is captured in "kr04a" -- to simplify the data frame, the "b2_kr" variable will be restructured to only reflect household ID ("hhid97"), monthly/annual rent indicator ("kr04"), and housing rent expenditure ("kr04a")
rent_exp_w3 <- b2_kr3 %>% 
  select(hhid00, kr04) %>% 
  rename(hhid = hhid00)

rent_exp_w3$kr04[is.na(rent_exp_w3$kr04)] <- 0 #fill in the NAs with 0
rent_exp_w3 <- rent_exp_w3 %>% 
  select(hhid, kr04) %>% 
  rename(rent_exp = kr04)

# Non-food (Education-related expenditure)
#Education-related expenditure (annual) is captured in "ks10aa", "ks11aa", "ks12aa"
#The data is already in a wide format. 
edu_exp_w3 <- b1_ks03 %>% 
  select(hhid00, ks10aa, ks11aa, ks12aa) %>% 
  rename(hhid = hhid00)
#Since it is an annual expenditure, the data needs to be standardized into monthly education expenditure
edu_exp_w3$edu_exp <- (rowSums(edu_exp_w3[,-1]))/12
edu_exp_w3 <- edu_exp_w3[,c("hhid","edu_exp")]

## Aggregating the non-food expenditures
# Non-food = nonfood_exp1_w +nonfood_exp2_w + rent_exp_w + edu_exp_w
# Merging nonfood_exp1_w and nonfood_exp2_w
nonfood_exp_w3 <- merge(nonfood_exp1_w3, nonfood_exp2_w3, by="hhid")
# Merging nonfood_exp and rent_exp_w
nonfood_rent_w3 <- merge(nonfood_exp_w3,rent_exp_w3, by="hhid")
# Merging nonfood_rent_w and edu_exp_w
nonfood_totexp_w3 <- merge(nonfood_rent_w3, edu_exp_w3, by="hhid")
# Calculating total non food expenditure
attach(nonfood_totexp_w3)
nonfood_totexp_w3$sum_nonfood_exp <- nonfood1 + nonfood2 + rent_exp + edu_exp
## Aggregating the non-food and food expenditures 
household_exp_w3 <- merge(nonfood_totexp_w3, food_exp_w3, by="hhid")
household_exp_w3 <- household_exp_w3 %>%
  mutate(sum_exp = sum_nonfood_exp + food)

## Generating household size  
#Household size data are available in"bk_ar14"                     
#ar01a is a question on whether the listed household member is still a part of the same household
#Only include answers ar01a=1, ar01a=2, and ar01a=5, and ar01a=11 -- ar01a=0 is a code for "has died" and ar01a=3 is a code for "no longer part of the same household"                  
household_size <- bk_ar13[bk_ar13$ar01a == 1 | bk_ar13$ar01a == 2 | bk_ar13$ar01a == 5 | bk_ar13$ar01a == 11,]
household_size_w3 <- data.frame(count(household_size,hhid00))
household_size_w3 <- household_size_w3 %>% rename(hhid = hhid00)
## Merging household expenditure and household size 
household_exp_w3 <- merge(household_exp_w3, household_size_w3, by="hhid")
household_exp_w3 <- household_exp_w3 %>% 
  mutate(pce = sum_exp/n)
# Transform per capita expenditure into log natural
household_exp_w3 <- household_exp_w3 %>% 
  mutate(lnpce = log(pce))
# Divide data into Expenditure Groups 1 to 10 by sum of expenditure
expenditure_group_breaks <- quantile(household_exp_w3$sum_exp, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
expenditure_group_labels <- paste("Expenditure Group", 1:10)

household_exp_w3$expenditure_category <- cut(household_exp_w3$sum_exp, breaks = expenditure_group_breaks, labels = expenditure_group_labels, include.lowest = TRUE)

# The food data is captured in "ks1type" and "ks02"
b1_ks14 <- b1_ks14 %>% 
  select(hhid07, ks1type, ks02) %>% 
  rename(hhid = hhid07)
# The data is currently structured in a long format. It needs to get restructured in a wide format.
food_exp_w4 <- b1_ks14 %>% 
  pivot_wider(names_from = ks1type, values_from = ks02)
# Calculate household food expenditure 
food_exp_w4$food <- (rowSums(food_exp_w4[,-1]))*52/12 #calculating monthly expenditure
food_exp_w4 <- food_exp_w4[,c("hhid","food")]
# Generating household monthly non-food expenditure
# Non-food expenditure data is available in "b1_ks04", "b1_ks24", "b1_ks34", and "b2_kr4"
b1_ks24 <- b1_ks24 %>% 
  select(hhid07, ks2type, ks06) %>% 
  rename(hhid = hhid07)
# The data is currently structured in a long format. It needs to get restructured in a wide format.
nonfood_exp1_w4 <- b1_ks24 %>% 
  pivot_wider(names_from = ks2type, values_from = ks06)
# Now that the data is already in a wide format. It needs to get calculated in monthly non-food total expenditure.
nonfood_exp1_w4$nonfood1 <- (rowSums(nonfood_exp1_w4[,2:13])) #calculating monthly non-food expenditure
nonfood_exp1_w4 <- nonfood_exp1_w4[,c("hhid","nonfood1")]

b1_ks34 <- b1_ks34 %>% 
  select(hhid07, ks3type, ks08) %>% 
  rename(hhid = hhid07)
# The data is currently structured in a long format. It needs to get restructured in a wide format.
nonfood_exp2_w4 <- b1_ks34 %>% 
  pivot_wider(names_from = ks3type, values_from = ks08)
# Now that the data is already in a wide format. It needs to get calculated in monthly non-food total expenditure.
nonfood_exp2_w4$nonfood2 <- (rowSums(nonfood_exp2_w4[,-1]))/12 #calculating monthly non-food expenditure
nonfood_exp2_w4 <- nonfood_exp2_w4[,c("hhid","nonfood2")]

# Non-food (Housing Rent)
#Note: The non-food data (housing rent) is captured in "kr04a" -- to simplify the data frame, the "b2_kr" variable will be restructured to only reflect household ID ("hhid07"), monthly/annual rent indicator ("kr04ax"), and housing rent expenditure ("kr04a")
rent_exp_w4 <- b2_kr4 %>% 
  select(hhid07, kr04ax, kr04a) %>% 
  rename(hhid = hhid07)
rent_exp_w4$kr04a[is.na(rent_exp_w4$kr04a)] <- 0 #fill in the NAs with 0
rent_exp_w4$kr04ax[is.na(rent_exp_w4$kr04ax)] <- 0 #fill in the NAs with 0

rent_exp_w4$rent_exp <- ifelse(rent_exp_w4$kr04ax == 1,(rowSums(rent_exp_w4[-1:-2])/12), # if kr04ax == 1, kr04a is an annual housing rent expenditure -- It needs to be standardized into monthly housing rent expenditure
                               rent_exp_w4$kr04a) # if kr04ax == 2, kr04a is a monthly housing rent expenditure

rent_exp_w4 <- rent_exp_w4 %>% 
  select(hhid, rent_exp)

# Non-food (Education-related expenditure)
#Education-related expenditure (annual) is captured in "ks10aa", "ks11aa", "ks12aa"
#The data is already in a wide format. 
edu_exp_w4 <- b1_ks04 %>% 
  select(hhid07, ks10aa, ks11aa, ks12aa) %>% 
  rename(hhid = hhid07)
#Since it is an annual expenditure, the data needs to be standardized into monthly education expenditure
edu_exp_w4$edu_exp <- (rowSums(edu_exp_w4[,-1]))/12
edu_exp_w4 <- edu_exp_w4[,c("hhid","edu_exp")]

## Aggregating the non-food expenditures
# Non-food = nonfood_exp1_w +nonfood_exp2_w + rent_exp_w + edu_exp_w
# Merging nonfood_exp1_w and nonfood_exp2_w
nonfood_exp_w4 <- merge(nonfood_exp1_w4, nonfood_exp2_w4, by="hhid")
# Merging nonfood_exp and rent_exp_w
nonfood_rent_w4 <- merge(nonfood_exp_w4,rent_exp_w4, by="hhid")
# Merging nonfood_rent_w and edu_exp_w
nonfood_totexp_w4 <- merge(nonfood_rent_w4, edu_exp_w4, by="hhid")
# Calculating total non food expenditure
attach(nonfood_totexp_w4)
nonfood_totexp_w4$sum_nonfood_exp <- nonfood1 + nonfood2 + rent_exp + edu_exp
## Aggregating the non-food and food expenditures 
household_exp_w4 <- merge(nonfood_totexp_w4, food_exp_w4, by="hhid")
household_exp_w4 <- household_exp_w4 %>%
  mutate(sum_exp = sum_nonfood_exp + food)

## Generating household size  
#Household size data are available in"bk_ar14"                     
#ar01a is a question on whether the listed household member is still a part of the same household
#Only include answers ar01a=1, ar01a=2, and ar01a=5, and ar01a=11 -- ar01a=0 is a code for "has died" and ar01a=3 is a code for "no longer part of the same household"                  
household_size <- bk_ar14[bk_ar14$ar01a == 1 | bk_ar14$ar01a == 2 | bk_ar14$ar01a == 5 | bk_ar14$ar01a == 11,]
household_size_w4 <- data.frame(count(household_size,hhid07))
household_size_w4 <- household_size_w4 %>% rename(hhid = hhid07)
## Merging household expenditure and household size 
household_exp_w4 <- merge(household_exp_w4, household_size_w4, by="hhid")
household_exp_w4 <- household_exp_w4 %>% 
  mutate(pce = sum_exp/n)
# Transform per capita expenditure into log natural
household_exp_w4 <- household_exp_w4 %>% 
  mutate(lnpce = log(pce))
# Divide data into Expenditure Groups 1 to 10 by sum of expenditure
expenditure_group_breaks <- quantile(household_exp_w4$sum_exp, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
expenditure_group_labels <- paste("Expenditure Group", 1:10)

household_exp_w4$expenditure_category <- cut(household_exp_w4$sum_exp, breaks = expenditure_group_breaks, labels = expenditure_group_labels, include.lowest = TRUE)

# The food data is captured in "ks1type" and "ks02"
b1_ks15 <- b1_ks15 %>% 
  select(hhid14, ks1type, ks02) %>% 
  rename(hhid = hhid14)

# The data is currently structured in a long format. It needs to get restructured in a wide format.
food_exp_w5 <- b1_ks15 %>% 
  pivot_wider(names_from = ks1type, values_from = ks02)

# Calculate household food expenditure 
food_exp_w5$food <- (rowSums(food_exp_w5[,-1]))*52/12 #calculating monthly expenditure
food_exp_w5 <- food_exp_w5[,c("hhid","food")]
# Generating household monthly non-food expenditure
# Non-food expenditure data is available in "b1_ks05", "b1_ks25", "b1_ks35", and "b2_kr5"
b1_ks25 <- b1_ks25 %>% 
  select(hhid14, ks2type, ks06) %>% 
  rename(hhid = hhid14)
# Check for empty or missing values in the ks2type column
empty_names <- b1_ks25$ks2type == ""
missing_names <- is.na(b1_ks25$ks2type)
# Replace empty or missing names with a placeholder value
b1_ks25$ks2type[empty_names | missing_names] <- "Unknown"
# The data is currently structured in a long format. It needs to get restructured in a wide format.
nonfood_exp1_w5 <- b1_ks25 %>%
  pivot_wider(names_from = ks2type, values_from = ks06)
# Now that the data is already in a wide format. It needs to get calculated in monthly non-food total expenditure.
nonfood_exp1_w5$nonfood1 <- (rowSums(nonfood_exp1_w5[,2:13])) #calculating monthly non-food expenditure
nonfood_exp1_w5 <- nonfood_exp1_w5[,c("hhid","nonfood1")]

b1_ks35 <- b1_ks35 %>% 
  select(hhid14, ks3type, ks08) %>% 
  rename(hhid = hhid14)
# The data is currently structured in a long format. It needs to get restructured in a wide format.
nonfood_exp2_w5 <- b1_ks35 %>% 
  pivot_wider(names_from = ks3type, values_from = ks08)
# Now that the data is already in a wide format. It needs to get calculated in monthly non-food total expenditure.
nonfood_exp2_w5$nonfood2 <- (rowSums(nonfood_exp2_w5[,-1]))/12 #calculating monthly non-food expenditure
nonfood_exp2_w5 <- nonfood_exp2_w5[,c("hhid","nonfood2")]

# Non-food (Housing Rent)
# The non-food data (housing rent) is captured in "kr04a" -- to simplify the data frame, the "b2_kr" variable will be restructured to only reflect household ID ("hhid07"), monthly/annual rent indicator ("kr04ax"), and housing rent expenditure ("kr04a")
rent_exp_w5 <- b2_kr5 %>% 
  select(hhid14, kr04ax, kr04a) %>% 
  rename(hhid = hhid14)
rent_exp_w5$kr04a[is.na(rent_exp_w5$kr04a)] <- 0 #fill in the NAs with 0
rent_exp_w5$kr04ax[is.na(rent_exp_w5$kr04ax)] <- 0 #fill in the NAs with 0

rent_exp_w5$rent_exp <- ifelse(rent_exp_w5$kr04ax == 1,(rowSums(rent_exp_w5[-1:-2])/12), # if kr04ax == 1, kr04a is an annual housing rent expenditure -- It needs to be standardized into monthly housing rent expenditure
                               rent_exp_w5$kr04a) # if kr04ax == 2, kr04a is a monthly housing rent expenditure

rent_exp_w5 <- rent_exp_w5 %>% 
  select(hhid, rent_exp)

# Non-food (Education-related expenditure)
# Education-related expenditure (annual) is captured in "ks10aa", "ks11aa", "ks12aa"
# The data is already in a wide format. 
edu_exp_w5 <- b1_ks05 %>% 
  select(hhid14, ks10aa, ks11aa, ks12aa) %>% 
  rename(hhid = hhid14)

# Since it is an annual expenditure, the data needs to be standardized into monthly education expenditure
edu_exp_w5$edu_exp <- (rowSums(edu_exp_w5[,-1]))/12
edu_exp_w5 <- edu_exp_w5[,c("hhid","edu_exp")]

## Aggregating the non-food expenditures
# Non-food = nonfood_exp1_w +nonfood_exp2_w + rent_exp_w + edu_exp_w
# Merging nonfood_exp1_w and nonfood_exp2_w
nonfood_exp_w5 <- merge(nonfood_exp1_w5, nonfood_exp2_w5, by="hhid")
# Merging nonfood_exp and rent_exp_w
nonfood_rent_w5 <- merge(nonfood_exp_w5, rent_exp_w5, by="hhid")
# Merging nonfood_rent_w and edu_exp_w
nonfood_totexp_w5 <- merge(nonfood_rent_w5, edu_exp_w5, by="hhid")
attach(nonfood_totexp_w5)
# Calculating total non food expenditure
nonfood_totexp_w5$sum_nonfood_exp <- nonfood1 + nonfood2 + rent_exp + edu_exp
## Aggregating the non-food and food expenditures 
household_exp_w5 <- merge(nonfood_totexp_w5, food_exp_w5, by="hhid")
household_exp_w5 <- household_exp_w5 %>%
  mutate(sum_exp = sum_nonfood_exp + food)

## Generating household size  
# Household size data are available in"bk_ar15"                     
# ar01a is a question on whether the listed household member is still a part of the same household
# Only include answers ar01a=1, ar01a=2, and ar01a=5, and ar01a=11 -- ar01a=0 is a code for "has died" and ar01a=3 is a code for "no longer part of the same household"                  
household_size <- bk_ar15[bk_ar15$ar01a == 1 | bk_ar15$ar01a == 2 | bk_ar15$ar01a == 5 | bk_ar15$ar01a == 11,]
household_size_w5 <- data.frame(count(household_size,hhid14))
household_size_w5 <- household_size_w5 %>% rename(hhid = hhid14)

## Merging household expenditure and household size 
household_exp_w5 <- merge(household_exp_w5, household_size_w5, by="hhid")
household_exp_w5 <- household_exp_w5 %>% 
  mutate(pce = sum_exp/n)
# Transform per capita expenditure into log natural
household_exp_w5 <- household_exp_w5 %>% 
  mutate(lnpce = log(pce))

# Divide data into Expenditure Groups 1 to 10 by sum of expenditure
expenditure_group_breaks <- quantile(household_exp_w5$sum_exp, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
expenditure_group_labels <- paste("Expenditure Group", 1:10)

household_exp_w5$expenditure_category <- cut(household_exp_w5$sum_exp, breaks = expenditure_group_breaks, labels = expenditure_group_labels, include.lowest = TRUE)

# All variable related to household asset
# assets_w2
# Household asset data are available in "b2_hr12"
total_assets_w2 <- b2_hr12 %>% 
  select(hhid97, hrtype, hr02) %>% 
  rename(hhid = hhid97)

total_assets_w2[is.na(total_assets_w2)] = 0  #transforming NAs into 0
# The data is currently structured in a long format. It needs to get restructured in a wide format.
total_assets_w2 <- pivot_wider(total_assets_w2, names_from = hrtype, values_from = hr02)
# Calculating the sum of the value of each asset type
total_assets_w2$sum_asset <- rowSums(total_assets_w2[, -1], na.rm = TRUE)
total_assets_w2 <- total_assets_w2[,c("hhid", "sum_asset")]
## Merging hh expenditure and assets 
hh_ses_w2 <- merge(household_exp_w2, total_assets_w2, by="hhid")
# Divide data into Wealth Group 1 to 10 by sum of asset
hh_ses_w2 <- hh_ses_w2 %>%
  mutate(wealthc = cut(sum_asset, 
                               breaks = quantile(sum_asset, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
                               labels = paste("Wealth Group", 1:10, sep = " ")))
# Display the distribution of households in each wealth group
table(hh_ses_w2$wealthc)
# Create a new variable "economic_group" based on wealth levels
hh_ses_w2$economic_group <- ifelse(hh_ses_w2$wealthc %in% c("Wealth Group 1", "Wealth Group 2"), "Low",
                                 ifelse(hh_ses_w2$wealthc %in% c("Wealth Group 3", "Wealth Group 4", "Wealth Group 5"), "Lower-Middle",
                                        ifelse(hh_ses_w2$wealthc %in% c("Wealth Group 6", "Wealth Group 7", "Wealth Group 8"), "Upper-Middle",
                                               ifelse(hh_ses_w2$wealthc %in% c("Wealth Group 9", "Wealth Group 10"), "High", NA))))
# Summary of the economic groups
table(hh_ses_w2$economic_group)

# assets_w3
# Household asset data are available in "b2_hr14"
total_assets_w3 <- b2_hr13 %>% 
  select(hhid00, hrtype, hr02) %>% 
  rename(hhid = hhid00)

total_assets_w3[is.na(total_assets_w3)] = 0  #transforming NAs into 0
# The data is currently structured in a long format. It needs to get restructured in a wide format.
total_assets_w3 <- pivot_wider(total_assets_w3, names_from = hrtype, values_from = hr02)
# Calculating the sum of the value of each asset type
total_assets_w3$sum_asset <- rowSums(total_assets_w3[, -1], na.rm = TRUE)
total_assets_w3 <- total_assets_w3[,c("hhid", "sum_asset")]
## Merging hh expenditure and assets 
hh_ses_w3 <- merge(household_exp_w3, total_assets_w3, by="hhid")
# Divide data into Wealth Group 1 to 10 by sum of asset
hh_ses_w3 <- hh_ses_w3 %>%
  mutate(wealthc = cut(sum_asset, 
                               breaks = quantile(sum_asset, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
                               labels = paste("Wealth Group", 1:10, sep = " ")))
# Display the distribution of households in each wealth group
table(hh_ses_w3$wealthc)
# Create a new variable "economic_group" based on wealth levels
hh_ses_w3$economic_group <- ifelse(hh_ses_w3$wealthc %in% c("Wealth Group 1", "Wealth Group 2"), "Low",
                                   ifelse(hh_ses_w3$wealthc %in% c("Wealth Group 3", "Wealth Group 4", "Wealth Group 5"), "Lower-Middle",
                                          ifelse(hh_ses_w3$wealthc %in% c("Wealth Group 6", "Wealth Group 7", "Wealth Group 8"), "Upper-Middle",
                                                 ifelse(hh_ses_w3$wealthc %in% c("Wealth Group 9", "Wealth Group 10"), "High", NA))))
# Summary of the economic groups
table(hh_ses_w3$economic_group)

# assets_w4
# Household asset data are available in "b2_hr14"
total_assets_w4 <- b2_hr14 %>% 
  select(hhid07, hrtype, hr02) %>% 
  rename(hhid = hhid07)

total_assets_w4[is.na(total_assets_w4)] = 0  #transforming NAs into 0
# The data is currently structured in a long format. It needs to get restructured in a wide format.
total_assets_w4 <- pivot_wider(total_assets_w4, names_from = hrtype, values_from = hr02)
# Calculating the sum of the value of each asset type
total_assets_w4$sum_asset <- rowSums(total_assets_w4[,-1])
total_assets_w4 <- total_assets_w4[,c("hhid", "sum_asset")]
## Merging hh expenditure and assets 
hh_ses_w4 <- merge(household_exp_w4, total_assets_w4, by="hhid")
# Divide data into Wealth Group 1 to 10 by sum of asset
hh_ses_w4 <- hh_ses_w4 %>%
  mutate(wealthc = cut(sum_asset, 
                               breaks = quantile(sum_asset, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
                               labels = paste("Wealth Group", 1:10, sep = " ")))
# Display the distribution of households in each wealth group
table(hh_ses_w4$wealthc)
# Create a new variable "economic_group" based on wealth levels
hh_ses_w4$economic_group <- ifelse(hh_ses_w4$wealthc %in% c("Wealth Group 1", "Wealth Group 2"), "Low",
                                   ifelse(hh_ses_w4$wealthc %in% c("Wealth Group 3", "Wealth Group 4", "Wealth Group 5"), "Lower-Middle",
                                          ifelse(hh_ses_w4$wealthc %in% c("Wealth Group 6", "Wealth Group 7", "Wealth Group 8"), "Upper-Middle",
                                                 ifelse(hh_ses_w4$wealthc %in% c("Wealth Group 9", "Wealth Group 10"), "High", NA))))
# Summary of the economic groups
table(hh_ses_w4$economic_group)

# assets_w5
# Household asset data are available in "b2_hr15"
total_assets_w5 <- b2_hr15 %>% 
  select(hhid14, hrtype, hr02_a, hr02_b, hr02_c, hr02_d1, hr02_d2, hr02_d3, hr02_e,
         hr02_f, hr02_g, hr02_h, hr02_j, hr02_k1, hr02_k2) %>% 
  rename(hhid = hhid14)

total_assets_w5[is.na(total_assets_w5)] = 0  #transforming NAs into 0
# The data is currently structured in a long format. It needs to get restructured in a wide format.
total_assets_w5 <- pivot_wider(total_assets_w5, names_from = hrtype, 
                               values_from = c(hr02_a, hr02_b, hr02_c, hr02_d1, hr02_d2, hr02_d3, hr02_e, hr02_f, hr02_g, hr02_h, hr02_j, hr02_k1, hr02_k2))
# Calculating the sum of the value of each asset type
total_assets_w5$sum_asset <- rowSums(total_assets_w5[,-1])
total_assets_w5 <- total_assets_w5[,c("hhid", "sum_asset")]

## Merging hh expenditure and assets 
hh_ses_w5 <- merge(household_exp_w5, total_assets_w5, by="hhid")
# Divide data into Wealth Group 1 to 10 by sum of asset
hh_ses_w5 <- hh_ses_w5 %>%
  mutate(wealthc = cut(sum_asset, 
                               breaks = quantile(sum_asset, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
                               labels = paste("Wealth Group", 1:10, sep = " ")))
# Display the distribution of households in each wealth group
table(hh_ses_w5$wealthc)
# Create a new variable "economic_group" based on wealth levels
hh_ses_w5$economic_group <- ifelse(hh_ses_w5$wealthc %in% c("Wealth Group 1", "Wealth Group 2"), "Low",
                                   ifelse(hh_ses_w5$wealthc %in% c("Wealth Group 3", "Wealth Group 4", "Wealth Group 5"), "Lower-Middle",
                                          ifelse(hh_ses_w5$wealthc %in% c("Wealth Group 6", "Wealth Group 7", "Wealth Group 8"), "Upper-Middle",
                                                 ifelse(hh_ses_w5$wealthc %in% c("Wealth Group 9", "Wealth Group 10"), "High", NA))))
# Summary of the economic groups
table(hh_ses_w5$economic_group)

# All variables related to Rural/Urban and Java-Bali/Non-Java Bali
# region_w2
region_w2 <- bk_sc2 %>% 
  select(hhid97, sc05, sc01, sc02, sc03) %>% 
  mutate(
    rural = ifelse(sc05 == 2, 1, 0),
    java_bali = ifelse(sc01 %in% c(31:36, 51), 1, 0)
  ) %>% 
  rename(hhid = hhid97)

# region_w3
region_w3 <- bk_sc3 %>% 
  select(hhid00, sc05, sc01, sc02, sc03) %>% 
  mutate(
    rural = ifelse(sc05 == 2, 1, 0),
    java_bali = ifelse(sc01 %in% c(31:36, 51), 1, 0)
  ) %>% 
  rename(hhid = hhid00)

# region_w4
region_w4 <- bk_sc4 %>% 
  select(hhid07, sc05, sc010707, sc020707, sc030707) %>% 
  mutate(
    rural = ifelse(sc05 == 2, 1, 0),
    java_bali = ifelse(sc010707 %in% c(31:36, 51), 1, 0)
  ) %>% 
  rename(hhid = hhid07)

# region_w5
region_w5 <- bk_sc5 %>% 
  select(hhid14, sc05, sc01_14_14, sc02_14_14, sc03_14_14) %>% 
  mutate(
    rural = ifelse(sc05 == 2, 1, 0),
    java_bali = ifelse(sc01_14_14 %in% c(31:36, 51), 1, 0)
  ) %>% 
  rename(hhid = hhid14)

# personal weighted from ptrack
pweight <- ptrack %>% 
  select(pidlink, pwt97inl, pwt93_97_00_07l, pwt_5_waves_l)

#### End ####

### Setting Panel Data ###
## Merge Variables ##
# data_w2 
data_w2 <- observations_w2 %>% 
  filter(ar08yr >= 1965 & ar08yr <= 1982) %>% 
  rename(hhid = hhid97)

# Tabulate data frame to check the certain ages are included
table(data_w2$ar08yr)

# Check duplication
duplicated_rows_data_w2 <- data_w2[duplicated(data_w2['pidlink']), ]

# Sort observation due to duplicate pidlink
data_w2 <- data_w2 %>%
  group_by(pidlink) %>%
  filter(!(any(grepl("00$", hhid)) & n() > 1 & !duplicated(pidlink))) %>%
  ungroup()

# Merge addchild_w and data_w
data_w2 <- data_w2 %>% select(pidlink, hhid, ar08yr, father_pidlink, mother_pidlink)
data_w2 <- merge(addchild_w2, data_w2, by = c("hhid", "pidlink"), all.x = T, all.y = F)

# Merge birth_w and data_w
birth_w2 <- birth_w2 %>% select(-br09, -br10, -br16)
data_w2 <- merge(data_w2, birth_w2, by = "pidlink", all.x = T, all.y = F)

# Merge married_w and data_w
data_w2 <- merge(data_w2, married_w2, by = "pidlink", all.x = T, all.y = F)

# Merge marstat_w and data_w
data_w2 <- merge(data_w2, marstat_w2, by = "pidlink", all.x = T, all.y = F)

# Merge demake_summary_w and data_w
data_w2 <- merge(data_w2, demake_w2_summary, by = "pidlink", all.x = T, all.y = F)

# Merge timemar_w and data_w
data_w2 <- merge(data_w2, timemar_w2, by = "pidlink", all.x = T, all.y = F)

# Merge livearr_w and data_w
livearr_w2 <- livearr_w2 %>% 
  select(-kw14g)

data_w2 <- merge(data_w2, livearr_w2, by = "pidlink", all.x = T, all.y = F)

# Merge sex_w and data_w
data_w2 <- merge(data_w2, sex_w2, by = "pidlink", all.x = T, all.y = F)

# Merge age_w and data_w
data_w2 <- merge(data_w2, age_w2, by = c("hhid", "pidlink"), all.x = T, all.y = F)

# Merge contracept_w and data_w
data_w2 <- merge(data_w2, contracept_w2, by = "pidlink", all.x = T, all.y = F)

# Merge education_w and data_w
data_w2 <- merge(data_w2, education_w2, by = "pidlink", all.x = T, all.y = F)

# Merge ethnic_w and data_w
data_w2 <- merge(data_w2, ethnic_w2, by = "pidlink", all.x = T, all.y = F)

# Merge main_activity_w and data_w
data_w2 <- merge(data_w2, main_activity_w2, by = "pidlink", all.x = T, all.y = F)

# Merge birth_order_w and data_w
birth_order_w2 <- birth_order_w2 %>% 
  select(-mother_pidlink)

data_w2 <- merge(data_w2, birth_order_w2, by = c("hhid", "pidlink"), all.x = T, all.y = F)

# Merge household_expenditure_w and data_w
hh_ses_w2 <- hh_ses_w2 %>% 
  rename(hhsize = n, food_exp = food) %>% 
  select(-nonfood1, -nonfood2)

data_w2 <- merge(data_w2, hh_ses_w2, by = "hhid", all.x = T, all.y = F)

# Merge region_w and data_w
region_u2 <- region_w2 %>% 
  select(hhid, rural, java_bali)

data_w2 <- merge(data_w2, region_u2, by = "hhid", all.x = T, all.y = F)

# Merge religion_w and data_w
data_w2 <- merge(data_w2, religion_w2, by = "pidlink", all.x = T, all.y = F)

# Replace 95 with NA in the 'inc' column 95 = Up to God
data_w2$inc <- ifelse(data_w2$inc == 95, NA, data_w2$inc)

# data_w3 
data_w3 <- observations_w3 %>% 
  filter(ar08yr >= 1965 & ar08yr <= 1982) %>% 
  rename(hhid = hhid00)

# Tabulate data frame to check the certain ages are included
table(data_w3$ar08yr)

# Check duplication
duplicated_rows_data_w3 <- data_w3[duplicated(data_w3['pidlink']), ]

# Sort observation due to duplicate pidlink
data_w3 <- data_w3 %>%
  group_by(pidlink) %>%
  filter(!(any(grepl("00$", hhid)) & n() > 1 & !duplicated(pidlink))) %>%
  ungroup()

# Merge addchild_w and data_w
data_w3 <- data_w3 %>% select(pidlink, hhid, ar08yr, father_pidlink, mother_pidlink)
data_w3 <- merge(addchild_w3, data_w3, by = c("hhid", "pidlink"), all.x = T, all.y = F)

# Merge birth_w and data_w
birth_w3 <- birth_w3 %>% select(-br09, -br10, -br16)
data_w3 <- merge(data_w3, birth_w3, by = "pidlink", all.x = T, all.y = F)

# Merge married_w and data_w
data_w3 <- merge(data_w3, married_w3, by = "pidlink", all.x = T, all.y = F)

# Merge marstat_w and data_w
data_w3 <- merge(data_w3, marstat_w3, by = "pidlink", all.x = T, all.y = F)

# Merge demake_summary_w and data_w
data_w3 <- merge(data_w3, demake_w3_summary, by = "pidlink", all.x = T, all.y = F)

# Merge timemar_w and data_w
data_w3 <- merge(data_w3, timemar_w3, by = "pidlink", all.x = T, all.y = F)

# Merge livearr_w and data_w
livearr_w3 <- livearr_w3 %>% 
  select(-kw14g)

data_w3 <- merge(data_w3, livearr_w3, by = "pidlink", all.x = T, all.y = F)

# Merge sex_w and data_w
data_w3 <- merge(data_w3, sex_w3, by = "pidlink", all.x = T, all.y = F)

# Merge age_w and data_w
data_w3 <- merge(data_w3, age_w3, by = c("hhid", "pidlink"), all.x = T, all.y = F)

# Merge contracept_w and data_w
data_w3 <- merge(data_w3, contracept_w3, by = "pidlink", all.x = T, all.y = F)

# Merge education_w and data_w
data_w3 <- merge(data_w3, education_w3, by = "pidlink", all.x = T, all.y = F)

# Merge ethnic_w and data_w
data_w3 <- merge(data_w3, ethnic_w3, by = "pidlink", all.x = T, all.y = F)

# Merge main_activity_w and data_w
data_w3 <- merge(data_w3, main_activity_w3, by = "pidlink", all.x = T, all.y = F)

# Merge birth_order_w and data_w
birth_order_w3 <- birth_order_w3 %>% 
  select(-mother_pidlink)

data_w3 <- merge(data_w3, birth_order_w3, by = c("hhid", "pidlink"), all.x = T, all.y = F)

# Merge household_expenditure_w and data_w
hh_ses_w3 <- hh_ses_w3 %>% 
  rename(hhsize = n, food_exp = food) %>% 
  select(-nonfood1, -nonfood2)

data_w3 <- merge(data_w3, hh_ses_w3, by = "hhid", all.x = T, all.y = F)

# Merge region_w and data_w
region_u3 <- region_w3 %>% 
  select(hhid, rural, java_bali)

data_w3 <- merge(data_w3, region_u3, by = "hhid", all.x = T, all.y = F)

# Merge religion_w and data_w
data_w3 <- merge(data_w3, religion_w3, by = "pidlink", all.x = T, all.y = F)

# Replace 95 with NA in the 'inc' column 95 = Up to God
data_w3$inc <- ifelse(data_w3$inc == 95, NA, data_w3$inc)

# data_w4 
data_w4 <- observations_w4 %>% 
  filter(ar08yr >= 1965 & ar08yr <= 1982) %>% 
  rename(hhid = hhid07)

# Tabulate data frame to check the certain ages are included
table(data_w4$ar08yr)

# Check duplication
duplicated_rows_data_w4 <- data_w4[duplicated(data_w4['pidlink']), ]

# Sort observation due to duplicate pidlink
data_w4 <- data_w4 %>%
  group_by(pidlink) %>%
  filter(!(any(grepl("00$", hhid)) & n() > 1 & !duplicated(pidlink))) %>%
  ungroup()

# Merge addchild_w and data_w
data_w4 <- data_w4 %>% select(pidlink, hhid, ar08yr, father_pidlink, mother_pidlink)
data_w4 <- merge(addchild_w4, data_w4, by = c("hhid", "pidlink"), all.x = T, all.y = F)

# Merge birth_w and data_w
birth_w4 <- birth_w4 %>% select(-br09, -br10, -br16)
data_w4 <- merge(data_w4, birth_w4, by = "pidlink", all.x = T, all.y = F)

# Merge married_w and data_w
data_w4 <- merge(data_w4, married_w4, by = "pidlink", all.x = T, all.y = F)

# Merge marstat_w and data_w
data_w4 <- merge(data_w4, marstat_w4, by = "pidlink", all.x = T, all.y = F)

# Merge demake_summary_w and data_w
data_w4 <- merge(data_w4, demake_w4_summary, by = "pidlink", all.x = T, all.y = F)

# Merge timemar_w and data_w
data_w4 <- merge(data_w4, timemar_w4, by = "pidlink", all.x = T, all.y = F)

# Merge livearr_w and data_w
livearr_w4 <- livearr_w4 %>% 
  select(-kw14g)

data_w4 <- merge(data_w4, livearr_w4, by = "pidlink", all.x = T, all.y = F)

# Merge sex_w and data_w
data_w4 <- merge(data_w4, sex_w4, by = "pidlink", all.x = T, all.y = F)

# Merge age_w and data_w
data_w4 <- merge(data_w4, age_w4, by = c("hhid", "pidlink"), all.x = T, all.y = F)

# Merge contracept_w and data_w
data_w4 <- merge(data_w4, contracept_w4, by = "pidlink", all.x = T, all.y = F)

# Merge education_w and data_w
data_w4 <- merge(data_w4, education_w4, by = "pidlink", all.x = T, all.y = F)

# Merge ethnic_w and data_w
data_w4 <- merge(data_w4, ethnic_w4, by = "pidlink", all.x = T, all.y = F)

# Merge main_activity_w and data_w
data_w4 <- merge(data_w4, main_activity_w4, by = "pidlink", all.x = T, all.y = F)

# Merge birth_order_w and data_w
birth_order_w4 <- birth_order_w4 %>% 
  select(-mother_pidlink)

data_w4 <- merge(data_w4, birth_order_w4, by = c("hhid", "pidlink"), all.x = T, all.y = F)

# Merge household_expenditure_w and data_w
hh_ses_w4 <- hh_ses_w4 %>% 
  rename(hhsize = n, food_exp = food) %>% 
  select(-nonfood1, -nonfood2)

data_w4 <- merge(data_w4, hh_ses_w4, by = "hhid", all.x = T, all.y = F)

# Merge region_w and data_w
region_u4 <- region_w4 %>% 
  select(hhid, rural, java_bali)

data_w4 <- merge(data_w4, region_u4, by = "hhid", all.x = T, all.y = F)

# Merge religion_w and data_w
data_w4 <- merge(data_w4, religion_w4, by = "pidlink", all.x = T, all.y = F)

# Replace 95 with NA in the 'inc' column 95 = Up to God
data_w4$inc <- ifelse(data_w4$inc == 95, NA, data_w4$inc)

# data_w5 
data_w5 <- observations_w5 %>% 
  filter(ar08yr >= 1965 & ar08yr <= 1982) %>% 
  rename(hhid = hhid14)

# Tabulate data frame to check the certain ages are included
table(data_w5$ar08yr)

# Check duplication
duplicated_rows_data_w5 <- data_w5[duplicated(data_w5['pidlink']), ]

# Sort observation due to duplicate pidlink
data_w5 <- data_w5 %>%
  group_by(pidlink) %>%
  filter(!(any(grepl("00$", hhid)) & n() > 1 & !duplicated(pidlink))) %>%
  ungroup()

# Merge addchild_w and data_w
data_w5 <- data_w5 %>% select(pidlink, hhid, ar08yr, father_pidlink, mother_pidlink)
data_w5 <- merge(addchild_w5, data_w5, by = c("hhid", "pidlink"), all.x = T, all.y = F)

# Merge birth_w and data_w
birth_w5 <- birth_w5 %>% select(-br09, -br10, -br16)
data_w5 <- merge(data_w5, birth_w5, by = "pidlink", all.x = T, all.y = F)

# Merge married_w and data_w
data_w5 <- merge(data_w5, married_w5, by = "pidlink", all.x = T, all.y = F)

# Merge marstat_w and data_w
data_w5 <- merge(data_w5, marstat_w5, by = "pidlink", all.x = T, all.y = F)

# Merge demake_summary_w and data_w
data_w5 <- merge(data_w5, demake_w5_summary, by = "pidlink", all.x = T, all.y = F)

# Merge timemar_w and data_w
data_w5 <- merge(data_w5, timemar_w5, by = "pidlink", all.x = T, all.y = F)

# Merge livearr_w and data_w
livearr_w5 <- livearr_w5 %>% 
  select(-kw14g)

data_w5 <- merge(data_w5, livearr_w5, by = "pidlink", all.x = T, all.y = F)

# Merge sex_w and data_w
data_w5 <- merge(data_w5, sex_w5, by = "pidlink", all.x = T, all.y = F)

# Merge age_w and data_w
data_w5 <- merge(data_w5, age_w5, by = c("hhid", "pidlink"), all.x = T, all.y = F)

# Merge contracept_w and data_w
data_w5 <- merge(data_w5, contracept_w5, by = "pidlink", all.x = T, all.y = F)

# Merge education_w and data_w
data_w5 <- merge(data_w5, education_w5, by = "pidlink", all.x = T, all.y = F)

# Merge ethnic_w and data_w
data_w5 <- merge(data_w5, ethnic_w5, by = "pidlink", all.x = T, all.y = F)

# Merge main_activity_w and data_w
data_w5 <- merge(data_w5, main_activity_w5, by = "pidlink", all.x = T, all.y = F)

# Merge birth_order_w and data_w
birth_order_w5 <- birth_order_w5 %>% 
  select(-mother_pidlink)

data_w5 <- merge(data_w5, birth_order_w5, by = c("hhid", "pidlink"), all.x = T, all.y = F)

# Merge household_expenditure_w and data_w
hh_ses_w5 <- hh_ses_w5 %>% 
  rename(hhsize = n, food_exp = food) %>% 
  select(-nonfood1, -nonfood2)

data_w5 <- merge(data_w5, hh_ses_w5, by = "hhid", all.x = T, all.y = F)

# Merge region_w and data_w
region_u5 <- region_w5 %>% 
  select(hhid, rural, java_bali)

data_w5 <- merge(data_w5, region_u5, by = "hhid", all.x = T, all.y = F)

# Merge religion_w and data_w
data_w5 <- merge(data_w5, religion_w5, by = "pidlink", all.x = T, all.y = F)

# Replace 95 with NA in the 'inc' column 95 = Up to God
data_w5$inc <- ifelse(data_w5$inc == 95, NA, data_w5$inc)

# Save Data Set in xls format
openxlsx::write.xlsx(data_w2, "data_IFLS2.xlsx", rowNames = FALSE)
openxlsx::write.xlsx(data_w3, "data_IFLS3.xlsx", rowNames = FALSE)
openxlsx::write.xlsx(data_w4, "data_IFLS4.xlsx", rowNames = FALSE)
openxlsx::write.xlsx(data_w5, "data_IFLS5.xlsx", rowNames = FALSE)

# Generating Panel Data #
# Merge dataset for data_panel
# Final panel data
# Import IFLS data
data_w2 <- openxlsx::read.xlsx("data_IFLS2.xlsx")
data_w3 <- openxlsx::read.xlsx("data_IFLS3.xlsx")
data_w4 <- openxlsx::read.xlsx("data_IFLS4.xlsx")
data_w5 <- openxlsx::read.xlsx("data_IFLS5.xlsx")

# Merge the data frames with renamed columns
merged_data <- merge(data_w2, data_w3, by = "pidlink", all = FALSE)
merged_data2 <- merge(data_w4, data_w5, by = "pidlink", all = FALSE)
join_data <- merge(merged_data, merged_data2, by = "pidlink", all = FALSE)
sum(duplicated(join_data$pidlink)) 
# NB: x.x = 1997, y.x = 2000, x.y = 2007 and y.y = 2014
join_data_alt <- join_data[!duplicated(join_data$pidlink), ]

join_data_select <- join_data_alt[,c("pidlink",
                                     "hhid.x.x", "hhid.y.x", "hhid.x.y", "hhid.y.y",
                                     "age.x.x", "age.y.x", "age.x.y", "age.y.y",
                                     "sex.x.x", "sex.y.x", "sex.x.y", "sex.y.y",
                                     "inc.x.x", "inc.y.x", "inc.x.y", "inc.y.y",
                                     "able.x.x", "able.y.x", "able.x.y", "able.y.y",
                                     "wishmorechild.x.x", "wishmorechild.y.x", "wishmorechild.x.y", "wishmorechild.y.y",
                                     "numaddchild.x.x", "numaddchild.y.x", "numaddchild.x.y", "numaddchild.y.y",
                                     "numaddson.x.x", "numaddson.y.x", "numaddson.x.y", "numaddson.y.y",
                                     "numadddaugh.x.x", "numadddaugh.y.x", "numadddaugh.x.y", "numadddaugh.y.y",
                                     "menopause.x.x", "menopause.y.x", "menopause.x.y", "menopause.y.y",
                                     "givebirth.x.x", "givebirth.y.x", "givebirth.x.y", "givebirth.y.y",
                                     "numson.x.x", "numson.y.x", "numson.x.y", "numson.y.y",
                                     "numdaughter.x.x", "numdaughter.y.x", "numdaughter.x.y", "numdaughter.y.y",
                                     "expchilddead.x.x", "expchilddead.y.x", "expchilddead.x.y", "expchilddead.y.y",
                                     "numchild.x.x", "numchild.y.x", "numchild.x.y", "numchild.y.y",
                                     "badbirthexp.x.x", "badbirthexp.y.x", "badbirthexp.x.y", "badbirthexp.y.y",
                                     "mixgender.x.x", "mixgender.y.x", "mixgender.x.y", "mixgender.y.y",
                                     "ar08yr.x.x", "ar08yr.y.x", "ar08yr.x.y", "ar08yr.y.y",
                                     "ymar.x.x", "ymar.y.x", "ymar.x.y", "ymar.y.y",
                                     "agemar.x.x", "agemar.y.x", "agemar.x.y", "agemar.y.y",
                                     "marstat.x.x", "marstat.y.x", "marstat.x.y", "marstat.y.y",
                                     "ratio_demake.x.x", "ratio_demake.y.x", "ratio_demake.x.y", "ratio_demake.y.y",
                                     "ratio_pardemake.x.x", "ratio_pardemake.y.x", "ratio_pardemake.x.y", "ratio_pardemake.y.y",
                                     "ratio_parinlawdemake.x.x", "ratio_parinlawdemake.y.x", "ratio_parinlawdemake.x.y", "ratio_parinlawdemake.y.y",
                                     "ratio_joinpar.x.x", "ratio_joinpar.y.x", "ratio_joinpar.x.y", "ratio_joinpar.y.y",
                                     "ratio_joinilw.x.x", "ratio_joinilw.y.x", "ratio_joinilw.x.y", "ratio_joinilw.y.y",
                                     "contracept.x.x", "contracept.y.x", "contracept.x.y", "contracept.y.y",
                                     "years_of_education.x.x", "years_of_education.y.x", "years_of_education.x.y", "years_of_education.y.y",
                                     "ethnicity.x.x", "ethnicity.y.x", "ethnicity.x.y", "ethnicity.y.y",
                                     "main_activity.x.x", "main_activity.y.x", "main_activity.x.y", "main_activity.y.y",
                                     "naive_sibling_count.x.x", "naive_sibling_count.y.x", "naive_sibling_count.x.y", "naive_sibling_count.y.y",
                                     "birth_order_category.x.x", "birth_order_category.y.x", "birth_order_category.x.y", "birth_order_category.y.y",
                                     "expenditure_category.x.x", "expenditure_category.y.x", "expenditure_category.x.y", "expenditure_category.y.y",
                                     "sum_asset.x.x", "sum_asset.y.x", "sum_asset.x.y", "sum_asset.y.y",
                                     "economic_group.x.x", "economic_group.y.x", "economic_group.x.y", "economic_group.y.y",
                                     "parliv.x.x", "parliv.y.x", "parliv.x.y", "parliv.y.y",
                                     "parinlawliv.x.x", "parinlawliv.y.x", "parinlawliv.x.y", "parinlawliv.y.y",
                                     "rural.x.x", "rural.y.x", "rural.x.y", "rural.y.y",
                                     "java_bali.x.x", "java_bali.y.x", "java_bali.x.y", "java_bali.y.y",
                                     "sonliveelsewhere.x.x", "sonliveelsewhere.y.x", "sonliveelsewhere.x.y", "sonliveelsewhere.y.y",
                                     "daughterliveelsewhere.x.x", "daughterliveelsewhere.y.x", "daughterliveelsewhere.x.y", "daughterliveelsewhere.y.y",
                                     "hhsize.x.x", "hhsize.y.x", "hhsize.x.y", "hhsize.y.y",
                                     "years_of_education_factor.x.x", "years_of_education_factor.y.x", "years_of_education_factor.x.y", "years_of_education_factor.y.y",
                                     "religion.x.x", "religion.y.x", "religion.x.y", "religion.y.y",
                                     "marriagenum.x.x", "marriagenum.y.x", "marriagenum.x.y", "marriagenum.y.y",
                                     "timemar.x.x", "timemar.y.x", "timemar.x.y", "timemar.y.y",
                                     "pce.x.x", "pce.y.x", "pce.x.y", "pce.y.y")]

# Reshape the merge_adolescent data frame
data_panel <- reshape(
  join_data_select,
  idvar = "pidlink",
  varying = list(c("hhid.x.x", "hhid.y.x", "hhid.x.y", "hhid.y.y"),
                 c("age.x.x", "age.y.x", "age.x.y", "age.y.y"),
                 c("sex.x.x", "sex.y.x", "sex.x.y", "sex.y.y"),                         
                 c("inc.x.x", "inc.y.x", "inc.x.y", "inc.y.y"),
                 c("able.x.x", "able.y.x", "able.x.y", "able.y.y"),                      
                 c("wishmorechild.x.x", "wishmorechild.y.x", "wishmorechild.x.y", "wishmorechild.y.y"),
                 c("numaddchild.x.x", "numaddchild.y.x", "numaddchild.x.y", "numaddchild.y.y"),
                 c("numaddson.x.x", "numaddson.y.x", "numaddson.x.y", "numaddson.y.y"),
                 c("numadddaugh.x.x", "numadddaugh.y.x", "numadddaugh.x.y", "numadddaugh.y.y"),
                 c("menopause.x.x", "menopause.y.x", "menopause.x.y", "menopause.y.y"),
                 c("givebirth.x.x", "givebirth.y.x", "givebirth.x.y", "givebirth.y.y"),
                 c("numson.x.x", "numson.y.x", "numson.x.y", "numson.y.y"),
                 c("numdaughter.x.x", "numdaughter.y.x", "numdaughter.x.y", "numdaughter.y.y"),
                 c("expchilddead.x.x", "expchilddead.y.x", "expchilddead.x.y", "expchilddead.y.y"),
                 c("numchild.x.x", "numchild.y.x", "numchild.x.y", "numchild.y.y"),
                 c("badbirthexp.x.x", "badbirthexp.y.x", "badbirthexp.x.y", "badbirthexp.y.y"),
                 c("mixgender.x.x", "mixgender.y.x", "mixgender.x.y", "mixgender.y.y"),
                 c("ar08yr.x.x", "ar08yr.y.x", "ar08yr.x.y", "ar08yr.y.y"),
                 c("ymar.x.x", "ymar.y.x", "ymar.x.y", "ymar.y.y"),
                 c("agemar.x.x", "agemar.y.x", "agemar.x.y", "agemar.y.y"),                               
                 c("marstat.x.x", "marstat.y.x", "marstat.x.y", "marstat.y.y"),
                 c("ratio_demake.x.x", "ratio_demake.y.x", "ratio_demake.x.y", "ratio_demake.y.y"),
                 c("ratio_pardemake.x.x", "ratio_pardemake.y.x", "ratio_pardemake.x.y", "ratio_pardemake.y.y"),
                 c("ratio_parinlawdemake.x.x", "ratio_parinlawdemake.y.x", "ratio_parinlawdemake.x.y", "ratio_parinlawdemake.y.y"),
                 c("ratio_joinpar.x.x", "ratio_joinpar.y.x", "ratio_joinpar.x.y", "ratio_joinpar.y.y"),
                 c("ratio_joinilw.x.x", "ratio_joinilw.y.x", "ratio_joinilw.x.y", "ratio_joinilw.y.y"),
                 c("contracept.x.x", "contracept.y.x", "contracept.x.y", "contracept.y.y"),
                 c("years_of_education.x.x", "years_of_education.y.x", "years_of_education.x.y", "years_of_education.y.y"),
                 c("ethnicity.x.x", "ethnicity.y.x", "ethnicity.x.y", "ethnicity.y.y"),        
                 c("main_activity.x.x", "main_activity.y.x", "main_activity.x.y", "main_activity.y.y"),       
                 c("naive_sibling_count.x.x", "naive_sibling_count.y.x", "naive_sibling_count.x.y", "naive_sibling_count.y.y"),
                 c("birth_order_category.x.x", "birth_order_category.y.x", "birth_order_category.x.y", "birth_order_category.y.y"),        
                 c("expenditure_category.x.x", "expenditure_category.y.x", "expenditure_category.x.y", "expenditure_category.y.y"),
                 c("sum_asset.x.x", "sum_asset.y.x", "sum_asset.x.y", "sum_asset.y.y"),
                 c("economic_group.x.x", "economic_group.y.x", "economic_group.x.y", "economic_group.y.y"),
                 c("parliv.x.x", "parliv.y.x", "parliv.x.y", "parliv.y.y"),
                 c("parinlawliv.x.x", "parinlawliv.y.x", "parinlawliv.x.y", "parinlawliv.y.y"),
                 c("rural.x.x", "rural.y.x", "rural.x.y", "rural.y.y"),
                 c("java_bali.x.x", "java_bali.y.x", "java_bali.x.y", "java_bali.y.y"),
                 c("sonliveelsewhere.x.x", "sonliveelsewhere.y.x", "sonliveelsewhere.x.y", "sonliveelsewhere.y.y"),
                 c("daughterliveelsewhere.x.x", "daughterliveelsewhere.y.x", "daughterliveelsewhere.x.y", "daughterliveelsewhere.y.y"),
                 c("hhsize.x.x", "hhsize.y.x", "hhsize.x.y", "hhsize.y.y"),
                 c("years_of_education_factor.x.x", "years_of_education_factor.y.x", "years_of_education_factor.x.y", "years_of_education_factor.y.y"),
                 c("religion.x.x", "religion.y.x", "religion.x.y", "religion.y.y"),
                 c("marriagenum.x.x", "marriagenum.y.x", "marriagenum.x.y", "marriagenum.y.y"),
                 c("timemar.x.x", "timemar.y.x", "timemar.x.y", "timemar.y.y"),
                 c("pce.x.x", "pce.y.x", "pce.x.y", "pce.y.y")),
  times = c("1997", "2000", "2007", "2014"),
  v.names = c("hhid", "age", "sex", "inc", "able", "wishmorechild", "numaddchild", "numaddson", "numadddaughther",
              "menopause", "givebirth", "numson", "numdaughter", "expchilddead", "numchild", "badbirthexp",
              "mixgender", "yob", "ymar", "agemar", "marstat", "ratio_demake", "ratio_pardemake",
              "ratio_parinlawdemake", "ratio_joinpar", "ratio_joinilw", "contracept", "years_of_education", "ethnicity",
              "main_activity", "naive_sibling_count", "birth_order_category", "expenditure_category", "sum_asset", "economic_group","live_with_parent", "living_with_parentinlaw",
              "rural_resident", "java_bali_resident", "son_live_elsewhere", "daughter_live_elsewhere", "hhsize", "years_of_education_factor", "religion", "marriagenum", "timemar", "pcp_expenditure"),
  direction = "long"
)

# Sort by pidlink and time
data_panel <- data_panel %>% 
  rename(year = time)

data_panel <- arrange(data_panel, pidlink, year)

# View the reshaped data
View(data_panel)

# Save the data frame to an XLS file
write.xlsx(data_panel, "fertility_panel.xlsx", rowNames = FALSE)

## Clean Panel Data ##
data_panel <- read.xlsx("fertility_panel.xlsx")

# Check the balancedness of the data
plm::punbalancedness(data_panel) # gamma 1, nu 1
plm::pdim(data_panel)$balanced # TRUE
plm::pdim(data_panel) # Balanced Panel: n = 3237, T = 4, N = 12948
stargazer::stargazer(data_panel, type = "text")

# Exploratory Data Analysis #
colnames(data_panel)
glimpse(data_panel)
summary(data_panel)

# Variable Wave: set as numeric (year)
data_panel$year <- as.numeric(data_panel$year)

# Variable: Age and Yob (Year of Birth)
# Fill in NA values in the age column with year - yob if NA
data_filtered <- data_panel %>%
  filter(is.na(yob))

data_panel <- data_panel %>%
  group_by(pidlink) %>%
  fill(yob, .direction = "downup") %>%
  ungroup()

data_panel <- data_panel %>%
  group_by(pidlink) %>%
  mutate(yob = ifelse(is.na(yob), year - age, yob)) %>%
  ungroup()

data_filtered <- data_panel %>%
  filter(is.na(age))

data_panel <- data_panel %>%
  mutate(age = ifelse(is.na(age),  as.numeric(year) - yob, age))

data_panel <- data_panel %>%
  filter(!is.na(yob) & yob >= 1965 & yob <= 1982)

# Recalculate age based on the adjusted yob
data_panel <- data_panel %>%
  mutate(age = as.numeric(year) - yob)

# Check the balancedness of the data
plm::punbalancedness(data_panel) # gamma 0.9402642, nu 0.9826739
plm::pdim(data_panel)$balanced # FALSE
plm::pdim(data_panel) # Balanced Panel: n = 2032, T = 1-4, N = 7929

# Variable: Inc (Ideal Number of Children)
# Replace "inc" values greater than 15 with NA using mutate due to rationality 
data_panel <- data_panel %>%
  mutate(inc = ifelse(inc > 15, NA, inc))

# Variable: Able (Able to conceive a child again)
# Replace "able" value 9 with NA as missing, 3 recode for 0 to make it binary
data_panel <- data_panel %>%
  mutate(able = ifelse(able == 9, NA, 
                       ifelse(able == 3, 0, able)))

# Variable: Wishmorechild (Wishing another child)
# Replace "wishmorechild" value 9 with NA as missing, 3 recode for 0 to make it binary
data_panel <- data_panel %>%
  mutate(wishmorechild = ifelse(wishmorechild == 9, NA, 
                                ifelse(wishmorechild == 3, 0, wishmorechild)))

# Variable: Givebirth (Ever give birth)
# Replace "givebirth" value 9 with NA as missing, 3 recode for 0 to make it binary
data_panel <- data_panel %>%
  mutate(givebirth = ifelse(givebirth == 9, NA,
                            ifelse(givebirth == 3, 0, givebirth)))

# Variable: Expchilddead (Experiencing the death of children)
# Replace "expchilddead" values 3 recode for 0 to make it binary
data_panel <- data_panel %>%
  mutate(expchilddead = ifelse(expchilddead == 3, 0, expchilddead))

# Variable: Marstat (Marital status)
# Create "married" if marstat == 2, store as 1
data_panel <- data_panel %>%
  group_by(pidlink) %>%
  mutate(marstat = ifelse(is.na(marstat), first(marstat[!is.na(marstat)]), marstat)) %>%
  ungroup()

data_panel <- data_panel %>%
  mutate(married = ifelse(marstat == 2, 1, 0))

# Variable: Contracept (Respondent use contraceptive method to prevent a pregnancy)
# Replace "contracept" values = 9 with NA as missing, and 3 as 0 that indicates no use
data_panel <- data_panel %>%
  mutate(contracept = ifelse(contracept == 9, NA, 
                             ifelse(contracept == 3, 0, contracept)))

# Variable: Javanese (Respondent identify them-self as Javanese)
# Replace "javanese" values = 1, otherwise 0
# Create a vector of pidlink values to drop 
pidlink_to_drop <- c("015040004", "087020009", "121010004", "228250004", "234010002", "257150002")

# Remove observations with the specified pidlink values
data_panel <- subset(data_panel, !(pidlink %in% pidlink_to_drop))

# Create a vector of "pidlink" values and their corresponding new "ethnicity" values
pidlink_to_change <- c("044300003", "057240003", "078040002", "102090003", "104180003", "128100006", "186210001", "229040003", "230180003", "273270002")
new_ethnicity_values <- c(6, 2, 17, 1, 1, 1, 2, 1, 1, 1)

# Use ifelse to update the "ethnicity" variable based on the specified rules
data_panel$ethnicity <- ifelse(data_panel$pidlink %in% pidlink_to_change, 
                               new_ethnicity_values[match(data_panel$pidlink, pidlink_to_change)], 
                               data_panel$ethnicity)

data_panel <- data_panel %>%
  mutate(javanese = ifelse(ethnicity == 1, 1, 0))

# Variable: employed (Respondent identify them-self employer)
# Recode "main_activity" values = 1 for employer
data_panel <- data_panel %>%
  mutate(employed = ifelse(main_activity == 1, 1, 0))

# Variable: Ymar and Agemar (Year of marriage and Age when married)
data_panel <- data_panel %>%
  mutate(ymar = ifelse(ymar < 1977 | ymar > 2014, NA, ymar))

data_panel$ymar <- as.numeric(data_panel$ymar)

data_panel <- data_panel %>%
  group_by(pidlink) %>%
  fill(ymar, .direction = "downup") %>%
  ungroup()

data_panel <- data_panel %>%
  mutate(agemar = ifelse(is.na(agemar), ymar - yob, agemar)) %>%
  mutate(agemar = ifelse(agemar < 12 | agemar > 48, NA, agemar))

data_panel$agemar <- as.numeric(data_panel$agemar)

# Variable: Mardur (The duration of individual's marriage)
data_panel <- data_panel %>% 
  mutate(mardur = as.numeric(year) - as.numeric(ymar)) %>% 
  mutate(mardur = ifelse(mardur < 0, 0, mardur))

data_panel <- data_panel %>%
  group_by(pidlink) %>%
  mutate(mardur = ifelse(is.na(mardur), first(mardur[!is.na(mardur)]), mardur)) %>%
  ungroup()

# Variable: Female (Individual is female)
# Replace "sex" value 3 recode for 1 to make it binary
data_panel <- data_panel %>%
  rename(female = sex) %>%
  mutate(female = ifelse(female == 3, 1, female))

# Variable: Achieved INC (Individual achieve INC at each waves)
# Generate "achievedINC" value 1 if numchild = inc, 0 otherwise
data_panel <- data_panel %>%
  mutate(achievedinc = ifelse(inc == numchild, 1, 0))

# Variable: years_of_education (Recalculate Years of education)
# Fill in NA values in the years_of_education column with the non_NA first years_of_education from the observations
data_panel <- data_panel %>%
  group_by(pidlink) %>%
  mutate(years_of_education = ifelse(is.na(years_of_education), first(years_of_education[!is.na(years_of_education)]), years_of_education)) %>%
  ungroup()

data_panel <- data_panel %>%
  group_by(pidlink) %>%
  mutate(years_of_education_factor = ifelse(is.na(years_of_education_factor), first(years_of_education_factor[!is.na(years_of_education_factor)]), years_of_education_factor)) %>%
  ungroup()

# Variable: Religion (Recode Religion)
# Create religion with value: 1 as Islam, 0 as others
data_panel <- data_panel %>%
  mutate(Islam = ifelse(religion == 1, 1, 0))

# Create Variable Age-squared 
data_panel <- data_panel %>% 
  mutate(agesq = age^2)

# Rename Variable
data_panel <- data_panel %>% 
  rename(parliv = live_with_parent,
         parinlawliv = living_with_parentinlaw,
         demake = ratio_demake,
         pardemake = ratio_pardemake,
         parinlawdemake = ratio_parinlawdemake,
         joinpar = ratio_joinpar,
         joinilw = ratio_joinilw,
         numsibs = naive_sibling_count,
         birthorder = birth_order_category,
         yoschool = years_of_education,
         ruralresident = rural_resident,
         javabaliresident = java_bali_resident,
         expenditurec = expenditure_category,
         educationallevel = years_of_education_factor)

# Recode Variable
data_panel$able <- as.factor(data_panel$able)
data_panel$wishmorechild <- as.factor(data_panel$wishmorechild)
data_panel$givebirth <- as.factor(data_panel$givebirth)
data_panel$expchilddead <- as.factor(data_panel$expchilddead)
data_panel$contracept <- as.factor(data_panel$contracept)
data_panel$menopause <- as.factor(data_panel$menopause)
data_panel$badbirthexp <- as.factor(data_panel$badbirthexp)
data_panel$parliv <- as.factor(data_panel$parliv)
data_panel$parinlawliv <- as.factor(data_panel$parinlawliv)
data_panel$ruralresident <- as.factor(data_panel$ruralresident)
data_panel$javabaliresident <- as.factor(data_panel$javabaliresident)
data_panel$javanese <- as.factor(data_panel$javanese)
data_panel$married <- as.factor(data_panel$married)
data_panel$employed <- as.factor(data_panel$employed)
data_panel$achievedinc <- as.factor(data_panel$achievedinc)
data_panel$Islam <- as.factor(data_panel$Islam)

# Save the data frame to an XLS file
openxlsx::write.xlsx(data_panel, "fertilpref_final.xlsx", rowNames = FALSE)

# Save data as data.df
data.df <- openxlsx::read.xlsx("fertilpref_final.xlsx")

# Create a new variable "educational_level" based on educationallevel
data.df <- data.df %>%
  mutate(education_level = fct_collapse(
    educationallevel,
    "Elementary" = c("Elementary.0", "Elementary.1", "Elementary.2", "Elementary.3", "Elementary.4", "Elementary.5", "Elementary.6", "Elementary.7"),
    "Junior High School" = c("Junior High.0", "Junior High.1", "Junior High.2", "Junior High.3", "Junior High.6", "Junior High.7"),
    "Senior High School" = c("Senior High.0", "Senior High.1", "Senior High.2", "Senior High.3", "Senior High.7"),
    "University" = c("University.0", "University.1", "University.2", "University.3", "University.4", "University.5", "University.6", "University.7")
  ))

# Create dummy of wifeauth
data.df <- data.df %>% 
  mutate(wifeauth = ifelse(demake >= 0.5, 1, 0),
         baldemaked = ifelse(demake == 0.5, 1, 0),
         domdemaked = ifelse(demake > 0.5, 1, 0))

# Create dummy of pardemake and ilwdemaked
data.df <- data.df %>% 
  mutate(parentsdom = ifelse(pardemake != 0, 1, 0))

data.df <- data.df %>% 
  mutate(inlawsdom = ifelse(parinlawdemake != 0, 1, 0))

# Create dummy of joinpar and joinilw
data.df <- data.df %>% 
  mutate(parentsjoin = ifelse(joinpar != 0, 1, 0))

data.df <- data.df %>% 
  mutate(inlawsjoin = ifelse(joinilw != 0, 1, 0))

# Recode numchild == 99 as NA
data.df <- data.df %>%
  mutate(numchild = ifelse(numchild == 99, NA, numchild))

# Rename variable
data.df <- data.df %>%
  rename(matrilocal = parliv, patrilocal = parinlawliv)

# Create neolocal if matrilocal & patrilocal = 0
data.df <- data.df %>%
  mutate(neolocal = ifelse(matrilocal == 0 & patrilocal == 0, 1, 0))

# Create dummy variable: post-reform
data.df <- data.df %>% 
  mutate(post_reform = ifelse(year >= 2000, 1, 0))

# Creating a new data frame 'do.data' by selecting specific variables use from 'data.df'
do.data <- data.df %>% 
  
  # Selecting the following variables
  select(
    pidlink, hhid, year, inc, age, agesq, agemar, neolocal, matrilocal, patrilocal, 
    wifeauth, baldemaked, domdemaked, parentsdom, inlawsdom, parentsjoin, inlawsjoin, 
    employed, economic_group, hhsize, ruralresident, Islam, married, mardur,
    javabaliresident, education_level, javanese, female, post_reform
  )

# Drop missing observations for all variables
do.data <- na.omit(do.data)

# Labeling variable
var_label(do.data$pidlink)  <- "Personal ID"
var_label(do.data$hhid)  <- "Household ID"
var_label(do.data$year)  <- "Survey round"
var_label(do.data$inc)  <- "Ideal number of children"
var_label(do.data$age)  <- "Age (years)"
var_label(do.data$agesq)  <- "Age Squared (years)"
var_label(do.data$agemar)  <- "Age at marriage (years)"
var_label(do.data$matrilocal)  <- "Individual lives with parents"
var_label(do.data$patrilocal)  <- "Individual lives with in-laws"
var_label(do.data$neolocal)  <- "Individual lives with husband"
var_label(do.data$wifeauth)  <- "Individual has at least half authority in household decisions"
var_label(do.data$baldemaked)  <- "Individual has equal authority in household decisions"
var_label(do.data$domdemaked)  <- "Individual has dominant authority in household decisions"
var_label(do.data$parentsdom)  <- "Individual's parents are dominantly involved in some household decision-making"
var_label(do.data$inlawsdom)  <- "Individual's in-laws are dominantly involved in some household decision-making"
var_label(do.data$parentsjoin)  <- "Individual's in-laws are jointly involved in household decision-making"
var_label(do.data$inlawsjoin)  <- "Individual's parents are jointly involved in household decision-making"
var_label(do.data$employed)  <- "Currently employed"
var_label(do.data$economic_group)  <- "Wealth economic group"
var_label(do.data$hhsize)  <- "Household size"
var_label(do.data$ruralresident)  <- "Rural residence"
var_label(do.data$Islam)  <- "Individual is Moslem"
var_label(do.data$married)  <- "Individual is currently married"
var_label(do.data$mardur)  <- "Duration of marriage"
var_label(do.data$javabaliresident)  <- "Java-Bali residence"
var_label(do.data$education_level)  <- "Highest education"
var_label(do.data$javanese)  <- "Individual is Javanese"
var_label(do.data$female)  <- "Individual is female"
var_label(do.data$post_reform) <- "Post-reform Era"

# Recode the format of variables
do.data$female <- as.factor(do.data$female)
do.data$wifeauth <- as.factor(do.data$wifeauth)
do.data$baldemaked <- as.factor(do.data$baldemaked)
do.data$domdemaked <- as.factor(do.data$domdemaked)
do.data$parentsdom <- as.factor(do.data$parentsdom)
do.data$inlawsdom <- as.factor(do.data$inlawsdom)
do.data$parentsjoin <- as.factor(do.data$parentsjoin)
do.data$inlawsjoin <- as.factor(do.data$inlawsjoin)

# Filter individuals who are married = 1 in all survey years
do.data <- do.data %>%
  group_by(pidlink) %>%
  filter(all(year %in% c(1997, 2000, 2007, 2014)) & all(married == 1)) %>%
  ungroup()

# Filter data_panel to keep observations with pidlink available in all survey periods
do.data <- do.data %>%
  group_by(pidlink) %>%
  filter(all(c(1997, 2000, 2007, 2014) %in% year)) %>%
  ungroup()

# Save the data frame to an XLS file
openxlsx::write.xlsx(do.data, "do.data_update.xlsx", rowNames = FALSE)

# Formatting in panel data.frane
do.data <- pdata.frame(do.data, index = c('pidlink', 'year'), drop.index = FALSE)

# Check the balancedness of the data
punbalancedness(do.data) # gamma 1, nu 1
pdim(do.data)$balanced # TRUE
pdim(do.data) # Balanced Panel: n = 976, T = 4, N = 3904

# Describe and summarize
do.data %>% summary
do.data %>% str
do.data %>% head(12)

# Check the stability of the values of several variables #
result_dif_matrilocal <- do.data %>% 
  group_by(pidlink) %>% 
  summarise(has_difference = length(unique(matrilocal)) > 1)

result_dif_patrilocal <- do.data %>% 
  group_by(pidlink) %>% 
  summarise(has_difference = length(unique(patrilocal)) > 1)

result_dif_neolocal <- do.data %>% 
  group_by(pidlink) %>% 
  summarise(has_difference = length(unique(neolocal)) > 1)

result_dif_inc <- do.data %>% 
  group_by(pidlink) %>% 
  summarise(has_diff_inc = length(unique(inc)) > 1)

# Merge datasets
do.data <- merge(do.data, result_dif_inc, by = "pidlink")

# Create inc_stability variable
do.data$inc_stability <- ifelse(do.data$has_diff_inc == FALSE, "stable", "unstable")

# Alternative Model Subsample
do.inc_stable <- subset(do.data, inc_stability %in% c("stable"))
do.inc_unstable <- subset(do.data, inc_stability %in% c("unstable"))

# Save the data frame to an XLS file
# Remove has_diff_inc variable
do.data$has_diff_inc <- NULL
write.xlsx(do.data, "stability.data.update.xlsx", rowNames = FALSE)

#### End ####

