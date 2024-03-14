
#### ECHILD Course Day 2: Developing e-Cohorts #### 

#### Part 1 ####

# data.table is a very helpful package for working with big datasets.
# Aside from providing helpful syntax, it is very efficient memory-wise.
# install.packages("data.table")
library(data.table)

# Set your working directory and load data
rm(list = ls()); gc()
setwd("your_directory/")

hes <- fread("HES.csv")
ks1 <- fread("KS1.csv")
spine <- fread("spine.csv")

# Alternatively, you can download directly:
# hes <- fread("https://rdr.ucl.ac.uk/ndownloader/files/44972194")
# ks1 <- fread("https://rdr.ucl.ac.uk/ndownloader/files/44972182")
# spine <- fread("https://rdr.ucl.ac.uk/ndownloader/files/44972191")

# We are defining a birth cohort using HES data, so we'll focus there first.
summary(hes)


# 1) What birth years have we provided you?
# To answer, we can define the calendar year of birth from epistart.
class(hes$epistart)
hes[, epistart_cyear := format(epistart, "%Y")]
table(hes$epistart_cyear, useNA = "always")

# 2) What is the start age for people in this dataset?
table(hes$startage, useNA = "always")

# 3) What epitype did we provide you? What does this mean?
table(hes$epitype, useNA = "always")

# 4) What admimeth did we provide you? What does this mean?
table(hes$admimeth, useNA = "always")

# 5) What ICD10 code did we provide you in DIAG_01? What does this mean?
table(hes$diag_01, useNA = "always")

# 6) How many unique individuals are there in the dataset?
length(unique(hes$tokenid)); nrow(hes)

# 7) Identify  which variable represents the region of residence.
# 8) Calculate how many individuals' region of residence at birth is Scotland or Wales.
table(hes$resgor, useNA = "always")
sum(hes$resgor %in% c("S", "W"))

# 9) How many individuals are left once you only keep those who resided in English regions at birth?
nrow(hes[!(substr(resgor, 1, 1) %in% c("S", "W"))])
hes <- hes[!(substr(resgor, 1, 1) %in% c("S", "W"))]





#### Part 2 ####
# 1) What ICD10 codes have we included in DIAG_02?
# 2) Using the ICD-10 browser, what do these codes represent?
table(hes$diag_02, useNA = "always")

# 3) How many patients have a major congenital anomaly code? Once you know, you can drop these patients
mca <- c("Q24",
         "Q35",
         "Q37",
         "Q43")

sum(hes$diag_02 %in% mca)
sum(hes$diag_02 %in% mca) / nrow(hes) # To get a proportion

# Now we can drop them
hes <- hes[!(diag_02 %in% mca)]


# 4) How many patients have a linked record in the spine?

# We can link to NPD via the spine. We will use the merge() function to do this.
# First, let's ensure there is no duplication of tokenid in the spine (which
# would represent multiple matches in real ECHILD) as these will cause duplication
# in our merged data.table.
summary(spine)
spine
length(unique(spine$tokenid)); nrow(spine)

hes <- merge(hes,
             spine,
             by = "tokenid",
             all.x = T)

table(is.na(hes$PupilMatchingRefAnonymous)) # Missing means no PMR, which means no link



#### Part 3 ####
# 1) Are there differences in linkage rates between HES and NPD based on sex,
# deprivation and ethnicity? Once you have answered this question, you can drop
# the students without a link.

# Let's first explore our sex, deprivation and ethnicity variables.
# Remember to consult the data dictionary for meaning.
table(hes$sex, useNA = "always")
table(hes$ethnos, useNA = "always")
table(hes$imd04_decile, useNA = "always")

# Now examine non-linkage rates by each variable. Here we have defined a custom
# function that wraps the N and percentage up together for use within the
# aggregate() function.
my_table <- function(vec) {
  d <- length(vec)
  n <- sum(vec)
  p <- round((n / d) * 100, 1)
  return(paste0(n, " (", p, "%)"))
}

aggregate(is.na(PupilMatchingRefAnonymous) ~ sex,
          data = hes,
          FUN = my_table)

aggregate(is.na(PupilMatchingRefAnonymous) ~ ethnos,
          data = hes,
          FUN = my_table)

aggregate(is.na(PupilMatchingRefAnonymous) ~ imd04_decile,
          data = hes,
          FUN = my_table)


# Now we have analysed linkage rates, We can drop those who did not link and 
# focus on our research question.
hes <- hes[!is.na(PupilMatchingRefAnonymous)]


# 2) What is the relationship between gestational age and KS1 results
# (unadjusted and adjusted for sex, deprivation and ethnicity)?

# To start this, we need to link the KS1 data, using the PupilMatchingReference
# that we merged in earlier.
summary(ks1)
length(unique(ks1$PupilMatchingRefAnonymous)); nrow(ks1) # Checking uniqueness
hes <- merge(hes,
             ks1[, c("PupilMatchingRefAnonymous", "KS1_MATH")],
             by = "PupilMatchingRefAnonymous",
             all.x = T)

hist(hes$KS1_MATH)
summary(hes$KS1_MATH)

table(hes$gestat, useNA = "always") # Let's recode gestat as a factor: it's quite messy as character

hes[, gestat := factor(gestat,
                       levels = c("less_than_28",
                                  "28_29",
                                  "30_31",
                                  32:42))]
hes[, gestat := relevel(gestat, ref = "40")] # so 40 is baseline in our model

table(hes$gestat, useNA = "always") # Better

# Univariable analysis:
aggregate(KS1_MATH ~ gestat, hes, function(x) c(mean(x), sd(x)))

# Linear regression:
m1 <- lm(KS1_MATH ~ gestat, hes)
m2 <- lm(KS1_MATH ~ gestat + sex + imd04_decile + ethnos, hes)

summary(m1)
summary(m2)
