### R code from vignette source '/home/travis/build/SPE-R/SPE/build/dplyr-s.rnw'

###################################################
### code chunk number 1: dplyr-s.rnw:23-27
###################################################
library(Epi)
suppressPackageStartupMessages(library(dplyr))

data(births) 


###################################################
### code chunk number 2: dplyr-s.rnw:36-38
###################################################
class(births)
head(births)


###################################################
### code chunk number 3: dplyr-s.rnw:42-43
###################################################
str(births)


###################################################
### code chunk number 4: dplyr-s.rnw:48-55
###################################################
births_tbl <- as_tibble(births)

class(births_tbl)
births_tbl

## another alternative is to use the glimpse function
glimpse(births_tbl)


###################################################
### code chunk number 5: dplyr-s.rnw:70-72
###################################################
head(births, 4)
births %>% head(4)


###################################################
### code chunk number 6: dplyr-s.rnw:79-88
###################################################
## classical way
grepl('r', 'spe-r')

## wrong chained way
## here the test done is: does 'r' contain 'spe-r'? 
'spe-r' %>% grepl('r')

## we have to specify explicitly that the chained object is the second argument
'spe-r' %>% grepl('r', .)


###################################################
### code chunk number 7: dplyr-s.rnw:95-109
###################################################
births_tbl <-
  births_tbl %>%
  mutate(
    ## modify hyp varible (conversion into factor)
    hyp = factor(hyp, labels = c("normal", "hyper")),
    ## creating a new variable aggrep
    agegrp = cut(matage, breaks = c(20, 25, 30, 35, 40, 45), right = FALSE),
    ## modify sex variable (conversion into factor)
    sex = factor(sex, levels = c(1, 2), labels = c("M", "F")),
    ## creating a new variable gest4
    gest4 = cut(gestwks, breaks = c(20, 35, 37, 39, 45), right = FALSE)
  )

births_tbl


###################################################
### code chunk number 8: dplyr-s.rnw:117-122
###################################################
births_tbl %>%
  ## select only id, women age group, sex and birth weight of the baby
  select(id, agegrp, sex, bweight) %>%
  ## keep only babies weighing more than 4000g
  filter(bweight > 4000) 


###################################################
### code chunk number 9: dplyr-s.rnw:127-137
###################################################
births_tbl %>%
  ## select only id, women age group, sex and birth weight of the baby
  select(
    id, 
    'Age group' = agegrp, 
    Sex = sex, 
    'Birth weight' = bweight
  ) %>%
  ## rearrange rows to put the heaviest newborn on top
  arrange(desc(`Birth weight`))


###################################################
### code chunk number 10: dplyr-s.rnw:143-153
###################################################
births_tbl %>%
  ## select only id, women age group, sex and birth weight of the baby
  select(
    id, 
    'Age group' = agegrp, 
    Sex = sex, 
    'Birth weight' = bweight
  ) %>%
  ## rearrange rows to put the heaviest newborn on top
  arrange(Sex, desc(`Birth weight`))


###################################################
### code chunk number 11: dplyr-s.rnw:164-172
###################################################
births.01 <-
  births_tbl %>%
  ## group the data according to the sex attribute
  group_by(sex) %>%
  ## count the number of rows/individuals in each group
  summarise(
    count = n()
  )


###################################################
### code chunk number 12: dplyr-s.rnw:178-183
###################################################
births.02 <-
  births.01 %>%
  mutate(
    percent = count / sum(count) * 100
  )


###################################################
### code chunk number 13: dplyr-s.rnw:190-196
###################################################
births.03 <-
  births.02 %>%
  summarise_if(
    is.numeric,
    sum
  )


###################################################
### code chunk number 14: dplyr-s.rnw:200-204
###################################################
births.03 %>%
  rename_all(
    ~ paste0(., '.tot')
  )


###################################################
### code chunk number 15: dplyr-s.rnw:208-215
###################################################
births.05 <-
  births_tbl %>%
  group_by(sex) %>%
  summarise(
    count = n(),
    bweight.mean = mean(bweight)
  )


###################################################
### code chunk number 16: dplyr-s.rnw:220-232
###################################################
births.05 %>%
  summarise(
    count.tot = sum(count),
    bweight.mean.tot = weighted.mean(bweight.mean, count)
  )

# this is equivalent to
births_tbl %>%
  summarise(
    count.tot = n(),
    bweight.mean.tot = mean(bweight)
  )


###################################################
### code chunk number 17: dplyr-s.rnw:242-248
###################################################
births.06 <-
  births_tbl %>%
  group_by(sex, lowbw) %>%
  summarise(
    count = n()
  )


###################################################
### code chunk number 18: dplyr-s.rnw:253-263
###################################################
births.06 %>%
  mutate(
    percent = count / sum(count) * 100
  )

births.06 %>%
  ungroup() %>%
  mutate(
    percent = count / sum(count) * 100
  )


###################################################
### code chunk number 19: dplyr-s.rnw:273-279
###################################################
births_tbl %>%
  group_by(gest4) %>%
  summarise(
    count = n(),
    bweight.mean = mean(bweight)
  )


###################################################
### code chunk number 20: dplyr-s.rnw:286-301
###################################################
births_tbl %>%
  ## keep only the newborn with defined gesational time category
  filter(
    !is.na(gest4)
  ) %>%
  group_by(lowbw, gest4) %>%
  ## compute the number of babies in each cross category
  summarise(
    count = n()
  ) %>%
  ## compute the percentage of babies in each gestational time category per 
  ## birth weight category
  mutate(
    percent = count / sum(count, na.rm = TRUE)
  )


###################################################
### code chunk number 21: dplyr-s.rnw:305-318
###################################################
births_tbl %>%
  filter(
    !is.na(gest4)
  ) %>%
  group_by(gest4, lowbw) %>%
  summarise(
    count = n()
  ) %>%
  ## compute the percentage of babies in each birth weight category per gestational 
  ## time category
  mutate(
    percent = count / sum(count, na.rm = TRUE)
  )


###################################################
### code chunk number 22: dplyr-s.rnw:328-359
###################################################
if(!require(kableExtra)) install.packages('kableExtra')
library(kableExtra)

births.08 <-
  births_tbl %>%
  filter(
    !is.na(gest4)
  ) %>%
  group_by(gest4) %>%
  summarise(
    N = n()
  ) %>%
  mutate(
    `(%)` = (N / sum(N)) %>% scales::percent()
  )

## default
births.08

## markdown flavour (useful fo automatic repport production with knitr)
births.08 %>%
  knitr::kable(fromat = 'markdown')

## create an html version of the table and save it on the hard drive
births.08 %>%
  kable() %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE
  ) %>%
  save_kable(file = 'births.08.html', self_contained = TRUE)


###################################################
### code chunk number 23: dplyr-s.rnw:362-368 (eval = FALSE)
###################################################
## ## trick to create dplyr-s.rnw file.
## ## this part have to be lauch manually
## dplyr_e.path <- '/mnt/data/georgesd/_PROJECTS/_SPE/SPE/pracs/dplyr-e.rnw'
## dplyr_e <- readLines(dplyr_e.path)
## dplyr_s <- purrr::map_chr(dplyr_e, ~ sub('results=verbatim', 'results=verbatim', .x))
## writeLines(dplyr_s, sub('-e.rnw$', '-s.rnw', dplyr_e.path))


