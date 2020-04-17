############################################################
# author(s): Jenine Harris 
# author contact info: harrisj@wustl.edu
# file location: /Users/harrisj/Box/research/2020-chisquared-tutorial
# data location: /Users/harrisj/Box/research/2020-chisquared-tutorial/pcg17Public
# date created: March 1, 2020 
# date last edited: April 4, 2020 
# project purpose: examining job field by sex using 2017 NSF data and reproducible research practices
# packages needed: Hmisc, tidyverse, descr, lsr
############################################################

# download the zip file for the 2017 Survey of College Graduates 
# from https://ncsesdata.nsf.gov/datadownload/ and unzip the file
# replace "pcg17Public" with the location where epcg17.xpt is saved locally 
collGrad17 <- Hmisc::sasxport.get(file = "pcg17Public/epcg17.xpt")

# open tidyverse for data management
library(package = "tidyverse")

# recode variables to include meaningful category labels
collGrad17small <- collGrad17 %>%
  select(n2ocprmg, gender) %>% 
  rename(field = n2ocprmg, sex = gender) %>% 
  mutate(field = recode(.x = field, 
                        "1" = "comp sci, math, engineer",
                        "2" = "other science",
                        "3" = "other science",
                        "4" = "other science",
                        "5" = "comp sci, math, engineer",
                        "6" = NA_character_, 
                        "7" = "nonscience",
                        "8" = NA_character_)) %>% 
  mutate(sex = recode(.x = sex, 
                      "M" = "male",
                      "F" = "female")) %>% 
  drop_na(sex) %>% 
  drop_na(field)

# check data management results
summary(object = collGrad17small)

# frequencies & percentages for career field by sex
collGrad17small %>%
  group_by(field, sex) %>%
  count() %>% 
  group_by(sex) %>% 
  mutate(perc = 100 * n / sum(n))

# graphing career field by sex
collGrad17small %>%
  group_by(field, sex) %>%
  count() %>% 
  group_by(sex) %>% 
  mutate(perc = 100 * n / sum(n)) %>% 
  ggplot(aes(x = sex, y = perc, fill = field)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_minimal() +
  scale_fill_manual(values = c("dodgerblue3", "lightgray", "darkgray"),
                    name = "Current career field") +
  labs(x = "Sex", y = "Percent within group") 

# open the descr package to use CrossTable
library(package = "descr")

# chi-squared examining job field by sex
# examine the observed and expected values, chi-squared,
# and standardized residuals
CrossTable(x = collGrad17small$field, y = collGrad17small$sex, 
           expected = TRUE, prop.r = FALSE,
           prop.c = FALSE, prop.t = FALSE, 
           prop.chisq = FALSE, chisq = TRUE,
           sresid = TRUE,
           dnn = c("career field", "sex"))

# compute Cramér's V effect size for job field and sex
# chi-squared analysis
library(package = "lsr")
cramersV(x = collGrad17small$field, 
         y = collGrad17small$sex)