############################################################
# author(s):  
# author contact info: 
# file location: 
# data location: 
# date created: 
# date last edited: 
# project purpose: 
# packages needed: 
############################################################

# download the zip file for the 2017 Survey of College Graduates 
# from https://ncsesdata.nsf.gov/datadownload/ and unzip the file
# replace "pcg17Public" with the location where epcg17.xpt is saved locally 
___________ <- Hmisc::sasxport.get(_______ = "pcg17Public/epcg17.xpt")

# open tidyverse for data management
library(__________ = "tidyverse")

# recode variables to include meaningful category labels
collGrad17small <- _______ %>%
  select(n2ocprmg, gender) %>% 
  rename(field = n2ocprmg, sex = gender) %>% 
  mutate(field = recode(.x = field, 
                        "1" = "_______",
                        "2" = "_______",
                        "3" = "_______",
                        "4" = "_______",
                        "5" = "_______",
                        "6" = NA_character_, 
                        "7" = "_______",
                        "8" = NA_character_)) %>% 
  mutate(sex = recode(.x = sex, 
                      "M" = "_______",
                      "F" = "_______")) %>% 
  drop_na(sex) %>% 
  drop_na(field)

# check data management results
summary(_______ = _______)

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
  _______(aes(x = sex, y = perc, fill = field)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_minimal() +
  scale_fill_manual(values = c("_______", "_______", "_______"),
                    name = "Current career field") +
  labs(x = "_______", y = "_______") 

# open the descr package to use CrossTable
_______(_______ = "descr")

# chi-squared examining job field by sex
# examine the observed and expected values, chi-squared,
# and standardized residuals
CrossTable(x = collGrad17small$_______, y = collGrad17small$_______, 
           expected = TRUE, prop.r = FALSE,
           prop.c = FALSE, prop.t = FALSE, 
           prop.chisq = FALSE, chisq = TRUE,
           sresid = TRUE,
           dnn = c("career field", "sex"))

# compute Cramér's V effect size for job field and sex
# chi-squared analysis
_______(_______ = "lsr")
cramersV(x = collGrad17small$_______, 
         y = collGrad17small$_______)