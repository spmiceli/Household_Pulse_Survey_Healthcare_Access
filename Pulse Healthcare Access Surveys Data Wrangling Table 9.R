library(tidyverse)
library(readxl)

wd <- "D:\\Data sets and Coding Files\\Household Pulse Surveys Healthcare Access\\Table9"
setwd(wd)
getwd()

dt <- tibble(
  file = dir(wd, full.names = FALSE),
  data = map(file, read_xlsx, sheet = "IN", skip = 5, na = "-")
)

dt$week <- as.numeric(str_remove(str_remove(dt$file, "health9_week"), ".xlsx"))
dt <- dt %>% arrange(week)

dt_2 <- dt %>% select(data,week) %>% unnest(cols=c(data))

str(dt_2)

data <- dt_2 %>% select(-...2)    


names(data) <- c("demog", "hcp_closed", "hcp_limited", "hcp_concern_going", "change_in_insur",  "household_member_covid_pos",
                 "household_member_in_contact", "none_of_above", 'child_miss_appt_DNR', "child_did_not_miss_appt", "q1_DNR",
                 "child_had_distance_appt_Y", "child_had_distance_appt_N", "child_had_distance_appt_DNR", "week")

data_2 <- data %>% add_column("Total" = NA, "Age" = NA, "Sex"=NA,
                              "Hispanic origin and Race"=NA,"Education" = NA, 
                              "Marital status"=NA, "Household size" = NA,
                              "Presence of children under 18 years old"=NA, 
                              "Respondent or household member experienced loss of employment income" =NA, 
                              "Respondent currently employed"=NA, 
                              "Household income" = NA, "Used in the last 7 days to meet spending needs*" = NA, 
                              "Active duty military*" = NA, "Difficulty seeing" = NA, "Difficulty hearing" = NA,
                              "Difficulty remembering or concentrating" = NA, "Difficulty walking or climbing stairs" = NA) %>% 
  filter(is.na(demog)==FALSE)

data_2 <- data_2 %>% mutate(Total = ifelse(demog=="Total",  TRUE, NA))

for(i in 1:length(data_2$demog)){
  if((data_2$demog %in% colnames(data_2))[i] == FALSE)
    data_2[i,j] = data_2$demog[i]
  if((data_2$demog %in% colnames(data_2))[i] == TRUE)
    j = match(data_2$demog[i], colnames(data_2))
}



data_2 <- data_2 %>% filter(demog %in% colnames(data_2)[-match("Total",colnames(data_2))] == FALSE) 

data_2 <- data_2 %>% filter( ( (data_2$demog == "* Totals may not sum to 100% as the question allowed for multiple categories to be marked.") |
                                 (demog == "** The Census Bureau considers estimated coefficients of variation (standard error divided by the estimate times 100) over 30 percent to indicate potentially serious data quality issues related to sampling error.") ) == FALSE)



data_2 <- data_2 %>% drop_na(demog)

data_final <- data_2[,-1]

rm(data, data_2, dt_2, i, j)

write.csv(data_final, "D:\\Data sets and Coding Files\\Household Pulse Surveys Healthcare Access\\HPS_HA_T9_Wrang.csv", row.names = FALSE)
