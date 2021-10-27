library(tidyverse)
library(readxl)

wd <- "D:\\Data sets and Coding Files\\Household Pulse Surveys Healthcare Access\\Table5"
setwd(wd)
getwd()

dt <- tibble(
  file = dir(wd, full.names = FALSE),
  data = map(file, read_xlsx, sheet = "IN", skip = 6, na = "-")
)

dt$week <- as.numeric(str_remove(str_remove(dt$file, "health5_week"), ".xlsx"))
dt <- dt %>% arrange(week)

dt_2 <- dt %>% select(data,week) %>% unnest(cols=c(data))

str(dt_2)

dt_2$`...13` <- coalesce(dt_2$`...13`, as.double(dt_2$...14))

dt_2$`Did not report...12` <- coalesce(dt_2$`Did not report...12`, as.double(dt_2$`Did not report...13`))

data <- dt_2 %>% select(...1, Total...3, `Received or plan to receive all required doses`,
                        `Have not received/do not plan to receive all required doses`, `Did not report...6`,
                        Total...7, `Will definitely get a vaccine`, `Will probably get a vaccine`, `Unsure about getting a vaccine`,
                        `Will probably not get a vaccine`, `Will definitely not get a vaccine`, `Did not report...12`,
                        ...13, week)  

names(data) <- c("demog", "yes_total", "yes_plan_to_get", "yes_not_all_doses", "yes_dnr",
                 "no_total", "no_def_will_get", "no_prob_will_get", "no_unsure", "no_prob_not_get", "no_will_not_get", 
                 "no_dnr", "dnr", "week")

data_2 <- data %>% add_column("Total"=NA, "Age" = NA, "Sex"=NA,
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

data_2 <- data_2 %>% filter(demog %in% colnames(data_2)[-15] == FALSE)

data_2 <- data_2 %>% drop_na(demog)

data_final <- data_2[,-1]

rm(data, data_2, dt_2, i, j)

write.csv(data_final, "D:\\Data sets and Coding Files\\Household Pulse Surveys Healthcare Access\\HPS_HA_T5_Wrang.csv", row.names = FALSE)
