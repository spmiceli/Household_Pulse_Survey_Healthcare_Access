library(tidyverse)
library(readxl)

wd <- "D:\\Data sets and Coding Files\\Household Pulse Surveys Healthcare Access\\Table1"
setwd(wd)
getwd()

dt <- tibble(
  file = dir(wd, full.names = FALSE),
  data = map(file, read_xlsx, sheet = "IN", skip = 5, na = "-")
)


dt$week <- as.numeric(str_remove(str_remove(dt$file, "health1_week"), ".xlsx"))
dt <- dt %>% arrange(week)


dt_2 <- dt %>% select(data,week) %>% unnest(cols=c(data))

dt_2$Yes...2 <- coalesce(coalesce(dt_2$Yes...2, as.double(dt_2$`Delayed getting medical care because of coronavirus pandemic...2`)), 
                as.double(dt_2$`Delay getting medical care because of coronavirus pandemic...2`))

dt_2$No...3 <- coalesce(coalesce(dt_2$No...3, as.double(dt_2$`Delayed getting medical care because of coronavirus pandemic...3`)),
                as.double(dt_2$`Delay getting medical care because of coronavirus pandemic...3`))

dt_2$`Did not report...4` <- coalesce(coalesce(dt_2$`Did not report...4`, 
                                      as.double(dt_2$`Delayed getting medical care because of coronavirus pandemic...4`)),
                                      as.double(dt_2$`Delay getting medical care because of coronavirus pandemic...4`))

dt_2$Yes...5 <- coalesce(coalesce(dt_2$Yes...5, 
                         as.double(dt_2$`Needed medical care for something unrelated to coronavirus, but did not get it...5`)),
                         as.double(dt_2$`Need medical care for something unrelated to coronavirus, but did not get it...5`))

dt_2$No...6 <- coalesce(coalesce(dt_2$No...6, 
                        as.double(dt_2$`Needed medical care for something unrelated to coronavirus, but did not get it...6`)),
                        as.double(dt_2$`Need medical care for something unrelated to coronavirus, but did not get it...6`))

dt_2$`Did not report...7` <- coalesce(coalesce(dt_2$`Did not report...7`,
                                      as.double(dt_2$`Needed medical care for something unrelated to coronavirus, but did not get it...7`)),
                                      as.double(dt_2$`Need medical care for something unrelated to coronavirus, but did not get it...7`))

data <- dt_2 %>% select(...1, Yes...2, No...3, `Did not report...4`, `Yes...5`, `No...6`, `Did not report...7`, 
                `Had an appointment with a doctor, nurse, or other health professional by video or phone...8`,
                `Had an appointment with a doctor, nurse, or other health professional by video or phone...9`,
                `Had an appointment with a doctor, nurse, or other health professional by video or phone...10`, week)

names(data) <- c("demog", "delayed_care_Y", "delayed_care_N", "delayed_care_DNR", 
                      "needed_care_Y", "needed_care_N", "needed_care_DNR", 
                      "distance_appointment_Y", "distance_appointment_N", "distance_appointment_DNR", "week")
  
data_2 <- data %>% add_column("Total" = NA, "Age" = NA, "Sex"=NA, "Hispanic origin and Race"=NA,"Education" = NA, 
                                    "Marital status"=NA, "Household size" = NA,
                                    "Presence of children under 18 years old"=NA, 
                                    "Respondent or household member experienced loss of employment income" =NA, 
                                    "Respondent currently employed"=NA, 
                                    "Household income" = NA, "Used in the last 7 days to meet spending needs*" = NA, 
                                    "Active duty military*" = NA, "Difficulty seeing" = NA, "Difficulty hearing" = NA,
                                    "Difficulty remembering or concentrating" = NA, "Difficulty walking or climbing stairs" = NA) %>% 
                   filter(is.na(demog)==FALSE)

data_2 <- data_2 %>% mutate(Total = ifelse(demog=="Total",  TRUE, NA))

j = 12
for(i in 1:length(data_2$demog)){
  if((data_2$demog %in% colnames(data_2))[i] == FALSE)
      data_2[i,j] = data_2$demog[i]
  if((data_2$demog %in% colnames(data_2))[i] == TRUE)
      j = match(data_2$demog[i], colnames(data_2))
}
data_2 <- data_2 %>% filter(demog %in% colnames(data_2)[-13] == FALSE)

data_2 <- data_2 %>% drop_na(demog)

data_final <- data_2[,-1]
rm(data, data_2, dt_2, i, j)


write.csv(data_final, "D:\\Data sets and Coding Files\\Household Pulse Surveys Healthcare Access\\HPS_HA_T1_Wrang.csv", row.names = FALSE)
