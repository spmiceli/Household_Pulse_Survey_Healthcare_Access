library(tidyverse)
library(readxl)

wd <- "D:\\Data sets and Coding Files\\Household Pulse Surveys Healthcare Access\\Table4"
setwd(wd)
getwd()

dt <- tibble(
  file = dir(wd, full.names = FALSE),
  data = map(file, read_xlsx, sheet = "IN", skip = 5, na = "-")
)

dt$week <- as.numeric(str_remove(str_remove(dt$file, "health4_week"), ".xlsx"))
dt <- dt %>% arrange(week)


dt_2 <- dt %>% select(data,week) %>% unnest(cols=c(data))


str(dt_2)

dt_2$Yes...3 <- coalesce(dt_2$Yes...3, as.double(dt_2$`Received counseling or therapy from a mental health professional such as a psychiatrist, psychologist, psychiatric nurse, or clinical social worker...3`))

dt_2$No...4 <- coalesce(dt_2$No...4, as.double(dt_2$`Received counseling or therapy from a mental health professional such as a psychiatrist, psychologist, psychiatric nurse, or clinical social worker...4`))

dt_2$`Did not report...5` <- coalesce(dt_2$`Did not report...5`, as.double(dt_2$`Received counseling or therapy from a mental health professional such as a psychiatrist, psychologist, psychiatric nurse, or clinical social worker...5`))

dt_2$Yes...6 <- coalesce(dt_2$`Yes...6`, as.double(dt_2$`Needed counseling or therapy from a mental health professional, but did not get it for any reason...6`))

dt_2$No...7 <- coalesce(dt_2$`No...7`, as.double(dt_2$`Needed counseling or therapy from a mental health professional, but did not get it for any reason...7`))

dt_2$`Did not report...8` <- coalesce(dt_2$`Did not report...8`, as.double(dt_2$`Needed counseling or therapy from a mental health professional, but did not get it for any reason...8`))

dt_2$Yes...9 <- coalesce(dt_2$Yes...9, as.double(dt_2$`Took prescription medication to help with any emotions or with your concentration, behavior or mental health...9`))

dt_2$No...10 <- coalesce(dt_2$`No...10`, as.double(dt_2$`Took prescription medication to help with any emotions or with your concentration, behavior or mental health...10`))

dt_2$`Did not report...11` <- coalesce(dt_2$`Did not report...11`, 
                                       as.double(dt_2$`Took prescription medication to help with any emotions or with your concentration, behavior or mental health...11`))

data <- dt_2 %>% select(...1, `Yes...3`, `No...4`, `Did not report...5`, 
                        `Yes...6`, `No...7`, `Did not report...8`,
                        `Yes...9`, `No...10`, `Did not report...11`, week)                  


names(data) <- c("demog", "rec_couns_Y", "rec_couns_N", "rec_couns_DNR", "need_couns_Y", "need_couns_N", 
                 "need_couns_DNR", "med_Y", "med_N", "med_DNR", "week")

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

j = 13
for(i in 1:length(data_2$demog)){
  if((data_2$demog %in% colnames(data_2))[i] == FALSE)
    data_2[i,j] = data_2$demog[i]
  if((data_2$demog %in% colnames(data_2))[i] == TRUE)
    j = match(data_2$demog[i], colnames(data_2))
}

data_2 <- data_2 %>% filter(demog %in% colnames(data_2)[-12] == FALSE)

data_2 <- data_2 %>% drop_na(demog)

data_final <- data_2[,-1]

rm(data, data_2, dt_2, i, j)

write.csv(data_final, "D:\\Data sets and Coding Files\\Household Pulse Surveys Healthcare Access\\HPS_HA_T4_Wrang.csv", row.names = FALSE)
