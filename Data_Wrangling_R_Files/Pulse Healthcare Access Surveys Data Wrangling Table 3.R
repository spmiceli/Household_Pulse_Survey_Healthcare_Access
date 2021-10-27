library(tidyverse)
library(readxl)

wd <- "D:\\Data sets and Coding Files\\Household Pulse Surveys Healthcare Access\\Table3"
setwd(wd)
getwd()

dt <- tibble(
  file = dir(wd, full.names = FALSE),
  data = map(file, read_xlsx, sheet = "IN", skip = 4, na = "-")
)

dt$week <- as.numeric(str_remove(str_remove(dt$file, "health3_week"), ".xlsx"))
dt <- dt %>% arrange(week)

dt_2 <- dt %>% select(data,week) %>% unnest(cols=c(data))

str(dt_2)

dt_2$`...1` <- coalesce(dt_2$`...1`, as.character(dt_2$`Select characteristics`))

dt_2$Total <- coalesce(coalesce(dt_2$Total, as.double(dt_2$`Insured *...2`), as.double(dt_2$`Insured*...2`)))

dt_2$Private <- coalesce(coalesce(dt_2$Private, as.double(dt_2$`Insured *...3`), as.double(dt_2$`Insured*...3`)))                       

dt_2$Public <- coalesce(coalesce(dt_2$Public, as.double(dt_2$`Insured *...4`), as.double(dt_2$`Insured*...4`)))

dt_2$`...5` <- coalesce(dt_2$`...5`, as.double(dt_2$Uninsured))

dt_2$`...6` <- coalesce(dt_2$`...6`, as.double(dt_2$`Did not report or don't know`))

data <- dt_2 %>% select(...1, `Total`, `Private`, `Public`, `...5`, `...6`, week)    


names(data) <- c("demog", "total_insured", "private_insured", "public_insured", "uninsured", "dnr",  "week")

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

data_2 <- data_2 %>% filter(demog %in% colnames(data_2)[-8] == FALSE)

data_2 <- data_2 %>% drop_na(demog)

data_final <- data_2[,-1]

rm(data, data_2, dt_2, i, j)

write.csv(data_final, "D:\\Data sets and Coding Files\\Household Pulse Surveys Healthcare Access\\HPS_HA_T3_Wrang.csv", row.names = FALSE)
