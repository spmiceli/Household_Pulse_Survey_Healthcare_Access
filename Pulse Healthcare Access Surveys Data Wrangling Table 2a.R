library(tidyverse)
library(readxl)

wd <- "D:\\Data sets and Coding Files\\Household Pulse Surveys Healthcare Access\\Table2a"
setwd(wd)
getwd()

dt <- tibble(
  file = dir(wd, full.names = FALSE),
  data = map(file, read_xlsx, sheet = "IN", skip = 5, na = "-")
)

dt$week <- as.numeric(str_remove(str_remove(dt$file, "health2a_week"), ".xlsx"))
dt <- dt %>% arrange(week)


dt_2 <- dt %>% select(data,week) %>% unnest(cols=c(data))


str(dt_2)

dt_2$`Not at all...2` <- coalesce(dt_2$`Not at all...2`, 
                                  as.double(dt_2$`Frequency of feeling nervous, anxious, or on edge...2`))
dt_2$`Several days...3` <- coalesce(dt_2$`Several days...3`, 
                                  as.double(dt_2$`Frequency of feeling nervous, anxious, or on edge...3`))
dt_2$`More than half the days...4` <- coalesce(dt_2$`More than half the days...4`, 
                  as.double(dt_2$`Frequency of feeling nervous, anxious, or on edge...4`))
dt_2$`Nearly everyday...5` <- coalesce(dt_2$`Nearly everyday...5`, 
                  as.double(dt_2$`Frequency of feeling nervous, anxious, or on edge...5`))
dt_2$`Did not report...6` <- coalesce(dt_2$`Did not report...6`, 
                  as.double(dt_2$`Frequency of feeling nervous, anxious, or on edge...6`))
dt_2$`Not at all...7` <- coalesce(dt_2$`Not at all...7`, 
                  as.double(dt_2$`Frequency of not being able to stop or control worrying...7`))
dt_2$`Several days...8` <- coalesce(dt_2$`Several days...8`, 
                  as.double(dt_2$`Frequency of not being able to stop or control worrying...8`))
dt_2$`More than half the days...9` <- coalesce(dt_2$`More than half the days...9`, 
                  as.double(dt_2$`Frequency of not being able to stop or control worrying...9`))
dt_2$`Nearly everyday...10` <- coalesce(dt_2$`Nearly everyday...10`, 
                  as.double(dt_2$`Frequency of not being able to stop or control worrying...10`))
dt_2$`Did not report...11` <- coalesce(dt_2$`Did not report...11`, 
                  as.double(dt_2$`Frequency of not being able to stop or control worrying...11`))
                  
data <- dt_2 %>% select(...1, `Not at all...2`, `Several days...3`, `More than half the days...4`, `Nearly everyday...5`,
                        `Did not report...6`, `Not at all...7`, `Several days...8`, `More than half the days...9`,
                        `Nearly everyday...10`, `Did not report...11`, week)                  

                  
names(data) <- c("demog", "feel_anx_NAA", "feel_anx_SVD", "feel_anx_MTH", "feel_anx_NED", "feel_anx_DNR", 
                 "freq_worry_NAA", "freq_worry_SVD", "freq_worry_MTH", "freq_worry_NED", "freq_worry_DNR", "week")

data_2 <- data %>% add_column("Total" = NA, "Age" = NA, "Sex"=NA, "Gender"=NA, "Sexual Orientation"= NA,
                              "Lesbian, Gay, Bisexual, Transgender" = NA,
                              "Hispanic origin and Race"=NA,"Education" = NA, 
                              "Marital status"=NA, "Household size" = NA,
                              "Presence of children under 18 years old"=NA, 
                              "Respondent or household member experienced loss of employment income" =NA, 
                              "Respondent currently employed"=NA, 
                              "Household income" = NA, "Used in the last 7 days to meet spending needs*" = NA, 
                              "Active duty military*" = NA, "Difficulty seeing" = NA, "Difficulty hearing" = NA,
                              "Difficulty remembering or concentrating" = NA, "Difficulty walking or climbing stairs" = NA) %>% 
  filter(is.na(demog)==FALSE)





data_2 <- data_2 %>% mutate(demog = replace(demog, demog == "Respondent or household member experienced loss of employment income in last 4 weeks",
                                            "Respondent or household member experienced loss of employment income")) %>% 
  mutate(demog = replace(demog, demog == "Respondent employed in the last 7 days",
                         "Respondent currently employed"))

#data_2 %>% filter(grepl(c("loss of employment", "employed"),demog, fixed=TRUE)) %>% View()

data_2 <- data_2 %>% mutate(Total = ifelse(demog=="Total",  TRUE, NA))

for(i in 1:length(data_2$demog)){
    if((data_2$demog %in% colnames(data_2))[i] == FALSE)
      data_2[i,j] = data_2$demog[i]
    if((data_2$demog %in% colnames(data_2))[i] == TRUE)
      j = match(data_2$demog[i], colnames(data_2))
}

data_2 <- data_2 %>% filter(demog %in% colnames(data_2)[-12] == FALSE)

data_2 <- data_2 %>% drop_na(demog)

data_final <- data_2[,-1]

rm(data, data_2, dt_2, i, j, w)

write.csv(data_final, "D:\\Data sets and Coding Files\\Household Pulse Surveys Healthcare Access\\HPS_HA_T2a_Wrang.csv", row.names = FALSE)
