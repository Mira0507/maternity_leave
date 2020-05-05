library(tidyverse)
library(ggplot2)
library(stringr)
h <- head
s <- summary  
g <- glimpse  
t <- tail

gen <- read.csv("Gender_StatsData.csv", stringsAsFactors = FALSE)
indicator <- unique(gen$Indicator.Name)
country <- unique(gen$ï..Country.Name)
gen1 <- gen %>% 
        rename(Country = ï..Country.Name)

mat_leave <- c("GDP per capita (Current US$)",
               "Length of paid maternity leave (days)",
               "Maternity leave benefits (% of wages paid)",
               "Mothers are guaranteed an equivalent position after maternity leave (1=yes; 0=no)")

gen2 <- gen1 %>%
        filter(Indicator.Name %in% mat_leave) %>%
        select(-c(Indicator.Code, X)) %>%
        gather(Year, value, -c(Country, Country.Code, Indicator.Name))

gen2$Year <- str_replace_all(gen2$Year, "X", "")

gen3 <- gen2[complete.cases(gen2), ]
