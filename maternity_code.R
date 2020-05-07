library(tidyverse)
library(ggplot2)
library(stringr)
h <- head
s <- summary  
g <- glimpse  
t <- tail

gen <- read.csv("Gender_StatsData.csv", stringsAsFactors = FALSE)
gen1 <- gen %>% 
        rename(Country = ï..Country.Name)
indicator <- unique(gen1$Indicator.Name)
country <- unique(gen1$Country)

# Country Category
by_region <- c("World",
               "Arab World",
               "Central Europe and the Baltics",
               "East Asia & Pacific",
               "Euro area",
               "Europe & Central Asia",
               "European Union",
               "Latin America & Caribbean",
               "Middle East & North Africa",
               "South Asia",
               "Sub-Saharan Africa",
               "North America")

by_income <- c("World",
               "High income", 
               "Upper middle income",
               "Middle income",
               "Lower middle income",
               "Low income")

by_country <- country[47:length(country)]

# Indicator.Name category
topic <- c("GDP per capita (Current US$)",
           "Length of paid maternity leave (days)",
           "Maternity leave benefits (% of wages paid)",
           "Mothers are guaranteed an equivalent position after maternity leave (1=yes; 0=no)",
           "Labor force, female (% of total labor force)",
           "Labor force with advanced education, female (% of female working-age population with advanced education)", 
           "Labor force with basic education, female (% of female working-age population with basic education)",
           "Labor force with intermediate education, female (% of female working-age population with intermediate education)",
           "Unemployment with advanced education, female (% of female labor force with advanced education)",                                               
           "Unemployment with basic education, female (% of female labor force with basic education)",                                                     
           "Unemployment with intermediate education, female (% of female labor force with intermediate education)",                                       
           "Unemployment, female (% of female labor force) (modeled ILO estimate)",                                                                        
           "Unemployment, female (% of female labor force) (national estimate)")

# Primary data cleaning: filtering variables
gen2 <- gen1 %>%
        filter(Indicator.Name %in% topic) %>%
        select(-c(Indicator.Code, X)) %>%
        gather(Year, value, -c(Country, Country.Code, Indicator.Name))

gen2$Year <- str_replace_all(gen2$Year, "X", "")

gen3 <- gen2[complete.cases(gen2), ]

# Secondary data cleaning: filtering observations by country and indicator
gen4 <- gen3 %>%
        filter(Country %in% by_country,
               Indicator.Name %in% c("GDP per capita (Current US$)",
                                      "Labor force, female (% of total labor force)",
                                     "Labor force with advanced education, female (% of female working-age population with advanced education)", 
                                     "Labor force with basic education, female (% of female working-age population with basic education)",
                                     "Labor force with intermediate education, female (% of female working-age population with intermediate education)",
                                      "Length of paid maternity leave (days)",
                                      "Maternity leave benefits (% of wages paid)",
                                      "Unemployment with advanced education, female (% of female labor force with advanced education)",                                               
                                      "Unemployment with basic education, female (% of female labor force with basic education)",                                                     
                                      "Unemployment with intermediate education, female (% of female labor force with intermediate education)"))
gen5 <- spread(gen4, Indicator.Name, value)
names(gen5) <- c("Country",
                 "Country_Code", 
                 "Year",
                 "GDP_per_capita_US_dol", 
                 "Female_labor_force_w_advanced_education", 
                 "Female_labor_force_w_basic_education",
                 "Female_labor_force_w_intermediate_education",
                 "Female_labor_force_percent_of_total", 
                 "Length_of_paid_maternity_leave_days",
                 "Percent_wages_paid_during_maternity_leave",
                 "Female_unemployment_w_advanced_education",
                 "Female_unenployment_w_basic_education",
                 "Female_unemployment_w_intermediate_education")

# data cleaning for plotting GDP vs female labor force by Education level
gen6_1 <- gen5 %>%
        select(Country, 
               Year, 
               GDP_per_capita_US_dol, 
               Female_labor_force_w_advanced_education,
               Female_labor_force_w_basic_education, 
               Female_labor_force_w_intermediate_education, 
               Female_labor_force_percent_of_total)

gen6_2 <- gen6_1[complete.cases(gen6_1), ] %>%
        filter(Year == "2014") %>%
        gather(key = Education_Level,
               value = Female_Labor_Force, -c(Country, Year, 
                                              GDP_per_capita_US_dol, 
                                              Female_labor_force_percent_of_total)) %>%
        mutate(Education_Level = 
                       recode(Education_Level,
                              Female_labor_force_w_advanced_education = "Advanced",
                              Female_labor_force_w_basic_education = "Basic",
                              Female_labor_force_w_intermediate_education = "Intermediate")) %>%
        mutate(Education_Level = factor(Education_Level, 
                                        levels = c("Basic", 
                                                   "Intermediate",
                                                   "Advanced")))

# data cleaning for GDP vs Female labor force 
gen7_1 <- gen5 %>%
        select(Country, Year, GDP_per_capita_US_dol, Female_labor_force_percent_of_total) 
gen7_2 <- gen7_1[complete.cases(gen7_1), ] %>%
        filter(Year == "2014")
table(gen7_2$Year)

female_labor_force_US <- filter(gen7_2, Country == "United States")[, "Female_labor_force_percent_of_total"]
female_labor_force_kor <- filter(gen7_2, Country == "Korea, Rep.")[, "Female_labor_force_percent_of_total"]


# data cleaning for GDP vs maternity leave 
gen8_1 <- gen5 %>% 
        select(Country, Year, GDP_per_capita_US_dol, 
               Length_of_paid_maternity_leave_days, 
               Percent_wages_paid_during_maternity_leave)
gen8_2 <- gen8_1[complete.cases(gen8_1), ] %>%
        filter(Year == "2017")
table(gen8_2$Year)

length_of_mleave_kor <- filter(gen8_2, Country == "Korea, Rep.")[, "Length_of_paid_maternity_leave_days"]
benefits_of_mleave_kor <- filter(gen8_2, Country == "Korea, Rep.")[, "Percent_wages_paid_during_maternity_leave"]


# data cleaning for maternity leave vs female labor force 
gen9_1 <- gen5 %>% 
        select(Country, 
               Year, 
               Female_labor_force_percent_of_total,
               Length_of_paid_maternity_leave_days,
               Percent_wages_paid_during_maternity_leave)

gen9_2 <- gen9_1[complete.cases(gen9_1), ] %>%
        filter(Year == "2017") %>%
        rename(Percent_Wages_Paid = Percent_wages_paid_during_maternity_leave)
table(gen9_2$Year)



# plotting
gdp_vs_female_labor_by_edu_plot <- 
        ggplot(gen6_2, aes(x = GDP_per_capita_US_dol,
                           y = Female_Labor_Force,
                           color = Education_Level)) + 
        geom_jitter(alpha = 0.6, size = 2) +
        geom_smooth(se = FALSE) + 
        ggtitle("GDP vs Female Labor Force by Education Level") +
        xlab("GDP per Capita (US dollars)") +
        ylab("Labor Force (%) of Working Age Population \nfrom Each Education Level") + 
        theme(panel.background = element_blank(),
              axis.line.x = element_line("black"),
              axis.line.y = element_line("black"))

female_labor_force_by_edu <-
        ggplot(gen6_2, aes(x = Education_Level,
                   y = Female_Labor_Force,
                   fill = Education_Level)) + 
        geom_boxplot(alpha = 0.5) + 
        theme(panel.background = element_blank(),
              axis.line.x = element_line("black"),
              axis.line.y = element_line("black"),
              axis.text.x = element_text(size = 13, color = "black"),
              axis.title.x = element_blank()) + 
        ggtitle("Female Labor Force by Education Level") + 
        ylab("Labor Force (%) of Working Age Population \nfrom Each Education Level")

total_female_labor_force_vs_specific_female_labor_force_by_edu_plot <-
        ggplot(gen6_2, aes(x = Female_labor_force_percent_of_total,
                           y = Female_Labor_Force,
                           color = Education_Level)) + 
        geom_jitter(alpha = 0.6, size = 2) +
        geom_smooth(se = FALSE) + 
        ggtitle("Female Labor Force (% of Total) vs Female Labor Force (% of Female Labor Force) \n by Education Level") +
        xlab("Female Labor Force (% of Total)") +
        ylab("Labor Force (%) of Working Age Population \nfrom Each Education Level") + 
        theme(panel.background = element_blank(),
              axis.line.x = element_line("black"),
              axis.line.y = element_line("black"))


gdp_vs_female_labor_force_plot <- 
        ggplot(gen7_2, aes(x = GDP_per_capita_US_dol, y = Female_labor_force_percent_of_total)) + 
        geom_jitter(alpha = 0.3, size = 2) + 
        geom_smooth(se = FALSE, color = "red") + 
        ggtitle("Female Labor Force vs GDP") + 
        xlab("GDP per Capita (US dollars)") +
        ylab("Female Labor Force (% of Total)") + 
        theme(panel.background = element_blank(),
              axis.line.x = element_line("black"),
              axis.line.y = element_line("black"))

# female_labor_force_US = 46
# female_labor_force_kor = 42
distribution_of_female_labor_force_plot <-
        ggplot(gen7_2, aes(x = Female_labor_force_percent_of_total)) + 
        geom_density(fill = "purple", alpha = 0.3, color = "purple") + 
        theme(panel.background = element_blank(),
              axis.line.x = element_line("black"),
              axis.line.y = element_line("black")) + 
        xlim(c(0, 100)) + 
        ggtitle("Distribution of Female Labor Force (% of Total) in 180 Countries") + 
        xlab("Female Labor Force (% of Total)") + 
        ylab("Proportion in Total Countries") +
        geom_vline(xintercept = female_labor_force_US) + 
        geom_vline(xintercept = female_labor_force_kor, linetype = "dashed")
        

maternity_leave_vs_gdp <-
        ggplot(gen8_2, aes(x = GDP_per_capita_US_dol, y = Length_of_paid_maternity_leave_days)) +
        geom_jitter(alpha = 0.4, size = 2, color = "navy") + 
        geom_smooth(se = FALSE, color = "red") + 
        theme(panel.background = element_blank(),
              axis.line.x = element_line("black"),
              axis.line.y = element_line("black")) +
        xlab("GDP per Capita (US dollars)") + 
        ylab("Length of Paid Maternity Leave (Days)") + 
        ggtitle("Length of Paid Maternity Leave vs GDP")
        
 
length_of_maternity_leave_plot <- 
        ggplot(gen8_2, aes(x = Length_of_paid_maternity_leave_days)) + 
        geom_density(alpha = 0.2, fill = "navy", color = "navy") + 
        theme(panel.background = element_blank(),
              axis.line.x = element_line("black"),
              axis.line.y = element_line("black")) +
        scale_x_continuous(breaks = seq(0, 650, by = 50)) + 
        ggtitle("Length of Paid Maternity Leave") + 
        xlab("Length of Paid Maternity Leave (Days)") + 
        ylab("Proportion in Total Countries") +
        geom_vline(xintercept = length_of_mleave_kor, linetype = "dashed")
       
maternity_leave_wages_vs_gdp_plot <-
        ggplot(gen8_2, aes(x = GDP_per_capita_US_dol,
                   y = Percent_wages_paid_during_maternity_leave)) + 
        geom_jitter(alpha = 0.5, size = 2, color = "saddlebrown") + 
        geom_smooth(se = FALSE, color = "red") + 
        theme(panel.background = element_blank(),
              axis.line.x = element_line("black"),
              axis.line.y = element_line("black")) + 
        ggtitle("Maternity Leave Benefits vs GDP") + 
        xlab("GDP per Capita (US dollars)") + 
        ylab("Wages Paid during Maternity Leave (% of Total)")

maternity_leave_benefits_plot <- 
        ggplot(gen8_2, aes(x = Percent_wages_paid_during_maternity_leave)) + 
        geom_density(alpha = 0.2, fill = "saddlebrown", color = "saddlebrown") + 
        theme(panel.background = element_blank(),
              axis.line.x = element_line("black"),
              axis.line.y = element_line("black")) + 
        xlab("Wages Paid during Maternity Leave (% of Total)") +
        ylab("Proportion in Total Countries") + 
        ggtitle("Maternity Leave Benefits") + 
        geom_vline(xintercept = benefits_of_mleave_kor, linetype = "dashed")

maternity_leave_benefits_vs_length_of_maternity_leave_plot <-
        ggplot(gen8_2, aes(x = Length_of_paid_maternity_leave_days,
                           y = Percent_wages_paid_during_maternity_leave)) + 
        geom_jitter(alpha = 0.4, size = 2, color = "steelblue") + 
        geom_smooth(se = FALSE, color = "red") + 
        theme(panel.background = element_blank(),
              axis.line.x = element_line("black"),
              axis.line.y = element_line("black")) + 
        ggtitle("Maternity Leave Benefits vs Length of Paid Maternity Leave") +
        xlab("Length of Paid Maternity Leave (Days)") + 
        ylab("Wages Paid during Maternity Leave (% of Total)")

female_labor_force_vs_length_of_paid_maternity_leave <- 
        ggplot(gen9_2, aes(x = Length_of_paid_maternity_leave_days, 
                   y = Female_labor_force_percent_of_total,
                   col = Percent_Wages_Paid)) + 
        geom_jitter(alpha = 0.5,
                    size = 2) + 
        geom_smooth(se = FALSE, color = "red") +
        theme(panel.background = element_blank(),
              axis.line.x = element_line("black"),
              axis.line.y = element_line("black")) + 
        ggtitle("Relationship between Female Labor Force and \nLength of Paid Maternity Leave") +
        xlab("Length of Paid Maternity Leave (Days)") +
        ylab("Female Labor Force (% of Total)")

# data cleaning for creating a table        
table_maternity_leave <- gen8_2 %>%
        arrange(desc(Length_of_paid_maternity_leave_days)) %>%
        select(Country, 
               Length_of_paid_maternity_leave_days, 
               Percent_wages_paid_during_maternity_leave)

table_maternity_leave_over_200 <- table_maternity_leave[1:9, ] %>%
        rename(Length_of_Paid_Maternity_Leave = Length_of_paid_maternity_leave_days,
               Percent_Wages_Paid = Percent_wages_paid_during_maternity_leave)

                           