rm(list=ls())
setwd("/Users/sejalgupta/gith/dsci_final")
data = read.csv("WDI_CSV/WDICSV.csv") #world development

#libraries
library(ggplot2)

#used in scatterplot vis (#2)
library(tidyr) 
#library(ggimage)


#only want to keep countries after the "world" observation (everything above is regions)
last_world_row <- max(which(data$Country.Name == "World")) #72373
df <- data[(72374):nrow(data), ]
#df_fil = df[complete.cases(df) | rowSums(is.na(df)) <= 60, ] #just to easily look for what indicators countries have

#VISUALIZATION 1
#examine life expectancy globally (create a map)
life_expectancy = subset(df, Indicator.Code == "SP.DYN.LE00.IN") #217 observations = 217 countries, data for most

#full countries shapefile
library(sf)
world_df = st_read("WB_countries_Admin0_10m") #want to read the entire folder
head(world_df) #use column WB_NAME to join

world_df$WB_NAME[world_df$WB_NAME == "United States of America"] <- "United States" #so they merge properly

world_df$Country.Name = world_df$WB_NAME #to join with life_expectancy$Country.Name

map_df = merge(world_df, life_expectancy, by='Country.Name')

map.theme<-theme(axis.line=element_blank(),axis.text.x=element_blank(),
                 axis.text.y=element_blank(),axis.ticks=element_blank(),
                 axis.title.x=element_blank(),
                 axis.title.y=element_blank(),
                 panel.background=element_blank(),panel.border=element_blank(),
                 panel.grid.major=element_blank(),
                 panel.grid.minor=element_blank(),plot.background=element_blank())

ggplot(data=map_df) +
  geom_sf(aes(fill=X2021)) +
  map.theme+
  labs(title="something abt findings", subtitle = "something abt 2021?", fill="Average Life Expectancy" )+
  scale_fill_distiller(palette="YlOrRd", direction =1)

#the map works, major countries are there
#i just want to play with the theme code to get it to look better



#VISUALIZATION 2 scatterplot 
#gdp per capita and life expectancy per country in 2021
library("ggimage")
countries_of_interest <- c("India", "China", "United States", 
                           "Indonesia", "Pakistan", "Nigeria", "Brazil",
                           "Bangladesh", "Japan", "Mexico") #10 highly populated countries
scatter <- df[df$Country.Name %in% countries_of_interest, ] #only want specific countries
table(scatter$Country.Name) 
scatter = scatter[scatter$Indicator.Code %in% c("SP.DYN.LE00.IN", "NY.GDP.PCAP.KD"), ] #only want specific indicators

scatter = scatter %>%
  pivot_wider(
    id_cols = Country.Name,
    names_from = Indicator.Code,
    values_from = X2021)

scatter$img[scatter$Country.Name == "India"] = "/Users/sejalgupta/gith/dsci_final/flags//india.png"
scatter$img[scatter$Country.Name == "China"] = "/Users/sejalgupta/gith/dsci_final/flags//china.png"
scatter$img[scatter$Country.Name == "United States"] = "/Users/sejalgupta/gith/dsci_final/flags//us.png"
scatter$img[scatter$Country.Name == "Indonesia"] = "/Users/sejalgupta/gith/dsci_final/flags//indonesia.png"
scatter$img[scatter$Country.Name == "Pakistan"] = "/Users/sejalgupta/gith/dsci_final/flags//pakistan.png"
scatter$img[scatter$Country.Name == "Nigeria"] = "/Users/sejalgupta/gith/dsci_final/flags//nigeria.png"
scatter$img[scatter$Country.Name == "Brazil"] = "/Users/sejalgupta/gith/dsci_final/flags//brazil.png"
scatter$img[scatter$Country.Name == "Bangladesh"] = "/Users/sejalgupta/gith/dsci_final/flags/bangladesh.png"
scatter$img[scatter$Country.Name == "Japan"] = "/Users/sejalgupta/gith/dsci_final/flags//japan.png"
scatter$img[scatter$Country.Name == "Mexico"] = "/Users/sejalgupta/gith/dsci_final/flags//mexico.png"


(ggplot(data=scatter)+ #scatterplot, instead of points it's images
  geom_image(aes(x=NY.GDP.PCAP.KD, y=SP.DYN.LE00.IN, image=img), size=.08)+ #figure out if there's a way to make the flag have a border
    #if i can't put a border on the flags, i'll need to color the background so u can see the flags
  theme_minimal())+  #run this with the plot screen not minimized so that it doesn't shrink
  labs(x= "GDP per Capita (US Dollars)", y="Life Expectancy (Years)", 
       title="something abt 2021 life expectancy corr w wealth",
       subtitle = "something abt top ten most populated countries")
  
#fix coloring and theme to match whatever i end up with

#VISUALIZATION 3
#substantive effects plot - how does gdp per capita predict life expectancy? controlling for ______ 
#look thru indicators, figure out what u wanna predict ("SP.DYN.LE00.IN" for life expectancy)
#main independent variable (gdp per capita)
#https://datacatalog.worldbank.org/search/dataset/0037712/World-Development-Indicators
#this will require a lot of looking through the data
#remove countries that have NAs for most of the dependent variables 

#looking for potential regressors
sum(rowSums(is.na(subset(df, Indicator.Code == "SP.DYN.LE00.IN") )) > 30) #9 countries, life expectancy
sum(rowSums(is.na(subset(df, Indicator.Code == "NY.GDP.PCAP.KD") )) > 30) #48 countries, gdp per capita


indic_list = c("SP.DYN.LE00.IN",
               "NY.GDP.PCAP.KD",
               "SH.XPD.CHEX.GD.ZS",
               "SE.XPD.TOTL.GD.ZS",
               "SP.POP.TOTL",
               "EN.POP.DNST",
               "SH.ALC.PCAP.LI",
               "SH.PRV.SMOK",
               "SH.MED.PHYS.ZS",
               "SH.STA.ODFC.ZS",
               "SH.H2O.BASW.ZS",
               "SH.STA.BASS.ZS",
               "PV.EST")

#subsetting the df to make it easier to see what years are missing data

v3 = df[df$Indicator.Code %in% indic_list, ]

library(tidyverse)

v3_long = v3 %>%
  gather(key = "Year", value = "Value", -Country.Name, -Country.Code, -Indicator.Code, -Indicator.Name) %>%
  select(-Country.Code, -Indicator.Name) %>%
  spread(key = "Indicator.Code", value = "Value")


names = c(
  "life_expectancy",  # Life expectancy
  "gdp_per_capita",  # GDP per capita
  "health_expenditure",  # Current health expenditure (% of GDP)
  "educ_expenditure",  # Government expenditure on education, total (% of GDP)
  "total_population",  # Population, total
  "population_density",  # Population density (people per sq. km of land area)
  "alc_consumption",  # Total alcohol consumption per capita (liters of pure alcohol, projected estimates, 15+ years of age)
  "tobacco",  # Prevalence of current tobacco use (% of adults)
  "physicians",  # Physicians (per 1,000 people)
  "open_defect",  # People practicing open defecation (% of population)
  "drinking_water",  # People using at least basic drinking water services (% of population)
  "sanitation",  # People using at least basic sanitation services (% of population)
  "political_stability"  # Political Stability and Absence of Violence/Terrorism: Estimate
)

v3_dataset = v3_long %>%
  rename(
    Country = Country.Name,
    !!!setNames(indic_list, names) #this works properly, we just do this to clean the names
  )
v3_dataset$Year <- as.numeric(sub("X", "", v3_dataset$Year))

colSums(is.na(v3_dataset))

#now need to write the regression
no_fe = lm(life_expectancy ~ gdp_per_capita + health_expenditure + 
             educ_expenditure + total_population +
             population_density + alc_consumption +
             tobacco + physicians + open_defect +
             drinking_water + sanitation + political_stability, 
           data = v3_dataset)
summary(no_fe) #352 degrees of freedom, (13306 observations deleted due to missingness)

#adding time fixed effects! will not do country fixed effects
model = plm(formula = life_expectancy ~ gdp_per_capita 
             + health_expenditure + educ_expenditure 
             + population_density   #including total_population makes the model bug
             + drinking_water + open_defect + sanitation
             + alc_consumption + tobacco 
             + physicians + political_stability, 
             data = v3_dataset, index = c("Year"), model = "within") #is it better to run with the full version?
summary(model) #n = 5


v3_dataset_cropped <- v3_dataset[complete.cases(v3_dataset$gdp_per_capita, v3_dataset$life_expectancy), ]
#colSums(is.na(v3_dataset_cropped)) #removed the observations w/o life expectancy or gdp per capita
model_cropped = plm(formula = life_expectancy ~ gdp_per_capita 
                            + health_expenditure + educ_expenditure 
                            + population_density  
                            + drinking_water + open_defect + sanitation
                            + alc_consumption + tobacco 
                            + physicians + political_stability, 
                            data = v3_dataset_cropped, index = c("Year"), model = "within") 
summary(model_cropped)

#now make the substantive effects plot

#VISUALIZATION 4
#to do some comparisons of world development characteristics btwn countries]]
#can do this for multiple data points in 2015 across three countries ?
#can do a by time thing
us_only <- subset(df, Country.Name == "United States")
us_only_fil <- us_only[complete.cases(us_only) | rowSums(is.na(us_only)) <= 40, ]
india_only <- subset(df, Country.Name == "India")
india_only_fil <- india_only[complete.cases(india_only) | rowSums(is.na(india_only)) <= 40, ]
china_only <- subset(df, Country.Name == "China")
china_only_fil <- china_only[complete.cases(china_only) | rowSums(is.na(china_only)) <= 40, ]
#do a query to see what indicators are common btwn or just pick indicators im interested in seeing
#like life expectancy

#VISUALIZATION IDEA
#another us, india, china comparison?
#VISUALIZATION IDEA
#SE.ENR.TERT.FM.ZS - gender parity index of ppl enrolled in higher educ (Tertiary) over time in US
#make like the one screenshotted in google docs















#old work
#df = read.csv("CollegeAdmissions_Data.csv") #codebook https://opportunityinsights.org/wp-content/uploads/2023/07/CollegeAdmissions_Codebook.pdf
#df = read.csv("mrc_table10.csv")
#https://opportunityinsights.org/data/?geographic_level=0&topic=0&paper_id=0#resource-listing
#https://opportunityinsights.org/data/ 
#library(readxl)
#dta<-read_excel("Detailed enrollments.xlsx")
#df = read.csv("data/MERGED1999_00_PP.csv")
#df = read.csv("EdStats_CSV/EdStatsData.csv") #https://datacatalog.worldbank.org/search/dataset/0038480 i need to find the csv
#df = read_excel("pwt1001.xlsx")