rm(list=ls())
setwd("/Users/sejalgupta/gith/dsci_final")
data = read.csv("WDI_CSV/WDICSV.csv") #world development'
#only want to keep countries after the "world" observation (everything above is regions)
last_world_row <- max(which(data$Country.Name == "World")) #72373
df <- data[(72374):nrow(data), ] 
#df_fil = df[complete.cases(df) | rowSums(is.na(df)) <= 60, ] #just to easily look for what indicators countries have

#libraries
library(ggplot2)
#used in scatterplot vis
library(tidyr) 
library("ggimage")
#used in the regression
library(tidyverse)
library(plm)
library(ggeffects)
library(dagitty)
library(ggdag)
library(stargazer)
#used in the map
library(sf)
#used in the animation 
library(gganimate)
library(gifski)


#VISUALIZATION 1
#examine life expectancy globally (create a map)
life_expectancy = subset(df, Indicator.Code == "SP.DYN.LE00.IN") #217 observations = 217 countries, data for most

#full countries shapefile
#library(sf) #i might move this back here bc i think the order of the packages makes a difference
world_df = st_read("WB_countries_Admin0_10m") #want to read the entire folder
head(world_df) #use column WB_NAME to join

world_df$WB_NAME[world_df$WB_NAME == "United States of America"] <- "United States" #so they merge properly
#unfortunately no democratic republic of congo in the data

world_df$Country.Name = world_df$WB_NAME #to join with life_expectancy$Country.Name

map_df = merge(world_df, life_expectancy, by='Country.Name')

map.theme<-theme(axis.line=element_blank(), axis.text.x=element_blank(),
                 axis.text.y=element_blank(), axis.ticks=element_blank(),
                 axis.title.x=element_blank(),
                 axis.title.y=element_blank(),
                 panel.background=element_blank(),panel.border=element_blank(),
                 panel.grid.major=element_blank(),
                 panel.grid.minor=element_blank(),plot.background=element_blank(),
                 legend.title=element_text(size = 8), #to make legend title smaller than default (11)
                 plot.title = element_text(hjust = 0.5)) #to center the title over the map
  #https://ggplot2.tidyverse.org/reference/theme.html 
  #https://ggplot2.tidyverse.org/reference/element.html can use these to make my plots pretty!
  #http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/ color palletes
  #i've decided that changing the fonts is too much

ggplot(data=map_df) +
  geom_sf(aes(fill=X2021)) +
  map.theme+
  labs(title="Life Expectancy in 2021", fill="Average Life \n Expectancy") +
  scale_fill_distiller(palette="YlGn", direction = 1) #change palette depending on website

#the map works, major countries are there
#i just want to play with the theme code to get it to look better

#https://loading.io/color/feature/YlGnBu-8/


#VISUALIZATION 2 bar graph
  #horizontal, countries in order from highest to least life expectancy in 2021, color coded by continent (if thats possible)
  #uses the df made in visualization 1 bc that has continent 

#only do countries of top 30 population
#topcountries <- map_df %>%
#  filter(rank(desc(POP_EST))<=30)

map_df$CONTINENT[map_df$CONTINENT == "Seven seas (open ocean)"] <- "Seven Seas" #for cleanliness

bar.theme<-theme(plot.background=element_blank(),
                     plot.title = element_text(hjust = 0.5),
                     plot.subtitle = element_text(hjust = 0.5)) #to center the title over the map

subregion <- st_drop_geometry(map_df) %>%
  select(Country.Name, SUBREGION, X2021) %>%
  group_by(SUBREGION) %>%
  summarize(life_expectancy = mean(X2021, na.rm = TRUE))


ggplot(data=subregion, mapping=aes(x=reorder(SUBREGION, -life_expectancy), y=life_expectancy, fill=life_expectancy))+
  geom_bar(stat="identity")+
  coord_flip()+
  labs(y= "Life Expectancy (Years)",
       x= "",
       fill="Continent",
       title= "Life Expectancy in 2021: By Region") +
  bar.theme + 
  scale_fill_distiller(palette="YlGn", direction = 1) +
  guides(fill = FALSE) #get rid of legend

continent <- st_drop_geometry(map_df) %>%
  select(Country.Name, CONTINENT, X2021) %>%
  group_by(CONTINENT) %>%
  summarize(life_expectancy = mean(X2021, na.rm = TRUE))


ggplot(data=continent, mapping=aes(x=reorder(CONTINENT, -life_expectancy), y=life_expectancy, fill=life_expectancy))+
  geom_bar(stat="identity")+
  coord_flip()+
  labs(y= "Life Expectancy (Years)",
       x= "",
       fill="Continent",
       title= "Life Expectancy in 2021: By Continent") +
  bar.theme + 
  scale_fill_distiller(palette="YlGn", direction = 1) +
  guides(fill = FALSE) #get rid of legend


# cont_codes = c("Seven Seas" = "#edf8b1",
#                "Africa" = "#c7e9b4",
#                "Asia" = "#7fcdbb",
#                "Europe" = "#41b6c4", 
#                "North America" = "#1d91c0", 
#                "South America" = "#225ea8", 
#                "Oceania" = "#0c2c84")

# this makes for some not very helpful bar graphs
# top20 <- map_df %>%
#   filter(!is.na(X2021) & X2021 > 0) %>%
#   #arrange(POP_EST) %>%
#   filter(rank(desc(X2021)) <=20)
# 
# low20 <- map_df %>%
#   filter(!is.na(X2021) & X2021 > 0) %>%
#   #arrange(POP_EST) %>%
#   filter(rank(X2021) <= 20)
# 
# ggplot(data=low20, mapping=aes(x=reorder(Country.Name, -X2021), y=X2021, fill=CONTINENT))+
#   geom_bar(stat="identity")+
#   coord_flip()+
#   labs(y= "Life Expectancy (Years)", 
#        x= "",
#        fill="Continent",
#        title= "Life Expectancy in 2021",
#        subtitle="Countries with Lowest Average Life Expectancies")+
#   bar.theme +
#  scale_fill_manual(values=cont_codes)
# 
# ggplot(data=top20, mapping=aes(x=reorder(Country.Name, -X2021), y=X2021, fill=CONTINENT))+
#   geom_bar(stat="identity")+
#   coord_flip()+
#   labs(y= "Life Expectancy (Years)", 
#        x= "",
#        fill="Continent",
#        title= "Life Expectancy in 2021",
#        subtitle="Countries with Highest Average Life Expectancies")+
#   bar.theme +
#   scale_fill_manual(values=cont_codes)




#VISUALIZATION 3 scatterplot 
#gdp per capita and life expectancy per country in 2021
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

scatter.theme<-theme(panel.background = element_rect(fill = "white", #change depending on website theme
                                                       colour = "white", linetype = "solid"),
                    panel.grid.major = element_line(size = .2, colour = "grey"),
                    panel.grid.minor = element_line(size = .2, colour = "grey"),
                 panel.border=element_blank(),
                 plot.background=element_blank(),
                 plot.title = element_text(hjust = 0.5),
                 plot.subtitle = element_text(hjust = 0.5)) #to center the title over the map

(ggplot(data=scatter)+ #scatterplot, instead of points it's images
  geom_image(aes(x=NY.GDP.PCAP.KD, y=SP.DYN.LE00.IN, image=img), size=.13)+
  scatter.theme +  #run this with the plot screen not minimized so that it doesn't shrink
  labs(x= "GDP per Capita (US Dollars)", y="Life Expectancy (Years)", 
       title="No Obvious Correlation Between Wealth and Life Expectancy",
       subtitle = "2021 Data from Ten Most Populated Countries"))
  
#fix coloring and theme to match whatever i end up with

#VISUALIZATION 4: india, china, us time series
#us_only <- subset(df, Country.Name == "United States")
#india_only <- subset(df, Country.Name == "India")
#china_only <- subset(df, Country.Name == "China")
#us_only_fil <- us_only[complete.cases(us_only) | rowSums(is.na(us_only)) <= 40, ]
#india_only_fil <- india_only[complete.cases(india_only) | rowSums(is.na(india_only)) <= 40, ]
#china_only_fil <- china_only[complete.cases(china_only) | rowSums(is.na(china_only)) <= 40, ]
time = df[df$Country.Name %in% c("India", "China", "United States"), ] 
time = time[time$Indicator.Code %in% c("SP.DYN.LE00.IN","NY.GDP.PCAP.KD"), ]
time = time %>% #same code as i did in visualization 4
  gather(key = "Year", value = "Value", -Country.Name, -Country.Code, -Indicator.Code, -Indicator.Name) %>%
  select(-Country.Code, -Indicator.Name) %>%
  spread(key = "Indicator.Code", value = "Value")
time$Year <- as.numeric(sub("X", "", time$Year))

country_colors <- c("United States" = "#41ab5d", "India" = "#41b6c4", "China" = "#0c2c84")

gdp_over_time <- ggplot(time, aes(x = Year, y = NY.GDP.PCAP.KD, color = Country.Name)) +
  geom_point() +
  #geom_line() +
  labs(title = "GDP Per Capita Over Time",
       x = "Year",
       y = "GDP (US Dollars)",
       color = "Country") +
  scatter.theme +
  scale_color_manual(values = country_colors)
gdp_over_time
gdp_anim = gdp_over_time+transition_states(Year, wrap = FALSE)+shadow_mark()
animate(gdp_anim, renderer=gifski_renderer())
anim_save("gdp_over_time.gif",animation=gdp_anim, renderer=gifski_renderer())

life_over_time <- ggplot(time, aes(x = Year, y = SP.DYN.LE00.IN, color = Country.Name)) +
  geom_point() +
  labs(title = "Life Expectancy Over Time",
       x = "Year",
       y = "Life Expectancy (Years)",
       color = "Country") +
  scatter.theme +
  scale_color_manual(values = country_colors)
life_anim = life_over_time+transition_states(Year, wrap = FALSE)+shadow_mark()
animate(life_anim, renderer=gifski_renderer())
anim_save("life_over_time.gif",animation=life_anim, renderer=gifski_renderer())

#VISUALIZATION 5
#substantive effects plot - how does gdp per capita predict life expectancy? controlling for ______ 
#look thru indicators, figure out what u wanna predict ("SP.DYN.LE00.IN" for life expectancy)
#main independent variable (gdp per capita)
#https://datacatalog.worldbank.org/search/dataset/0037712/World-Development-Indicators
#this will require a lot of looking through the data
#remove countries that have NAs for most of the dependent variables 
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

#these are the indicators we would potentially use for the regression
v4 = df[df$Indicator.Code %in% indic_list, ] #this should be v4 since its the last visualization but i am silly and moved code around

v4_long = v4 %>%
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

v4_dataset = v4_long %>%
  rename(Country = Country.Name, !!!setNames(indic_list, names)) #this works properly, we just do this to clean the names
v4_dataset$Year <- as.numeric(sub("X", "", v4_dataset$Year))

colSums(is.na(v4_dataset))

#i deleted the code that shows this, but i checked for correlation btwn variables 
  #(which is why i got rid of total population since i have population density)

#regression without time fixed effects
no_fe = lm(life_expectancy ~ gdp_per_capita + health_expenditure + 
         educ_expenditure + total_population +
          population_density + alc_consumption +
          tobacco + physicians + open_defect +
          drinking_water + sanitation + political_stability,
          data = v4_dataset)
summary(no_fe) #352 degrees of freedom, (13306 observations deleted due to missingness)

#time fixed effects (column year)
model = lm(life_expectancy ~ gdp_per_capita 
                    + health_expenditure + educ_expenditure 
                    + population_density  
                    + drinking_water + open_defect + sanitation
                    + alc_consumption + tobacco 
                    + physicians + political_stability + factor(Year), #doing factor(Year) makes year a fixed effect
                    data = v4_dataset)
summary(model) #349 degrees of freedom 
  #note to self: model automatically 

#export regression tables
stargazer(no_fe, model, type = "html", out = "regression_results.html")

substantive.theme<-theme(panel.background = element_rect(fill = "white", #change depending on website theme
                                                         colour = "white", linetype = "solid"),
                         panel.grid.major = element_line(size = .2, colour = "grey"),
                         panel.grid.minor = element_line(size = .2, colour = "grey"),
                     axis.line=element_blank(),
                     plot.margin = unit(c(1,1,1,1),"cm"), 
                     panel.border = element_blank(),
                     plot.background = element_blank(),
                     plot.title = element_text(hjust = 0.5),
                     plot.subtitle = element_text(hjust = 0.5)) #to center the title over the map

#now make the substantive effects plot
plot(ggpredict(model, "gdp_per_capita"), colors = "#41ab5d")+
  ggtitle("Higher GDP (Statistically) Leads to a Longer Life!")+
  substantive.theme +
  labs(y= "Life Expectancy", x = "GDP Per Capita")

dag = dagify(life_expectancy ~ gdp_per_capita 
             + health_expenditure + educ_expenditure 
             + population_density  
             + drinking_water + open_defect + sanitation
             + alc_consumption + tobacco 
             + physicians + political_stability,
            labels = c("life_expectancy" = "life expectancy",
                       "gdp_per_capita" = "GDP per capita",
                       "health_expenditure" = "government health expenditure",
                       "educ_expenditure" = "government education expenditure",
                       "drinking_water" = "drinking water",
                       "open_defect" = "open defectation",
                       "sanitation" = "sanitation",
                       "alc_consumption" = "alcohol consumption",
                       "tobacco" = "tobacco",
                       "physicians" = "physicians",
                       "political_stability" = "political stability"))
ggdag(dag, text=FALSE, use_labels="label") + theme_dag_blank() 
  # + ggtitle("Determinants of Life Expectancy") i don't know if a title is super neccessary












