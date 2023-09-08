install.packages("ggmap")

library(readr)
library(tidyverse)
library(ggmap)
options(scipen =999)
#####Section1: loading datasets #####
population_data<- read.csv("C:\\Users\\91956\\Desktop\\R\\data\\population.csv",header = TRUE, skip = 4)
gdp_data<- read.csv("C:\\Users\\91956\\Desktop\\R\\data\\gdp.csv",header = TRUE, skip = 4)
kwh_data<- read.csv("C:\\Users\\91956\\Desktop\\R\\data\\rnew_kwh.csv",header = TRUE, skip = 4)
rnew_data<- read.csv("C:\\Users\\91956\\Desktop\\R\\data\\rnew.csv",header = TRUE, skip = 4)
#####Section2: Tidying the datasets######
str(rnew_data)
####Section2.1: Creating Renwable Energy Production Ratio Dataset####
rnew_data<- rnew_data %>%
  pivot_longer(c("X1960", "X1961", "X1962", "X1963", "X1964", "X1965",
                 "X1966", "X1967", "X1968", "X1969", "X1970", "X1971", "X1972", "X1973", "X1974", "X1975", "X1976", "X1977", "X1978", "X1979", 
                 "X1980", "X1981", "X1982", "X1983", "X1984", "X1985", "X1986", "X1987", "X1988", "X1989", "X1990", "X1991", "X1992", "X1993", 
                 "X1994", "X1995", "X1996", "X1997", "X1998", "X1999", "X2000", "X2001", "X2002", "X2003", "X2004", "X2005", "X2006", "X2007", 
                 "X2008", "X2009", "X2010", "X2011", "X2012", "X2013", "X2014", "X2015", "X2016", "X2017", "X2018", "X2019", "X2020", "X2021"), names_to = "year", values_to = "Energy From Renewable Sources in %")
rnew_data<-
  select(all_of(rnew_data),-X,-Indicator.Code, -Indicator.Name) 
rnew_data<-rnew_data %>% 
  mutate(year = as.numeric(gsub("X", "", year)))

rnew_data<-rnew_data %>%
  filter(!is.na(`Energy From Renewable Sources in %`))
view(rnew_data)

population_data<- population_data %>%
  pivot_longer(c("X1960", "X1961", "X1962", "X1963", "X1964", "X1965",
                 "X1966", "X1967", "X1968", "X1969", "X1970", "X1971", "X1972", "X1973", "X1974", "X1975", "X1976", "X1977", "X1978", "X1979", 
                 "X1980", "X1981", "X1982", "X1983", "X1984", "X1985", "X1986", "X1987", "X1988", "X1989", "X1990", "X1991", "X1992", "X1993", 
                 "X1994", "X1995", "X1996", "X1997", "X1998", "X1999", "X2000", "X2001", "X2002", "X2003", "X2004", "X2005", "X2006", "X2007", 
                 "X2008", "X2009", "X2010", "X2011", "X2012", "X2013", "X2014", "X2015", "X2016", "X2017", "X2018", "X2019", "X2020", "X2021"), names_to = "year", values_to = "Population")

population_data<-
  select(all_of(population_data),-X,-Indicator.Code, -Indicator.Name) 
population_data<-population_data %>% 
  mutate(year = as.numeric(gsub("X", "", year)))

view(population_data)
###Section2.3:Creating GDP dataset####
gdp_data<- gdp_data %>%
  pivot_longer(c("X1960", "X1961", "X1962", "X1963", "X1964", "X1965",
                 "X1966", "X1967", "X1968", "X1969", "X1970", "X1971", "X1972", "X1973", "X1974", "X1975", "X1976", "X1977", "X1978", "X1979", 
                 "X1980", "X1981", "X1982", "X1983", "X1984", "X1985", "X1986", "X1987", "X1988", "X1989", "X1990", "X1991", "X1992", "X1993", 
                 "X1994", "X1995", "X1996", "X1997", "X1998", "X1999", "X2000", "X2001", "X2002", "X2003", "X2004", "X2005", "X2006", "X2007", 
                 "X2008", "X2009", "X2010", "X2011", "X2012", "X2013", "X2014", "X2015", "X2016", "X2017", "X2018", "X2019", "X2020", "X2021"), names_to = "year", values_to = "GDP in US$")
gdp_data<-
  select(all_of(gdp_data),-X,-Indicator.Code, -Indicator.Name) 
gdp_data<-gdp_data %>% 
  mutate(year = as.numeric(gsub("X", "", year)))

gdp_data<-gdp_data %>%
  filter(!is.na(`GDP in US$`))
view(gdp_data)
####Section2.4: Creating Renwable Energy Production Dataset####
kwh_data<- kwh_data %>%
  pivot_longer(c("X1960", "X1961", "X1962", "X1963", "X1964", "X1965",
                 "X1966", "X1967", "X1968", "X1969", "X1970", "X1971", "X1972", "X1973", "X1974", "X1975", "X1976", "X1977", "X1978", "X1979", 
                 "X1980", "X1981", "X1982", "X1983", "X1984", "X1985", "X1986", "X1987", "X1988", "X1989", "X1990", "X1991", "X1992", "X1993", 
                 "X1994", "X1995", "X1996", "X1997", "X1998", "X1999", "X2000", "X2001", "X2002", "X2003", "X2004", "X2005", "X2006", "X2007", 
                 "X2008", "X2009", "X2010", "X2011", "X2012", "X2013", "X2014", "X2015", "X2016", "X2017", "X2018", "X2019", "X2020", "X2021"), names_to = "year", values_to = "Renewable energy in kwh")
kwh_data<-
  select(all_of(kwh_data),-X,-Indicator.Code, -Indicator.Name) 
kwh_data<-kwh_data %>% 
  mutate(year = as.numeric(gsub("X", "", year)))

kwh_data<-kwh_data %>%
  filter(!is.na(`Renewable energy in kwh`))
view(kwh_data)
######Scetion3: Mearging all the clean datasets to a new tidy dataframe#########
dataframe <- inner_join(dataframe,kwh_data, by = c("Country.Name","Country.Code","year"))
dataframe <- inner_join(dataframe,population_data, by = c("Country.Name","Country.Code","year"))
dataframe <- inner_join(dataframe,gdp_data, by = c("Country.Name","Country.Code","year"))
view(dataframe)
dataframe<- dataframe %>%         # tidy dataframe
  rename(
    Country_Name = Country.Name
  )
dataframe<- dataframe %>%
  mutate(Country_Name = str_replace(Country_Name,"United States", "USA")) %>%
  mutate(Country_Name = str_replace(Country_Name,"Russian Federation", "Russia")) %>%
  mutate(Country_Name = str_replace(Country_Name,"United Kingdom", "UK"))

worldData=map_data("world")  #to take the world data
str(worldData) #to see the structure of the world data
view(worldData)

dataframe <-dataframe %>%
  filter(Country_Name %in% worldData$region)

#####Section4: Data Visualization######

###Section4.1: Renewable energy production throughout the world Map###
worldmap <- dataframe %>%
  filter(year == 2015) 
write.csv(worldmap, "C:\\Users\\91956\\Downloads\\package\\People.csv", row.names=FALSE)
view(worldmap)
worldmap <-worldmap[c("Country_Name","Renewable energy in kwh")]
view(worldmap)
worldmap <- worldmap %>%
  mutate(across('Country_Name',str_replace, 'United States','USA')) %>%
  mutate(across('Country_Name',str_replace, 'Russian Federation','Russia')) %>%
  mutate(across('Country_Name',str_replace, 'United Kingdom','UK'))

worldmap<- worldmap %>% 
  rename(
    Renewable_energy = `Renewable energy in kwh`)
  




tot <- worldData[worldmap$Country_Name %in% worldmap$Country_Name, ]
tot$value <- worldmap$Renewable_energy[match(tot$region, worldmap$Country_Name)]
view(tot)

ggplot(tot, aes(x=long,y=lat, group = group, fill = value)) +
  geom_polygon(colour =  "white") +
  scale_fill_continuous(low = "grey", high = "darkgreen", guide = "colorbar") +
  theme_bw() +
  labs(fill = "Renewable_energy", title = "Renewable Energy prodction in the year 2015", x="", y="") +
  scale_y_continuous(breaks = c()) +
  scale_x_continuous(breaks = c()) +
  theme(panel.border = element_blank())

######Section4.2: World GDP VS Renewable energy production from 1990 to 2015#####

con <- dataframe %>%
  group_by(year) %>%
  summarise(TotalEnergyProduction = sum(`Renewable energy in kwh`), gdp = sum(`GDP in US$`))
a = 100

ggplot(con, aes(x = as.integer(year))) +
   geom_line(aes(y = `TotalEnergyProduction`, colour = "TotalEnergyProduction", size =.4)) + geom_line(aes(y = `gdp`/a ,colour = "gdp",size = .4 )) +   guides(color = guide_legend(title = "Trend")) +xlab("year") +
  ggtitle("Rising Trend of GDP and Renewable energy production of the World")
  
  #####Section4.3: Countires with highest Renewable Energy production Ratio line graph ######
  
  top_con <- dataframe %>%
    arrange(year,Country_Name,`Energy From Renewable Sources in %`) %>%
    group_by(year) %>%
    mutate(rank = rank(desc(`Energy From Renewable Sources in %`))) %>%
    filter(rank <=5)
  
  view(top_con)
  
  ggplot(top_con, aes(x = (year), y = `Energy From Renewable Sources in %`,colour = Country_Name)) +
  geom_line(size =2) +
  ggtitle("Countrys with highest percentage of Renewable energy production wrt Total Energy Production")
  
  
  ##########Section4.4: GDp VS Renewable Energy Scatter plot#####
  top_con %>%
    filter(year == 2015) %>%
    ggplot( aes(x = (`GDP in US$`), y = `Energy From Renewable Sources in %`,colour = Country_Name)) +
    geom_point( size = 3) +
    ggtitle("Top 5 countries having highest GDP VS Renwable engergy production Ratio")
  
  
  #####Section4.5 Correlation between Population and Renewable Energy Production#####
  
  Pop_rnew <- dataframe %>%
    filter(year == 2015) %>%
    mutate(outlier_countries = ifelse(`Renewable energy in kwh`>50000000000,Country_Name,NA))
  Pop_rnew %>%
    ggplot(aes(x=Population, y= `Renewable energy in kwh`, label = outlier_countries)) + 
    geom_point(color = "maroon",size = 2) + 
    geom_text(size = 4, hjust = 0.5,vjust = 1) +
    ggtitle("Correlation between Population and Renewable energy production")

 