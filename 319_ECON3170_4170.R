#### Importing Packages ####
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(purrr)
library(zoo)
library(stringr)
library(magrittr)
library(splitstackshape)
library(here)
library(caret)
library(glmnet)
library(rpart.plot)
library(rgl)


#### Importing Data ####

# Daily Interest Rates
dir <- read_delim("OneDrive - Universitetet i Oslo/H21/ECON3170/Term Paper/Data/Daily_Policy_Rates.csv",";", 
                  escape_double = FALSE, trim_ws = TRUE)

# Keeping only the Sight Deposit Rate 
keyinterest <- dir %>%
  filter(TENOR %in% c("SD"))%>%
  select(TIME_PERIOD, OBS_VALUE)

# Daily Exchange Rates: NOKTWI
Exchange_NOK_TWI <- read_delim("OneDrive - Universitetet i Oslo/H21/ECON3170/Term Paper/Data/Exchange_NOK_TWI.csv",
                               ";", escape_double = TRUE, trim_ws = TRUE)

# Sum of Norwegian Maritime Transport : source Statistics Norway
Norwegian_Maritime_Transport <- read_delim("OneDrive - Universitetet i Oslo/H21/ECON3170/Term Paper/Data/Norwegian_Maritime_Transport.csv", 
                                           ";", escape_double = FALSE, trim_ws = TRUE, 
                                           skip = 2, locale = locale(encoding = "ISO-8859-1"))

# Finding the position of city names with norwegian characters to remove

#Finding position of names of names with Norwegian characters
names(Norwegian_Maritime_Transport) 

#changing them to English freindly charcters
oldcitynames = c(11, 23, 25, 26, 31, 34, 37)
newcitynames = c("Tonsberg", "Floro", "Maloy (Nordfjord)", "Aleseund", "Bronnoysund (Broennoey)", "Bodo", "Tromso")

Norwegian_Maritime_Transport <- Norwegian_Maritime_Transport %>% 
  rename_at(vars(all_of(oldcitynames)), ~newcitynames) %>% 
  mutate('Norway (whole country)' = rowSums(Filter(is.numeric, Norwegian_Maritime_Transport))) %>% 
  filter(!quarter %in% c("2021K1", "2021K2", "2021K3", "2021K4"))

NMT2 <- Norwegian_Maritime_Transport %>% 
  pivot_longer(cols = -c(commodity, flag, `loaded/unloaded`, `domestic/foreign`, quarter, contents), names_to = "Location", values_to = "Tonnes")









  




#### Wrangling Data ####

# Creating new Tibble from NMT2
shippingbyquarter <- NMT2 %>% 
  select(c(`loaded/unloaded`, `domestic/foreign`, quarter, Location, Tonnes, commodity)) %>% 
  filter(Location %in% c("Norway (whole country)"))

  
h <- as.data.frame(shippingbyquarter)

for (i in h$quarter){
  i = str_replace(h$quarter, "K", "Q")
  i = sub("\\s+$", "", gsub('(.{4})', '\\1 ', i))
} 

quarterStart <- as.Date(as.yearqtr(i, format = "%Y Q%q"))
quarterEnd <- as.Date(as.yearqtr(i, format = "%Y Q%q"), frac = 1)

shippingbyquarter2 <- shippingbyquarter %>% 
  add_column(`Date start` = quarterStart) %>% 
  add_column(`Date end` = quarterEnd)

Exchange_NOK_TWI <- Exchange_NOK_TWI %>% 
  mutate(quarter = as.yearqtr(TIME_PERIOD, format = "%Y-%m-%d")) %>% 
  rename(`Currency Value` = OBS_VALUE) %>% 
  rename(Dates = TIME_PERIOD)


foreignexport0 <- shippingbyquarter2 %>% 
  filter(`domestic/foreign` %in% c("Foreign"), `loaded/unloaded` %in% c("Loaded") ) %>% 
  select(c(`loaded/unloaded`, `domestic/foreign`, Tonnes, `Date start`, commodity)) %>% 
  group_by(`loaded/unloaded`, `domestic/foreign`, `Date start`, commodity) %>% 
  summarise(Tonnes = sum(Tonnes), .groups = 'drop') %>% 
  mutate(quarter = as.yearqtr(`Date start`, format = "%Y-%m-%d"))
         

ForeignExport <- keyinterest %>% 
  mutate(quarter = as.yearqtr(`TIME_PERIOD`, format = "%Y-%m-%d")) %>% 
  inner_join(foreignexport0, by='quarter') %>%
  inner_join(Exchange_NOK_TWI, by='quarter') %>% 
  select(c(TIME_PERIOD, OBS_VALUE, quarter, Tonnes, `Currency Value`, commodity))
  

keyinterest <- keyinterest %>% 
  mutate(quarter = as.yearqtr(TIME_PERIOD, format = "%Y-%m-%d"))



create_table <- function(cargo, route, df) {
  df %>% 
    filter(`domestic/foreign` %in% c(route), `loaded/unloaded` %in% c(cargo) ) %>% 
    select(c(`loaded/unloaded`, `domestic/foreign`, Tonnes, `Date start`)) %>% 
    group_by(`loaded/unloaded`, `domestic/foreign`, `Date start`) %>% 
    summarise(Tonnes = sum(Tonnes), .groups = 'drop') %>% 
    mutate(quarter = as.yearqtr(`Date start`, format = "%Y-%m-%d")) %>% 
    inner_join(keyinterest, by='quarter') %>%
    inner_join(Exchange_NOK_TWI, by='quarter') %>% 
    select(c(OBS_VALUE, quarter, Tonnes, `Currency Value`)) 
    
}

ForeignImport <- create_table("Unloaded", "Foreign", shippingbyquarter2)
DomesticBuy <- create_table("Unloaded", "Domestic", shippingbyquarter2)
DomesticSell <- create_table("Loaded", "Domestic", shippingbyquarter2)
  

  




#### Presenting Data ####

#creating a function to spare time and space in creating graphs
createGraph <- function(df){
  interestgraph <- ggplot(df, aes(x = quarter))+
    geom_line (aes(y = Tonnes/1000), color = "#295278")+
    geom_point(aes(y = Tonnes/1000), color = "#295278")+
    geom_line(aes(y = OBS_VALUE*1000), color = "#CB2583")+
    geom_point(aes(y = OBS_VALUE*1000), color = "#CB2583")+
    
    
    

    scale_y_continuous(
      name = "Deadweight tonnage in kilo tonnes (kt)",
      breaks = scales::pretty_breaks(n=16),
      sec.axis = sec_axis(~./1000, name="Interest Rates (%)", scales::pretty_breaks(n=16))
      
      )+
    ggthemes::theme_economist(
      base_family = "sans",
      horizontal = TRUE,
      dkpanel = TRUE
    )+
    ggthemes::scale_color_economist()+
    #facet_grid(. ~ am )+
    ggthemes::theme_economist(dkpanel=TRUE)+
    scale_x_yearqtr(name= "Quarter", format = '%Y Q%q', n= 24)+
    theme(
      axis.title.y = element_text( size=9, margin = margin(t= 0, r= 20, b= 0, l =  0), color = "#295278"),
      axis.title.y.right = element_text( size = 9, margin = margin(t=0, r=0, b=0, l=20), angle = -90, color = "#CB2583"),
      axis.title.x = element_text(size = 9, margin = margin(t=20, r=0, b=0, l=0)),
      axis.text.x = element_text( angle = 90, vjust = 0.5, hjust = 1.5, size = 7),
      axis.text = element_text(size = 7),
      #axis.text.y.right = element_text(size = 7, color = "#CB2583")
    )+
    
    
    ggtitle(paste0("Interest rates on", gsub("([A-Z])", " \\1", deparse(substitute(df)))), 
            paste0("quaterly from 2012 - 2021"))
    
  
  currencygraph <- ggplot(df, aes(x = quarter))+
    geom_line (aes(y = Tonnes/1000), color = "#295278")+
    geom_point(aes(y = Tonnes/1000), color = "#295278")+
    geom_line(aes(y = `Currency Value`*28), color = "#90353B")+
    
    scale_y_continuous(
      name = "Deadweight tonnage in kilo tonnes (kt)",
      breaks = scales::pretty_breaks(n=16),
      sec.axis = sec_axis(~./28, name="NOK to Trade Weight Exchange Rate (in NOK)", scales::pretty_breaks(n=16))
      
    )+
    ggthemes::theme_economist(
      base_family = "sans",
      horizontal = TRUE,
      dkpanel = TRUE
    )+
    ggthemes::scale_color_economist()+
    #facet_grid(. ~ am )+
    ggthemes::theme_economist(dkpanel=TRUE)+
    scale_x_yearqtr(name= "Quarter", format = '%Y Q%q', n= 24)+
    theme(
      axis.title.y = element_text( size=9, margin = margin(t= 0, r= 20, b= 0, l =  0), color = "#295278"),
      axis.title.y.right = element_text( size = 9, margin = margin(t=0, r=0, b=0, l=20), angle = -90, color = "#90353B"),
      axis.title.x = element_text(size = 9, margin = margin(t=20, r=0, b=0, l=0)),
      axis.text.x = element_text( angle = 90, vjust = 0.5, hjust = 1.5, size = 7),
      axis.text = element_text(size = 7),
      #axis.text.y.right = element_text(size = 7, color = "#CB2583")
    )+
    
    
    ggtitle(paste0("Currency Exchange Rates on", gsub("([A-Z])", " \\1", deparse(substitute(df)))), 
            paste0("quaterly from 2012 - 2021"))
    
    ggsave(filename = here("OneDrive - Universitetet i Oslo/H21/ECON3170/Term Paper/Outputs", paste0(deparse(substitute(df)), ".png")), gridExtra::arrangeGrob(interestgraph, currencygraph), width = 14, height = 14)
  
    
}

#loading the graphs from the function
createGraph(DomesticSell)
createGraph(DomesticBuy)
createGraph(ForeignExport)
createGraph(ForeignImport)

create2Graph <- function(df){
  ggplot(df, aes(x= OBS_VALUE, y = Tonnes, col = as.factor(commodity)))+
    geom_point()
}

create3Graph <- function(df){
  ggplot(df, aes(x= `Currency Value`, y = Tonnes, col = as.factor(commodity)))+
    geom_point()
}

create2Graph(ForeignExport)
create3Graph(ForeignExport)




#Finding unique values to create categories
unique(NMT2$commodity)


#Creating table for loaded (export)
loaded <- NMT2 %>% 
  select(commodity, `loaded/unloaded`, `domestic/foreign`, quarter, Location, Tonnes) %>% 
  filter(`loaded/unloaded` %in% c("Loaded")) %>% 
  select(commodity, `domestic/foreign`, quarter, Location, Tonnes) %>%
  mutate(type = case_when(commodity %in% c("Agricultural products, forestry products, fishing products", "Food products, beverages, tobaco and animal fodder",  "Other manufactured goods, grouped goods and other goods")  ~ "Consume", 
                          commodity %in% c("Coal, oil and chemicals and chemical products", "Metal ores, stone, sand, gravel, clay, salt, cement, lime, fertilizer, manufactured construction materials") ~ "Investment")
  ) %>% 
  select(type, quarter, Location, Tonnes) %>% 
  group_by( quarter, Location, type) %>% 
  summarise(Tonnes = sum(Tonnes), .groups = 'drop') %>% 
  subset(Location !="Norway - Unspecified, quarterly survey" & Location !="Norway (whole country)") %>% 
  mutate()


#changing the column to a date quarterly
loaded$quarter = str_replace_all(loaded$quarter, "K", "Q")
loaded$quarter = as.yearqtr(loaded$quarter)




#Creating a table for unloaded (import)
unloaded <- NMT2 %>% 
  select(commodity, `loaded/unloaded`, `domestic/foreign`, quarter, Location, Tonnes) %>% 
  filter(`loaded/unloaded` %in% c("Unloaded")) %>% 
  select(commodity, `domestic/foreign`, quarter, Location, Tonnes) %>%
  mutate(type = case_when(commodity %in% c("Agricultural products, forestry products, fishing products", "Food products, beverages, tobaco and animal fodder",  "Other manufactured goods, grouped goods and other goods")  ~ "Consume", 
                          commodity %in% c("Coal, oil and chemicals and chemical products", "Metal ores, stone, sand, gravel, clay, salt, cement, lime, fertilizer, manufactured construction materials") ~ "Investment")
  ) %>% 
  select(type, quarter, Location, Tonnes) %>% 
  group_by(quarter, Location, type) %>% 
  summarise(Tonnes = sum(Tonnes), .groups = 'drop') %>% 
  subset(Location !="Norway - Unspecified, quarterly survey" & Location !="Norway (whole country)")


#changing the column to a date quarterly
unloaded$quarter = str_replace_all(loaded$quarter, "K", "Q")
unloaded$quarter = as.yearqtr(unloaded$quarter)


# Create graph for export by type  when change in conversion rate and interest rate
graphByType <- function(df){
  interestGraph <- ggplot(df, aes(x=quarter))+
    geom_line(aes(y= log(Tonnes), group=Location, color=Location))+
    #geom_smooth(method = glm, formula = df$Tonnes ~ ForeignExport$OBS_VALUE)+
    facet_wrap(vars(type))+
    scale_x_yearqtr(name= "Quarter", format = '%Y Q%q', n=10)+ 
    ggthemes::theme_economist(
      base_family = "sans",
      horizontal = TRUE,
      dkpanel = TRUE
    )+
    scale_y_continuous(
      
      breaks = scales::pretty_breaks(n=16),
      sec.axis = sec_axis(~./10, name="Interest Rates (%)", scales::pretty_breaks(n=16))
      
    )+
    theme(
      axis.title.y = element_text( size=9, margin = margin(t= 0, r= 20, b= 0, l =  0), color = "#295278"),
      axis.title.y.right = element_text( size = 9, margin = margin(t=0, r=0, b=0, l=20), angle = -90, color = "#90353B"),
      axis.title.x = element_text(size = 9, margin = margin(t=20, r=0, b=0, l=0)),
      axis.text.x = element_text( angle = 90, vjust = 0.5, hjust = 1.5, size = 7),
      axis.text = element_text(size = 7),
      legend.text = element_text(size = 5),
      legend.key.size = unit(3, "mm")
      )+
    ggtitle("Interest on type of good")
  
  currencyGraph <- ggplot(df, aes(x=quarter))+
    geom_line(aes(y= log(Tonnes), group=Location, color=Location))+
    #geom_smooth(method = "glm", formula = y ~ x)+
    facet_wrap(vars(type))+
    scale_x_yearqtr(name= "Quarter", format = '%Y Q%q', n=10)+ 
    ggthemes::theme_economist(
      base_family = "sans",
      horizontal = TRUE,
      dkpanel = TRUE
    )+
    scale_y_continuous(
      breaks = scales::pretty_breaks(n=16),
      sec.axis = sec_axis(~.*10, name="NOK to Trade Weight Exchange Rate (in NOK)", scales::pretty_breaks(n=16))
      
    )+
    theme(
      axis.title.y = element_text( size=9, margin = margin(t= 0, r= 20, b= 0, l =  0), color = "#295278"),
      axis.title.y.right = element_text( size = 9, margin = margin(t=0, r=0, b=0, l=20), angle = -90, color = "#90353B"),
      axis.title.x = element_text(size = 9, margin = margin(t=20, r=0, b=0, l=0)),
      axis.text.x = element_text( angle = 90, vjust = 0.5, hjust = 1.5, size = 7),
      axis.text = element_text(size = 7),
      legend.text = element_text(size = 5),
      legend.key.size = unit(3, "mm")
    )+
    ggtitle("Currency on type of good")
  
  ggsave(filename = here("OneDrive - Universitetet i Oslo/H21/ECON3170/Term Paper/Outputs", paste0("Log of", deparse(substitute(df)), ".png")), gridExtra::arrangeGrob(interestGraph, currencyGraph), width = 14, height = 14)
  
}


#loading the graphs
graphByType(loaded)
graphByType(unloaded)


#### Regressions ####

# Linear regression for Foreign Export
reg1 = lm(Tonnes ~ OBS_VALUE, data = ForeignExport)
lm(Tonnes ~ `Currency Value`, data = ForeignExport)
lm(Tonnes ~ OBS_VALUE + `Currency Value`, data = ForeignExport)
#plot(1 and 2)
#summary("PUT CODES INSIDE HERE")


predict3d::ggPredict(reg1)

# Linear regression for Foreign Import
lm(Tonnes ~ OBS_VALUE, data = ForeignImport)
lm(Tonnes ~ `Currency Value`, data = ForeignImport)
lm(Tonnes ~ OBS_VALUE + `Currency Value`, data = ForeignImport)
#plot(1 and 2)
#summary("PUT CODES INSIDE HERE")
summary(lm(Tonnes ~ OBS_VALUE, data = ForeignExport))
summary(lm(Tonnes ~ OBS_VALUE, data = ForeignImport))
summary(lm(Tonnes ~ OBS_VALUE + `Currency Value`, data = ForeignImport))

fit <- rpart(Tonnes ~ OBS_VALUE + `Currency Value`,method="anova",data = ForeignExport) 
rpart.plot(fit,box.palette="GnBu",space = 0,     nn=TRUE, tweak = 1.2)
plotcp(fit)
pfit<- prune(fit, cp=1)


# Correlation 
cor(ForeignExport$OBS_VALUE, ForeignExport$`Currency Value`)
##means that these 2 variables are highly correlated meaning that when
##interest rates increase by 1% the currency depriciates (nearly same effect)


#### Dashboard ####

# DASHBOARD AS FILE. DASHBOARD ".RMD" FILE IN FOLDER
# DID NOT COME VERY FAR ON THE DASHBOARD BECAUSE OF TIME

















