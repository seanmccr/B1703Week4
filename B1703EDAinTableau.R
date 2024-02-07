# ----- B1703 Week 4 | Exploratory Data Analysis in Tableau | 07.02.2024 -----
# ----- 1. EDA in R -----
##### 1. Loading Libraries and Datasets #####

library(tidyverse)

Merged_data<-read.csv("/Users/seanmccrone/Desktop/MASTERS DEGREE/Course Material/B1703/Week 4/Practical 4&5 Files/P3_output1.csv")
Population<-read.csv("/Users/seanmccrone/Desktop/MASTERS DEGREE/Course Material/B1703/Week 4/Practical 4&5 Files/P3_output2.csv")

##### 2. Creating Dataframe #####
# Grouped by Name and the year they participate in
DatabyAthletePerYear <- Merged_data %>%
  group_by(Name, Year) %>%
  reframe(Sex, #reframe is the new version of summarize
          Age = mean(Age, na.rm = TRUE),
          Height = mean(Height, na.rm=TRUE),
          Weight = mean(Weight, na.rm=TRUE),
          NumberEvents = sum(!is.na(Event)),
          NumberMedals = sum(Medal=="Gold"|Medal=="Silver"|Medal=="Bronze"),
          Sport,
          Country)%>% 
  distinct(Name,Year, .keep_all=TRUE) %>% #This line removes the duplicate lines for each athlete
  rename(Athlete=Name) #Renaming this column in preperation for the next steps


##### 3. Another Dataframe Overview of performers over the years #####

DatabyAthleteAvg <- DatabyAthletePerYear %>%
  group_by(Athlete)%>%
  reframe(Year=toString(unique(Year)),
          NumberOlympics = sum(!is.na(Year)),
          Age = mean(Age, na.rm=TRUE),
          Height = mean(Height, na.rm=TRUE),
          Weight = mean(Weight, na.rm=TRUE),
          NumberMedals=sum(NumberMedals),
          Sport,
          Country)%>%
  distinct(Athlete, .keep_all=TRUE) #Keep 1 row per athlete

##### 4. Visualise Data using Overview Tables #####

library(flextable)
# First we will calculate the outcomes we want in a df called table
Table<-DatabyAthletePerYear %>%
  filter(Year==1988 | Year == 2016) %>% #filter for relevant years
  group_by(Year) %>% #group day by year
  reframe(Age_avg= mean(Age, na.rm=TRUE),
          Age_sd=sd(Age, na.rm=TRUE),
          Height_avg= mean(Height, na.rm=TRUE),
          Height_sd=sd(Height, na.rm=TRUE),
          weight_avg= mean(Weight, na.rm=TRUE),
          Weight_sd=sd(Weight, na.rm=TRUE),
          NumberEvents_avg= mean(NumberEvents, na.rm=TRUE),
          NumberEvents_sd=sd(NumberEvents, na.rm=TRUE),
          NumberMedals_sum=sum(NumberMedals,na.rm=TRUE),
          Athlete_freq=n_distinct(Athlete, na.rm=TRUE),
          Sex_freq=n_distinct(Sex, na.rm=TRUE),
          Sport_freq=n_distinct(Sport, na.rm=TRUE),
          Country_freq=n_distinct(Country, na.rm=TRUE))

TableFig <- flextable(Table) %>%
  colformat_double() %>%
  separate_header() %>%
  theme_vanilla() %>%
  align(align = "center", part = "all") %>%
  valign(valign = "center", part = "header") %>% 
  autofit()

TableFig <- add_header_row(TableFig, values = c("Athlete", "Olympics"), colwidths = c(9, 5))%>%
  bold(j=1,bold=TRUE,part="body") 
TableFig

#If we wanted to add significance or a footnote that's also possible by adding text below (this add significance * for age.  
TableFig <-footnote(TableFig, i=1:2, j=2,
  value=as_paragraph(c("Significant at p<0.05")),
  ref_symbols=c("*"),
  part="body",inline=TRUE)

##### 4.1. Seperating by sex #####
Table<-DatabyAthletePerYear %>%
  filter(Year==1988 | Year == 2016) %>% #filter for relevant years
  group_by(Year,Sex) %>% #group day by year
  reframe(Age_avg= mean(Age, na.rm=TRUE),
          Age_sd=sd(Age, na.rm=TRUE),
          Height_avg= mean(Height, na.rm=TRUE),
          Height_sd=sd(Height, na.rm=TRUE),
          Weight_avg= mean(Weight, na.rm=TRUE),
          Weight_sd=sd(Weight, na.rm=TRUE),
          NumberEvents_avg= mean(NumberEvents, na.rm=TRUE),
          NumberEvents_sd=sd(NumberEvents, na.rm=TRUE),
          NumberMedals_sum=sum(NumberMedals,na.rm=TRUE),
          Athlete_freq=n_distinct(Athlete, na.rm=TRUE),
          Sex_freq=n_distinct(Sex, na.rm=TRUE),
          Sport_freq=n_distinct(Sport, na.rm=TRUE),
          Country_freq=n_distinct(Country, na.rm=TRUE))

TableFig <- flextable(Table) %>%
  colformat_double() %>%
  separate_header() %>%
  theme_vanilla() %>%
  align(align = "center", part = "all") %>%
  valign(valign = "center", part = "header") %>% 
  autofit()

TableFig <- add_header_row(TableFig, values = c("Athlete", "Olympics"), colwidths = c(10, 5))%>%
  bold(j=1,bold=TRUE,part="body") 
TableFig

TableFig<- merge_v(TableFig,j="Year") #This merges the year cells for nicer formatting.
TableFig

# Grouping By Total Medal Numbers won per country per year

Team_events <- Merged_data %>%
  group_by(Year, Event, Country) %>%
  reframe(Golds=sum(Medal=="Gold"), Silver=sum(Medal=="Silver"), Bronze=sum(Medal=="Bronze"))%>%
  filter(Golds>1 | Silver>1 | Bronze >1)

# Five individual sports slipped through the net (Gymnastics in 1988 and 50 Freestyle in 2000 men and women, 1996 gymnastics individual all-round and 2008 athletics 100m Women for which 2 athletes same color medal). We will delete these before we assign a team code.  
Team_events<-Team_events[-c(94, 595, 957, 958,  1359),]

Team_events <- Team_events %>%  
  mutate(Team = 1)

##### 5. Merging Two Datasets #####

Merged_data2 <- merge(Merged_data,Team_events,by= c("Year", "Event", "Country"), all.x=TRUE)

# Assign 0 to non-team events
Merged_data2$Team[is.na(Merged_data2$Team)]<- 0

# Create a column that gives each athlete 1 medal for a team event (instead of the number for the team)
Merged_data2$TeamMedal <- ifelse(Merged_data2$Medal!="No Medal" & Merged_data2$Team==1, 1, NA)

## First we will calculate the total medals per country, year, event and medal type. I also filtered out the No Medals as we are not interested in that

TotalsPerCountry <- Merged_data2 %>%
  group_by(Country, Year, Event, Medal) %>%
  reframe(TotalMedalsIndividual= sum(((Medal=="Gold")|(Medal=="Silver")|(Medal=="Bronze")) & Team==0),
          TotalMedalsTeam= sum(TeamMedal,na.rm = TRUE),
          NOC=NOC
  )%>%
  distinct(Year, Event, Country, Medal, .keep_all=TRUE)

#Next we want to replace the total medal count for the team event to 1 for each medal color (we can only win one gold, one silver or one bronze)
TotalsPerCountry <- TotalsPerCountry %>%
  mutate(TotalMedalsTeam= ifelse(TotalMedalsTeam > 0,1,0)) %>%
  distinct(Year,Event,Country, Medal, .keep_all=TRUE)

# > 0 is part of the else if section of coding
# The ‘,1’ part is what happens if the conditions were met
# The ‘,0’ part is what happens if the conditions are not met.
# These values can be swapped in and out/changed depending on the context of the else if segment

#Now lets calculate the medals per country per year
TotalsPerCountryYear <- TotalsPerCountry %>%
  group_by(Year,NOC,Country) %>%
  reframe(TotalMedalsIndividual= sum(TotalMedalsIndividual, na.rm=TRUE),
          TotalMedalsTeam= sum(TotalMedalsTeam,na.rm = TRUE),
          TotalMedals=TotalMedalsIndividual+TotalMedalsTeam
  )

##### 6. Merging TotalsPerCountryYear and Population #####
#First we will rename 'Country Code' to 'NOC' to make the merge easier.
Population <- rename(Population, NOC=Country.Code)

CountryDatawithPop <- merge(TotalsPerCountryYear, Population,by= c("Year","NOC"), all.x=TRUE)

#delete notes and duplicate country columns
CountryDatawithPop <- CountryDatawithPop[,-7:-8]


##### 7. Checking correlation between medals won and population for olympics since 2000 #####

Corvars<-CountryDatawithPop %>% 
  select(1,3,6,7) %>% 
  filter(Year>=2000)

PearsTest <- cor.test(Corvars$TotalMedals, Corvars$Population, method="pearson") 
PearsTest

Scatter<-ggplot(Corvars, aes(TotalMedals, Population))+geom_point()
Scatter

##### 8. Saving Dataframes #####

save(DatabyAthleteAvg, DatabyAthletePerYear, TotalsPerCountryYear, CountryDatawithPop, file="Practical5.RData")
