# Libraries loaded
library(dplyr)
library (lubridate)

# Getting csv file onto R
dataset <- read.csv("C:/Users/Shwetha/Desktop/Crime_Data.csv", stringsAsFactors = FALSE)
View(dataset)


#Filtering out the columns I don't need and creating a sample of dataset
dataset$Report.Number <- NULL
dataset$Beat <- NULL
sample_dataset <- sample_n(dataset, 5000)
View(sample_dataset)

#Getting distinct values of the colunmn Crime.Subcategory
View(distinct)

##Identifying inciendence for each crime subcategory 
#BURGLARY-COMMERCIAL
burglary <- sample_dataset %>% 
  filter(Crime.Subcategory == "BURGLARY-COMMERCIAL") %>% 
  group_by(Neighborhood) %>% 
  tally() %>% 
  arrange(-n) %>% 
  top_n(10)
View(burglary)
#DUI
dui <- sample_dataset %>% 
  filter(Crime.Subcategory == "DUI") %>% 
  group_by(Neighborhood) %>% 
  tally() %>% 
  arrange(-n) %>% 
  top_n(10)
View(dui)
#THEFT-ALL OTHER
theft <- sample_dataset %>% 
  filter(Crime.Subcategory == "THEFT-ALL OTHER") %>% 
  group_by(Neighborhood) %>% 
  tally() %>% 
  arrange(-n) %>%
  top_n(10)

View(theft)
#AGGRAVATED ASSAULT
aa <- sample_dataset %>% 
  filter(Crime.Subcategory == "AGGRAVATED ASSAULT") %>% 
  group_by(Neighborhood) %>% 
  tally() %>% 
  arrange(-n) %>%
  top_n(10)

View(aa)
#THEFT-BUILDING
theft2 <- sample_dataset %>% 
  filter(Crime.Subcategory == "THEFT-BUILDING") %>% 
  group_by(Neighborhood) %>% 
  tally() %>% 
  arrange(-n) %>%
  top_n(10)

View(theft2)
#FAMILY OFFENSE-NONVIOLENT
fon <- sample_dataset %>% 
  filter(Crime.Subcategory == "FAMILY OFFENSE-NONVIOLENT") %>% 
  group_by(Neighborhood) %>% 
  tally() %>% 
  arrange(-n)
View(fon)
#MOTOR VEHICLE THEFT
mvt <- sample_dataset %>% 
  filter(Crime.Subcategory == "MOTOR VEHICLE THEFT") %>% 
  group_by(Neighborhood) %>% 
  tally() %>% 
  arrange(-n)
View(mvt)
#TRESPASS
trespass <- sample_dataset %>% 
  filter(Crime.Subcategory == "TRESPASS") %>% 
  group_by(Neighborhood) %>% 
  tally() %>% 
  arrange(-n)
View(trespass)
#THEFT-SHOPLIFT
theft3 <- sample_dataset %>% 
  filter(Crime.Subcategory == "THEFT-SHOPLIFT") %>% 
  group_by(Neighborhood) %>% 
  tally() %>% 
  arrange(-n)
View(theft3)
#BURGLARY-RESIDENTIAL-SECURE PARKING
burglary1 <- sample_dataset %>% 
  filter(Crime.Subcategory == "BURGLARY-RESIDENTIAL-SECURE PARKING") %>% 
  group_by(Neighborhood) %>% 
  tally() %>% 
  arrange(-n)
View(burglary1)
#HOMICIDE
homicide <- sample_dataset %>% 
  filter(Crime.Subcategory == "HOMICIDE") %>% 
  group_by(Neighborhood) %>% 
  tally() %>% 
  arrange(-n)
View(homicide)
#WEAPON
WEAPON <- sample_dataset %>% 
  filter(Crime.Subcategory == "WEAPON") %>% 
  group_by(Neighborhood) %>% 
  tally() %>% 
  arrange(-n)
View(WEAPON)
#ARSON
ARSON <- sample_dataset %>% 
  filter(Crime.Subcategory == "ARSON") %>% 
  group_by(Neighborhood) %>% 
  tally() %>% 
  arrange(-n)
View(ARSON)
#BURGLARY-COMMERCIAL-SECURE PARKING
bcsp <- sample_dataset %>% 
  filter(Crime.Subcategory == "BURGLARY-COMMERCIAL-SECURE PARKING") %>% 
  group_by(Neighborhood) %>% 
  tally() %>% 
  arrange(-n)
View(bcsp)
#PROSTITUTION
PROSTITUTION <- sample_dataset %>% 
  filter(Crime.Subcategory == "PROSTITUTION") %>% 
  group_by(Neighborhood) %>% 
  tally() %>% 
  arrange(-n)
View(PROSTITUTION)
#LIQUOR LAW VIOLATION
llv <- sample_dataset %>% 
  filter(Crime.Subcategory == "LIQUOR LAW VIOLATION") %>% 
  group_by(Neighborhood) %>% 
  tally() %>% 
  arrange(-n)
View(llv)
#LOITERING
LOITERING <- sample_dataset %>% 
  filter(Crime.Subcategory == "LOITERING") %>% 
  group_by(Neighborhood) %>% 
  tally() %>% 
  arrange(-n)
View(LOITERING)
#PORNOGRAPHY
PORNOGRAPHY <- sample_dataset %>% 
  filter(Crime.Subcategory == "PORNOGRAPHY") %>% 
  group_by(Neighborhood) %>% 
  tally() %>% 
  arrange(-n)
View(PORNOGRAPHY)
#DISORDERLY CONDUCT
DISORDERLY_CONDUCT <- sample_dataset %>% 
  filter(Crime.Subcategory == "DISORDERLY CONDUCT") %>% 
  group_by(Neighborhood) %>% 
  tally() %>% 
  arrange(-n)
View(DISORDERLY_CONDUCT)

# Student - Robbery 
student_robbery_street <- sample_dataset %>% 
                          filter(Crime.Subcategory == "ROBBERY-STREET") %>% 
                          group_by(Neighborhood) %>% 
                          tally() %>% 
                          arrange(-n)
View(student_robbery_street)

# Student - Rape
student_sex_offence <- sample_dataset %>% 
                          filter(Crime.Subcategory == "RAPE") %>% 
                          group_by(Neighborhood) %>% 
                          tally() %>% 
                          arrange(-n)
View(student_sex_offence)

# Student - Bicycle Theft 
student_bicycle_theft <- sample_dataset %>% 
                          filter(Crime.Subcategory == "THEFT-BICYCLE") %>% 
                          group_by(Neighborhood) %>% 
                          tally() %>% 
                          arrange(-n)
View(student_bicycle_theft)

# Family - Narcotics 
family_narcotics <- sample_dataset %>% 
  filter(Crime.Subcategory == "NARCOTIC") %>% 
  group_by(Neighborhood) %>% 
  tally() %>% 
  arrange(-n)
View(family_narcotics)

# Family - Sexual Offence - Other 
family_sex_offence <- sample_dataset %>% 
                        filter(Crime.Subcategory == "SEX OFFENSE-OTHER") %>% 
                        group_by(Neighborhood) %>% 
                        tally() %>% 
                        arrange(-n)
View(family_sex_offence)

# Family - Burglary 
family_residential <- sample_dataset %>% 
                  filter(Crime.Subcategory == "BURGLARY-RESIDENTIAL") %>% 
                  group_by(Neighborhood) %>% 
                  tally() %>% 
                  arrange(-n)
View(family_residential) 

# Elderly - Aggravated Assaults
elderly_assaults <- sample_dataset %>% 
                      filter(Crime.Subcategory == "AGGRAVATED ASSAULT-DV") %>% 
                      group_by(Neighborhood) %>% 
                      tally() %>% 
                      arrange(-n)
View(elderly_assaults)

# Elderly - Burglaries 
elderly_burglaries <- sample_dataset %>% 
                        filter(Crime.Subcategory == "BURGLARY-RESIDENTIAL") %>% 
                        group_by(Neighborhood) %>% 
                        tally() %>% 
                        arrange(-n)
View(elderly_burglaries)

# Elderly - Car Prowls
elderly_car_prowls <- sample_dataset %>% 
                        filter(Crime.Subcategory == "CAR PROWL") %>% 
                        group_by(Neighborhood) %>% 
                        tally() %>% 
                        arrange(-n)
View(elderly_car_prowls)

# Most crime 
total_crime <- sample_dataset %>% 
                group_by(Neighborhood) %>% 
                tally() %>% 
                arrange(-n)
View(total_crime)

# Top unsafe areas for students 
selected_crimes_students <- sample_dataset %>% c("RAPE","THEFT-BICYCLE","ROBBERY-STREET")
total_filter_students <- sample_dataset %>%
                  filter(Crime.Subcategory %in% selected_crimes_students) %>% 
                  group_by(Neighborhood) %>%
                  tally() %>%
                  arrange(-n)
View(total_filter_students)

# Top unsafe areas for families 
selected_crimes_family <- sample_dataset %>% c("NARCOTIC","SEX OFFENSE-OTHER","BURGLARY-RESIDENTIAL")
total_filter_family <- sample_dataset %>%
  filter(Crime.Subcategory %in% selected_crimes_family) %>% 
  group_by(Neighborhood) %>%
  tally() %>%
  arrange(-n)
View(total_filter_family)

# Top unsafe areas for elderly 
selected_crimes_elderly <- sample_dataset %>% c("AGGRAVATED ASSAULT-DV","BURGLARY-RESIDENTIAL","CAR PROWL")
total_filter_elderly <- sample_dataset %>%
  filter(Crime.Subcategory %in% selected_crimes_elderly) %>% 
  group_by(Neighborhood) %>%
  tally() %>%
  arrange(-n)
View(total_filter_elderly)
