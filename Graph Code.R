library(lubridate)
library(knitr)
library(ggplot2)
library(zoo)
library(dplyr)
library(odbc)

con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "LAPTOP-7HFLMLPF\\GAURAV",
                 Database = "Crime_dataset_final_09122019",
                 Trusted_Connection = "YES")

#Graph1

df1=dbGetQuery(con,"select cf.DATEKEY_FACT,cd.PRIMARY_TYPE,dd.case_year as Year from CRIME_FACT cf ,
		  CRIMETYPE_DIM cd, date_dim dd
		  where cf.CRIMTYPETYPE_ID_FACT= cd.IUCR_CODE_ID
		  and cf.DATEKEY_FACT = dd.datekey")

names(df1)=c("Date","Primary_Type","Year") ##Renaming the coulumn

df1$Count <- 1 #creating new column count with value 1
df1$Date <- as.Date(df1$Date, format="%m/%d/%Y") #Extract date from Given time

#### Grouping the Primary_Type Column as per the crime category

df1$Type_grouped[df1$Primary_Type == "THEFT" | df1$Primary_Type == "MOTOR VEHICLE THEFT" ] <- "Theft-Stolen"
df1$Type_grouped[df1$Primary_Type == "BATTERY"] <- "Batery"
df1$Type_grouped[df1$Primary_Type == "CRIMINAL DAMAGE"] <- "Criminal damage"
df1$Type_grouped[df1$Primary_Type == "NARCOTICS" | df1$Primary_Type == "OTHER NARCOTIC VIOLATION"] <- "Narcotics-Drugs"
df1$Type_grouped[df1$Primary_Type == "ASSAULT"] <- "Assault"
df1$Type_grouped[df1$Primary_Type == "BURGLARY"] <- "Burglary"
df1$Type_grouped[df1$Primary_Type == "ROBBERY" ] <- "Robery"
df1$Type_grouped[df1$Primary_Type == "ARSON"  | df1$Primary_Type == "CRIMINAL TRESPASS" |df1$Primary_Type == "CONCEALED CARRY LICENSE VIOLATION"|
                   df1$Primary_Type == "GAMBLINGS" | df1$Primary_Type == "HUMAN TRAFFICKING" |df1$Type == "KIDNAPPING" |
                   df1$Primary_Type == "INTERFERENCE WITH PUBLIC OFFICER" | df1$Primary_Type == "INTIMIDATION" | df1$Type == "LIQUOR LAW VIOLATION" | 
                   df1$Primary_Type == "NON-CRIMINAL" | df1$Primary_Type == "NON - CRIMINAL" | df1$Primary_Type == "OBSCENITY" | df1$Primary_Type == "OFFENSE INVOLVING CHILDREN"| 
                   df1$Primary_Type == "PROSTITUTION"| df1$Primary_Type == "PUBLIC INDECENCY"| df1$Primary_Type == "PUBLIC PEACE VIOLATION"|
                   df1$Primary_Type == "STALKING"| df1$Primary_Type == "WEAPONS VIOLATION"| df1$Primary_Type == "HOMICIDE"| df1$Primary_Type == "CRIM SEXUAL ASSAULT" |
                   df1$Primary_Type == "OTHER OFFENSE" | df1$Primary_Type == "SEX OFFENSE"| df1$Primary_Type == "DECEPTIVE PRACTICE" ] <- "Others"

df1 <- df1[!df1$Year == 2016|2015,] #inflated month

# Data  exploration with R

df_aggr <- aggregate(Count ~ Year, data = df1, FUN = sum)  ### Aggregating Objects as per Years


### In which years crime evolved over time in the city of Chicago?  

###Figure1 Plotting total crimes as per year
ggplot(df_aggr, aes(x=Year, y= Count)) + geom_line(colour = "steelblue") +  theme_minimal() +
  geom_point(colour = "steelblue")  + theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) 

###Figure2 

df_aggr2 <- aggregate(Count ~ Type_grouped + Year, data = df1, FUN = sum)
# Plot the graph
ggplot(data=df_aggr2, aes(x=Year, y=Count, group = Type_grouped, colour = Type_grouped)) +
  geom_line() + geom_point() + theme_minimal() + theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank())+ theme(legend.title=element_blank())



### As per time of day do  crime occurs most?

df2=dbGetQuery(con,"select dd.DATEKEY,cd.PRIMARY_TYPE from DATE_DIM dd , CRIMETYPE_DIM cd , CRIME_FACT cf
where cf.DATEKEY_FACT = dd.DATEKEY
and cd.IUCR_CODE_ID = cf.CRIMTYPETYPE_ID_FACT")

names(df2)=c("Date","Primary_Type")
df2$Count=1

df2$Hour <- substring(df2$Date, 12,13) #creating new column as Hour and extracting the Hour field from column


#### Grouping the Primary_Type Column as per the crime category

df2$Type_grouped[df2$Primary_Type == "THEFT" | df2$Primary_Type == "MOTOR VEHICLE THEFT" ] <- "Theft"
df2$Type_grouped[df2$Primary_Type == "BATTERY"] <- "Batery"
df2$Type_grouped[df2$Primary_Type == "CRIMINAL DAMAGE"] <- "Criminal damage"
df2$Type_grouped[df2$Primary_Type == "NARCOTICS" | df2$Primary_Type == "OTHER NARCOTIC VIOLATION"] <- "Narcotics"
df2$Type_grouped[df2$Primary_Type == "ASSAULT"] <- "Assault"
df2$Type_grouped[df2$Primary_Type == "BURGLARY"] <- "Burglary"
df2$Type_grouped[df2$Primary_Type == "ROBBERY" ] <- "Robery"
df2$Type_grouped[df2$Primary_Type == "ARSON" | df2$Primary_Type == "CONCEALED CARRY LICENSE VIOLATION" | df2$Primary_Type == "CRIMINAL TRESPASS" | 
                   df2$Primary_Type == "INTERFERENCE WITH PUBLIC OFFICER" |df2$Primary_Type == "GAMBLINGS" | df2$Primary_Type == "HUMAN TRAFFICKING" |
                   df2$Primary_Type == "INTIMIDATION" | df2$Type == "KIDNAPPING" | df2$Type == "LIQUOR LAW VIOLATION" | df2$Primary_Type == "NON-CRIMINAL" |
                   df2$Primary_Type == "NON - CRIMINAL" | df2$Primary_Type == "OBSCENITY" | df2$Primary_Type == "OFFENSE INVOLVING CHILDREN"| df2$Primary_Type == "PROSTITUTION"|
                   df2$Primary_Type == "PUBLIC INDECENCY"| df2$Primary_Type == "PUBLIC PEACE VIOLATION"| df2$Primary_Type == "STALKING"| df2$Primary_Type == "WEAPONS VIOLATION"|
                   df2$Primary_Type == "HOMICIDE"| df2$Primary_Type == "CRIM SEXUAL ASSAULT" | df2$Primary_Type == "SEX OFFENSE"| df2$Primary_Type == "DECEPTIVE PRACTICE" |
                   df2$Primary_Type == "OTHER OFFENSE"] <- "Others"


#Fig3 Number of Crimes Vs Hours(24)
ggplot(df2, aes(x=Hour))+geom_bar(stat="Count", width=0.8, fill = "steelblue")+ theme(axis.text.x = element_text(angle = 0, hjust = 1)) + 
  labs(x = "Hour", y = "Number of crimes") + theme_minimal()
  #theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank())


#Figure 4 Heat Map Number of crimes per hour


df_aggr3 <- aggregate(Count ~ Type_grouped + Hour, data = dd, FUN = sum)
# Plot graph
p1 <- ggplot(data = df_aggr3, aes(x = Hour, y = Type_grouped)) + geom_tile(aes(fill = Count), color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") 
p1 + theme_minimal()+ theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 6),
        axis.text.y = element_text(size= 8),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) 

#############
###################
#############
############
##############

  ### In which locations of the city is crime more likely to happen?
  
  df3=dbGetQuery(con,"select ad.LOC_DESC ,cd.PRIMARY_TYPE  from CRIMETYPE_DIM cd , CRIME_FACT cf , ADDRESS_DIM ad
where cd.IUCR_CODE_ID = cf.CRIMTYPETYPE_ID_FACT
and ad.ADDRESSID=cf.ADDRESSID_FACT")
 names(df3)=c("Location_Description","Primary_Type") 
  df3$Count=1
  names(df3)
  df3$Type_grouped[df3$Primary_Type == "THEFT" | df3$Primary_Type == "MOTOR VEHICLE THEFT" ] <- "Theft-Stolen"
  df3$Type_grouped[df3$Primary_Type == "BATTERY"] <- "Batery"
  df3$Type_grouped[df3$Primary_Type == "CRIMINAL DAMAGE"] <- "Criminal damage"
  df3$Type_grouped[df3$Primary_Type == "NARCOTICS" | df3$Primary_Type == "OTHER NARCOTIC VIOLATION"] <- "Narcotics-Drugs"
  df3$Type_grouped[df3$Primary_Type == "ASSAULT"] <- "Assault"
  df3$Type_grouped[df3$Primary_Type == "BURGLARY"] <- "Burglary"
  df3$Type_grouped[df3$Primary_Type == "ROBBERY" ] <- "Robery"
  df3$Type_grouped[df3$Primary_Type == "ARSON"  | df3$Primary_Type == "CRIMINAL TRESPASS" |df3$Primary_Type == "CONCEALED CARRY LICENSE VIOLATION"|
                     df3$Primary_Type == "GAMBLINGS" | df3$Primary_Type == "HUMAN TRAFFICKING" |df3$Type == "KIDNAPPING" |
                     df3$Primary_Type == "INTERFERENCE WITH PUBLIC OFFICER" | df3$Primary_Type == "INTIMIDATION" | df3$Type == "LIQUOR LAW VIOLATION" | 
                     df3$Primary_Type == "NON-CRIMINAL" | df3$Primary_Type == "NON - CRIMINAL" | df3$Primary_Type == "OBSCENITY" | df3$Primary_Type == "OFFENSE INVOLVING CHILDREN"| 
                     df3$Primary_Type == "PROSTITUTION"| df3$Primary_Type == "PUBLIC INDECENCY"| df3$Primary_Type == "PUBLIC PEACE VIOLATION"|
                     df3$Primary_Type == "STALKING"| df3$Primary_Type == "WEAPONS VIOLATION"| df3$Primary_Type == "HOMICIDE"| df3$Primary_Type == "CRIM SEXUAL ASSAULT" |
                     df3$Primary_Type == "OTHER OFFENSE" | df3$Primary_Type == "SEX OFFENSE"| df3$Primary_Type == "DECEPTIVE PRACTICE" ] <- "Others"
  
  
  
  df3$Location_grouped[df3$Location_Description == "\"SCHOOL- PRIVATE- BUILDING\"" | df3$Location_Description == "\"SCHOOL- PUBLIC- BUILDING\"" | 
                         df3$Location_Description == "\"SCHOOL- PUBLIC- GROUNDS\"" | df3$Location_Description ==  "\"SCHOOL- PRIVATE- GROUNDS\""  |
                         df3$Location_Description == "COLLEGE/UNIVERSITY GROUNDS" | df3$Location_Description == "COLLEGE/UNIVERSITY RESIDENCE HALL"| 
                         df3$Location_Description == "SCHOOL YARD"]<- "School/university"
  df3$Location_grouped[df3$Location_Description == "" | df3$Location_Description == "ABANDONED BUILDING"| df3$Location_Description == "AIRCRAFT" |
                         df3$Location_Description == "ANIMAL HOSPITAL" | df3$Location_Description == "ATHLETIC CLUB" | df3$Location_Description == "AUTO" | 
                         df3$Location_Description == "BASEMENT"  | df3$Location_Description == "BOAT/WATERCRAFT" | df3$Location_Description == "CHA GROUNDS" |
                         df3$Location_Description == "CHA HALLWAY"  | df3$Location_Description == "CHA HALLWAY/STAIRWELL/ELEVATOR" | df3$Location_Description == "CHURCH" |
                         df3$Location_Description == "CHURCH/SYNAGOGUE/PLACE OF WORSHIP"| df3$Location_Description == "COIN OPERATED MACHINE"| 
                         df3$Location_Description == "CONSTRUCTION SITE"|
                         df3$Location_Description == "OTHER"| df3$Location_Description == "CHURCH"| df3$Location_Description == "OTHER RAILROAD PROP / TRAIN DEPOT" |
                         df3$Location_Description =="SEWER" | df3$Location_Description =="STAIRWELL"| df3$Location_Description == "VACANT LOT" |
                         df3$Location_Description =="VACANT LOT/LAND"| df3$Location_Description == "VESTIBULE" | df3$Location_Description =="WOODED AREA" |
                         df3$Location_Description == "CTA STATION" | df3$Location_Description == "CTA BUS STOP" | df3$Location_Description == "AIRPORT BUILDING NON-TERMINAL - NON-SECURE AREA" |
                         df3$Location_Description == "CTA TRACKS - RIGHT OF WAY" | df3$Location_Description == "AIRPORT BUILDING NON-TERMINAL - SECURE AREA" | df3$Location_Description == "AIRPORT EXTERIOR - NON-SECURE AREA" | df3$Location_Description == "AIRPORT EXTERIOR - SECURE AREA" | df3$Location_Description == "AIRPORT PARKING LOT" | df3$Location_Description ==  "AIRPORT TERMINAL LOWER LEVEL - NON-SECURE AREA" | df3$Location_Description == "AIRPORT TERMINAL LOWER LEVEL - SECURE AREA" | df3$Location_Description == "AIRPORT TERMINAL MEZZANINE - NON-SECURE AREA"| df3$Location_Description == "AIRPORT TERMINAL UPPER LEVEL - NON-SECURE AREA"| df3$Location_Description == "AIRPORT TERMINAL UPPER LEVEL - SECURE AREA"| df3$Location_Description == "AIRPORT TRANSPORTATION SYSTEM (ATS)"| df3$Location_Description == "AIRPORT VENDING ESTABLISHMENT" | df3$Location_Description == "AIRPORT/AIRCRAFT" | df3$Location_Description == "COMMERCIAL / BUSINESS OFFICE"| df3$Location_Description == "FACTORY" |
                         df3$Location_Description == "DAY CARE CENTER" | df3$Location_Description =="FACTORY/MANUFACTURING BUILDING" | 
                         df3$Location_Description =="FEDERAL BUILDING"| df3$Location_Description == "FIRE STATION"| df3$Location_Description == "GOVERNMENT BUILDING/PROPERTY"|
                         df3$Location_Description == "HOSPITAL"| df3$Location_Description == "HOSPITAL BUILDING/GROUNDS"| df3$Location_Description == "JAIL / LOCK-UP FACILITY"|
                         df3$Location_Description == "LIBRARY"| df3$Location_Description == "MOVIE HOUSE/THEATER"  | df3$Location_Description =="NURSING HOME/RETIREMENT HOME"|
                         df3$Location_Description == "POOL ROOM" | df3$Location_Description =="SPORTS ARENA/STADIUM"| df3$Location_Description == "WAREHOUSE" |
                         df3$Location_Description == "BANK"| df3$Location_Description == "CREDIT UNION"| df3$Location_Description == "CURRENCY EXCHANGE"|
                         df3$Location_Description == "SAVINGS AND LOAN"]<- "Others"
  df3$Location_grouped[df3$Location_Description == "ALLEY" | df3$Location_Description == "BOWLING ALLEY" | df3$Location_Description == "CHA BREEZEWAY" |
                         df3$Location_Description =="HALLWAY"]<- "Alley" 
  df3$Location_grouped[df3$Location_Description == "APARTMENT"| df3$Location_Description == "CHA APARTMENT"]<- "Apartment"
  df3$Location_grouped[df3$Location_Description == "APPLIANCE STORE" | df3$Location_Description == "BARBERSHOP" | df3$Location_Description == "CAR WASH" | 
                         df3$Location_Description == "CLEANING STORE" | df3$Location_Description ==  "CONVENIENCE STORE" | df3$Location_Description =="DEPARTMENT STORE" | 
                         df3$Location_Description =="DRUG STORE"| df3$Location_Description == "GAS STATION"| df3$Location_Description == "GAS STATION DRIVE/PROP." | 
                         df3$Location_Description == "GARAGE/AUTO REPAIR" |df3$Location_Description =="GROCERY FOOD STORE"| df3$Location_Description == "MEDICAL/DENTAL OFFICE" | 
                         df3$Location_Description =="NEWSSTAND"| df3$Location_Description == "OFFICE"| df3$Location_Description == "PAWN SHOP" | df3$Location_Description =="RETAIL STORE"| 
                         df3$Location_Description == "SMALL RETAIL STORE"]<- "Store/small business"
  df3$Location_grouped[df3$Location_Description == "ATM (AUTOMATIC TELLER MACHINE)" | df3$Location_Description == "BRIDGE"| df3$Location_Description == "DRIVEWAY"| 
                         df3$Location_Description == "GANGWAY"| df3$Location_Description == "HIGHWAY/EXPRESSWAY"| df3$Location_Description == "LAKEFRONT/WATERFRONT/RIVERBANK"| 
                         df3$Location_Description == "SIDEWALK" | df3$Location_Description == "STREET"]<- "Street"
  df3$Location_grouped[df3$Location_Description == "BAR OR TAVERN"| df3$Location_Description == "HOTEL"| df3$Location_Description == "HOTEL/MOTEL"|
                         df3$Location_Description == "RESTAURANT"| df3$Location_Description == "TAVERN"| df3$Location_Description == "TAVERN/LIQUOR STORE"]<- "Restaurant/bar/hotel"
  df3$Location_grouped[df3$Location_Description == "CHA PARKING LOT" | df3$Location_Description =="PARK PROPERTY" | df3$Location_Description =="PARKING LOT"|
                         df3$Location_Description == "PARKING LOT/GARAGE(NON.RESID.)" | df3$Location_Description =="POLICE FACILITY/VEH PARKING LOT"]<- "Parking lot"
  df3$Location_grouped[df3$Location_Description == "TRAILER" | df3$Location_Description == "TRUCK" | df3$Location_Description == "VEHICLE - DELIVERY TRUCK"| 
                         df3$Location_Description ==  "VEHICLE - OTHER RIDE SERVICE"| df3$Location_Description ==  "VEHICLE NON-COMMERCIAL" | df3$Location_Description == "VEHICLE-COMMERCIAL" | df3$Location_Description =="DELIVERY TRUCK" |df3$Location_Description == "CTA TRAIN"| df3$Location_Description == "CTA BUS" | df3$Location_Description =="TAXICAB"  | df3$Location_Description == "OTHER COMMERCIAL TRANSPORTATION"]<- "Vehicle" 
  df3$Location_grouped[df3$Location_Description == "CTA GARAGE / OTHER PROPERTY" | df3$Location_Description =="GARAGE" |df3$Location_Description =="DRIVEWAY - RESIDENTIAL" | df3$Location_Description =="HOUSE" | df3$Location.Description == "PORCH" | df3$Location.Description =="RESIDENCE" | df3$Location.Description == "RESIDENCE PORCH/HALLWAY"| df3$Location.Description == "RESIDENCE-GARAGE"| df3$Location.Description == "RESIDENTIAL YARD (FRONT/BACK)"| df3$Location.Description == "YARD"]<- "Residence"
  
##Figure 5 Location Vs Number of Crimes
df_aggr4<- aggregate(Count ~ Location_grouped, data = df3, FUN = sum) #Aggregateing
# Order values
df_aggr4$Location_grouped <- factor(df_aggr4$Location_grouped, levels = df_aggr4$Location_grouped[order(-df_aggr4$Count)])
# Plot the graph 
ggplot(df_aggr4, aes(x = Location_grouped, y = Count)) + theme_minimal() + geom_bar(stat="identity", width=0.7, fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(x = "Location", y = "Number of crimes")

###Figure 6 


df_aggr5 <- aggregate(Count ~  Type_grouped + Location_grouped, data = df3, FUN = sum)
# Plot the graph
p2 <- ggplot(data = df_aggr5, aes(x = Location_grouped, y = Type_grouped)) + geom_tile(aes(fill = Count), color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") 
p2+ theme_minimal()+ theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 6),
        axis.text.y = element_text(size= 8),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) 

  #########
#########
#######	### Which districts are more potentially dangerous?
df4=dbGetQuery(con,"select cd.PRIMARY_TYPE crime_type ,ad.DISTRICT   from
   CRIMETYPE_DIM cd,
   ADDRESS_DIM ad ,
   CRIME_FACT cf
where cd.IUCR_CODE_ID = cf.CRIMTYPETYPE_ID_FACT
and ad.ADDRESSID = cf.ADDRESSID_FACT")
df4$Count=1
names(df4)=c("Primary_Type","District","Count")

df4$District<- as.factor(df4$District)

dd_sub <- subset(df4, District!="21" & District!="31")
# Create aggregated object
df_aggr6 <- aggregate(Count ~ District, data = dd_sub, FUN = sum)

###Figure 7 dist vs count


df_aggr6$District <- factor(df_aggr6$District, levels = df_aggr6$District[order(-df_aggr6$Count)])
# Plot the graph  dis
ggplot(df_aggr6, aes(x=District, y = Count)) + theme_minimal() + geom_bar(stat="identity", width=0.7, fill = "steelblue") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(x = "District", y = "Number of crimes") 

###Figure 8

df4$Type_grouped[df4$Primary_Type == "THEFT" | df4$Primary_Type == "MOTOR VEHICLE THEFT" ] <- "Theft"
df4$Type_grouped[df4$Primary_Type == "BATTERY"] <- "Batery"
df4$Type_grouped[df4$Primary_Type == "CRIMINAL DAMAGE"] <- "Criminal damage"
df4$Type_grouped[df4$Primary_Type == "NARCOTICS" | df4$Primary_Type == "OTHER NARCOTIC VIOLATION"] <- "Narcotics"
df4$Type_grouped[df4$Primary_Type == "ASSAULT"] <- "Assault"
df4$Type_grouped[df4$Primary_Type == "BURGLARY"] <- "Burglary"
df4$Type_grouped[df4$Primary_Type == "ROBBERY" ] <- "Robery"
df4$Type_grouped[df4$Primary_Type == "ARSON" | df4$Primary_Type == "CONCEALED CARRY LICENSE VIOLATION" | df4$Primary_Type == "CRIMINAL TRESPASS" | 
                   df4$Primary_Type == "INTERFERENCE WITH PUBLIC OFFICER" |df4$Primary_Type == "GAMBLINGS" | df4$Primary_Type == "HUMAN TRAFFICKING" |
                   df4$Primary_Type == "INTIMIDATION" | df4$Type == "KIDNAPPING" | df4$Type == "LIQUOR LAW VIOLATION" | df4$Primary_Type == "NON-CRIMINAL" |
                   df4$Primary_Type == "NON - CRIMINAL" | df4$Primary_Type == "OBSCENITY" | df4$Primary_Type == "OFFENSE INVOLVING CHILDREN"| df4$Primary_Type == "PROSTITUTION"|
                   df4$Primary_Type == "PUBLIC INDECENCY"| df4$Primary_Type == "PUBLIC PEACE VIOLATION"| df4$Primary_Type == "STALKING"| df4$Primary_Type == "WEAPONS VIOLATION"|
                   df4$Primary_Type == "HOMICIDE"| df4$Primary_Type == "CRIM SEXUAL ASSAULT" | df4$Primary_Type == "SEX OFFENSE"| df4$Primary_Type == "DECEPTIVE PRACTICE" |
                   df4$Primary_Type == "OTHER OFFENSE"] <- "Others"




###

df_aggr7 <- aggregate(Count ~ Type_grouped + District, data = dd_sub, FUN = sum)
# Plot the graph
p3<-ggplot(data = df_aggr7, aes(x = District, y = Type_grouped)) +
  geom_tile(aes(fill = Count), color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue")  
p3+ theme_minimal()+ theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 6),
        axis.text.y = element_text(size= 8),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) 