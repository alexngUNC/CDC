rm(list=ls())


library("tidyverse")

# Read in the data
#details <- read.csv("Datasets/StormEvents_details-ftp_v1.0_d2022_c20220921.csv")
#ead(details)


summary(datCas)

data50 <- read.csv(url("https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/StormEvents_fatalities-ftp_v1.0_d1970_c20210803.csv.gz"))
fatal <- read.csv("Datasets/StormEvents_fatalities-ftp_v1.0_d2022_c20220921.csv")
locations <- read.csv("Datasets/StormEvents_locations-ftp_v1.0_d2022_c20220921.csv")

head(data50)
# Summaries and dimensions
dim(details)
summary(details)
dim(fatal)
summary(fatal)
dim(locations)
summary(locations)

head(fatal)
names(details)

# Convert prop and crop damage to numeric
detail2=details%>%
  separate(DAMAGE_PROPERTY,into=c("prop_damage","k"),sep="K")%>%
  mutate(prop_damage=as.numeric(prop_damage)*1000)%>%
  separate(DAMAGE_CROPS,into=c("crop_damage","kilo"),sep="K")%>%
  mutate(crop_damage=as.numeric(crop_damage)*1000)

# Check for missing data
summary(detail2$prop_damage)

# Convert missing values to 0
detail2$prop_damage[is.na(detail2$prop_damage)]=0
detail2$crop_damage[is.na(detail2$crop_damage)]=0

summary(detail2$prop_damage)
max(detail2$prop_damage) / 1000000
# Histogram of property damage
hist(detail2$prop_damage)


# Group data
detail3 <- detail2 %>% group_by(prop_damage) %>% filter(prop_damage <= 1000)
summary(detail3$prop_damage)

# Histogram with equal bins
ggplot(detail2, aes(prop_damage)) +            # ggplot2 histogram with manual bins
  geom_histogram()
names(detail2)

# Change month from letters to numbers
detail2$BEGIN_DATE_TIME <- str_replace_all(detail2$BEGIN_DATE_TIME, "JAN", "01")
detail2$BEGIN_DATE_TIME <- str_replace_all(detail2$BEGIN_DATE_TIME, "FEB", "02")
detail2$BEGIN_DATE_TIME <- str_replace_all(detail2$BEGIN_DATE_TIME, "MAR", "03")
detail2$BEGIN_DATE_TIME <- str_replace_all(detail2$BEGIN_DATE_TIME, "APR", "04")
detail2$BEGIN_DATE_TIME <- str_replace_all(detail2$BEGIN_DATE_TIME, "MAY", "05")
detail2$BEGIN_DATE_TIME <- str_replace_all(detail2$BEGIN_DATE_TIME, "JUN", "06")

# Change month from letters to numbers
detail2$END_DATE_TIME <- str_replace_all(detail2$END_DATE_TIME, "JAN", "01")
detail2$END_DATE_TIME <- str_replace_all(detail2$END_DATE_TIME, "FEB", "02")
detail2$END_DATE_TIME <- str_replace_all(detail2$END_DATE_TIME, "MAR", "03")
detail2$END_DATE_TIME <- str_replace_all(detail2$END_DATE_TIME, "APR", "04")
detail2$END_DATE_TIME <- str_replace_all(detail2$END_DATE_TIME, "MAY", "05")
detail2$END_DATE_TIME <- str_replace_all(detail2$END_DATE_TIME, "JUN", "06")

# Duration calculation using difftime
typeof(detail2$END_DATE_TIME[1])
difftime(detail2$END_DATE_TIME[1], detail2$BEGIN_DATE_TIME[1], units="mins")

# Add a duration column
detail3 <- detail2 %>%
  mutate(duration = difftime(as.POSIXct(END_DATE_TIME), as.POSIXct(BEGIN_DATE_TIME), units = "mins"))

# Remove space and minutes so duration is only numeric
detail3$duration <- as.numeric(detail3$duration)

# Distribution
hist(detail3$duration)
head(detail3)
summary(detail3$duration)
head(detail3$CZ_TYPE)

head(detail3)

# Make casualties variable
detail4 <- detail3 %>%
  mutate(casualties = INJURIES_DIRECT + INJURIES_INDIRECT + DEATHS_DIRECT + DEATHS_INDIRECT)


# Only data that has damage
onlyDamage <- detail4 %>%
  filter(prop_damage > 0 | crop_damage > 0)

casualtiesOrDamage <- detail4 %>%
  filter(prop_damage > 0 | crop_damage > 0 | casualties > 0)

dim(casualtiesOrDamage)

dim(onlyDamage)
onlyCasualties <- detail4 %>%
  filter(casualties > 0)

onlyDeaths <- onlyCasualties %>%
  filter(DEATHS_DIRECT > 0 | DEATHS_INDIRECT > 0)

summary(detail4)
# Bar plot of injuries and event type
ggplot(detail3, aes(x=factor(EVENT_TYPE)))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal()
