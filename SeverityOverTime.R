rm(list=ls())
library("tidyverse")

#  ------- 1950
#con50 <- gzcon(url(paste("https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/",
     #                    "StormEvents_details-ftp_v1.0_d1950_c20210803.csv.gz", sep="")))
#txt50 <- readLines(con50)
#dat50 <- read.csv(textConnection(txt50))

# dat50Sev <- dat50 %>%
#   separate(DAMAGE_PROPERTY,into=c("prop_damage","k"),sep="K")%>%
#   mutate(prop_damage=as.numeric(prop_damage)*1000)%>%
#   separate(DAMAGE_CROPS,into=c("crop_damage","kilo"),sep="K")%>%
#   mutate(crop_damage=as.numeric(crop_damage)*1000) %>%
#   mutate(casualties = INJURIES_DIRECT + INJURIES_INDIRECT + DEATHS_DIRECT + DEATHS_INDIRECT) %>%
#   filter(casualties > 0  | crop_damage > 0 | prop_damage > 0)

giantDataset=data.frame()
for (i in 50:71) {
  dataset = gzcon(url(paste("https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/",
                          "StormEvents_details-ftp_v1.0_d19",as.character(i),"_c20210803.csv.gz", sep="")))
  txt50 <- readLines(dataset)
  dat50 <- read.csv(textConnection(txt50))
  giantDataset <- rbind(giantDataset, dat50)
}


giantDataset2=data.frame()
for (i in 72:99) {
  dataset = gzcon(url(paste("https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/",
                            "StormEvents_details-ftp_v1.0_d19",as.character(i),"_c20220425.csv.gz", sep="")))
  txt50 <- readLines(dataset)
  dat50 <- read.csv(textConnection(txt50))
  giantDataset2 <- rbind(giantDataset2, dat50)
}

giantDataset3=data.frame()
for (i in 2000:2013) {
  dataset = gzcon(url(paste("https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/",
                            "StormEvents_details-ftp_v1.0_d",as.character(i),"_c20220425.csv.gz", sep="")))
  txt50 <- readLines(dataset)
  dat50 <- read.csv(textConnection(txt50))
  giantDataset3 <- rbind(giantDataset3, dat50)
}

giantDataset4=data.frame()
# Add 2014
con14 <- gzcon(url(paste("https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/",
                         "StormEvents_details-ftp_v1.0_d2014_c20220719.csv.gz", sep="")))
txt14 <- readLines(con14)
dat14 <- read.csv(textConnection(txt14))
giantDataset4 <- rbind(giantDataset4, dat14)

# Add 2015
con15 <- gzcon(url(paste("https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/",
                         "StormEvents_details-ftp_v1.0_d2015_c20220425.csv.gz", sep="")))
txt15 <- readLines(con15)
dat15 <- read.csv(textConnection(txt15))
giantDataset4 <- rbind(giantDataset4, dat15)

# Add 2017
con17 <- gzcon(url(paste("https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/",
                         "StormEvents_details-ftp_v1.0_d2017_c20220719.csv.gz", sep="")))
txt17 <- readLines(con17)
dat17 <- read.csv(textConnection(txt17))
giantDataset4 <- rbind(giantDataset4, dat17)

# Add 2018
con18 <- gzcon(url(paste("https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/",
                         "StormEvents_details-ftp_v1.0_d2018_c20220425.csv.gz", sep="")))
txt18 <- readLines(con18)
dat18 <- read.csv(textConnection(txt18))
giantDataset4 <- rbind(giantDataset4, dat18)

# Add 2019
con19 <- gzcon(url(paste("https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/",
                         "StormEvents_details-ftp_v1.0_d2019_c20220425.csv.gz", sep="")))
txt19 <- readLines(con19)
dat19 <- read.csv(textConnection(txt19))
giantDataset4 <- rbind(giantDataset4, dat19)

# Add 2020
con20 <- gzcon(url(paste("https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/",
                         "StormEvents_details-ftp_v1.0_d2020_c20220816.csv.gz", sep="")))
txt20 <- readLines(con20)
dat20 <- read.csv(textConnection(txt20))
giantDataset4 <- rbind(giantDataset4, dat20)

# Add 2021
con21 <- gzcon(url(paste("https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/",
                         "StormEvents_details-ftp_v1.0_d2021_c20220921.csv.gz", sep="")))
txt21 <- readLines(con21)
dat21 <- read.csv(textConnection(txt21))
giantDataset4 <- rbind(giantDataset4, dat21)

# Add 2022
con22 <- gzcon(url(paste("https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/",
                         "StormEvents_details-ftp_v1.0_d2022_c20220921.csv.gz", sep="")))
txt22 <- readLines(con22)
dat22 <- read.csv(textConnection(txt22))
giantDataset4 <- rbind(giantDataset4, dat22)

hugeDataset <- rbind(giantDataset, giantDataset2)
hugeDataset <- rbind(hugeDataset, giantDataset3)
hugeDataset <- rbind(hugeDataset, giantDataset4)

write_csv(hugeDataset, "hugeDataset.csv")

hugeDataset_tidy=hugeDataset%>%
  mutate(casualty=INJURIES_DIRECT+INJURIES_INDIRECT+DEATHS_DIRECT+DEATHS_INDIRECT)%>%
  separate(DAMAGE_CROPS,into=c("crop_damage","kilo"),sep="K")%>%
  mutate(crop_damage=as.numeric(crop_damage)*1000)%>%
  separate(DAMAGE_PROPERTY,into=c("prop_damage","kilo"),sep="K")%>%
  mutate(prop_damage=as.numeric(prop_damage)*1000)%>%
  separate(BEGIN_YEARMONTH,into=c("year","month"),sep=4)%>%
  mutate(year=as.numeric(year))
hugeDataset_tidy$prop_damage[is.na(hugeDataset_tidy$prop_damage)]=0
hugeDataset_tidy$crop_damage[is.na(hugeDataset_tidy$crop_damage)]=0
hugeDataset_tidy$total_damage=hugeDataset_tidy$prop_damage+hugeDataset_tidy$crop_damage
names(hugeDataset_tidy)
