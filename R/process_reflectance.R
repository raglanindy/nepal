# Title: process_reflectance_ALL_SAT.R
# Author details: Author: Mat Allan, Contact details: mat.g.allan@gmail.com
# Script and data info: This script anlayses sattelite data for lakes.  
# Data consists S2. S2, Landsat, MERIS.
# Data was collected in 2020. 
# Copyright statement: This script is the product of WRC.

#https://towardsai.net/p/deep-learning/house-price-predictions-using-keras

Sys.setenv(TZ = "UTC")
#load
library(ggplot2)
library(dplyr) # joins

source("R/functions.R")

#read all sat data
sat_data<-read_sat()
#read in situ
field_data<-read_field()


s2<-sat_data[['s2_gee']]
s2$date<-substr(s2$system.index, 1, 8)
s2$date<-as.POSIXct(s2$date, format = "%Y%m%d")

s2$lid<-as.character(s2$Hylak_id)

##mask
s2<-s2 %>%
  filter(MSK_CLDPRB ==0)
s2<-s2 %>%
  filter(B11 <=0.1)

s2$rlh<- s2$B5-s2$B4-(s2$B7-s2$B4*((0.705-0.665)*1000.0))/((0.783-0.665)*1000.0)
s2$mci <- s2$B5-((0.74-0.705)/(0.74-0.665))*s2$B4-(1.0-(0.74-0.705)/(0.74-0.665))*s2$B6

#predict chla
#super_model <- readRDS("models/s2_chl_lawa_7_unnorm.rds")
#s2$chl<- predict(super_model, s2)


#predict cyano
model<-readRDS("models/s2_cyano_lawa_7_unnorm.rds")
s2$cyano_biov <- predict(model, s2)

##write results
s2$.geo<-NULL
write.table(s2, file = "data/estimated_wq.csv", append = FALSE, quote = FALSE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")

##summarize mean
#s2_mean<- s2 %>%
#  group_by(lid)  %>%
#  mutate(mean_cyano=mean(cyano_biov)) %>%
#  ungroup()

#s2_mean<- s2 %>%
#  group_by(lid)  %>%
#  slice(which.mean(cyano_biov)) 
#s2_mean<-NULL
#s2_mean<- s2 %>%
#  ungroup %>%
 # dplyr::group_by('lid')  %>%
 # dplyr::summarise(mean_cyano = mean(cyano_biov), across())%>%
 # dplyr::distinct(lid, .keep_all=TRUE)

s2_mean = aggregate(s2$cyano_biov ,by=list(s2$lid),FUN=mean, na.rm=TRUE,  drop = FALSE)

colnames(s2_mean)<-c("Hylak_id", "mean_cyano")

write.table(s2_mean, file = "data/estimated_wq_lake_mean_cyano.csv", append = FALSE, quote = FALSE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")
