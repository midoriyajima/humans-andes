#-------------------------------------------------
#
#           RESULTS ANALYSIS 
#       (from datasheet compilation)
#
#-----------------------------------------------


#number of suitable records
lapd_andes_suitable<-LAPD_Andes%>%
  filter(!DONE %in% c("C14 higher than 2ka","Not Andes","Peru"))

view(lapd_andes_suitable%>%filter(Country=="Colombia"))
view(lapd_andes_suitable%>%filter(Country=="Venezuela"))
view(lapd_andes_suitable%>%filter(Country=="Ecuador"))

     
#number of records in the dataset
lapd_andes_done<-lapd_andes_suitable%>%
  filter(DONE %in% c( "DONE", "DONE\r\n"))

#number of records does not match with the one in Datasheet
sitesname2<-distinct(Datasheet_shaved, `Site Name`)
nomatch2<-match(lapd_andes_done$SiteName,sitesname2$`Site Name`)
which(is.na(nomatch2))
sites_nomatch2<-lapd_andes_done[c(29,33,36),]
sites_nomatch2$SiteName



