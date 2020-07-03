#-------------------------------------------------
#
#           RESULTS ANALYSIS 
#       (from datasheet compilation)
#
#-----------------------------------------------

require(tidyverse)

#number of suitable records
lapd_andes_suitable<-LAPD_Andes%>%
  filter(!DONE %in% c("C14 higher than 2ka",
                      "Not Andes","Peru",
                      "No diagram in PDF",
                      "No diagram in PDF\r\n",
                      "No diagram in pdf",
                      "Diagram unreadable",
                      "Too short" ))

view(lapd_andes_suitable%>%filter(Country=="Colombia"))
view(lapd_andes_suitable%>%filter(Country=="Venezuela"))
view(lapd_andes_suitable%>%filter(Country=="Ecuador"))
view(lapd_andes_suitable%>%filter(DONE=="Check Hardcopy"))
     
#number of records in the dataset
lapd_andes_done<-lapd_andes_suitable%>%
  filter(DONE %in% c( "DONE", "DONE\r\n"))

view(lapd_andes_done%>%filter(Country=="Colombia"))
view(lapd_andes_done%>%filter(Country=="Venezuela"))
view(lapd_andes_done%>%filter(Country=="Ecuador"))

#number of records does not match with the one in Datasheet
sitesname2<-distinct(Datasheet_shaved, `Site Name`)
nomatch2<-match(lapd_andes_done$SiteName,sitesname2$`Site Name`)
which(is.na(nomatch2))
sites_nomatch2<-lapd_andes_done[c(29,33,36),]
sites_nomatch2$SiteName

##BOXPLOT
Datasheet_shaved%>%distinct(`Site Name`, Altitude)%>%count(Altitude<2000)
Datasheet_shaved%>%distinct(`Site Name`, Altitude)%>%count(Altitude %in% 2000:3000)
Datasheet_shaved%>%distinct(`Site Name`, Altitude)%>%count(Altitude %in% 3000:4215)

Span<-as.data.frame(table(Datasheet_shaved$Bin_num))

tot<-Datasheet_shaved %>%
  group_by(`Site Name`) %>%
  summarise(MIN = min(Bin_num),
            MAX = max(Bin_num))%>%
  count(MAX)

Span$tot<-tot$n[match( Span$Var1, tot$MAX)]

Span[17:24,]%>%summarise(perc=sum(tot, na.rm = T)/38)
Span[9:16,]%>%summarise(perc=sum(tot, na.rm = T)/38)
Span[1:8,]%>%summarise(perc=sum(tot, na.rm = T)/38)
