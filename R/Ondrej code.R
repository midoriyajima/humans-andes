#################################################
#                                               #
#                                               #  
#     HUMAN INDICATORS NORTHERN ANDES           #
#             MIDORI YAJIMA                     #
#                                               #
#             MARCH 2020                        #
#                                               #      
#-----------------------------------------------#


# Libraries
library(readxl)
library(tidyverse)


# Load data
Datasheet <- read_excel("data/Datasheet.xlsx")
view(Datasheet)
str(Datasheet)


#### FIGURE 1 ---------------------------------

## number of sites per time bin
ggplot(Datasheet)+
  geom_bar(aes(x= reorder(Datasheet$Bin,-Datasheet$Bin_num)))+
  theme_bw()+
  theme(axis.text.x  = element_text(angle = 70, hjust=1))+
  xlab("Time bins")+
  ggtitle("Number of sites per time bin")

## number of records per time bin (Suzette)
ggplot(Datasheet)+
  geom_bar(aes(x = Bin_num)) +
  theme_bw() +
  theme(axis.text.x  = element_text(angle = 70, hjust=1)) +
  xlab("Time bins") +
  ylab("Number of records") + 
  scale_x_reverse() +
  ggtitle("Number of pollen records per time bin") 


#### FIGURE 2 ---------------------------------
## number of times human indicators are counted in total

# Step 1: Filter indicators from dataset
Datasheet.indicators <- Datasheet %>%
 select(.,-c("LAPD_ID\r\n","Site Name",
"Reference (short)","Bin number","Bin", "Bin_num" ))

str(Datasheet.indicators) # take a look at the dataframe

# Step 2: Convert counts from character to numeric
Datasheet.indicators <- apply (Datasheet.indicators,2,
                               FUN= function(x) as.numeric(unlist(x)))

# Step 3: View summary per human indicator 
Datasheet.indicators %>% summary

# Step 4: Make datasheet with site vs number of times an indicator is counted  
Datasheet.indicators <- as.data.frame(Datasheet.indicators)

Datasheet.indicators.full <- bind_cols(Datasheet %>%                
                                         select(.,c("Site Name")),
                                       Datasheet.indicators)  
glimpse(Datasheet.indicators.full) # take a look at the tibble
view(Datasheet.indicators.full)

# Step 5: Make plot of how often indicators counted in all bins
DF.indicators.SUM <-data.frame(IN=apply(Datasheet.indicators, 2, 
                                        FUN = function(x) sum(x,na.rm = T))) 

DF.indicators.SUM$IN.name <- row.names(DF.indicators.SUM) %>% as.factor()

DF.indicators.SUM %>%
  filter(IN!=0)%>%
  ggplot(.,aes(x=reorder(IN.name,IN,FUN = max),y=IN)) +
  #drop_na() +
  geom_bar(stat = "identity") +
  theme(axis.text.x  = element_text(angle = 70, hjust=1)) +
  geom_text(aes(label=IN), vjust=-0.5, size=2, hjust=0.3)+
  xlab("Human Indicators") + ylab("Count")+
  ggtitle("Number of bins where Indicators found")

#### FIGURE 3 ---------------------------------
## number of times human indicators are counted in each site
Datasheet.indicators.full %>% 
  reshape2::melt() %>%
  group_by(`Site Name`, variable)%>%
  drop_na()%>%
  summarise(Count = sum(value)) %>%
  filter(Count!=0)%>%
  ggplot(aes(x = variable, y = Count))+
  geom_bar(stat = "identity")+
  facet_wrap(~`Site Name`)+
  theme(axis.text.x = element_text(angle = 90, hjust=1))


##how often indicators found in each site (showing sites per indicator)
reshape2::melt(Datasheet.indicators.full) %>%
  group_by(variable,`Site Name`) %>%
  summarise(Count = sum(value)) %>%
  filter(Count!=0)%>%
  ggplot(aes(x=`Site Name`,y=Count))+
  geom_bar(stat = "identity")+
  facet_wrap(~variable)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))

library(readxl)
Human_indicators <- read_excel("data/01_Human indicators_V1.xlsx")


##For direct indicators
Direct<-Human_indicators%>%filter(Indicator=="Direct")%>%select(`Group (Taxa)`)
Direct<-as.vector(Direct$`Group (Taxa)`)
DF.Direct<-Datasheet[,Direct]
DF.Direct<-apply(DF.Direct,2, FUN = function(x) as.numeric(x))
#DF.Direct.full<-bind_cols(Datasheet%>%select(c("Site Name", "Bin number","Bin")),DF.Direct)
#DF.Direct.full[,4:28]<-apply (DF.Direct.full[,4:28],2,FUN= function(x) as.numeric(unlist(x)))
DF.Direct.Sum<-data.frame(SUM=apply(DF.Direct,1,FUN = function(x) sum(x,na.rm = T)))
##two ways to add direct indicators sum
#Datasheet.ind.sum<-bind_cols(Datasheet,DF.Direct.Sum)
Datasheet.ind.sum$Sum_direct<-DF.Direct.Sum$SUM

##for indirect
Indirect<-Human_indicators%>%filter(Indicator=="Indirect")%>%select(`Group (Taxa)`)
Indirect<-as.vector(Indirect$`Group (Taxa)`)
DF.Indirect<-Datasheet[,Indirect]
DF.Indirect<-apply(DF.Indirect,2, FUN = function(x) as.numeric(x))
DF.Indirect.Sum<-data.frame(SUM=apply(DF.Indirect,1,FUN = function(x) sum(x,na.rm = T)))
Datasheet.ind.sum$Sum_indirect<-DF.Indirect.Sum$SUM

#Absolute frequency of direct indicators through time per site
ggplot(data=Datasheet.ind.sum,aes(x=reorder(Datasheet.ind.sum$Bin,-Datasheet.ind.sum$Bin_num),y=Sum_direct))+
  geom_bar(stat = "identity",width = 0.5)+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  geom_text(aes(label=Sum_direct), vjust=-0.3, size=3)+
  ylab("Absolute frequency")+
  xlab("Time bins")
  

##Absolute frequency of indirect indicators through time per site
ggplot(data=Datasheet.ind.sum,aes(x=reorder(Datasheet.ind.sum$Bin,-Datasheet.ind.sum$Bin_num),y=Sum_indirect))+
  geom_bar(stat = "identity",width = 0.5)+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  geom_text(aes(label=Sum_indirect), vjust=-0.3, size=3)+
  ylab("Absolute frequency")+
  xlab("Time bins")
  
##for a single site
Datasheet.ind.sum%>%filter(`Site Name`=="Pantano de Genagra")%>%
  ggplot(aes(x=reorder(`Bin`,-Bin_num),y=Sum_indirect))+
  geom_bar(stat = "identity",width = 0.9)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  geom_text(aes(label=Sum_indirect), vjust=-0.3, size=3)+
  ylab("Absolute frequency")+
  xlab("Time bins")
  ggtitle("Pantano de Genagra")

##to have a single figure for direct and indirect indicators
DF.Indirect.Sum<-DF.Indirect.Sum%>%mutate(IND=rep("Indirect"))
DF.Direct.Sum<-DF.Direct.Sum%>%mutate(IND=rep("Direct"))
Datasheet.D.full<-bind_cols(Datasheet,DF.Direct.Sum)
Datasheet.I.full<-bind_cols(Datasheet,DF.Indirect.Sum)
Datasheet.full<-rbind(Datasheet.D.full,Datasheet.I.full)
  
ggplot(data=Datasheet.full,aes(x=reorder(Datasheet.full$Bin,-Datasheet.full$Bin_num),y=SUM,fill=IND))+
  geom_bar(position="dodge",stat = "identity",width = 0.5)+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  geom_text(aes(label=SUM), vjust=-0.3, size=3)+
  ylab("Absolute frequency")+
  xlab("Time bins")

##for a single site
#dodged bars
Datasheet.full%>%filter(`Site Name`=="Pantano de Genagra")%>%
  ggplot(aes(x=reorder(`Bin`,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 0.9, position = "dodge")+
  geom_text(aes(label=SUM), vjust=-0.3, size=3, position = "dodge")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  ylab("Absolute frequency")+
  xlab("Time bins")+
ggtitle("Pantano de Genagra")

#stacked bars
Datasheet.full%>%filter(`Site Name`=="Pantano de Genagra")%>%
  ggplot(aes(x=reorder(`Bin`,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 0.9, position = "stack")+
  geom_text(aes(label=SUM), vjust=0.5, size=3, position = "stack")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Pantano de Genagra")

#stacked bars with the same height
Datasheet.full%>%filter(`Site Name`=="Pantano de Genagra")%>%
  ggplot(aes(x=reorder(`Bin`,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 0.9, position = "fill")+
  geom_text(aes(label=SUM), vjust=0.5, size=3, position = "fill")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Pantano de Genagra")


