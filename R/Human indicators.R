#################################################
#                                               #
#                                               #  
#     HUMAN INDICATORS NORTHERN ANDES           #
#             MIDORI YAJIMA                     #
#                                               #
#             MARCH 2020                        #
#                                               #      
#-----------------------------------------------#

## CONTRIBUTORS
# Ondrej XX <email>
# Suzette Flantua <s.g.a.flantua@gmail.com>



# ---------------------------------------------

# Libraries
library(readxl)
library(tidyverse)


# Load data
Datasheet <- read_excel("data/Datasheet.xlsx")
view(Datasheet)
str(Datasheet)


#### FIGURE 1 ---------------------------------

## number of records per time bin
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
 select(.,-c("LAPD_ID","Site Name",
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
# Take a look at the data
glimpse(Datasheet.indicators.full)
view(Datasheet.indicators.full)


# Step 5: Make plot of how often indicators are counted in total

# calculate per human indicator the sum
DF.indicators.SUM <-data.frame(IN=apply(Datasheet.indicators, 2, 
                                        FUN = function(x) sum(x,na.rm = T)))
# str(DF.indicators.SUM)
# view(DF.indicators.SUM)

DF.indicators.SUM$IN.name <- row.names(DF.indicators.SUM) %>% as.factor() # why do this step?

DF.indicators.SUM.no0 <- DF.indicators.SUM %>% filter(IN > 0) # remove all indicators with 0 values
# glimpse (DF.indicators.SUM)
# view(DF.indicators.SUM)

# plot      
DF.indicators.SUM.no0 %>%
  ggplot(.,aes(x = reorder(IN.name,IN,FUN = max),y=IN)) + # order the x-axis (human indicators) from large to small
  geom_bar(stat = "identity") +
  theme(axis.text.x  = element_text(angle = 70, hjust=1)) +
  xlab("Human Indicators") + ylab("Count") +
  ggtitle("Number of times human indicators are counted in total") 





#### FIGURE 3 ---------------------------------
## number of times human indicators are counted in each site

# Step 1: remove all indicators that were not counted 0
df <- Datasheet.indicators.full

# remove rows with NAs
df_pres <- df %>% drop_na() # This removes rows with value 0 (empthy bins for a site)
str(df_pres)

# Step 2: We are going to use the var function to check the variance of the different columns
# check which human indicators = 0
summary(df_pres)
apply(df_pres, 2, var)

# remove all columns (human indicators) that have a variance of 0
df_pres <- df_pres[ - as.numeric(which(apply(df_pres, 2, var) == 0))]
summary(df_pres) # voilá

# plot
reshape2::melt(df_pres) %>%
  group_by(`Site Name`,variable) %>%
  drop_na(variable) %>% 
  summarise(Count = sum(value)) %>%
  ggplot(aes(x=variable,y=Count))+
  geom_bar(stat = "identity")+
  facet_wrap(~`Site Name`)+
  theme(axis.text.x = element_text(angle = 90, hjust=1))



#### FIGURE 4 ---------------------------------
## in which sites are which indicators found and how often? 

reshape2::melt(df_pres) %>%
  group_by(variable,`Site Name`) %>%
  summarise(Count = sum(value)) %>%
  ggplot(aes(x=`Site Name`,y=Count))+
  geom_bar(stat = "identity")+
  facet_wrap(~variable)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))


#### Analyses of direct/indirect indicators

# load data
Human_indicators <- read_excel("data/01_Human indicators_V1.xlsx")

indic <- Human_indicators %>% 
  select (Indicator, 'Group (Taxa)')

## Select only direct indicators
Direct <- Human_indicators %>% 
  filter(Indicator=="Direct") %>%
  select(`Group (Taxa)`) 


Direct <- as.vector(Direct$`Group (Taxa)`)

DF.Direct<-Datasheet[,Direct]
df_direct <- df_pres[,Direct]
DF.Direct<-apply(DF.Direct,2, FUN = function(x) as.numeric(x))


#DF.Direct.full <- bind_cols(Datasheet %>% select(c("Site Name", "Bin number","Bin")),DF.Direct)
#DF.Direct.full[,4:28]<-apply (DF.Direct.full[,4:28],2,FUN= function(x) as.numeric(unlist(x)))

DF.Direct.Sum <- data.frame(SUM=apply(DF.Direct,1,FUN = function(x) sum(x,na.rm = T)))

## two ways to add direct indicators sum
Datasheet.ind.sum<-bind_cols(Datasheet,DF.Direct.Sum)
# Datasheet.ind.sum$Sum_direct<-DF.Direct.Sum$SUM
str(Datasheet.ind.sum)


## for indirect
Indirect<-Human_indicators %>% 
  filter(Indicator=="Indirect") %>% 
  select(`Group (Taxa)`)

Indirect<-as.vector(Indirect$`Group (Taxa)`)
DF.Indirect<-Datasheet[,Indirect]
DF.Indirect<-apply(DF.Indirect,2, FUN = function(x) as.numeric(x))
DF.Indirect.Sum<-data.frame(SUM=apply(DF.Indirect,1,FUN = function(x) sum(x,na.rm = T)))
Datasheet.ind.sum$Sum_indirect<-DF.Indirect.Sum$SUM



# Absolute frequency of direct indicators through time per site - NEW
Datasheet.ind.sum %>% 
  ggplot(aes(x=reorder(Bin,-Bin_num), y=SUM)) +  #(Midori) Bin column is charachter, in this way bars are displayed and ordered from old to recent (otherwise they would be alphabetically ordered)
  geom_bar(stat = "identity",width = 0.5) +
  facet_wrap(~`Site Name`) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust=1)) +
  geom_text(aes(label=SUM), vjust=-0.3, size=3) +
  ylab("Absolute frequency")+
  xlab("Time bins")



## Absolute frequency of indirect indicators through time per site
# Also, only the df_pres was made to only show human indicators that were counted
Datasheet.ind.sum %>% 
  ggplot(aes(x=reorder(Bin,-Bin_num), y=Sum_indirect)) + 
  geom_bar(stat = "identity",width = 0.5)+
  facet_wrap(~`Site Name`)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  geom_text(aes(label=Sum_indirect), vjust=-0.3, size=3)+
  ylab("Absolute frequency")+
  xlab("Time bins")
  
## for a single site
Datasheet.ind.sum %>% filter(`Site Name`=="Pantano de Genagra")%>%
  ggplot(aes(x = reorder(Bin,-Bin_num), y = Sum_indirect))+
  geom_bar(stat = "identity",width = 0.9)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  geom_text(aes(label=Sum_indirect), vjust=-0.3, size=3)+
  ylab("Absolute frequency")+
  xlab("Time bins")
  ggtitle("Pantano de Genagra")

## to have a single chart for direct and indirect indicators
DF.Indirect.Sum <- DF.Indirect.Sum %>% 
  mutate(IND=rep("Indirect"))

DF.Direct.Sum <- DF.Direct.Sum %>% 
  mutate(IND=rep("Direct"))
Datasheet.D.full<-bind_cols(Datasheet,DF.Direct.Sum)
Datasheet.I.full<-bind_cols(Datasheet,DF.Indirect.Sum)
Datasheet.full<-rbind(Datasheet.D.full,Datasheet.I.full)
  
ggplot(data=Datasheet.full,aes(x = reorder(Bin,-Bin_num),y=SUM,fill=IND))+
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
Datasheet.full %>% 
  filter(`Site Name`=="Pantano de Genagra") %>%
  ggplot(aes(x=reorder(`Bin`,-Bin_num),y=SUM,fill=IND))+
  geom_bar(stat = "identity",width = 0.9, position = "fill")+
  geom_text(aes(label=SUM), vjust=0.5, size=3, position = "fill")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  ylab("Absolute frequency")+
  xlab("Time bins")+
  ggtitle("Pantano de Genagra")

#### PLAYING WITH DATASHEET 

#new shorter datasheet
#select record spannind last 12000 yr
Datasheet$Bin_num<-as.numeric(Datasheet$Bin_num)
Datasheet_shaved<-Datasheet%>%filter(Bin_num<12500)

#select only indicators actually found
library(writexl)

DF.zeros<-(DF.indicators.SUM%>%filter(IN==0))
Todrop<-as.vector(DF.zeros$IN.name)
Datasheet_shaved<-Datasheet_shaved[,!names(Datasheet_shaved)%in% Todrop]

write_xlsx(Datasheet_shaved,"Datasheet_shaved.xlsx")

#add metadata absent in Datasheet but found in LAPD
which(is.na(LAPD_Andes_MY$Altitude))

LAPD_Andes_MY[72,"Altitude"]=2608

write_xlsx(LAPD_Andes_MY, "LAPD_Andes_MY.xlsx")
