library(readxl)
Datasheet <- read_excel("C:/Users/Midori/Desktop/MscThesis/Datasheet.xlsx")

##new dataframe with only indicators 
Datasheet.indicators <-Datasheet %>%
  select(.,-c("LAPD_ID\r\n","Site Name",
              "Reference (short)","Bin_number","Bin" )) 

##every indicator as numerical (not considering them as list)
Datasheet.indicators <- apply (Datasheet.indicators,2,
                               FUN= function(x) as.numeric(unlist(x)))

Datasheet.indicators %>% summary
Datasheet.indicators <- as.data.frame(Datasheet.indicators)

##new datasheet, binding the previous one and Sites
Datasheet.indicators.full <- bind_cols(Datasheet %>%                
                                         select(.,c("Site Name")),
                                       Datasheet.indicators)  


glimpse(Datasheet.indicators.full)
##how often indicators found in each site
reshape2::melt(Datasheet.indicators.full) %>%
  group_by(`Site Name`,variable) %>%
  summarise(Count = sum(value)) %>%
  ggplot(aes(x=variable,y=Count))+
  geom_bar(stat = "identity")+
  facet_wrap(~`Site Name`)

##how often indicators counted in all bins
DF.indicators.SUM <-data.frame(IN=apply(Datasheet.indicators, 2, FUN = function(x) sum(x,na.rm = T))) 
DF.indicators.SUM$IN.name <- row.names(DF.indicators.SUM) %>% as.factor()

DF.indicators.SUM %>%
  ggplot(.,aes(x=reorder(IN.name,IN,FUN = max),y=IN))+
  geom_bar(stat = "identity")+
  theme(axis.text.x  = element_text(angle = 90, hjust=1))

##how often indicators found in each site
reshape2::melt(Datasheet.indicators.full) %>%
  group_by(variable,`Site Name`) %>%
  summarise(Count = sum(value)) %>%
  ggplot(aes(x=`Site Name`,y=Count))+
  geom_bar(stat = "identity")+
  facet_wrap(~variable)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))


