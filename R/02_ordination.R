# --------------------------------------------------------------- 
#
#                         ORDINAtION
#
# ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(vegan)
library(ggpubr)  
library(ggpubr)

# Load data
# Datasheet_shaved <- read_excel("data/Datasheet_shaved.xlsx")
# Sites<-Datasheet_shaved %>% 
# distinct(`Site Name`,Latitude,Longitude, Country, Altitude)
Human_indicators <- read_excel("data/Human_indicators.xlsx")


# selecte only indicator data and tunr ten into numbers
# and replace all missing values with zeros ( composional data cannot have NA)
cca.data <-  Datasheet_shaved %>% 
  dplyr::select(-c("LAPD_ID\r\n","Site Name","Reference (short)","Bin",
                   "Bin_num","Country","Latitude","Longitude","Length","Altitude")) %>%
  apply(., 2, FUN= function(x){
    x[x=="NA"] <-0;
    y<- as.numeric(x);
    return(y)
  })  

#add row names
row.names(cca.data) <- Datasheet_shaved$`Site Name`

# select enviromrntal values (time)
cca.env <- Datasheet_shaved %>%
  select(c("Bin_num","Site Name"))

# subset the data to exlude rows and cols with SUM = 0
cca.env <- cca.env[rowSums(cca.data, na.rm = T) != 0, ]
cca.data <- cca.data[rowSums(cca.data, na.rm = T) != 0, colSums(cca.data, na.rm = T) != 0]

# check if everything is OK
all(rowSums(cca.data)>0)
all(colSums(cca.data)>0)

# --------------------------------------------------------------- 
#                         CALCULATION
# ---------------------------------------------------------------

# the data is in binary so lineral prediction is better. But just for sure
# DCA on data
decorana(cca.data)
# first axis lengt is under 2.5 -> linear predictor -> RDA

rda.1 <- rda(cca.data ~ cca.env$Bin_num + Condition(cca.env$`Site Name`), scale =T) 

# summary
smry <- summary(rda.1)

df2  <- data.frame(smry$species[,1:3]) %>% # loadings for PC1 and PC2
  rownames_to_column() %>%
  rename(IND = rowname) %>%
  left_join(.,Human_indicators %>%
              rename(IND = `Group (Taxa)`), by= "IND")%>%
  rename(Potential_food_source = 'Potential food source',
         North_Andean_fossil_records = 'North Andean fossil records? [yes/no]'
         ) %>%
  as_tibble()

x_lim = c(-0.7,0.5)
y_lim =c(-1.6,0.5)

rda.plot.ind.base <- ggplot(df2, aes(x=0, xend=RDA1, y=0, yend=PC1))+
  geom_hline(yintercept=0, linetype="dotted") +
  geom_vline(xintercept=0, linetype="dotted") +
  coord_fixed(xlim= x_lim, ylim = y_lim)+ 
  theme_classic()

rda.plot.ind.01 <- rda.plot.ind.base+
  geom_segment(arrow=arrow(length=unit(0.01,"npc")), aes(color=Indicator)) +
  geom_text(data=df2,check_overlap = T,
            aes(x=RDA1,y=PC1,label=IND,color=Indicator,
                hjust=0.5*(1-sign(RDA1)),vjust=0.5*(1-sign(PC1))), size=4)+
  labs(x="RDA1",y="PCA1")+
  theme(legend.position = "bottom")

rda.plot.ind.01.legend <- get_legend(rda.plot.ind.01)

rda.plot.ind.02 <- rda.plot.ind.base+
    geom_segment(arrow=arrow(length=unit(0.01,"npc")), aes(color=Family)) +
    geom_text(data=df2,check_overlap = T,
              aes(x=RDA1,y=PC1,label=IND,color=Family,
                  hjust=0.5*(1-sign(RDA1)),vjust=0.5*(1-sign(PC1))), size=4)+
    labs(x="RDA1",y="PCA1")+
    guides(color=F)
  
rda.plot.ind.03 <- rda.plot.ind.base+
  geom_segment(arrow=arrow(length=unit(0.01,"npc")), aes(color=Potential_food_source)) +
  geom_text(data=df2,check_overlap = T,
            aes(x=RDA1,y=PC1,label=IND,color=Potential_food_source,
                hjust=0.5*(1-sign(RDA1)),vjust=0.5*(1-sign(PC1))), size=4)+
  labs(x="RDA1",y="PCA1")+
  theme(legend.position = "bottom")

rda.plot.ind.03.legend <- get_legend(rda.plot.ind.03)

rda.plot.ind.sum <- ggarrange(
  rda.plot.ind.01+guides(color=F),
  rda.plot.ind.02,
  rda.plot.ind.03+guides(color=F),
  nrow=1, align = "h", labels = c("A","B","C")
)

rda.plot.ind.legend <- ggarrange(
  rda.plot.ind.01.legend,
  rda.plot.ind.03.legend,
  nrow = 1
)


# --------------------------------------------------------------- 
#                         FINAL PLOT
# ---------------------------------------------------------------

ggarrange(rda.plot.ind.sum,
          rda.plot.ind.legend,
          ncol = 1, heights = c(10,1))

# A = colored by direct/indirect
# B = colored by the Family
# C = colored by Potentional food source 
