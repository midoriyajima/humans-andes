library(readxl)
library(vegan)
library(tidyverse)

# Load data
Datasheet <- read_excel("data/Datasheet.xlsx")

# selecte only indicator data and tunr ten into numbers
cca.data <-  Datasheet %>% 
  select(-c("LAPD_ID\r\n","Site Name","Bin","Reference (short)","Bin number","Bin_num")) %>%
  apply(., 2, FUN= function(x){
    y<- as.numeric(x)
    return(y)
  })  

row.names(cca.data) <- Datasheet$`Site Name`

# replace all missing values with zeros ( composional data cannot have NA)
cca.data[is.na(cca.data)] <- 0

# select enviromrntal values (time)
cca.env <- Datasheet %>%
  select(c("Bin_num","Site Name"))


# sumbset the data to exlude rows with ROWSUM = 0
cca.env <- cca.env[rowSums(cca.data, na.rm = T) != 0, ]
cca.data <- cca.data[rowSums(cca.data, na.rm = T) != 0, ]

# DCA on data
decorana(cca.data)

# first axis lengt is under 2.5 -> linear predictor


rda.1 <- rda(cca.data ~ cca.env$Bin_num + Condition(cca.env$`Site Name`), scale = F)
plot(rda.1)



# ploting 
smry <- summary(rda.1)

df1  <- data.frame(smry$sites[,1:2])       # PC1 and PC2
df1$plot <- row.names(cca.data)

df2  <- data.frame(smry$species[,1:2])     # loadings for PC1 and PC2

# calculate the lenght of each predictor 
df2$length <- sqrt(abs(df2$RDA1)**2 + abs(df2$PC1)**2)

# order df by length of predictors
df2.sub <- df2 %>%
  rownames_to_column() %>%
  arrange(-length) 

# select the  10 most important  
df2.sub <- df2.sub[1:10,]


rda.plot <- ggplot(df1, aes(x=RDA1, y=PC1)) + 
  #geom_text(aes(label=rownames(df1)),size=4) +
  geom_line(aes(group=plot))+
  geom_point(aes(color=cca.env$Bin_num), size=3)+
  geom_hline(yintercept=0, linetype="dotted") +
  geom_vline(xintercept=0, linetype="dotted") +
  coord_fixed()+
  scale_color_gradient(low="gray",high = "black")
rda.plot



rda.biplot <- rda.plot +
  geom_segment(data=df2.sub, aes(x=0, xend=RDA1, y=0, yend=PC1), 
               color="red", arrow=arrow(length=unit(0.01,"npc"))) +
  geom_text(data=df2.sub, 
            aes(x=RDA1,y=PC1,label=rowname,
                hjust=0.5*(1-sign(RDA1)),vjust=0.5*(1-sign(PC1))), 
            color="red", size=4)
rda.biplot



