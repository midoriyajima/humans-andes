
# --------------------------------------------------------------- 
#
#             REDUCING THE AMOUNT OF INDICATORS
#
# ---------------------------------------------------------------


# --------------------------------------------------------------- 
# correlation beween all indicators without acknowlaging the time
# ---------------------------------------------------------------
ggcorrplot(cor(cca.data)) 

# select treshold of correlation betwen indicators 
treshold <- 0.75 # correlation of 0.75 is standard,

# check what indicators are corelated higher than the treshold
cor(cca.data) %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  pivot_longer(cols = -c(rowname)) %>%
  mutate(SAME = rowname == name) %>%
  filter(.,SAME != TRUE & abs(value) > treshold) %>%
  select(-c(SAME))

# Eucalyptus and Pinus! Therefore on of those can be deleted
# can play with the treshold (.06 - 0.8) is all reasonable

# --------------------------------------------------------------- 
#                 correlation using ordination
# ---------------------------------------------------------------

# ---------------------
# ----- APROACH 1 -----
# ---------------------

# what is the correlation between individual indicators based on their ordination scores?

# plot correlation
smry$species %>%
  t() %>%
  cor() %>%
  ggcorrplot()


# treshold <- 0.75 
# create a dataframe with correlation abowe selected treshold ordered by the correlation
cor.ser.by.ord <- smry$species %>%
  t() %>%
  cor() %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  pivot_longer(cols = -c(rowname)) %>%
  mutate(SAME = rowname == name) %>%
  filter(.,SAME != TRUE & abs(value) > treshold) %>%
  select(-c(SAME)) %>%
  rename(ONE = rowname, TWO = name) %>%
  mutate (COMB = apply(., 1 , FUN = function(x){
    y<- sort((c(as.character(x[1]), as.character(x[2]))))
    z <- paste(y[1],"-",y[2])
    return(z)
  })) %>%
  distinct(COMB,value, .keep_all= T) %>%
  arrange(desc(abs(value)) )

cor.ser.by.ord

# see the dataframe
View(cor.ser.by.ord) # Quite a lot!

# -> Now go trought the list and namualy delete on of the higly correlated indicators




#install.packages("FD")
library(FD)

temmp <- cca.env %>%
  pivot_wider(id_cols = `Site Name`, 
              names_from = Bin_num, 
              values_from = ZONE, 
              values_fn = list(ZONE = mean)) 

gow.dis<- temmp %>%
  select(-c(`Site Name`)) %>%
  as.data.frame() %>%
  gowdis(x=.) %>%
  as.matrix() %>%
  as.data.frame()

names(gow.dis) <- temmp$`Site Name`
row.names(gow.dis) <- temmp$`Site Name`


# Similarity of sites based on the ZONING of bins
ggcorrplot(gow.dis)

