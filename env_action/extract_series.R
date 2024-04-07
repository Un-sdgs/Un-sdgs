# Final SDG Relationships extract the individual series
library(tidyverse)

rel <- read_csv("action_env_synergies_sub_dataOver20_postConsult.csv")
ind <-which(rel$action_ind=="11.3.1" | rel$env_ind=="11.3.1")
rel <- rel[-ind,]
write_csv(rel,"action_env_synergies_aidan.csv")

rel <- read_csv("action_env_synergies_aidan.csv")
ind <- which(rel$action_sub_code== "EN_HAZ_TREATV" | rel$action_sub_code=="SG_SCP_POLINS" |
               rel$env_sub_code== "EN_HAZ_TREATV" | rel$env_sub_code=="SG_SCP_POLINS" |
               rel$action_sub_code== "SG_HAZ_CMRMNTRL" | rel$env_sub_code=="SG_HAZ_CMRMNTRL")

rel <- rel[-ind,]

write_csv(rel,"action_env_synergies_aidan.csv")

sdg <- c(rel$action_ind,rel$env_ind)
series <- c(rel$action_sub_code,rel$env_sub_code)
df <- data.frame(sdg,series)

df <- unique(df)
ind <- which(df$series=="No series code")
df <- df[-ind,]

for(i in 1:(dim(df)[1])){
  
  goo <- read_csv(paste(df$sdg[i],".csv",sep=""))
  ind <- which(goo$SeriesCode==df$series[i])
  goo <- goo[ind,]
  write_csv(goo,paste("./series/",df$series[i],".csv",sep=""))
  
}