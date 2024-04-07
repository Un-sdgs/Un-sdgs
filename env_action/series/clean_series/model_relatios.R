# model the relationships

rel <- read_csv("action_env_synergies_aidan.csv")
ind <- which(rel$action_sub_code == "No series code")
rel$action_sub_code[ind] <- rel$action_ind[ind]
gdp <- read_csv("./world_bank_data/world_bank_gdp_data.csv")
pop <- read_csv("./world_bank_data/world_bank_pop_data.csv")

coco <- read_csv("./world_bank_data/country-codes.txt")

num_obs <- vector("numeric",length=dim(rel)[1])
rho <- vector("numeric",length=dim(rel)[1])
cor_p_value <- vector("numeric",length=dim(rel)[1])
rsq <- vector("numeric",length=dim(rel)[1])
mod_coef <- vector("numeric",length=dim(rel)[1])
mod_p_value <- vector("numeric",length=dim(rel)[1])

for(i in 1:dim(rel)[1]){
  
  env <- read_csv(paste("clean_",rel$env_sub_code[i],".csv",sep=""))
  soc <- read_csv(paste("clean_",rel$action_sub_code[i],".csv",sep=""))
  env <- env[,1:4]
  names(env)[names(env)=="Value"] <- "env_value"
  soc <- soc[,1:4]
  names(soc)[names(soc)=="Value"] <- "soc_value"
  
  combn <- left_join(env,soc)
  combn$env_value <- gsub("<","" , combn$env_value)
  combn$soc_value <- gsub("<","" , combn$soc_value)
  combn$env_value <-as.numeric(combn$env_value)
  combn$soc_value <-as.numeric(combn$soc_value)
  
  
  # Now remove regions so only individual countries are present
  foo <- match(combn$GeoAreaCode,coco$M49)
  ind<- which(is.na(foo))
  if(length(ind)>0){
    combn <- combn[-ind,]
  }
  foo <- match(combn$GeoAreaCode,coco$M49)
  combn$region <- coco$`Region Name`[foo]
  combn <- drop_na(combn)
  
  combn <-left_join(combn,pop[,c(2,3,8)])
  combn <-left_join(combn,gdp[,c(2,3,8)])
  
  
  
  combn <- drop_na(combn)
  
  num_obs[i] <- dim(combn)[1]
  rho[i] <- round(cor(combn$env_value,combn$soc_value,use="complete.obs"),3)
  z <- cor.test(combn$env_value,combn$soc_value)
  cor_p_value[i] <- round(z$p.value,4)
  
  fit <- lm(log(env_value+1) ~ log(soc_value+1)+log(GDP)+log(Population)+region,data=combn)
  #fit <- lm(env_value ~ soc_value+log(GDP)+log(Population)+region,data=combn)
  fit <- lm(log(soc_value+.1) ~ log(env_value+.1)+log(GDP)+log(Population)+region,data=combn)
  summary(fit)
  
  x <- summary(fit)
  rsq[i] <- round(x$r.squared,3)
  mod_coef[i] <- round(x$coefficients[2,1],3)
  mod_p_value[i] <- round(x$coefficients[2,4],4)
  
}

rel$num_obs <- num_obs
rel$rho <- rho
rel$rsq <-rsq
rel$mod_coef <- mod_coef
rel$mod_p_value <- mod_p_value
rel$cor_p_value <- cor_p_value
rel$cor_sig <- rel$cor_p_value<.05
rel$mod_sig <- rel$mod_p_value <.05
write_csv(rel,"soc_action_env_synergies_results.csv")

ind <- which(rel$mod_sig==T)