res_soc_env <- read_csv("soc_env_synergies_results.csv")
ind <- which(res_soc_env$mod_sig==T)
res_soc_env <- res_soc_env[ind,]
res_act_env <- read_csv("action_env_synergies_results.csv")
res_842_env <- read_csv("842_env_synergies_results.csv")

foo <- rbind(res_soc_env,res_act_env,res_842_env)
