# Daniela S. Gil
# Sense my FEUP, City and Mood comparison Analysis 

#Dataframes used are in SenseMyFeup.Rmd PArt IV 

#########################   #Sessions   ########################

# Sense my FEUP. 
smf_nsessions <- df_smf_all %>% 
  n_distinct(.$session_id)

# Sense my City. 
smc_nsessions <- df_smc_all %>% 
    n_distinct(.$session_id)
  
# Sense my Mood. 
smm_nsessions <- df_smm_all %>% 
  n_distinct(.$session_id)

  
#########################   #Userids   ########################

# Sense my FEUP.
smf_dailyuid <- df_smf_all %>% 
  n_distinct(.$daily_user_id)

# Sense my City.
smc_dailyuid <- df_smc_all %>% 
  n_distinct(.$daily_user_id)

# Sense my Mood.
smm_dailyuid <- df_smm_all %>% 
  n_distinct(.$daily_user_id)

#########################   #Points   ########################

# Sense my FEUP.
smm_points <- count(df_smf_all)
smc_points <- count(df_smc_all)
smm_points <- count(df_smm_all)

#########################   Table    ########################


V1 <- c("SenseMyCity", "SenseMyFEUP", "SenseMyMood")
V2 <- c(smf_nsessions, smc_nsessions, smm_nsessions)
V3 <- c(smf_dailyuid, smc_dailyuid, smm_dailyuid)
V4 <- c(smf_points, smc_points, smm_points)

df_sm_compare <- data.frame(matrix(c(V1,V2,V3,V4), ncol = 3))

colnames(df_sm_compare) <- c("Data","Sessions","Ids","Points")
