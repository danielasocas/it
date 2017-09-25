# Daniela S. Gil
# Sense my FEUP, City and Mood comparison Analysis 

#Dataframes used are in SenseMyFeup.Rmd PArt IV 

#########################   Setting up  ########################

df_smf_all$time <- as.POSIXct(df_smf_all$seconds, 
                                              origin="1970-01-01")


df_smf_april16pt <- df_smf_all %>% 
  filter(month(time) == 4, year(time) == 2016)

df_smc_all$time <- as.POSIXct(df_smc_all$seconds, 
                              origin="1970-01-01")
df_smc_april16pt <- df_smc_all %>% 
  filter(month(time) == 4, year(time) == 2016)


df_smm_all$time <- as.POSIXct(df_smm_all$seconds, 
                              origin="1970-01-01")
df_smm_april16pt <- df_smm_all %>% 
  filter(month(time) == 4, year(time) == 2016)


#########################   #Sessions   ########################

# Sense my FEUP. 
smf_nsessions <-   df_smf_april16pt$session_id %>% 
  n_distinct() 

# Sense my City. 
smc_nsessions <-   df_smc_april16pt$session_id %>% 
    n_distinct()
  
# Sense my Mood. 
smm_nsessions <- df_smm_april16pt$session_id %>% 
  n_distinct()

  
#########################   #Userids   ########################

# Sense my FEUP.
smf_dailyuid <-  df_smf_april16pt$daily_user_id %>% 
  n_distinct()

# Sense my City.
smc_dailyuid <- df_smc_april16pt$daily_user_id %>% 
  n_distinct()

# Sense my Mood.
smm_dailyuid <-   df_smm_april16pt$daily_user_id %>% 
  n_distinct()

#########################   #Points   ########################

  
  
# Sense my FEUP.
smf_points <- df_smf_april16pt %>% 
  summarise(n())
smc_points <- df_smc_april16pt %>% 
   summarise(n())
smm_points <- df_smm_april16pt %>% 
   summarise(n())

#########################   Table    ########################


V1 <- c("SenseMyFEUP", "SenseMyCity", "SenseMyMood")
V2 <- c(smf_nsessions, smc_nsessions, smm_nsessions)
V3 <- c(smf_dailyuid, smc_dailyuid, smm_dailyuid)
V4 <- c(smf_points, smc_points, smm_points)

df_sm_compare <- data.frame(matrix(c(V1,V2,V3,V4), ncol = 4))

colnames(df_sm_compare) <- c("Data","Sessions","Ids","Points")
