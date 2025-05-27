

## The code is the same as in "ML_estimation.R" only with a small modification in the dataset

ml_data <- genDesign(respondents = nrow(allcountries)) %>% 
  select(ID ="RID" ,series, starts_with(c("p_" , "X_") )) %>% 
  group_by(ID) %>%  mutate(lotteryNo = 1:n()) %>% ungroup  %>% 
  group_by(ID, series)  %>%  mutate(serieslotteryNo = 1:n())%>%  ungroup %>% 
  left_join(allcountries %>%  select(starts_with("series"), Country, ID, IDfarm, c("Age", "NbChildren", "Trust" , "FarmSize" , "LandOwned" , "IndivOwner", "pw" , "DateEnquete") ) , by="ID"  ) %>% 
  mutate(ChooseA = case_when(series==1 & serieslotteryNo <=series_1_question ~1 , series==2 & serieslotteryNo <=series2_question ~1,  series==3 & serieslotteryNo <=series3_question ~1,   TRUE ~ 0),
         X_LA =case_when(DateEnquete < "2010-03-12" & series==3 & X_LA==10 ~ 50, TRUE~X_LA)
        ) %>% 
  relocate(ID, lotteryNo,  series, serieslotteryNo, ends_with("question"), ChooseA)





ml_data_short    <- ml_data %>% 
  group_by(ID,series) %>% 
  mutate(AlwaysA=all(ChooseA==0) ,AlwaysB=all(ChooseA==1)) %>%
  filter((AlwaysA==TRUE & row_number()==n()) | (AlwaysB==TRUE & row_number()==1)  | ChooseA!=lag(ChooseA)|ChooseA!=lead(ChooseA)) %>% 
  ungroup %>% 
  arrange(ID, lotteryNo)



ml_data <- ml_data_short

ml_data_backup <- ml_data
mlmodels_EU = list()
mlmodels_EU_cov = list()

mlmodels_EX = list()
mlmodels_EX_cov = list()

mlmodels_PT = list()
mlmodels_PT_cov = list()

 for (c in c("All Countries",levels(ml_data_backup$Country)) ) {


  
if(c!="All Countries")  ml_data<- ml_data_backup[ml_data_backup$Country==c,] else ml_data <- ml_data_backup[ml_data_backup$Country!="BJR2014",]

covar <- c("Age", "NbChildren", "Trust" , "FarmSize" , "LandOwned" ) 

tempmodEU     <- mplLL(logLik =  eumodel ,method ="NR",start =c(r= 0.2),y =ChooseA, p_LA=p_LA , p_RA = p_RA, p_LB= p_LB , p_RB = p_RB , X_LA = X_LA , X_RA=X_RA, X_LB=X_LB, X_RB=X_RB, d=ml_data, cluster=ID)
tempmodEU_cov <- mplLL(covar_ll=covar , logLik =  eumodel ,method ="NR",start =c(r= 0.2 , Age=0, NbChildren=0 , Trust=0, FarmSize=0,LandOwned=0),y =ChooseA, p_LA=p_LA , p_RA = p_RA, p_LB= p_LB , p_RB = p_RB , X_LA = X_LA , X_RA=X_RA, X_LB=X_LB, X_RB=X_RB, d=ml_data, cluster=ID)


mlmodels_EU[[c]]  <- tempmodEU
mlmodels_EU_cov[[c]] <- tempmodEU_cov

tempmodEX <- (mplLL(logLik = expower ,method ="NR",start =c(alpha = 0.2, beta=0.2),y =ChooseA, p_LA=p_LA , p_RA = p_RA, p_LB= p_LB , p_RB = p_RB , X_LA = X_LA , X_RA=X_RA, X_LB=X_LB, X_RB=X_RB,d=ml_data ,cluster=ID))
mlmodels_EX[[c]]  <- tempmodEX
print(c)


try(tempmodEX_cov <- mplLL(covar_ll=covar ,logLik =expower, method ="NR",start =c(alpha = 0.2, alpha_Age=0, alpha_NbChildren=0,  alpha_Trust=0, alpha_FarmSize=0,alpha_LandOwned=0 ,beta=0.2 , beta_Age=0, beta_NbChildren=0,  beta_Trust=0, beta_FarmSize=0,beta_LandOwned=0 ),y =ChooseA, p_LA=p_LA , p_RA = p_RA, p_LB= p_LB , p_RB = p_RB , X_LA = X_LA , X_RA=X_RA, X_LB=X_LB, X_RB=X_RB,d=ml_data,cluster=ID))

try(mlmodels_EX_cov[[c]] <- tempmodEX_cov)


try(tempmodPT  <- mplLL(logLik= pt,method ="NR",start =c(sigma =0.2, lambda = 0.2, gamma = 0.2),y =ChooseA, p_LA=p_LA , p_RA = p_RA, p_LB= p_LB , p_RB = p_RB , X_LA = X_LA , X_RA=X_RA, X_LB=X_LB, X_RB=X_RB,d=ml_data,cluster=ID ) )
try( tempmodPT_cov  <- mplLL(covar_ll=covar, logLik= pt,method ="BHHH",start =c(sigma =0.2, sigma_Age=0, sigma_NbChildren=0,  sigma_Trust=0, sigma_FarmSize=0, sigma_LandOwned=0 ,
                                                                           lambda = 0.2, lambda_Age=0, lambda_NbChildren=0,  lambda_Trust=-0, lambda_FarmSize=0,lambda_LandOwned=0 ,
                                                                           gamma = 0.2 , gamma_Age=0, gamma_NbChildren=0,  gamma_Trust=-0.2, gamma_FarmSize=0,gamma_LandOwned=0) ,
                                                                           y =ChooseA, p_LA=p_LA , p_RA = p_RA, p_LB= p_LB , p_RB = p_RB , X_LA = X_LA , X_RA=X_RA, X_LB=X_LB, X_RB=X_RB,d=ml_data,cluster=ID ) )

try(mlmodels_PT[[c]]  <- tempmodPT)
try(mlmodels_PT_cov[[c]] <- tempmodPT_cov)


if(c=="BJR2014") {
  
  mlmodels_EU[["BJR2014 (weighted)"]] <- mplLL(logLik =  eumodel ,method ="NR",start =c(r= 0.2),y =ChooseA, p_LA=p_LA , p_RA = p_RA, p_LB= p_LB , p_RB = p_RB , X_LA = X_LA , X_RA=X_RA, X_LB=X_LB, X_RB=X_RB, d=ml_data, weights = pw, cluster=ID)
  mlmodels_EX[["BJR2014 (weighted)"]] <- mplLL(logLik =  expower ,method ="NR",start =c(alpha = 0.2, beta=0.2),y =ChooseA, p_LA=p_LA , p_RA = p_RA, p_LB= p_LB , p_RB = p_RB , X_LA = X_LA , X_RA=X_RA, X_LB=X_LB, X_RB=X_RB,d=ml_data, weights=pw ,cluster=ID)
  mlmodels_PT[["BJR2014 (weighted)"]] <- mplLL(logLik= pt,method ="NR",start =c(sigma =0.2, lambda = 0.2, gamma = 0.2),y =ChooseA, p_LA=p_LA , p_RA = p_RA, p_LB= p_LB , p_RB = p_RB , X_LA = X_LA , X_RA=X_RA, X_LB=X_LB, X_RB=X_RB,d=ml_data ,  weights=pw ,cluster=ID ) 

}


rm(list=ls(pattern="tempmod"))

}



## save models
save(list=(ls(pattern = "mlmodels_")), file = "storedResults/mlmodels_rob1.RData")


