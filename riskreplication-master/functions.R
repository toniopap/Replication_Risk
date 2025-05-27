###### Help Functions ####



switchfunction <- function(seriesno,design="tanaka") {
  switchingparas <-NULL
  for (rval in seq(from=-2, to=2, by=0.01)) {
    if (design=="tanaka") candidate<- genDesign(r = rval) %>% filter(series==seriesno)
    if (design=="HL") candidate<- genDesign_HL(r = rval) 
    switchingparas <-  rbind.data.frame(switchingparas, c(min(99,which(candidate$deltaEU<0)),rval) )
    
    names(switchingparas)<-c("Switch_point", "r_Value") 
  }
  
  summaryswitch <- switchingparas %>%  
    group_by(Switch_point) %>% 
    summarise(min(r_Value), max(r_Value) )
  
  return(summaryswitch)
  
}

##### Generate the design similar to Tarnaka et al. (2010) ####
genDesign <- function(respondents = 1, r =0.2 , err_sc=0.5) {
  nsets=33
  
  
  
  design <- data.frame(
    series= c(rep(1,times =12), rep(2,times=14) , rep(3 ,times =7) ) ,
    p_LA = c(rep(0.3,times =12), rep(0.9,times =14) ,  rep(0.5,times =7) ) ,
    X_LA = (c(rep(400,times=26) ,250, 40, rep(10, times=5) ))    ,
    X_RA =  c(rep(100,times =12), rep(300,times =14) ,  rep(-40,times =4) ,rep(-80,times =3) )  ,
    p_LB = c(rep(0.1,times =12), rep(0.7,times =14) ,  rep(0.5,times =7) ) ,
    X_LB = c(680,750,830,930,1060,1250,1500,1850,2200,3000,4000,6000,540,560,580,600,620,650,680,720,770,830,900,1000,1100,1300, rep(300, times=7)) , 
    X_RB = (c(rep(50,times=26) ,rep(-210, times=3) , -160,-160 , -140, -110 )))%>% 
    slice(rep(row_number(), respondents)) %>% 
    mutate(RID = rep(1:respondents, each=nsets),
           p_RA=1-p_LA, 
           p_RB=1-p_LB ,
           E_A=p_LA*X_LA+p_RA*X_RA , 
           E_B=p_LB*X_LB+p_RB*X_RB , 
           delta_E= E_A-E_B ,
           
           U_LA=if_else(X_LA>0 , X_LA^(r), -(-X_LA)^(r) ) ,
           U_RA=if_else(X_RA>0 , X_RA^(r), -(-X_RA)^(r) ) , 
           U_LB=if_else(X_LB>0 , X_LB^(r), -(-X_LB)^(r) ) ,
           U_RB=if_else(X_RB>0 , X_RB^(r), -(-X_RB)^(r) ) , 
           
           # U_LA=if_else(X_LA>0 , X_LA^(1-r)/(1-r), -(-X_LA)^(1-r)/(1-r) ) ,
           # U_RA=if_else(X_RA>0 , X_RA^(1-r)/(1-r), -(-X_RA)^(1-r)/(1-r) ) , 
           # U_LB=if_else(X_LB>0 , X_LB^(1-r)/(1-r), -(-X_LB)^(1-r)/(1-r) ) ,
           # U_RB=if_else(X_RB>0 , X_RB^(1-r)/(1-r), -(-X_RB)^(1-r)/(1-r) ) , 
           EU_A= p_LA*U_LA + p_RA*U_RA ,
           EU_B= p_LB*U_LB + p_RB*U_RB ,
           
           deltaEU = EU_A-EU_B,
           
           EU_SaveA = E_A^r,
           EU_SaveB = E_B^r , 
           
           epsilonA=rgumbel(n(),0,err_sc),
           epsilonB=rgumbel(n(),0,err_sc),
           EV_A=EU_A+epsilonA,
           EV_B=EU_B+epsilonB ,
           r=r
    ) 
  design$Expvalchoice<- max.col(design[,c("E_A","E_B")])
  design$detchoice <- max.col(design[,c("EU_A","EU_B")])
  design$pref1 <- max.col(design[,c("EV_A","EV_B")])
  
  
  comment(design)<- paste0("this dataset is generated with an r value of ", r, " and consists of ", respondents, " respondent(s). ", "The variance of the error is scaled by ",err_sc ) 
  
  attr(design,"respondents")<- respondents
  attr(design,"r value")<- r
  
  return(design)
  
}  


###### Generate Holt Laury Design ####
genDesign_HL <- function(respondents = 1, r =0.2 , err_sc=0.5) {
  nsets=10
  
  
  
  design <- data.frame(
    series= c(rep(1,times =10)) ,
    p_LA = seq(from=0.1, to= 1 , by = 0.1) ,
    X_LA = rep(2,times=10)    ,
    X_RA =  rep(1.6,times =10)  ,
    p_LB = seq(from=0.1, to= 1 , by = 0.1) ,
    X_LB = rep(3.85,times =10) , 
    X_RB = rep(0.1,times =10))%>% 
    slice(rep(row_number(), respondents)) %>% 
    mutate(RID = rep(1:respondents, each=nsets),
           p_RA=1-p_LA, 
           p_RB=1-p_LB ,
           E_A=p_LA*X_LA+p_RA*X_RA , 
           E_B=p_LB*X_LB+p_RB*X_RB , 
           delta_E= E_A-E_B ,
           
           U_LA=if_else(X_LA>0 , X_LA^(1-r)/(1-r), -(-X_LA)^(1-r)/(1-r) ) ,
           U_RA=if_else(X_RA>0 , X_RA^(1-r)/(1-r), -(-X_RA)^(1-r)/(1-r) ) , 
           U_LB=if_else(X_LB>0 , X_LB^(1-r)/(1-r), -(-X_LB)^(1-r)/(1-r) ) ,
           U_RB=if_else(X_RB>0 , X_RB^(1-r)/(1-r), -(-X_RB)^(1-r)/(1-r) ) , 
           EU_A= p_LA*U_LA + p_RA*U_RA ,
           EU_B= p_LB*U_LB + p_RB*U_RB ,
           
           deltaEU = EU_A-EU_B,
           
           EU_SaveA = E_A^(1-r)/(1-r),
           EU_SaveB = E_B^(1-r)/(1-r) , 
           
           epsilonA=rgumbel(n(),0,err_sc),
           epsilonB=rgumbel(n(),0,err_sc),
           EV_A=EU_A+epsilonA,
           EV_B=EU_B+epsilonB ,
           r=r
    ) 
  design$Expvalchoice<- max.col(design[,c("E_A","E_B")])
  design$detchoice <- max.col(design[,c("EU_A","EU_B")])
  design$pref1 <- max.col(design[,c("EV_A","EV_B")])
  
  
  comment(design)<- paste0("this dataset is generated with an r value of ", r, " and consists of ", respondents, " respondent(s). ", "The variance of the error is scaled by ",err_sc ) 
  
  attr(design,"respondents")<- respondents
  attr(design,"r value")<- r
  
  return(design)
  
}  


switchfunction <- function(seriesno,design="tanaka") {
  switchingparas <-NULL
  for (rval in seq(from=-2, to=2, by=0.01)) {
    if (design=="tanaka") candidate<- genDesign(r = rval) %>% filter(series==seriesno)
    if (design=="HL") candidate<- genDesign_HL(r = rval) 
    switchingparas <-  rbind.data.frame(switchingparas, c(min(99,which(candidate$deltaEU<0)),rval) )
    
    names(switchingparas)<-c("Switch_point", "r_Value") 
  }
  
  summaryswitch <- switchingparas %>%  
    group_by(Switch_point) %>% 
    summarise(min(r_Value), max(r_Value) )
  
  return(summaryswitch)
  
}

#### Customize textreg output  ####
maxlik_texreg <- function(modelname){
  
  obj <- createTexreg(coef.names = names(modelname[["estimate"]]), coef = modelname[["estimate"]], se = modelname[["clSE"]] ,
                      gof.names =c("LL (NULL)", "LL (Converged)" , "Num. obs.", "Num. resp.", "BIC", "AIC", "AICc" , "Pseudo R2") , gof = modelname[["additional"]] , gof.decimal = c(T,T,F,F,T,T,T,T) )
  
}  

## functions used to add a second axis taken from https://github.com/tidyverse/ggplot2/issues/3171#issuecomment-614084750

guide_axis_label_trans <- function(label_trans = identity, ...) {
  axis_guide <- guide_axis(...)
  axis_guide$label_trans <- rlang::as_function(label_trans)
  class(axis_guide) <- c("guide_axis_trans", class(axis_guide))
  axis_guide
}

guide_train.guide_axis_trans <- function(x, ...) {
  trained <- NextMethod()
  trained$key$.label <- x$label_trans(trained$key$.label)
  trained
}



#function to include all data once again, used for by tables

CreateAllFacet <- function(df, col){
  ## here France2013 needs to be excluded if we would like these graphs
  df$facet <- df[[col]]
  temp <- df %>% dplyr::filter(facet!="BJR2014")
  temp$facet <- "All Countries"
  merged <-rbind(temp, df)
  
  # ensure the facet value is a factor
  merged[[col]] <- as.factor(merged[[col]])
  
  return(merged)
}
