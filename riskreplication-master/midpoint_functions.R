## midpoint technique 

# We derive thresholds by calculating utility functions by for several potential values of parameters. We then derive the choices which would be consistent with the parameter values 

#I would choose Lottery A from Row 1 to Row




a<-genDesign()    # This function generates the lottery series

## The following function corresponds to the CPT utility function and is constructed similar to the one from the ML estimations

pt2 <- function(pars, y, p_LA , p_RA, p_LB , p_RB , X_LA , X_RA, X_LB, X_RB, data ,testing=FALSE) {
  
  attach(data)
  on.exit(detach(data))

    
  sigma <- pars[1]
  lambda <- pars[2]
  gamma <- pars[3]
  
  
  U = function(x) {
    ifelse(x>0 , x^sigma, ifelse(x<0, -lambda*(-x)^sigma, 0))
  }  
  
  Omega = function(p) {exp(-(-log(p))^gamma)}
  
  EU = function(a,b,p) {
    
    ifelse(a*b>0, 
           Omega(p)*U(a)+ (1-Omega(p))*U(b) ,
           Omega(p)*U(a)+ Omega(1-p)*U(b)
    )   
    
  }
  

  lp <-data.frame(Util = EU(a=X_LA,b=X_RA, p=p_LA) - EU(a=X_LB,b=X_RB, p=p_LB), series) %>% 
    group_by(series) %>% 
    mutate( Noseries=1:n(),
      A_chosen=Util>0 , switch=A_chosen!=lead(A_chosen),
      switch2=replace_na(switch,FALSE),
      alwaysA=if_else(all(Util>0),TRUE,FALSE),
      alwaysB=if_else(all(Util<0),TRUE,FALSE),
      switch3=if_else(alwaysA==TRUE,"always A",if_else(alwaysB==TRUE,"always B", as.character(switch2)))
          ) %>% ungroup

switchpoints<- lp %>%   select(series,Noseries,switch3) %>% filter(switch3 %in% c("TRUE","always A","always B") ) %>% filter(!duplicated(series)) %>% 
                        mutate(point=case_when(
                          switch3==TRUE ~ Noseries, 
                          switch3=="always A" ~ as.integer(0), 
                          switch3=="always B" ~ as.integer(99)))
  
  
if(testing==TRUE)  return(list(lp,switchpoints))
  else{c(pars,S1=switchpoints[1,"point"], S2=switchpoints[2,"point"], S3=switchpoints[3,"point"])}
}


## this function generates a data frame with all potential parameter values which we want to estimate models on. Here, the intervalls and the range can be increased to get more precise values  

getPara <- function(sigma=seq(from=0, to=1.5, by=.5), lambda=seq(from=0, to=5, by=0.5),gamma=seq(from=0, to=1.5, by=0.5) ) {
  

  n <- max(length(sigma), length(lambda), length(gamma))
  length(sigma) <- n                      
  length(lambda) <- n
  length(gamma) <- n
  
return(na.omit(expand.grid(data.frame(sigma,lambda,gamma))))
  
}


## Here we just test the two functions

pt2(pars=c(sigma=0.3,lamdba=1.3, gamma=0.2), p_LA=p_LA , p_RA = p_RA, p_LB= p_LB , p_RB = p_RB , X_LA = X_LA , X_RA=X_RA, X_LB=X_LB, X_RB=X_RB, data=a)


testingpoints <-getPara(sigma=seq(from=0.05, to=2.5, by=0.025), lambda=seq(from=0.05, to=12, by=0.05),gamma=seq(from=0.05, to=2.5, by=0.025))


## The next call is the main part. For each row of the parameter value dataframe, we calculate the potential choice. This takes very long

r2= apply(testingpoints, MARGIN=1, FUN = function(x) pt2(x, p_LA=p_LA , p_RA = p_RA, p_LB= p_LB , p_RB = p_RB , X_LA = X_LA , X_RA=X_RA, X_LB=X_LB, X_RB=X_RB, data=a))


## now we make it one dataframe and save the result on disc
r2=do.call(rbind.data.frame,r2 )
saveRDS(r2, "thresholds.RDS")

summary(r2)





