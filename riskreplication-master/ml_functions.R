

mplLL<-function(d, covar_ll=NULL, demean=TRUE, weights=1, cluster=NULL, ...) {



  
  
  
    
if(!is.null(covar_ll)) d <-d[complete.cases(d[covar_ll]),]   ## delete all cases with NA in covariates
   

      
if(demean==TRUE) {
  
  for (var in covar_ll) {
   d[[var]]<-scale(d[[var]], scale = FALSE) ## demean covariates
     }  
  
print(summary(d[covar_ll]))
    
}   
 
attach(d)
on.exit(detach(d))  

.covar_ll <- covar_ll

.weights<-weights

.d = d

  
  output <- maxLik(... , covar_ll = .covar_ll, weights=.weights)
  

if(!is.null(cluster)) {   
output[["clSE"]] <- sqrt(diag(vcovCL(output,cluster = cluster)))
}
g<-list(...)


Nullmodel <- maxLik(logLik =  Nullm ,method ="NR",start =c(0), y=g$y)

llNull <- Nullmodel[["maximum"]]

n = length(output$gradientObs)
k = length(output$estimate)
ll = output$maximum

nResp = length(unique(d$ID))

BIC <- -2*ll+k*log(n)
AIC <-   -2*ll + 2*k
AICc <- AIC +(2*k^2+2*k)/(n-k-1) 
PseudoR2  <- 1-(ll/llNull)


output[["additional"]] <- c(LogLik_NULL=llNull, LogLik_conv =ll, Nobs = n, Nresp=nResp, BIC=BIC, AIC=AIC,AICc=AICc, PseudoR2=PseudoR2 )
  
  
  print(summary(output))
  print(summary(Nullmodel))
  
 return(output) 

}

## Expected Utility model

eumodel <- function(pars, y, p_LA  , p_RA, p_LB , p_RB , X_LA , X_RA, X_LB, X_RB , covar_ll, weights) {
  

  if (!is.null(covar_ll)) {
    
    cvars <-cbind(1,as.matrix(as.data.frame(mget(covar_ll , envir = as.environment("d")))))
    
    r <-cvars%*%pars
    

    }   else {r <-pars}
  
  
  
  
  U <-function(x) {ifelse(x>0,
                          x^(r),
                          -(-x)^(r))
  }
  
  lp <-  (p_LA*U(X_LA) +p_RA*U(X_RA) )  - (p_LB*U(X_LB) + p_RB*U(X_RB)  )
  
  
  
  
  return(
    
    (weights)*(y*log(pnorm(lp))+(1-y)*log(1-pnorm(lp)))
  
  )
  
}

## Exponential power model

expower <- function(pars, y, p_LA , p_RA, p_LB , p_RB , X_LA , X_RA, X_LB, X_RB ,  covar_ll, weights) {
 
  if (!is.null(covar_ll)) {
    
    cvars <-cbind(1,as.matrix(as.data.frame(mget(covar_ll , envir = as.environment("d")))))
    
    al <-cvars%*%pars[c("alpha",paste("alpha",covar_ll,sep = "_"))]
    be <-cvars%*%pars[c("beta",paste("beta",covar_ll,sep = "_"))]
    
    
  }   else {
        al <-pars[1]
        be <- pars[2]
  }
  
  
  U=function(x) {
    ifelse(x>0, 
           (1-exp(-be*x^(al)))/be ,
           (1-exp(be*(-x)^(al)))/be)
    
  }
  
  lp <-(p_LA*U(X_LA) + p_RA*U(X_RA) ) - (p_LB*U(X_LB)  + p_RB*U(X_RB) )
  
  return((
    
    (weights)*(y*log(pnorm(lp))+(1-y)*log(1-pnorm(lp)))
  )
  )
}


##prospect theory model

pt <- function(pars, y, p_LA , p_RA, p_LB , p_RB , X_LA , X_RA, X_LB, X_RB,  covar_ll, weights) {
  
 
  if (!is.null(covar_ll)) {
    
    cvars <-cbind(1,as.matrix(as.data.frame(mget(covar_ll , envir = as.environment("d")))))
    
    sigma <-cvars%*%pars[c("sigma",paste("sigma",covar_ll,sep = "_"))]
    lambda <-cvars%*%pars[c("lambda",paste("lambda",covar_ll,sep = "_"))]
    gamma <-cvars%*%pars[c("gamma",paste("gamma",covar_ll,sep = "_"))]
    
    
  }   else {
  
  sigma <- pars[1]
  lambda <- pars[2]
  gamma <- pars[3]
  } 
  
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
  
  
  
  lp <-(EU(a=X_LA,b=X_RA, p=p_LA) - EU(a=X_LB,b=X_RB, p=p_LB)
        
  )
  
  return((
    
    (weights)*(y*log(pnorm(lp))+(1-y)*log(1-pnorm(lp)))
  )
  )
}


Nullm <- function(pars,y){
  
  cons<-pars

  
  
 
  
  
  return((y*log(pnorm(cons))+(1-y)*log(1-pnorm(cons))) ) 
    
  
}


eumodel2 <- function(pars, y, p_LA  , p_RA, p_LB , p_RB , X_LA , X_RA, X_LB, X_RB , covar_ll, weights, Ferchner=FALSE) {
  
  
  if (!is.null(covar_ll)) {
    
    cvars <-cbind(1,as.matrix(as.data.frame(mget(covar_ll , envir = as.environment("d")))))
    
    r <-cvars%*%pars[c("r_const",paste("r",covar_ll,sep = "_"))]
    ferchner <- pars["ferchner"]
    
    
  }   
  else {r <-pars["r"]
            ferchner<- pars["ferchner"]
  
            }
  
 # if (Ferchner==TRUE) {
 #   r <- rbind(r,"noise")
 #   
 # } else(noise<-1)
  
  
  U <-function(x) {ifelse(x>0,
                          x^(r),
                          -(-x)^(r))
  }
  
  lp <-  ((p_LA*U(X_LA) +p_RA*U(X_RA) )  - (p_LB*U(X_LB) + p_RB*U(X_RB)  ))/ferchner
  
  
  
  
  return(
    
    (weights)*(y*log(pnorm(lp))+(1-y)*log(1-pnorm(lp)))
    
  )
  
}


#testmodel<-mplLL(covar_ll = c("Age", "NbChildren"), logLik =  eumodel2 ,method ="NR",start =c(r_const= 0.2,r_Age=0, r_NbChildren=0, ferchner=1),
#                 y =ChooseA, p_LA=p_LA , p_RA = p_RA, p_LB= p_LB , p_RB = p_RB , X_LA = X_LA , X_RA=X_RA, X_LB=X_LB, X_RB=X_RB, d=ml_data, cluster=ID)


#testmodel2<-mplLL( logLik =  eumodel2 ,method ="NR",start =c(r= 0.2, ferchner=1), fixed="ferchner",
#                 y =ChooseA, p_LA=p_LA , p_RA = p_RA, p_LB= p_LB , p_RB = p_RB , X_LA = X_LA , X_RA=X_RA, X_LB=X_LB, X_RB=X_RB, d=ml_data, cluster=ID)
