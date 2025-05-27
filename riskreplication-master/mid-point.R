

### read the threshold database which is generated in "midpoint_functions.R"

thresholds <- readRDS("storedResults/thresholds.RDS")


## thresholdtable produces the midpoints, i.e. the middle between the two extremes

thresholdtable <-thresholds %>% group_by(S1.point,S2.point,S3.point) %>% 
summarise(across(c("gamma","lambda","sigma" ), .f = list(round=function(x) x, Min=min, Max=max, Mid=function(x) (max(x)+min(x))/2 ) ), S1.point, S2.point, S3.point) %>% ungroup() %>% 
  select(ends_with(c("mid","point")) ) %>% distinct()
 
## We link the mid-points to respondents

midpoints <- thresholdtable %>% 
  mutate(series_1_question = recode(S1.point, `99`=12L,.default = S1.point),
         series2_question  = recode(S2.point, `99`=14L,.default = S2.point),
         series3_question  = recode(S3.point, `99`=7L,.default = S3.point)) %>% 
  inner_join(allcountries %>% select(starts_with("series"),ID , c("Age", "NbChildren", "Trust" , "FarmSize" , "LandOwned" , "IndivOwner", "Country")),
             by=c("series_1_question", "series2_question", "series3_question")) 




### Now we regress the estimated mid-points on covariates

MidpointRegressions <- list()

midpoints_backup <- midpoints

for (c in c("All Countries",levels(midpoints_backup$Country)) ) {

if(c!="All Countries")  midpoints<- midpoints_backup[midpoints_backup$Country==c,] else midpoints <- midpoints_backup[midpoints_backup$Country!="BJR2014",]

print(c)

MidpointRegressions[["Gamma"]][[c]]  <- (lm(gamma_Mid~ scale(Age,scale = FALSE) + scale(NbChildren,scale=FALSE) + scale(Trust,scale=FALSE) + scale(FarmSize,scale=FALSE) + scale(LandOwned,scale=FALSE) + scale(IndivOwner,scale=FALSE) ,data = midpoints))
MidpointRegressions[["Lambda"]][[c]] <- (lm(lambda_Mid~ scale(Age,scale = FALSE) + scale(NbChildren,scale=FALSE) + scale(Trust,scale=FALSE) + scale(FarmSize,scale=FALSE) + scale(LandOwned,scale=FALSE) + scale(IndivOwner,scale=FALSE) ,data = midpoints))
MidpointRegressions[["Sigma"]][[c]]  <- (lm(sigma_Mid~ scale(Age,scale = FALSE) + scale(NbChildren,scale=FALSE) + scale(Trust,scale=FALSE) + scale(FarmSize,scale=FALSE) + scale(LandOwned ,scale=FALSE)+ scale(IndivOwner,scale=FALSE) ,data = midpoints))

}



midpoints <- midpoints_backup

        