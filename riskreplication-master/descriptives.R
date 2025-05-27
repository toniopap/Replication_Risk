

## Tables with frequencies of lottery choices


series1_freq <- sjt.xtab(allcountries$series_1_question, allcountries$Country, show.col.prc = TRUE , var.labels = c("Series No", "Country"), show.summary = F)


series2_freq <- sjt.xtab(allcountries$series2_question, allcountries$Country, show.col.prc = TRUE , var.labels = c("Series No", "Country"), show.summary = F)

series3_freq <- sjt.xtab(allcountries$series3_question, allcountries$Country, show.col.prc = TRUE , var.labels = c("Series No", "Country"), show.summary = F)



## Tables with frequencies of responses to likert scale questions on comprehension

crosstabs_underst <- allcountries %>% filter(Country!="BJR2014") %>% droplevels()



understanding_mean=crosstabs_underst %>% group_by(Country) %>% summarize(across(c("Difficult" ,"ChoiceRandom", "ToomanyLot"), function(x) mean(x,na.rm=T) ) ) %>%
  mutate(across(c("Difficult" ,"ChoiceRandom", "ToomanyLot") ,~paste0(Country,"            \n(",round(.,digits=2),")")))

nU <- c("Difficult" ,"ChoiceRandom", "ToomanyLot")

questions <- c("It was difficult to understand the task" , "My choices were random" , "There were too many different lotteries")

Understanding <- list()

for (q in 1:3) {


Understanding[[q]]  <-  sjt.xtab(crosstabs_underst$Country,crosstabs_underst[[nU[q]]],  show.row.prc = TRUE,
           var.labels = c("Country", questions[q] ) ,
           show.summary = F,
           value.labels = list(Country= c(understanding_mean[[nU[q]]]),
                               ChoiceRandom=c("Stongly Disagree", "Disagree", "Neither nor" , "Agree" , "Strongly Agree")),
           string.total = paste0("Total (",round(mean(crosstabs_underst[[nU[q]]],na.rm=TRUE),digits=2),")" ),
           file = paste0("tables/",nU[q],".html") )
}



## Socio-dempgraphic descriptive statistics by country

desc_soc <- allcountries %>%
  select(Country, Age, NbChildren, EducSup, Trust, FarmSize,LandOwned,IndivOwner) %>%
  group_by(Country) %>%
  summarise( across(everything(), .f = list(mean = mean, sd = sd), na.rm = TRUE) , N =n()) %>% 
  relocate(Country, N)

