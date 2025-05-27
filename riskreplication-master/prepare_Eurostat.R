

### Uncomment the next lines to read in data from Eurostat database. Else locally stored data is used.

# farmdata <- get_eurostat("ef_m_farmang") %>% filter(time=="2016-01-01", grepl("DE$|IT$|ES$|NL$|HR$|FR$|SE$|AT$|SI$|PL$",geo), across(c(so_eur), function(x) x=="TOTAL" ) , indic_agr=="FARM_NR" )%>% 
# label_eurostat(fix_duplicated = T)

#saveRDS(farmdata, "Eurostat/farmdata.RDS")


farmd <- readRDS("Eurostat/farmdata.RDS")  %>% 
  mutate(across(where(is.character), ~na_if(., "Not applicable")) , 
         geo=recode(geo, "Germany (until 1990 former territory of the FRG)" = "Germany" , "France" = "France_I" , .default = geo) )


farmdata<- farmd %>% filter(geo=="France_I") %>% mutate(geo=recode(geo,  "France_I" = "France_II" , .default = geo) ) %>% 
  bind_rows( farmd)
  

# create separate dataframes for Eurostat variables FarmSize, Age, Sex

table_Size <- farmdata %>%
  filter( across(c(age, farmtype, sex), function(x) x=="Total" )   ) %>% select(category_FarmSize= "agrarea", Country ="geo", n="values")
  

table_Age <- farmdata %>%
  filter( across(c(agrarea, farmtype,sex), function(x) x=="Total" ), !is.na(age)   ) %>% select(category_Age= "age", Country ="geo", n="values")

table_Sex <- farmdata %>%
  filter( across(c(agrarea, age, farmtype), function(x) x=="Total" ) , !is.na(sex) ) %>% select(category_Sex= "sex", Country ="geo", n="values")




#prepare categories equivalent to Eurostat data

categories <- allcountries %>% filter(Country!="BJR2014" ) %>%
  select(CID,Country, FarmSize,Age,Sex)  %>% 
  mutate(FarmSize =FarmSize*100 , 
         category_FarmSize=cut(FarmSize, breaks=c(-Inf, 0, 2, 5,10,20,30,50,100,Inf), ordered_result =T,
                                                        labels=c("Zero ha","Less than 2 ha","From 2 to 4.9 ha" , "From 5 to 9.9 ha" , "From 10 to 19.9 ha",
                                                                 "From 20 to 29.9 ha" , "From 30 to 49.9 ha", "From 50 to 99.9 ha" , "100 ha or over" )),
         category_Age  =cut(Age, breaks=c(-Inf, 25, 35, 40,45,55,65,Inf), ordered_result =T,
                            labels=c("Less than 25 years","From 25 to 34 years","From 35 to 39 years" , "From 40 to 44 years" , "From 45 to 54 years",
                                     "From 55 to 64 years" , "65 years or over" )),
         category_Sex =Sex
         
         )



## Prepare tables comparing Eurostat Data with samples

table_Size_ours <- categories %>% filter(!is.na(FarmSize)) %>%  group_by(Country, category_FarmSize) %>% summarize(n=n()) %>% ungroup()  %>%
  bind_rows(table_Size,.id = c("survey"))  %>% 
  group_by(survey, Country) %>% 
  mutate(freq = if_else( survey==1  , round(n/sum(n,na.rm = T)*100, digits =1),  round(n/(0.5*sum(n,na.rm = T))*100, digits = 1)       ) , 
         survey = recode(survey, "1" = "Survey", "2" = "eurostat") )  %>%
  arrange(survey, Country) %>% 
  pivot_wider(names_from = survey , values_from = c("n", "freq") ) %>%
  mutate(freq_Survey = replace(freq_Survey, is.na(freq_Survey) & category_FarmSize == "Total", sum(freq_Survey,na.rm = T)), freq_Survey=tidyr::replace_na(freq_Survey,0)) %>%
  ungroup() %>% 
  mutate(category_FarmSize=factor(fct_relevel(category_FarmSize,"Zero ha" , "Less than 2 ha", "From 2 to 4.9 ha" , "From 5 to 9.9 ha","From 10 to 19.9 ha",  "From 20 to 29.9 ha","From 30 to 49.9 ha","From 50 to 99.9 ha","100 ha or over"), ordered = T)) %>% 
  select(-starts_with("n_")) %>% 
  arrange(Country, category_FarmSize) %>% 
    pivot_wider(names_from = Country, values_from = c("freq_eurostat" , "freq_Survey") , names_glue = "{Country}_{.value}") %>% 
  relocate(category_FarmSize,sort(colnames(.))) %>% 
  select(-France_II_freq_eurostat )


table_Age_ours <- categories %>% filter(!is.na(Age)) %>% select(-category_FarmSize) %>%   group_by(Country, category_Age) %>% summarize(n=n()) %>% ungroup()  %>%
  bind_rows(table_Age,.id = c("survey"))  %>% 
  group_by(survey, Country) %>% 
  mutate(freq = if_else( survey==1  , round(n/sum(n,na.rm = T)*100, digits =1),  round(n/(0.5*sum(n,na.rm = T))*100, digits = 1)       ) , 
         survey = recode(survey, "1" = "Survey", "2" = "eurostat") )  %>%
  arrange(survey, Country) %>% 
  pivot_wider(names_from = survey , values_from = c("n", "freq") ) %>%
  mutate(freq_Survey = replace(freq_Survey, is.na(freq_Survey) & category_Age == "Total", sum(freq_Survey,na.rm = T)), freq_Survey=tidyr::replace_na(freq_Survey,0)) %>%
  ungroup() %>% 
  mutate(category_Age=factor(fct_relevel(category_Age,"Less than 25 years","From 25 to 34 years","From 35 to 39 years" , "From 40 to 44 years" , "From 45 to 54 years",
                                                                              "From 55 to 64 years" , "65 years or over" ), ordered = T)) %>% 
  select(-starts_with("n_")) %>% 
  arrange(Country, category_Age) %>% 
  pivot_wider(names_from = Country, values_from = c("freq_eurostat" , "freq_Survey") , names_glue = "{Country}_{.value}") %>% 
  relocate(category_Age,sort(colnames(.))) %>% 
  select(-France_II_freq_eurostat )



table_Sex_ours <- categories %>% filter(!is.na(Sex)) %>% select(-c("category_FarmSize" , "category_Age")) %>%   group_by(Country, category_Sex) %>% summarize(n=n()) %>% ungroup()  %>%
  bind_rows(table_Sex,.id = c("survey"))  %>% 
  group_by(survey, Country) %>% 
  mutate(freq = if_else( survey==1  , round(n/sum(n,na.rm = T)*100, digits =1),  round(n/(0.5*sum(n,na.rm = T))*100, digits = 1)       ) , 
         survey = recode(survey, "1" = "Survey", "2" = "eurostat") )  %>%
  arrange(survey, Country) %>% 
  pivot_wider(names_from = survey , values_from = c("n", "freq") ) %>%
  mutate(freq_Survey = replace(freq_Survey, is.na(freq_Survey) & category_Sex == "Total", sum(freq_Survey,na.rm = T)), freq_Survey=tidyr::replace_na(freq_Survey,0)) %>%
  ungroup() %>% 
    select(-starts_with("n_")) %>% 
  arrange(Country, category_Sex) %>% 
  pivot_wider(names_from = Country, values_from = c("freq_eurostat" , "freq_Survey") , names_glue = "{Country}_{.value}") %>% 
  relocate(category_Sex,sort(colnames(.))) %>% 
  select(-France_II_freq_eurostat )

