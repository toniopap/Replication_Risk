### If you run this file, all code will be run, the appendix generated, the tables and graphs for the main text produced, the models estimated etc.
## Note that the data has been prepared before to assure anonymity of respondents. This code will not be made publicly available.


### read preamble to load packages, functions, data. ####
## If you run individual r scripts, make sure the preamble is read in before



rm(list=ls())                                                     ## This is the beginning

source("preamble.R")                                              ### read preamble to load packages, functions, data.


sessionInfo()



##  WARNING: THE NEXT SCIPTS TAKE VERY LONG TO RUN, THEREFORE RESULTS ARE PRECALCULATED AND SAVED IN "storedResults/". Uncomment if you want to run them anyways 

 # source("midpoint_functions.R")                                   ## functions to estimate mid points: 
 # 
 # source("ML estimation.R")                                        ## Estimate all structural models with maximum likelihood method
 # 
 # source("ML estimation_robustness1_drop_obs.R")                   ## Robustness test 1: Keep only observations next to the switching point 
 # 
 # source("ML estimation_robustness2_drop_speeder.R")               ## Robustness test 2: Drop all respondents who completed the questionnaire unter six minutes
 # 
 # source("ML estimation_robustness3_drop_uncertain.R")             ## Robustness test 3: Drop all respondents who stated that they were uncertain in their lottery choices or chose randomly


## FROM HERE ONWARDS, NO LONGER COMPUTATIONS ARE NECESSARY


source("mid-point.R")                                              ## Use the midpoint table generated in "midpoint_functions.R" and calculate individual CPT parameters




source("tables_manuscript.R")                                      ## Create tables and figures which are used in the main text. The results are stored in the folder "manuscript_files"


source("descriptives.R")                                           ## Make some descriptive statistics

source("graph_errorML.R")                                          ## Create Error Plot for ML Estimates

source("prepare_Eurostat.R")                                       ## Download and prepare Eurostat data and compare with sample data






