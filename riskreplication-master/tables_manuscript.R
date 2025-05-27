
## This script produces the tables for the manuscript

#### Socio-demographics descriptive table #### 


allcountries %>% filter(Country!="BJR2014") %>%
  # expss::apply_labels(Age = "Age of the subject (years)",
  #   NbChildren = "Number of children in the household",
  #   EducSup = "Dummy if education level beyond secondary school",
  #   Trust = "Dummy if self-reported as trusting other people" ,
  #   FarmSize = "Total arable area (100 ha) ",
  #   LandOwned = "Proportion of land out of the arable area which is owned",
  #   IndivOwner = "Dummy if the farm is a sole proprietorship or a society with only one associate" ) %>%
  select(Age, NbChildren, EducSup, Trust, FarmSize,LandOwned,IndivOwner) %>%
  descr( show = c("label" , "n" , "NA.prc" , "mean", "sd"),out = "viewer" , file="manuscript_files/tab3_sociodem.html")






#### ML Models ####

load("storedResults/mlmodels.RData")


mlmodels_EU_html <- lapply(mlmodels_EU,maxlik_texreg)  
mlmodels_EX_html <- lapply(mlmodels_EX,maxlik_texreg)  
mlmodels_PT_html <- lapply(mlmodels_PT,maxlik_texreg)

sh <- list(  digits = c(3) , ci.force = TRUE , ci.test=NA , stars = numeric(0)  ) ## general settings used in all tables


do.call(htmlreg, c(list(l=mlmodels_EU_html , file= "manuscript_files/mlestimatesEU.html" ,caption = "Results over all countries with Expected Utility model"),sh))  

do.call(htmlreg, c(list(l=mlmodels_EX_html, file= "manuscript_files/mlestimatesEX.html", caption = "Results over all countries with Expected Utility model"),sh))  

do.call(htmlreg, c(list(l=mlmodels_PT_html, file= "manuscript_files/mlestimatesPT.html" , caption = "Results over all countries with Expected Utility model"),sh))  


 
# uncomment if tables should be printed to word
 
# do.call(wordreg, c(list(l=mlmodels_EU_html , file= "manuscript_files/mlestimatesEU.doc" ,caption = "Results over all countries with Expected Utility model"),sh))  
# 
# do.call(wordreg, c(list(l=mlmodels_EX_html, file= "manuscript_files/mlestimatesEX.doc", caption = "Results over all countries with Expected Utility model"),sh))  
# 
# do.call(wordreg, c(list(l=mlmodels_PT_html, file= "manuscript_files/mlestimatesPT.doc" , caption = "Results over all countries with Expected Utility model"),sh))  


## Midpoint figure

# calculate mean and median of midpoints to include in graphic

meanmed<- midpoints %>% 
  group_by(Country) %>% 
  summarize(across(everything(c("gamma_Mid", "lambda_Mid" , "sigma_Mid")), .f = list(mean = mean, med = median), na.rm = TRUE)) %>% 
  mutate(gamma_Mid  = paste0(as.character(round(gamma_Mid_mean,digits=1)) , "/",as.character(round(gamma_Mid_med,digits=1))) , 
         lambda_Mid  = paste0(as.character(round(lambda_Mid_mean,digits=1)) , "/",as.character(round(lambda_Mid_med,digits=1))),
         sigma_Mid  = paste0(as.character(round(sigma_Mid_mean,digits=1)) , "/",as.character(round(sigma_Mid_med,digits=1)))
  )

graphs <- list()

ucnames<- c(gamma_Mid="\u03b3", lambda_Mid="\u03bb" ,sigma_Mid="\u03c3")

for (par in c("gamma_Mid", "lambda_Mid" , "sigma_Mid")) {
  

  p  = ggplot(midpoints_backup, aes(x = Country, y = get(par))) + 
    ## add half-violin from {ggdist} package
    ggdist::stat_halfeye(
      ## custom bandwidth
      adjust = .5, 
      ## adjust height
      width = .6, 
      ## move geom to the right
      justification = -.2, 
      ## remove slab interval
      .width = 0, 
      point_colour = NA
    ) + 
    geom_boxplot(
      width = .12, 
      ## remove outliers
      outlier.color = NA ## `outlier.shape = NA` works as well
    ) +
    # add dot plots from {ggdist} package
    ggdist::stat_dots(
      ## orientation to the left
      side = "left",
      ## move geom to the left
      justification = 1.1,
      ## adjust grouping (binning) of observations
      binwidth = .00025
    ) +
    ## remove white space on the left
    coord_cartesian(xlim = c(0.5, NA)) +
    #theme_stata(scheme = "s2color") +
    #scale_colour_stata("s1color") +
    labs(title="",
         x ="", y = ucnames[par]  ,tag = "Mean/Median") +
    theme(axis.text.x=element_text(angle=0, hjust=0.5) ,
          axis.title.y=element_text(angle=0,vjust = 0.5) , 
          plot.tag.position = c(0.05, 0.9) , plot.tag = element_text(size = 9 ,face = "plain" ) ,
          panel.background = element_rect( fill = "white") ,
          panel.grid.minor = element_line(color="lightgray") ,
          panel.grid.major = element_line(color="lightgray")) +
    guides(x.sec = guide_axis_label_trans(~(meanmed[[par]])))
  
  
  print(p)
  
  
  
 ### ggsave(device = "png", file= paste0("manuscript_files/",par,".png"))   ## uncomment for saving single graphs
  
  graphs[[par]]=ggplotGrob(p)    
  
}


ggarrange(graphs[["sigma_Mid"]],graphs[["lambda_Mid"]],graphs[["gamma_Mid"]] , nrow = 3 , vjust = 20,  widths = c(-5, 0.05) , hjust = -0.5)

ggsave(device = "png", file= "manuscript_files/fig1_midpoints.png" , height = 9 , width = 9)






