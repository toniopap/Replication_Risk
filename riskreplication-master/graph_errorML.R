#extract parameter estimates and standard errors from list 
ep<-lapply(mlmodels_PT, `[`, c('estimate', 'clSE'))
rnames <- names(mlmodels_PT)
cnames <- c("Country", "sigma", "lambda", "gamma", "SEsigma", "SElambda", "SEgamma")
#remove list and create dataframe 
epl<- unlist(ep)
eplmat <- matrix(epl,  nrow=length(rnames), byrow=TRUE)
eplmat <- cbind(rnames, eplmat)
epldat <- as.data.frame(eplmat)
colnames(epldat) <- cnames
epldat[1,1] = "New samples pooled" #change name of All countries

#convert character into numeric for columns 2-7
i <- 2:7
epldat[ , i] <-apply(epldat[ , i], 2, function(x) as.numeric(as.character(x)))

alpha = 0.05 ###set confidence level and compute margin of error
MEsigma<- qnorm(1-alpha/2)*epldat$SEsigma #get margin  of error for sigma
MElambda<- qnorm(1-alpha/2)*epldat$SElambda #get margin of error for lambda
MEgamma<- qnorm(1-alpha/2)*epldat$SEgamma #get margin of error for gamma

#add ME to dataframe 
epldat$MEsigma <- MEsigma  
epldat$MElambda <- MElambda  
epldat$MEgamma <- MEgamma  

#derive upper and lower CI bounds with margin of error (not necessary for plotting)
LBsigma <- epldat$sigma - MEsigma
UBsigma <- epldat$sigma + MEsigma 
LBlambda <- epldat$lambda - MElambda
UBlambda <- epldat$lambda + MElambda 
LBgamma <- epldat$gamma - MEgamma
UBgamma <- epldat$gamma + MEgamma  

#add CI bounds to dataframe 
epldat$LBsigma <- LBsigma  
epldat$UBsigma <- UBsigma  
epldat$LBlambda <- LBlambda  
epldat$UBlambda <- UBlambda  
epldat$LBgamma <- LBgamma  
epldat$UBgamma <- UBgamma  

###plotting 
#restructure data for grouped bar plotting
library("reshape2")
epldatsub <- subset(epldat, select = c(Country, sigma, lambda, gamma))
epldatmod <- melt(epldatsub, id.vars='Country')

epldatsub2 <- subset(epldat, select = c(Country, MEsigma, MElambda, MEgamma))
epldatmod2 <- melt(epldatsub2, id.vars='Country')

epldatsub3 <- subset(epldat, select = c(Country, SEsigma, SElambda, SEgamma))
epldatmod3 <- melt(epldatsub3, id.vars='Country')

epldatmod$ME = epldatmod2$value #add margin of error to dataframe for plotting
epldatmod$SE = epldatmod3$value #add standard errors

#grouped bar plot with three variables + error bars, use epldatmod 
ggplot(epldatmod, aes(x=factor(Country, 
                               levels = c("New samples pooled", "BJR2014", "BJR2014 (weighted)", "Austria", "Croatia", "France_I", "France_II",
                                          "Germany", "Italy", "Netherlands", "Poland", "Slovenia", "Spain", "Sweden")), #determine order of countries
                      y=value, fill=variable, width = 0.85)) +
  geom_bar(stat='identity', position='dodge') +
  geom_errorbar(aes(ymin=value-ME, ymax=value+ME), width=.2,position=position_dodge(.85)) +
  geom_text(aes(label= paste0(round(value, digits=2), "\n", "(", round(SE, digits=2), ")"), 
                y=value+ME), position=position_dodge(width=0.85), vjust=-.25, size =2.8) + #display parameter values and SE's
  scale_x_discrete(guide = guide_axis(angle = 45)) + NULL +
  xlab("Sample") +
  ylab("Parameter Value") +
  theme_bw() +
  scale_fill_manual(values = c("gray45","gray85", "gray65"), name = "Parameter", 
                    labels = c("\u03c3", "\u03bb", "\u03b3")) #change darkness of gray by adjusting number

#plot with error bars for sigma only 
#ggplot(epldat) +
#  geom_bar( aes(x=Country, y=sigma), stat="identity", position = "dodge", fill="grey", alpha=0.7) +
#  geom_errorbar( aes(x=Country, ymin= LBsigma, ymax = UBsigma), width = 0.4 , colour = "red", alpha=0.9, size = 0.5) +
#  scale_x_discrete(guide = guide_axis(angle = 90)) +
#  NULL
ggsave("manuscript_files/errorplot.tiff", dpi = "print", width = 15, height = 8.5)
