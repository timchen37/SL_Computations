#############  1. CURVE FITTING ###############
m1 <-  gamlss(SL_Closed_L ~ pb(age_years),
              sigma.formula = ~ pb(age_years), 
              nu.formula = ~ pb(age_years),  
              tau.formula = ~ pb(age_years), 
              family = BCPE, data = dat, n.cyc=200, c.crit=0.01)

m1_chosen <- chooseDist(ol, type = "realplus", k=c(2,4,log(13756)), parallel="snow", ncpus=4)
# BCPE chosen 

#############  2. LOG TRANSFORMATION ###############

dat$logSL_Closed_L <- log(dat$SL_Closed_L)

m2 <-  gamlss(logSL_Closed_L ~ pb(age_years),
                 sigma.formula = ~ pb(age_years), 
                 nu.formula = ~ pb(age_years),  
                 tau.formula = ~ pb(age_years), 
                 family = SHASH, data = dat, n.cyc=75, c.crit=0.01)

m2_chosen <- chooseDist(logol, type = "realline", k=c(2,4,log(13042)), parallel="snow", ncpus=4)
# logSEP3 chosen

gen.Family("SEP3", type="log")

m2 <-  gamlss(SL_CLosed_L ~ pb(age_years),
              sigma.formula = ~ pb(age_years), 
              nu.formula = ~ pb(age_years),  
              tau.formula = ~ pb(age_years), 
              family = logSEP3, data = dat, n.cyc=200, c.crit=0.01)

GAIC(m1,m2,k=4)
#m2 is the better fit

#############  3. LOCAL PENALTY ###############
m3<-gamlss(SL_Closed_L~pb(age_years, method="GAIC", k=4),
                  sigma.formula=~pb(age_years, method="GAIC",k=4),
                  nu.formula=~pb(age_years, method="GAIC", k=4),
                  tau.formula = ~ pb(age_years, method="GAIC", k=4), 
                  family=logSEP3,data=dat, n.cyc=220, c.crit=0.01)                                              

###CENTILES
centiles(m3,cent=c(2,5,10,15,25,50,75,90,95,98))
wp(m3, ylim.all=1)
