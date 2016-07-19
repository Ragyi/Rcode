#A/B Testing funciton 

#Construct a Confidence Interval for Conversion rates
abtestfunc <- function(ad1, ad2){
  sterror1 = sqrt( ad1[1] * (1-ad1[1]) / ad1[2] )
  sterror2 = sqrt( ad2[1] * (1-ad2[1]) / ad2[2] )
  minmax1 = c((ad1[1] - 1.96*sterror1) * 100, (ad1[1] + 1.96*sterror1) * 100)
  minmax2 = c((ad2[1] - 1.96*sterror2) * 100, (ad2[1] + 1.96*sterror2) * 100)
  print( round(minmax1,2) )
  print( round(minmax2,2) )
}

site1 = c(.2024, 163286) # pink
site2 = c(.1952, 156596) # black

abtestfunc(site1, site2)

#These are the confidance intervals for the 2 sites 

#[Control] 20.05 20.43
#[Experience A] 19.32 19.72

#2 Sample T-Test for the Conversion means

# H0: Cntrol = Exp A
# H1: Cntrl < Exp A

test <- t.test(cntrl$Conversions, expA$Conversions, aalternative = "greater", mu = 0, conf.level = 0.95 )
test

#Welch Two Sample t-test

#There is also a widely used modification of the t-test, known as Welch's t-test that adjusts the number of degrees of freedom when the variances are thought not to be equal to each other.

#data:  cntrl$Conversions and expA$Conversions
#t = 0.12495, df = 21.867, p-value = 0.9017
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -7037.169  7939.169
#sample estimates:
#  mean of x mean of y 
#6510.917  6059.917 

# ANOVA test

correlations <- cor(expA$Conversions, expA$Upsell.panel.interaction)
correlations
#0.9990857

#Linear model
linearmodel <- lm(expA$Conversions~expA$Visit+expA$Display.mboxes+expA$Upsell.panel.interaction)
summary(linearmodel)
plot(linearmodel)

#Call:
#  lm(formula = expA$Conversions ~ expA$Visit + expA$Display.mboxes + 
#       expA$Upsell.panel.interaction)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-602.69  -80.32   80.65  170.49  350.71 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)  
#(Intercept)                     5.1119   151.8554   0.034   0.9740  
#expA$Visit                     -4.7871     2.3641  -2.025   0.0775 .
#expA$Display.mboxes             4.7779     2.3418   2.040   0.0756 .
#expA$Upsell.panel.interaction   2.1861     0.6839   3.197   0.0127 *
 # ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 341.4 on 8 degrees of freedom
#Multiple R-squared:  0.9988,	Adjusted R-squared:  0.9984 
#F-statistic:  2265 on 3 and 8 DF,  p-value: 4.708e-12
