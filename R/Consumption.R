# PLEASE EXCUSE MY RATHER INELEGANT CODE! 
# ======================================
cons.df<- read.table("../Data/consump.dat.txt", header=T)

consump<- (cons.df$CONS-mean(cons.df$CONS))/1000      # TAKE DEVIATIONS ABOUT MEANS
income<-  (cons.df$Y-mean(cons.df$Y)) /1000           # AND SCALE DATA TO AVOID ROUNDING ERROR WHEN INTEGRATING
year<- cons.df$YEAR
mle<- lm(consump~income -1)                           # ASSUMING NORMAL ERRORS, WE NOW HAVE THE MLE OF B
c2<-consump^2
y2<-income^2
cy<-consump*income
pm<- 0.75                                            # ASSIGN A PRIOR MEAN FOR B
pv<- 0.0005                                           # ASSIGN THE PRIOR VARIANCE FOR B
n<- length(consump)
alpha<- (1/pv)*pm*(pm*(1-pm)-pv)                      # CONVERT PRIOR MEAN AND VARIANCE INTO THE ALPHA AND GAMMA PARAMETERS FOR THE BETA DITRSIBUTION
gamma<- (1/pv)*(1-pm)*(pm*(1-pm)-pv)
integrand<- function(b){(b^(alpha-1))*((1-b)^(gamma-1))/(sum(c2)+b^2*sum(y2)-2*b*sum(cy))^(n/2)}
norm<- 1/integrate(integrand, lower = 0, upper = 1)$value     
# WE NOW HAVE THE NORMALIZING CONSTANT FOR THE POSTERIOR
# NOW OBTAIN THE MEAN OF THE (NORMALIZED) MARGINAL POSTERIOR FOR "b":

integrand<-  function(b) {b*(b^(alpha-1))*((1-b)^(gamma-1))/(sum(c2)+b^2*sum(y2)-2*b*sum(cy))^(n/2)}
post_mean<- norm*integrate(integrand, lower = 0, upper = 1)$value


# NOW OBTAIN THE VARIANCE OF THE (NORMALIZED) MARGINAL POSTERIOR FOR "b":
integrand<-  function(b) {(b-post_mean)^2*(b^(alpha-1))*((1-b)^(gamma-1))/(sum(c2)+b^2*sum(y2)-2*b*sum(cy))^(n/2)}
post_var<- norm*integrate(integrand, lower = 0, upper = 1)$value
b<- seq(0.68 , 0.93, 0.00001)
prior<- dbeta(b,alpha, gamma)
sig<- summary(mle)$sigma
bmle<- summary(mle)$coef[1]                           # MLE  for beta
integrand<- function(b) {((n-1)*sig^2+(b-bmle)^2*sum(y2))^(-(n-1)/2)}    # Get normalizing constant for marginal LF for beta
norml<- 1/integrate(integrand, lower = 0, upper = 1)$value 
mlf<- norml*((n-1)*sig^2+(b-bmle)^2*sum(y2))^(-(n-1)/2)                  # Compute the normalized marginal LF for beta

posterior<- norm*((b)^(alpha-1))*((1-b)^(gamma-1))/(sum(c2)+b^2*sum(y2)-2*b*sum(cy))^(n/2)
o<- order(posterior)
temp<- b[o]
post_mode<- temp[25001]
# PLOT THE RESULTS
plot(b, posterior, type="l", lwd=2, col="blue",xlab="Beta", ylab="Prior, Likelihood, Posterior")
lines(b, prior, lwd=2,col="red")
lines(b, mlf, lwd=2, col="black")
title(main="Consumption Function Example")
mtext("(Prior Mean = 0.75 ; Prior Variance = 0.0005)")
legend(0.7,100,lty=c(1,1,1), lwd=c(2.5,2.5,2.5), col=c("red","black","blue"), c("Prior","Likelihood","Posterior"))

bmle                              # MLE of Beta
post_mean                         # Posterior Mean for Beta
post_var                          # Posterior Variance for Beta
post_mode                         # Posterior Mode for Beta
summary(mle)                      # MLE = OLS = Diffuse Prior Rsults
