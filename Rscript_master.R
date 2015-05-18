R functions 

Power calculation for Random Effects ANOVA

power.oneway.random<-function(n, A, alpha, var.ratio){
  df.1<-A-1
  df.2<-A*(n-1)
  divisor<-1+n*var.ratio
  Fcrit.star<-qf(1-alpha,df.1,df.2)/divisor
  power<-1-pf(Fcrit.star,df.1,df.2)
  return(power)}
ex.
power.oneway.random(10,3,.05,1)
[1] 0.7396868

Power calculation for Fixed Effects ANOVA

Power.oneway.fixed<-function(A,n,alpha, lambda)
{df.1<-A-1
 df.2<-A*(n-1)
 F.crit<-qf(1-alpha, df.1, df.2)
 Power<-1-pf(F.crit,df.1,df.2,lambda)
 return(Power)}
ex.
Power.oneway.fixed(3,3,.05,6)
[1] 0.3808947

Note: Power calculations for Fixed Effects ANOVAs require lambda.

F.Crit
Ex.
alpha<-.05
number.of.groups<-4
n.per.group<-10
df.1<-number.of.groups-1
df.2<-number.of.groups*(n.per.group-1)
f.crit<-qf(1-alpha, df.1,df.2)
f.crit
[1] 2.866266

T.Crit
t.crit<-qt(1-alpha,df)


Calculating Lambda (NCP for F statistic)
1. find grand mean (sum of means of groups/number of groups)
ex.
> grand.mean<-((5+6+13+14+22)/5)
> grand.mean
[1] 12
2. Find alphas for each group (alphaj=Mj-Mdot)
ex. 
> alpha.1<-(5-12)
> alpha.1
[1] -7
> alpha.2<-(6-12)
> alpha.2
[1] -6
> alpha.3<-(13-12)
> alpha.3
[1] 1
> alpha.4<-(14-12)
> alpha.4
[1] 2
> alpha.5<-(22-12)
> alpha.5
[1] 10
3. Find the Sum of Squared Effect Size
Add each (alpha^2)/sigma
Ex. 
> sum.of.squared.Es<-((alpha.1/10)^2+(alpha.2/10)^2+(alpha.3/10)^2+(alpha.4/10)^2+(alpha.5/10)^2)
> sum.of.squared.Es
[1] 1.9
4. Multiply n(the number per group) by the sum.of.squared.Es
Ex. 
> n<-5
> lambda<-n*sum.of.squared.Es
> lambda
[1] 9.5

Loading Data: 
  The gl function: allows you to label the dependent variable to create the different levels of an independent variable. 
gl(number.of.levels, n.per.level, total.N, labels=c(“first.level.name”, “second.level.name,” etc.)
   Ex. 
   > list_length <- gl(3,10,2*3*10, labels=c("12 Words","24 Words","48 Words"))
   Note: gl makes the independent variables into factors in R, so we don’t need to worry about doing this if we use this function
   Putting things into a data frame/ creating a table: 
     Use the data.frame function. 
   data.frame(dependent.variable, list.each.independent.variable)
   ex. 
   > mem.data <- data.frame(score, list_length,recall_type)
   Note: make sure you’ve made your independent variables factors before you do this.
   
   Creating an interaction plot:
     Interaction.plot(x.axis.indep.variable, line.indep.variable, response=dependent.variable)
   Ex. 
   > interaction.plot(list_length,recall_type,response=score)
   
   Reading a CSV in R: 
     Note: make sure file is saved as “.csv” and also be sure to change your working directory. 
   Ex.
   > X<-read.csv("GT3.p1.csv")
   
   Making a table:
     The rbind function put your data into rows, whereas the cbind function puts your data into columns. 
   > mean.tab<-rbind(means,sds,ns)
   Creating column labels: 
     > colnames(mean.tab) <- c('ExA','ExB','ExC','Con')
   
   Print function: 
     Print shows us the means of each of the conditions when we have 2 independent variables that have 2 and 3 levels respectively, creating a total of 6 conditions. 
   Ex
   > print(model.tables(aov1,"means"),digits=3)
   
   Running a Two-way ANOVA in R: 
     aov(dependent variable~indep.variable*indep.variable, data=name.of.your.data)
   Note: the indep*indep notation means all possible combinations. This is different from doing indep:indep because the colon notation specifies which interaction you’re interested in seeing. 
   Ex. 
   >aov1 <- aov(score~list_length*recall_type, data=mem.data)
   Use the summary function to see your ANOVA results: 
     Ex. 
   > summary(aov1)
   Df Sum Sq Mean Sq F value   Pr(>F)    
   list_length              2   2080    1040  115.56  < 2e-16 ***
     recall_type              1    240     240   26.67 3.58e-06 ***
     list_length:recall_type  2    280     140   15.56 4.62e-06 ***
     Residuals               54    486       9                     
   ---
     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
   Note: In a Two-way ANOVA, you have 2 main effects and one interaction. The residuals row tells you your df.2 and the MSresiduals. MSresiduals can be used to calculate a main effect by hand. 
   
   Calculating Main Effect of an independent variable by hand:
     1. Create a table with the means,ns,sds of each condition. 
   2. Find the means of the rows and columns. 
   3. If your factor is in columns, create a vector of the column means. 
   4. Find the variance of the column means. 
   5. Look in your ANOVA table at the MS Residuals. Use this number
   6. Add up the number of values per column. This is the n you use. 
   7. The F statistic is the variance of the column means divided by the MSresiduals/n.per.column. 
   Ex.  
   > Column.means<-c(10,14,24)
   > var(Column.means)
   [1] 52
   > MSresiduals<-9
   > n.per.column<-20
   > F<-var(Column.means)/(MSresiduals/n.per.column)
   > F
   [1] 115.5556
   
   Simple Main Effects in R: 
     Use the library(xtable)
   Ex.
   Run an ANOVA on a part of your dataset. Do this by using 
   aov(dep.variable~indep.variable(the one you want the simple main effect for), data=subset(table.name, indep.variable(the other one, not the one you want the simple main effect for)==”level.you.want”))
   Note: the double equal sign is very important here!!!
     Ex. 
   > fit.sme <- aov(score~list_length,data=subset(mem.data,
                                                  + recall_type=="Free Recall"))
   Use the summary function to see this ANOVA: 
     > summary(fit.sme)
   Df Sum Sq Mean Sq F value   Pr(>F)    
   list_length  2    420  210.00   23.43 1.26e-06 ***
     Residuals   27    242    8.96                     
   ---
     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
   
   Another way to do Simple Main Effects using the degrees of freedom from the overall ANOVA: 
     
     > Alternate.SME.method<-function(df.1,df.2, MS.factor, MS.residuals)
       + {pvalue<-1-pf(MS.factor/MS.residuals, df.1,df.2)
          + return(pvalue)}
   Ex.
   > Alternate.SME.method(2,54,210,9)
   [1] 4.974139e-08
   
   Note: df.2 & MS.residuals are found in the overall ANOVA table. MS.factor and df.1 are from the 1-way SME ANOVA table. 
   
   MBESS
   MBESS can be used with t-test and f-tests to find CI’s on Effect size using delta (in t-test situation) or lambda(in f-test situation). 
   MBESS used to find CI on delta in parentheses you put(t-value, df, percentage of CI)
   > conf.limits.nct(1.5993420, 30, .99)
   $Lower.Limit
   [1] -1.042603
   
   $Prob.Less.Lower
   [1] 0.005
   
   $Upper.Limit
   [1] 4.21681
   
   $Prob.Greater.Upper
   [1] 0.005
   
   MBESS used to find CI on effect size: (n.1 and n.2 are the group sizes)
   > ci.smd(ncp=1.599,n.1=18, n.2=14, conf.level=.99)
   $Lower.Conf.Limit.smd
   [1] -0.3716427
   
   $smd
   [1] 0.569801
   
   $Upper.Conf.Limit.smd
   [1] 1.502524
   
   For lambda CI: 
     > lambda.ci<-conf.limits.ncf(F.value=2.3099, df.1=3,df.2=36) 
   
   To convert lambda.ci to CI on Cohen’s f (adjusted effect size) do this to each of the CI endpoints: 
     > n.per.group<-10
   > number.of.groups<-4
   > upper<-sqrt(lambda.ci$Upper.Limit/(n.per.group*number.of.groups))
   
   Make.exact.data function: 
     ##The make.exact.data function isn’t in R, so you have to enter it each time: 
     
     z.score<-function(x){(x-mean(x))/sd(x)}
   
   rescale.numbers<- function(x,mean,sd){
     z <- z.score(x)
     return(z*sd + mean)}
   
   make.exact.data <- function(n, mean, sd){
     x <- rnorm(n)
     return(rescale.numbers(x,mean,sd))}
   
   ##See Lexa’s handout for an example!
   
   Generalized T function (runs a t.test or makes a CI)
     GeneralizedT<-function(means,sds,ns,wts,k0=0,CI=FALSE,conf=0.95,null=0)
     { if (CI)k0<-0
       if(null==0)tails <- 2 else tails <- 1
       J<-length(means)
       df<-sum(ns)-J
       VarEstimate <- sum((ns-1)*sds^2)/df
       num<-sum(wts*means)-k0
       den<-sqrt(sum(wts^2/ns)*VarEstimate)
       t<-num/den
       if(!CI){
         if(tails == 2) pval <- 2*pt(-abs(t),df)
         if(null < 0) pval <- 1 - pt(t,df)
         if(null > 0) pval <- pt(t,df)
         return(c(t,df,pval))}
       else{
         tcrit <- qt(1-(1-conf)/2,df)
         dist <- tcrit*den
         lower <- num - dist
         upper <- num + dist
         return(c(lower,upper))}}
   
   Ex. t-test
   GeneralizedT(means,sds,ns,wts,CI=TRUE)
   Ex. CI
   GeneralizedT(mean,sd,n,wts=1,CI=TRUE, conf=.99)
   
   Linear Pre-Combination: 
     In this case, our dataset was called X. This is how you subtract column 2 from column 1. 
   D02<-X[,1]-X[,2]
   CURVE CODE: 
     ###Generic Function for T Rejection Point
     T.Rejection.Point <- function(alpha,df,tails){
       if(tails==2)return(qt(1-alpha/2,df))
       if((tails^2) != 1) return(NA)
       return(tails*qt(1-alpha,df))}
   
   ### Generic Function for T-Based Power
   Power.T <- function(delta,df,alpha,tails){
     pow <- NA
     R <- T.Rejection.Point(alpha,df,abs(tails))
     if(tails==1)
       pow <- 1 - pt(R,df,delta)
     else if (tails==-1)
       pow <- pt(R,df,delta)
     else if (tails==2)
       pow <-  pt(-R,df,delta) + 1-pt(R,df,delta)
     return(pow)}
   
   ### Power Calc for One-Sample T
   
   Power.T1 <- function(mu,mu0,sigma,n,alpha,tails){
     delta = sqrt(n)*(mu-mu0)/sigma
     return(Power.T(delta,n-1,alpha,tails))}
   
   ## Power Calc for Two-Sample T
   
   Power.T2 <- function(mu1,mu2,sigma,n1,n2,alpha,tails,hypo.diff=0){
     delta = sqrt((n1*n2)/(n1+n2))*
       (mu1-mu2-hypo.diff)/sigma
     return(Power.T(delta,n1+n2-2,alpha,tails))}
   
   ##Power Calc for multiple comparisons
   Power.GT <- function(mus,ns,wts,sigma,alpha,tails,kappa0=0){
     W = sum(wts^2/ns)
     kappa = sum(wts*mus)
     delta = sqrt(1/W) * (kappa-kappa0)/sigma
     df = sum(ns)-length(ns)
     return(Power.T(delta,df,alpha,tails))}
   
   ###Idk what this one is used for
   curve.js <- function(f,a,b,points=100,type='l',...){
     ftext <- paste("g <- function(x){",f,"}")
     eval(parse(text=ftext))
     x <- seq(a,b,length=points)
     plot(x,mapply(g,x),type,...)}
   
   ##Making a graph of Power
   plot.curve <- function(f,a,b,points=100,type='l',...){
     x <- seq(a,b,length=points)
     plot(x,mapply(f,x),type,...)}
   
   ZZ Wrapper
   Note: with Power GT you have to use the zz wrapper before you can run the curve function (see example).  
   zz<-function(x){Power.GT(c(.25,.25,.25,.25),c(x,x,x,x),c(1,1,1,-1),1,.05,2,kappa0=0)}
   plot.curve(zz,50,150)
   Horizontal Lines: abline(h=.90) 
   Vertical Lines: abline(v=30) 
   Apply function:
     apply(data.frame,var.names,function)
   Ex. 
   > X
   Time.0 Time.1 Time.2 Time.3
   1       3      3      8     12
   2       4      3      7      9
   3       3      6     10      9
   4       4      3      9     12
   5       2      4      9     10
   6       0      5      7     10
   7       1      4      9     13
   8       2      5      8     13
   9       3      3      7     12
   10      4      7      8      9
   > means <- apply(X,2,mean)
   > means
   Time.0 Time.1 Time.2 Time.3 
   2.6    4.3    8.2   10.9 
   
   With function: allows you to go inside a dataset
   Ex. > with(data2,mean(score))
   
   
   Case Handout: 1-Sample T-test:
     Basic T Test (VARIANCE UNKNOWN)
   Hypothesized mean=65, xbar=65.9, s=2.16, n=25
   > t.score<-function(xbar,mean,s,n){(xbar-mean)/(s/sqrt(n))}
   > t.score(65.9, 65, 2.16, 25)
   [1] 2.083333
   > pnorm(2.083333, 0, 1)
   [1] 0.9813896
   95% Confidence interval for t’s with df of 24:
     (critical t value is 2.064)
   > upper.limit<-function(xbar,t,s,n){xbar+t*(s/sqrt(n))}
   > upper.limit(65.9, 2.064, 2.16, 25)
   [1] 66.79165
   > lower.limit<-function(xbar,t,s,n){xbar-t*(s/sqrt(n))}
   > lower.limit(65.9, 2.064, 2.16, 25)
   [1] 65.00835
   So the confidence interval is between 65.00835 and 66.79. This excludes 65 (just barely) so we would reject the hypothesized value of the mean. 
   
   Case Handout: 2-sample T-test:
     Two-Sample Independent Sample Tests on Means
   Note:be careful to notice s^2 versus s…you need to put s into the function because this function squares it for you. 
   Sigma hat squared
   > sigma.hat.squared<-function(n1,n2,s1,s2)
     + {((n1-1)*s1^2+(n2-1)*s2^2)/(n1+n2-2)}
   t test
   > two.sample.independent.t.test<-function(xbar1,xbar2,n1,n2,sigma.hat.squared)
     +{(xbar1-xbar2)/sqrt((1/n1+1/n2)*sigma.hat.squared)}
   > sigma.hat.squared(10,10,sqrt(23),sqrt(27))
   [1] 25
   > two.sample.independent.t.test(23, 19.8, 10,10,25)
   [1] 1.431084
   Our current result shows that the difference between the groups is not significant. The significant t-value for a t statistic with 18 degrees of freedom is 2.101, and our t-value is only 1.43.
   Confidence Interval: 
     Note: for a t-test with df of 18, at a 95% confidence interval with two-tails, the significant t-value is 2.101.
   > lower.limit<-function(xbar1,xbar2,t,n1,n2,sigma.hat.squared)
     + {xbar1-xbar2-t*sqrt((1/n1+1/n2)*sigma.hat.squared)}
   > lower.limit(23,19.8,2.101,10,10,25)
   [1] -1.497979
   > upper.limit<-function(xbar1,xbar2,t,n1,n2,sigma.hat.squared)
     + {xbar1-xbar2+t*sqrt((1/n1+1/n2)*sigma.hat.squared)}
   > upper.limit(23,19.8,2.101,10,10,25)
   [1] 7.897979
   The confidence interval includes the value zero within it’s endpoints. This means that there is no difference between the two groups’ means. 
   
   Case Handout: Dependent sample T-test:
     Dependent Sample Test on Means (uses difference scores)
   > dependent.sample.t.test<-function(dbar,s,n)
     + {dbar/sqrt(s^2/n)}
   > dependent.sample.t.test(-65.4,sqrt(5132.29), 10)
   [1] -2.886836
   The significant t-value for a test with 9 df, two-tailed, with 95% confidence interval is +/-2.262, so we decide this value is significant. 
   Confidence Intervals
   > lower.limit<-function(d,t,s,n)
     + {d-t*sqrt(s^2/n)}
   > lower.limit(-65.4,2.262,sqrt(5132.29), 10)
   [1] -116.6446
   > upper.limit<-function(d,t,s,n)
     + {d+t*sqrt(s^2/n)}
   > upper.limit(-65.4,2.262,sqrt(5132.29), 10)
   [1] -14.15539
   So the confidence interval is between -116.6446 and -14.15539. This interval does NOT include zero, which means we reject the null hypothesis. 
   
   
   
   
   
   
   
   