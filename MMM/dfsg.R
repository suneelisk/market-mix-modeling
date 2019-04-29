data=read.csv("F:\\suneel\\suneel\\MMM\\Advertising.csv")


fit.lm1=lm(sales~TV,data=data)
fit.lm2<-update(fit.lm1,.~.+radio)
fit.lm3<-update(fit.lm2,.~.+newspaper)
mtable(fit.lm1,fit.lm2,fit.lm3)

relative_importance=calc.relimp(fit.lm3, type = c("car"), rela = TRUE) #used 'car' as cost function; rela=TRUE give % that a variable contributed to the adjusted R2

relative_imp=c(relative_importance$car[1],relative_importance$car[2],relative_importance$car[3])
sales_sum=sum(data$sales)
relative_contribution_of_each_variable=relative_imp*sales_sum

Tvs_PE<-as.numeric(fit.lm2$coefficients["TV"] * mean(data$TV)/mean(data$sales))
radio_PE<-as.numeric(fit.lm2$coefficients["radio"]* mean(data$radio)/mean(data$sales))
newspaper_PE<-as.numeric(fit.lm3$coefficients["newspaper"]* mean(data$newspaper)/mean(data$sales))
base_PE <- 1-(0.4797934+0.3118914-0.002260621)
sssss=cbind(Tvs_PE,radio_PE,newspaper_PE,base_PE)
barplot(sssss,main="xxzjhj",horiz = TRUE)


library(QuantPsyc)
lm=lm(sales~.,data=data)
round(lm.beta(lm)[1]/sum(lm.beta(lm))*100,1)
round(lm.beta(lm)[2]/sum(lm.beta(lm))*100,1)
round(lm.beta(lm)[3]/sum(lm.beta(lm))*100,1)


M=cor(data)

p=M[,c(3,4)]
ads <- c("TV", "radio", "newspaper")
sales <- c(M[1,4], M[2,4], M[3,4])
important <- c("TV ads are more impact to increase sales","Radio ads are medium impact to increase sales", "newspaper ads are less impact to increase sales")
ttdd=data.frame(cbind(ads,sales))

ttdd

elasticities <- function(linmod){
  Ncoef <- nrow(data.frame(linmod$coefficients))
  for(i in 2:Ncoef){
    el <- as.numeric(linmod$coefficients[i] * colMeans(model.matrix(linmod))[i]/colMeans(model.matrix(linmod))[1])
    ifelse (i== 2, elasticity <- el, elasticity <- rbind(elasticity,el))
  }
  rownames(elasticity) <- names(coef(linmod)[-1])
  colnames(elasticity) <- 'elasticities'
  
  return(data.frame(elasticity))
}

a <- elasticities(fit.lm3)

require(stargazer)
stargazer(a, summary = FALSE, type = 'html')
