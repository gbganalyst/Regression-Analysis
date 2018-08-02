library(tidyverse, quietly = T)
#setwd('C:/Users/ogund/Desktop/toeiitoeyy')

# Question 7
data7= tribble(
  ~mother,~father,~daughter,
  64,73,65,
  66,70,65,
  62,72,61,
  70,72,69,
  70,72,67,
  58,63,59,
  66,75,69,
  66,75,70,
  64,72,68,
  67,69,70,
  65,77,70,
  66,70,65,
  68,74,70
)


g1=ggplot(data7,aes(mother, daughter))+geom_point()+
  theme_bw()+geom_smooth(method = 'lm',se = F)+
  labs(x="Mother's Height", y="Daughter's Height",title="A scatter plot of daughter and mother's height")
g1

g2=ggplot(data7,aes(father, daughter))+geom_point()+
  theme_bw()+geom_smooth(method = 'lm',se = F)+
  labs(x="Father's Height", y="Daughter's Height",title="A scatter plot of daughter and father's height")
g2

library(gridExtra)

g3=grid.arrange(g1,g2,ncol=1)

ggsave('first_plot.jpeg')


model2=lm(daughter~., data = data7)
summary(model2)
confint(model2,level = 0.95)


# A function for constructing ANOVA table in regression model
simpleAnova <- function(object, ...) {
  
  # Compute anova table
  tab <- anova(object, ...)
  
  # Obtain number of predictors
  p <- nrow(tab) - 1
  
  # Add predictors row
  predictorsRow <- colSums(tab[1:p, 1:2])
  predictorsRow <- c(predictorsRow, predictorsRow[2] / predictorsRow[1])
  
  # F-quantities
  Fval <- predictorsRow[3] / tab[p + 1, 3]
  pval <- pf(Fval, df1 = p, df2 = tab$Df[p + 1], lower.tail = FALSE)
  predictorsRow <- c(predictorsRow, Fval, pval)
  
  # Simplified table
  tab <- rbind(predictorsRow, tab[p + 1, ])
  row.names(tab)[1] <- "Predictors"
  return(tab)
  
}


mod <- lm(daughter~., data = data7)
mod0 <- lm(daughter~1, data = data7)
anova(mod)
anova(mod0, mod)
simpleAnova(mod)

newdata=data.frame(mother=64, father=74)
predict(model2,newdata)
predict(model2,newdata,interval = "prediction")
predict(model2,newdata,interval = "confidence")


# Question 8

data=tribble(~annual_income,~ira_contribution,
             28,0.3,
             25,0,
             34,1,
             43,1.3,
             48,3.3,
             39,2.2,
             74,8.5
)

ggplot(data,aes(annual_income,ira_contribution))+geom_point()+
  theme_bw()+geom_smooth(method = 'lm',se = F)+
  labs(x='Annual income (Thousands of dollars)', y='Ira Contribution (Thousands of dollars)',title='A scatter plot of annual income and IRA contribution')
ggsave('second_plot.jpeg')

model=lm(ira_contribution~annual_income, data = data)
summary(model)
confint(model,level = 0.95)

