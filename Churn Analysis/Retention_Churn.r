library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(corrplot)
library(plyr)

df <- read.csv("C:/Users/Amit R Kulkarni/Desktop/Customer Retention Analysis/Churn_Modelling.csv")



CreateGrp <- function(tn){
  if (tn >= 0 & tn <= 25){
    return('0-25')
  }else if(tn > 25 & tn <= 50){
    return('26-50')
  }else if (tn > 50 & tn <= 75){
    return('50-75')
  }else if (tn > 75 & tn <=Inf){
    return('Above 75')
  }
}

df$GrpAge <- sapply(df$Age,CreateGrp)
# set as factor the new column
df$GrpAge <- as.factor(df$GrpAge)

CreateGrp1 <- function(tn){
  if (tn >= 0 & tn <= 400){
    return('0-400')
  }else if(tn > 400 & tn <= 500){
    return('401-500')
  }else if (tn > 500 & tn <= 600){
    return('501-600')
  }else if (tn > 600 & tn <=700){
    return('601-700')
  }else if (tn > 700 & tn <=800){
    return('701-800')
  }else if (tn > 800){
    return('Above 800')
  }
}

df$GrpCS <- sapply(df$CreditScore,CreateGrp1)
# set as factor the new column
df$GrpCS <- as.factor(df$GrpCS)

CreateGrp2 <- function(tn){
  if (tn >= 0 & tn <= 2){
    return('0-2')
  }else if(tn > 2 & tn <= 5){
    return('3-5')
  }else if (tn > 5 & tn <= 8){
    return('6-8')
  }else if (tn >8 & tn <=10){
    return('9 and above')
  }
}

df$GrpTenure <- sapply(df$Tenure,CreateGrp2)
# set as factor the new column
df$GrpTenure <- as.factor(df$GrpTenure)




createplot <- function(dst, column, name) {
  plt <- ggplot(dst, aes(x=column, fill=(Exited))) + 
    ggtitle(name) + 
    xlab(name) +
    ylab("Percentage")  +
    geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.75) + 
    theme_minimal() +
    theme(legend.position="none", axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values=c("#33ff33", "#ff471a"))
  return(plt)
}

p1 <- createplot(df, df$Gender, "Gender") 
p2 <- createplot(df, df$GrpAge, "Age") 
p3 <- createplot(df,df$GrpCS, "CreditScore")
p4 <- createplot(df,df$Geography,"Countries")
p5 <- createplot(df,df$GrpTenure,"Tenure")



grid.arrange(p1,p2,p3,p4,p5,ncol=2)