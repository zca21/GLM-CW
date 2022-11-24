#' ---
#' title: "Untitled"
#' output: html_document
#' date: "2022-11-11"
#' ---
## -----------------------------------------------------------------------------------------------------------------------------------------------
#Setting up enviroment
setwd("~/Desktop/GLM/Coursework/GLM-CW")
airline.df <- read.table("airline.txt",header=T)

#Setting up inputs of mle function
n <- dim(airline.df)[[1]]
con <- rep(1,n)
X=matrix(c(con,airline.df$x),nrow=n,ncol=2)
y <- airline.df$y

#' 
## -----------------------------------------------------------------------------------------------------------------------------------------------
#Part 2

#Setting conditions of when to end fisher scoring algorithm
#max number of iterations of algorithm allowed (set high in case initial guess far off)
max.iter <- 1000
#Value to define convergence
eps<-0.001;

#setting up matrix to store values of beta at each iteration
beta.ini <- matrix(NA,max.iter,dim(X)[2])

#initial guess for beta
beta.ini[1,] <- c(0,1) 


flag <- 0
c<-1

#Loop to apply fisher scoring algorithm
while (flag==0 & c < max.iter){

#Finding mu for cth iteration  
mu <- exp(beta.ini[c,]%*%t(X))
#finding r where X^t%*%r is the score function
r <- c((y/mu)-1)
#applying fisher scoring algorithm to find c+1 iteration
beta <- beta.ini[c,]+(solve(t(X)%*%X))%*%t(X)%*%r
#storing value of new beta
beta.ini[(c+1),] <- beta

#checking for convergence
if (abs(beta.ini[c,1]-beta.ini[c+1,1]+beta.ini[c,2]-beta.ini[c+1,2])< eps){
flag <- 1
}

c <- c+1
}
#storing answers
beta.iteration <- beta.ini[complete.cases(beta.ini),]
beta.fin=beta.ini[c,]

beta.fin

#' 
## -----------------------------------------------------------------------------------------------------------------------------------------------
#Part 3
#finding the variance-covariance matrix
solve(t(X)%*%X)



#Finding the standard errors of betas
std.error=sqrt(diag(solve(t(X)%*%X)))
std.error


#Part 4 calculation
#T-statistic
T.stat <- beta.fin[2]/std.error[2]
T.stat 

qnorm(0.975) 

#p-value calculation
1-pnorm(T.stat)

#' 
## -----------------------------------------------------------------------------------------------------------------------------------------------
#Checking with R's own function
fit <- glm(y~X[,2], family  = Gamma(link="log"))
summary(fit,dispersion=1)  

#' 
