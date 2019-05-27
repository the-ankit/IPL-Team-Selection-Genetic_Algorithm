 
# @author: ankit


library(nsga2R)
df<-read.csv("cricket.csv")
newdf<-cbind(df[1:3],-1*df[16],df[36])
newdf$Bowling.average[df$Is.bowler.==0]=100
newdf<-cbind(newdf,df$Player.cost.USD)
newdf<-cbind(newdf,df[23])
newdf<-cbind(newdf,df[25])
cm<-as.matrix(newdf$`df$Player.cost.USD`)

obj<-function(x)
{
#temp<-subset(newdf,newdf$Variable.Tag %in% x)
  x<-round(x)
f1=sum(newdf$Batting.avg[x==1])
f2=sum(newdf$Bowling.average[x==1])
c(f1,f2)

}
Constr <- function(x)
  { 
  x<-round(x)
  c(5950000 - t(cm)%*%x,length(x[x==1])-15,15-length(x[x==1])) # Total budget >= total project costs
  
}

results <- nsga2(fn=obj, 129, 2, lower.bounds=rep(0,129), upper.bounds=rep(1,129),constraints = Constr,cdim=3,
                  popsize = 400, generations=400, cprob=0.9, cdist=10, mprob=0.05,mdist=20)
#results <- nsga2(fn=obj, 129, 2, lower.bounds=rep(0,129), upper.bounds=rep(1,129),
 #                popsize = 400, generations=20, cprob=0.9, cdist=10, mprob=0.05,mdist=20)

plot(results, xlab="Net Batting Performance", ylab="Net Bowling Performance", main="Objective Space")

#plot(results$par, xlab="x1", ylab="x2", main="Parameter space")
#plot(results$objectives)
#plot(results$parameters)
selected.team.combinations <- unique(round(results$par))
v1<-(selected.team.combinations)[2,]
print("Total Number of Players: ")
print(sum(v1))
print("Team")
which(v1==1)
resvec<-which(v1==1)
print(df$Name[resvec])
print(c("Total cost ", sum(df$Player.cost.USD[resvec])
))


