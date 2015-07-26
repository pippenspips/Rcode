library(quantmod)
library(PerformanceAnalytics)
#run pull DJIA constituents to get variables before running this script.

#I will be ranking returns by row
strat1=function(returns,signals,numberofstocks){ #returns matrix, signals matrix, number of stocks to buy on each day
ranking=as.xts(matrix(0,nrow(returns),ncol(returns)),order.by = index(returns)) #get all zeros xts
colnames(ranking)=colnames(returns) #add in column names

for (i in 1:dim(ranking)[1]){
  ordering=order(signals[i,]) #order by row
  ordering=ordering[1:numberofstocks] #only interseted in set parameter
  ranking[i,ordering]=1 #set buy indicators for the column as 1
}

stratmatrix=ranking*returns
stratreturns=apply(stratmatrix,1,sum)/apply(ranking,1,sum)
return(stratreturns)
}

port1=as.data.frame(strat1(returnsmatrix1,signalsmatrix1,8))
colnames(port1)='returns'
port2=as.data.frame(strat1(returnsmatrix2,signalsmatrix2,8))
colnames(port2)='returns'
port3=as.data.frame(strat1(returnsmatrix3,signalsmatrix3,8))
colnames(port3)='returns'
combinedport=as.xts(rbind(port3,port2,port1))

getSymbols('DIA',src='google',from='2012-09-24')
dowreturns=(Cl(DIA)-lag(Cl(DIA),1))/(lag(Cl(DIA),1))
dowreturns=na.omit(dowreturns)

comparing=cbind(combinedport,dowreturns)
colnames(comparing)=c('Strat',"Dow")

#Performance Results
charts.PerformanceSummary(comparing)
stratsharpe=sqrt(252)*mean(comparing[,1])/sd(comparing[,1])
dowsharpe=sqrt(252)*mean(comparing[,2])/sd(comparing[,2])


