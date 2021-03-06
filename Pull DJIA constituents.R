library(quantmod)
DJIA.constituents=read.csv('~/DJIA constituents.csv',stringsAsFactors = FALSE) #read in csv
names(DJIA.constituents)=as.Date(substr(names(DJIA.constituents),start=2,stop=11),format='%Y.%m.%d') #subset output... it was imported funky from excel
environmentnames=gsub(" ", "", as.yearmon(names(DJIA.constituents), '%Y-%m-%d'), fixed = TRUE) #environments cannot be dates. Hack around by changing dates to yearmon format and removing spaces

for (x in environmentnames) assign(x, new.env()) #assign environments for each set of dates
parsingdates=c(as.character(Sys.Date()), names(DJIA.constituents)[-length(names(DJIA.constituents))])

for (j in 1:length(environmentnames)){ #use all environments
  for(i in 1:dim(DJIA.constituents)[1]){ #pull symbols into each date
    getSymbols(DJIA.constituents[i,j], env = eval(parse(text=environmentnames[j])), src = "google", from=names(DJIA.constituents)[j],to=parsingdates[j]) #get all the symbols. Dont use yahoo
  }
}

for (j in 1:length(environmentnames)){ #loop through all environments
  for (i in 1:dim(DJIA.constituents)[1]){ #loop through all 30 symbols
    stock=eval(parse(text=paste(environmentnames[j],'$',DJIA.constituents[i,j],sep=''))) #set matrix with open, high, low, close, volume, adjusted. turning a string into a variable
    #closetoclose=(Ad(stock)-lag(Ad(stock),1))/(lag(Ad(stock),1)) #calculate returns using adjusted close
    #colnames(closetoclose)=paste(DJIA.constituents[i,j],'.CltClReturns', sep='')
    
    opentolow=(Op(stock)-lag(Lo(stock),1))/lag(Lo(stock),1) #signal which is return of yesterdays low to todays open
    colnames(opentolow)=paste(DJIA.constituents[i,j],'.OptLoReturns', sep='')
    
    closetoopen=(Cl(stock)-Op(stock))/Op(stock) #returns for the strategy that day
    colnames(closetoopen)=paste(DJIA.constituents[i,j],'.CltOpReturns', sep='')
    
    eval(parse(text=paste(environmentnames[j],'$',DJIA.constituents[i,j],'=cbind(stock,opentolow,closetoopen)',sep=''))) #bind the returns on to each stock
    #eval(parse(text=paste(DJIA.constituents[i,j],'=cbind(stock,returns)',sep='')))
  }
}


sample1=eval(parse(text=paste(environmentnames[1],'$',DJIA.constituents[1,1]))) #get rows and columns for newest dow constituents from 3/2015 to current
sample2=eval(parse(text=paste(environmentnames[2],'$',DJIA.constituents[1,2]))) #get rows and columns for dow constituents from 9/2013 to 3/2015
sample3=eval(parse(text=paste(environmentnames[3],'$',DJIA.constituents[1,3]))) #get rows and columns for dow constituents from 9/2012 to 9/2013
signalsmatrix1=as.xts(matrix(0,nrow(sample1),ncol=30),order.by = index(sample1)) #create xts of signals for sample1
signalsmatrix2=as.xts(matrix(0,nrow(sample2),ncol=30),order.by = index(sample2)) #create xts of signals for sample2
signalsmatrix3=as.xts(matrix(0,nrow(sample3),ncol=30),order.by = index(sample3)) #create xts of signals for sample3
returnsmatrix1=as.xts(matrix(0,nrow(sample1),ncol=30),order.by = index(sample1)) #create xts of returns
returnsmatrix2=as.xts(matrix(0,nrow(sample2),ncol=30),order.by = index(sample2)) #create xts of returns
returnsmatrix3=as.xts(matrix(0,nrow(sample3),ncol=30),order.by = index(sample3)) #create xts of returns
colnames(signalsmatrix1)=colnames(returnsmatrix1)=DJIA.constituents[,1] #columns are the tickers 
colnames(signalsmatrix2)=colnames(returnsmatrix2)=DJIA.constituents[,2] #columns are the tickers 
colnames(signalsmatrix3)=colnames(returnsmatrix3)=DJIA.constituents[,3] #columns are the tickers 



for (i in 1:30){ #pull returns and signals into one matrix
  returnsmatrix1[,i]=eval(parse(text=paste(environmentnames[1],'$',DJIA.constituents[i,1])))[,7] #create giant returns matrix from 3/2015 to current
  returnsmatrix2[,i]=eval(parse(text=paste(environmentnames[2],'$',DJIA.constituents[i,2])))[,7] #create giant returns matrix from 9/2013 to 3/2015
  returnsmatrix3[,i]=eval(parse(text=paste(environmentnames[3],'$',DJIA.constituents[i,3])))[,7] #create giant returns matrix from 9/2012 to 9/2013
  signalsmatrix1[,i]=eval(parse(text=paste(environmentnames[1],'$',DJIA.constituents[i,1])))[,6] #create giant signals matrix from 3/2015 to current
  signalsmatrix2[,i]=eval(parse(text=paste(environmentnames[2],'$',DJIA.constituents[i,2])))[,6] #create giant signals matrix from 9/2013 to 3/2015
  signalsmatrix3[,i]=eval(parse(text=paste(environmentnames[3],'$',DJIA.constituents[i,3])))[,6] #create giant signals matrix from 9/2012 to 9/2013
  
}

returnsmatrix1=returnsmatrix1[-1,]
returnsmatrix2=returnsmatrix2[-1,]
returnsmatrix3=returnsmatrix3[-1,]
signalsmatrix1=signalsmatrix1[-1,]
signalsmatrix2=signalsmatrix2[-1,]
signalsmatrix3=signalsmatrix3[-1,]

