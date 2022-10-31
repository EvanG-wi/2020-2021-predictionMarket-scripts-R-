#Gets vote totals from NYT, displays in dataframe, compares to last time this script was run and display difference (new votes)


library(xml2)
library(rvest)
library(openxlsx)


a=data.frame()
b=data.frame()
c=data.frame()
d=data.frame()
DF=list(a,b,c,d)


newVotes=function(data,ncols,DF,DFid){ #display the new votes added since last set
  for(i in 1:nrow(data)){
    data$TotalVotes[i]=sum(data[i,2:(ncols-1)])
    data$Margin[i]=data[i,2]/data[i,"TotalVotes"]-data[i,3]/data[i,"TotalVotes"]
  }
  
  data=data[order(data$County),]
  DF[[DFid]]=DF[[DFid]][order(DF[[DFid]]$County),]
  
  View(data)
  for(i in 2:ncols){
    data[,i]=data[,i]-DF[[DFid]][,i]} #difference
 
  for(i in 1:nrow(data)){
    data$TotalVotes[i]=sum(data[i,2:(ncols-1)])
    data$Margin[i]=data[i,2]/data[i,"TotalVotes"]-data[i,3]/data[i,"TotalVotes"]
  }
  
  recentVotes = data[which(0!=apply(data[,2:(ncols-1)],1,sum,na.rm=T)),] #show counties with a non-zero change in vote total
  View(recentVotes)
}

nyt=function(state,DF,initial=FALSE){ #call as DF=nyt("FL",DF,T)
   
  if(state=="MA"){DFid=1 ; site="https://www.nytimes.com/interactive/2020/09/01/us/elections/results-massachusetts-senate-primary-election.html"}
  if(state=="NY"){DFid=2 ; site="https://www.nytimes.com/interactive/2020/03/03/us/elections/results-massachusetts-president-democrat-primary-election.html"}
  if(state=="GA"){DFid=3 ; site="https://www.nytimes.com/interactive/2021/01/05/us/elections/results-georgia-senate-runoff-loeffler-warnock.html"}
  if(state=="NJ"){DFid=4 ; site="https://www.nytimes.com/interactive/2020/07/07/us/elections/results-new-jersey-president-democrat-primary-election.html"}
  
  H=read_html(site)
  info=html_text(html_nodes(H,".e-th , .e-reporting , .e-candidate div , .e-name"))
    ncols=grep("Rpt.",info)-grep("County|Town",info)+1
  data=t(matrix(info[-1:(-1*grep("County|Town",info)+1)],nrow=ncols)) ###nrow= rows in NYT table    ## -1:-20 = names above it)
  colnames(data)=data[1,] ; data=data[-1,] ; colnames(data)[1]="County" #this line to rename "Parish"
  data=gsub(",|%","",data)                              #remove , and %
  data=data.frame(data,stringsAsFactors = F)             #to data frame
  data[,-1]=sapply(data[,-1],as.numeric)                  #to numeric
  data=data[-(grep("View",data[,1])):-(length(data[,1])),] #Remove extra rows
  data[is.na(data)]=0 #NA=0
  
  if(initial){DF[[DFid]]=data} #set 'old' votes to current votes
  
  newVotes(data=data,ncols=ncols,DF=DF,DFid=DFid)#view new votes
  
  if(!initial){DF[[DFid]]=data}
  
  DF
}



DF=nyt("MA",DF,init=T) #first get
DF=nyt("MA",DF)     #subsequent gets
