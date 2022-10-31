#SenateVoteModel, predict votes of non-voted Republican senators from partial vote totals
library(caret)
library(openxlsx)
library(beepr)

votes=read.xlsx("cabinetRvotes.xlsx")
row.names(votes)=votes$Senator ; votes=votes[,-1]
votes=as.data.frame(t(votes)) #transpose votes

nDem=50

library(doSNOW)  #makes it faster
cl=makeCluster(8,type="SOCK")

registerDoSNOW(cl)




###############################################################################################################################################

x=read.xlsx("SenateVote2.0.xlsx",cols=c(1,4,5),rows=1:51)   #get current vote from excel


  x$Voted=x$Yea+x$Nay #who has voted
  sensVoted=votes[,!!x$Voted] #vote history of senators who have voted

  testing=t(x$Yea[!!x$Voted])
  colnames(testing)=colnames(sensVoted)


  for(i in which(x$Voted==0)){      #for each non-voted senator
    training=cbind(sensVoted,votes[i]) #bind to training group
    
    form=as.formula(paste(colnames(votes)[i],"~",paste(colnames(training)[1:ncol(sensVoted)],collapse="+")))
    
    fit=train(form,data=training,method="rf") #train model
    
    x[i,2]=round((predict(fit,testing)),2)
  }
  
  #done
  print(sum(x$Yea)+nDem)
  print(paste("HIGH: ",sum(x$Yea>.25)+nDem))
  print(paste("LOW: ",sum(x$Yea>.75)+nDem))
  beep(sound=1)
  View(x)



  #################################################################################################################################################
  
stopCluster(cl)
  