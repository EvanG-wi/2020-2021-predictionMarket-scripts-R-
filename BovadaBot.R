#Bovada Bot
#This bot looks at implied odds of a sports team winning a game on bovada (sportsbook) and compares to
#the price on polymarket(prediction market), and purchases shares on polymarket if there is a significant discrepancy
#This operates on the hypothesis that bovada is "better" at pricing than polymarket

asPercent=function(price){
  neg=F
  if(price<0){neg=T}
  price=100/(100+abs(price))
  if(neg){price=1-price}
  price
}


impliedOdds=function(team1,team2){
  team1Odds=mean(c(asPercent(team1),1-asPercent(team2)))
  team1Odds
}



buy1=function(topOrBottom="top"){
  
  if(topOrBottom=="top"){ greenButton=rd2$findElement(using="class",value="OutcomePicker_yesButtonContainer__1Viyr")}
  else{greenButton=rd2$findElement(using="class",value="OutcomePicker_noButtonContainer__1fo0X")}
  
  greenButton$clickElement()
  
  howMuchBox=rd2$findElement(using="xpath",value="/html/body/div[3]/main/main/article/div/main/section[1]/aside/div/div/form/label[2]/div/input")
  howMuchBox$clickElement()
  howMuchBox$sendKeysToElement(list("1.81"))  #buy amount
  
  buyButton=rd2$findElement(using="xpath",value="/html/body/div[3]/main/main/article/div/main/section[1]/aside/div/div/div[2]/button")
  buyButton$clickElement()
  
  checkBox=rd2$findElement(using="xpath",value="//*[@id='confirm']")
  checkBox$clickElement()
  
  confirmBox=rd2$findElement(using="xpath",value="/html/body/div[6]/div[3]/main/form/button")
  confirmBox$clickElement()
  
  #the page processes the transaction, usually for ~20 seconds, wait for the button that says "got it" to appear then click it
  Sys.sleep(10)
  while(!exists("gotIt")){
    try({gotIt=rd2$findElement(using="xpath",value="/html/body/div[6]/div[3]/main/div/button[2]")},1)
    #try({gotIt=rd2$findElement(using="xpath",value="/html/body/div[6]/div[3]/main/div/button")},1)
    Sys.sleep(.2)
  }

  gotIt$clickElement()
  rm(gotIt)
  refreshButton$clickElement()
  Sys.sleep(3) 
  
}


getPolyOdds=function(){
  topOdds=rd2$findElement(using="class",value="OutcomePicker_price__2TtOV")$getElementText()
  
  as.numeric(gsub('[$]','',topOdds))
}




#Initialize

library(RSelenium)

driver <- rsDriver(browser=c("firefox"),chromever="85.0.4183.87",port=4200L)
driver2<- rsDriver(browser=c("firefox"),chromever="85.0.4183.87",port=4201L)

rd <- driver[["client"]] #bovada
rd2=driver2[["client"]] #polymarket

rd$navigate("https://www.bovada.lv/sports/live") #navigate to appropriate pages
rd2$navigate("https://polymarket.com/")

log=data.frame(time=as.POSIXct(Sys.time()),polyOdds=0,impliedOdds=0)

refreshButton=rd2$findElement(using="xpath",value="/html/body/div[3]/main/main/article/div/main/section[1]/aside/div/div/form/label[1]/div[2]/button")


while(1){ #game loop
    
    #refreshPolyOdds
    refreshButton$clickElement()
    Sys.sleep(.05)
    
    #bovada odds to t1 and t2
    try({ 
       y=rd$findElements(using="class",value="bet-price") ;
       t1=as.numeric(y[[3]]$getElementText()) ;
       t2=as.numeric(y[[4]]$getElementText()) ;
       if(is.na(t1)){t1=100}
       if(is.na(t2)){t2=100}
       print(c(t1,t2))
    },1)
    
    polyOdds=getPolyOdds()
    bovOdds=impliedOdds(t1,t2)
    print(c(polyOdds,bovOdds))
    
    #compare-decide
    if(bovOdds-0.03>polyOdds){buy1("top") ; print(paste("impliedOdds:",bovOdds,"polyOdds:",polyOdds,"buying top"))}
    if(bovOdds+0.03<polyOdds){buy1("bottom") ; print(paste("impliedOdds:",bovOdds,"polyOdds:",polyOdds,"buying bot"))}
    
    log=rbind(log,data.frame(time=Sys.time(),polyOdds=polyOdds,impliedOdds=bovOdds))
}




######
        #plot the game log

log=log[-1,] 
plot(log$time,log$polyOdds,col="blue",pch=20)
points(log$time,log$impliedOdds,col="red",pch=20)

bought = subset(log,polyOdds-impliedOdds<(-.03))
sold = subset(log,polyOdds-impliedOdds>(.03))
points(bought$time,bought$polyOdds,col="green",pch=1,cex=3)
points(sold$time,sold$polyOdds,col="magenta",pch=1,cex=3)

######
