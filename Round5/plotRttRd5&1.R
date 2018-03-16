library(ggplot2)

getavgEast<-function(x,sdata){
  y<-nrow(sdata)*(x/100)
  print(y)
  y<-round(y, digits = 0)
  
  newdata<-sdata[1:y,]
  sumnewdata<-sum(newdata$jitterCol)
  print(sumnewdata)
  avgNewData<-sumnewdata/y
  #avgNewData<-round(avgNewData, digits = 0)
  return(avgNewData)
}

plotColumn = function(x, type)
{
  #Convert the errors to INF
  jitterCol="jitterCol"
  mydata = read.csv("https://raw.githubusercontent.com/CSUMB-CST499-S18/udp-mobile-analysis/jitter/Cumulative_Round01_Results_Final.csv", na.strings = c("N/A", " NA", "no effective service", " no effective service"))
  
  #mydatafiltered = mydata[c(1:8, 12, 30:53)]
  mydatafiltered = mydata[c(1:8, 12, 30:53, 20, 18, 15)]
  mydatafiltered = na.omit(mydatafiltered)
  
  View(mydatafiltered)
  
  names(mydatafiltered)[x]<-jitterCol
  mydatafiltered$jitterCol  <- gsub("^[a-z].*", -1, mydatafiltered$jitterCol)
  #mean(as.numeric(as.character(mydatafiltered$jitterCol)))
  mydatafiltered$jitterCol  <- gsub(-1,sum(as.numeric(as.character(mydatafiltered$jitterCol[mydatafiltered$jitterCol > 0])))/ length(as.numeric(as.character(mydatafiltered$jitterCol[mydatafiltered$jitterCol > 0]))), mydatafiltered$jitterCol)
  
  #convert char to numeric 
  mydatafiltered$jitterCol <- as.numeric(as.character(mydatafiltered$jitterCol))
  mydat<-mydatafiltered[mydatafiltered$Client.Type == " Phone",] #Client.Type for RD1
  
  View()
  
  #Sprint
  sprint<-mydat[mydat$Provider == "Sprint",]
  sprintsorted<-sprint[order(sprint$jitterCol),]
  
  #Verizon
  verizon<-mydat[mydat$Provider == "Verizon",]
  verizonsorted<-verizon[order(verizon$jitterCol),]
  
  #T-Mobile
  tmobile<-mydat[mydat$Provider == "T-Mobile",]
  tmobilesorted<-tmobile[order(tmobile$jitterCol),]
  
  #AT&T
  att<-mydat[mydat$Provider == "AT&T",]
  attsorted<-att[order(att$jitterCol),]
  
  #Sprint
  ss = c()
  for(i in 1:100)
    #for(i in 1:10)
  {
    ss<-c(ss,getavgEast(i,sprintsorted))
  }
  
  #Verizon
  sv =c()
  for(i in 1:100)
  {
    sv<-c(sv,getavgEast(i,verizonsorted))
  }
  #T-Mobile
  st =c()
  for(i in 1:100)
  {
    st<-c(st,getavgEast(i,tmobilesorted))
  }
  
  #ATT
  sa =c()
  for(i in 1:100)
  {
    sa<-c(sa,getavgEast(i,attsorted))
  }
  #####################################################################################
  percent = c()
  for(i in 1:100)
  {
    percent<-c(percent,i)
  }
  percent
  east=c()
  east<-cbind(percent,sv,sa,st,ss)
  
  east <- transform(east,  percent = as.numeric(percent), 
                    sv = as.numeric(sv), 
                    sa = as.numeric(sa), 
                    st = as.numeric(st), 
                    ss = as.numeric(ss))
  
  east
 if (type == "eRttAvg"  || type == "ePktAvg") {
    ggplot(east, aes(percent)) + 
      geom_line(aes(y = sv, colour = "Verizon"), linetype = "solid", size = 1) +
      geom_line(aes(y = sa, colour = "AT&T"), linetype = "solid", size = .90) +
      geom_line(aes(y = st, colour = "T-Mobile"), linetype = "solid", size = .80) +
      geom_line(aes(y = ss, colour = "Sprint"), linetype = "solid", size = .70) +
      scale_x_continuous(name="Percentage (%)", limits=c(0, 100)) +
      scale_y_continuous(name="East Average Loss (Milliseconds)") +
      ggtitle("East Rtt")
  }
}
par(mfrow=c(4,1))

plotColumn(35, "ePktAvg") 
#RD 05, Phone and carriers need a space in front of them.
#RD 01, Phone needs space infront of it, No space for carriers. 



