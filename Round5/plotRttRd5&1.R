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
  nas = c("N/A", " NA", "no effective service", " no effective service", " timeout", " connect_error1", "connect_error1",  " connect_error2")
  mydata = read.csv("https://raw.githubusercontent.com/CSUMB-CST499-S18/udp-mobile-analysis/jitter/Cumulative_Round12_Results_Final.csv", na.strings = nas)
  
  #mydatafiltered = mydata[c(1:8, 12, 30:53)]
  mydatafiltered = mydata[c(1:8, 12, 30:53, 20, 18, 15, 50:51, 54:55, 56:57, 60:61)] #18 = 35 ePktRtt, 37 & 39 = upload, 38 & 40 = Download   
  mydatafiltered = na.omit(mydatafiltered)
  
  View(mydatafiltered)
  
  names(mydatafiltered)[x]<-jitterCol
  mydatafiltered$jitterCol  <- gsub("^[a-z].*", -1, mydatafiltered$jitterCol)
  #mean(as.numeric(as.character(mydatafiltered$jitterCol)))
  mydatafiltered$jitterCol  <- gsub(-1,sum(as.numeric(as.character(mydatafiltered$jitterCol[mydatafiltered$jitterCol > 0])))/ length(as.numeric(as.character(mydatafiltered$jitterCol[mydatafiltered$jitterCol > 0]))), mydatafiltered$jitterCol)
  
  #convert char to numeric 
  mydatafiltered$jitterCol <- as.numeric(as.character(mydatafiltered$jitterCol))
  mydat<-mydatafiltered[mydatafiltered$DeviceType == "Phone",] #Client.Type for RD1 && Client_Type
  
  View()
  
  #Sprint                                   For Providers on Round 5 -> space in front of provider name
  sprint<-mydat[mydat$Provider == "Sprint",]
  sprintsorted<-sprint[order(sprint$jitterCol, decreasing = T),]
  
  #Verizon
  verizon<-mydat[mydat$Provider == "Verizon",]
  verizonsorted<-verizon[order(verizon$jitterCol, decreasing = T),]
  
  #T-Mobile
  tmobile<-mydat[mydat$Provider == "T-Mobile",]
  tmobilesorted<-tmobile[order(tmobile$jitterCol, decreasing = T),]
  
  #AT&T
  att<-mydat[mydat$Provider == "AT&T",]
  attsorted<-att[order(att$jitterCol, decreasing = T),]
  
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
 } else if (type == "eTCP_UP1"  || type == "eTCP_UP2" || type == "eTCPUp1" || type == "eTCPUp2") { #upload speed
    ggplot(east, aes(percent)) + 
      geom_line(aes(y = sv, colour = "Verizon"), linetype = "solid", size = 1) +
      geom_line(aes(y = sa, colour = "AT&T"), linetype = "solid", size = .90) +
      geom_line(aes(y = st, colour = "T-Mobile"), linetype = "solid", size = .80) +
      geom_line(aes(y = ss, colour = "Sprint"), linetype = "solid", size = .70) +
      scale_x_continuous(name="Percentage (%)", limits=c(0, 100)) +
      scale_y_continuous(name="East Upload Speed (kbps)") +
      ggtitle("East Upload Speed 2 (Round 12)")
  }  else if (type == "eTCP_DOWN1"  || type == "eTCP_DOWN2" || type == "eTCPDown1" || type == "eTCPDown2" ) { #upload speed
    ggplot(east, aes(percent)) + 
      geom_line(aes(y = sv, colour = "Verizon"), linetype = "solid", size = 1) +
      geom_line(aes(y = sa, colour = "AT&T"), linetype = "solid", size = .90) +
      geom_line(aes(y = st, colour = "T-Mobile"), linetype = "solid", size = .80) +
      geom_line(aes(y = ss, colour = "Sprint"), linetype = "solid", size = .70) +
      scale_x_continuous(name="Percentage (%)", limits=c(0, 100)) +
      scale_y_continuous(name="East Download Speed (kbps)") +
      ggtitle("East Download Speed 2 (Round 12)")
  }
}
par(mfrow=c(4,1))

#plotColumn(35, "ePktAvg") 
#RD 05, Phone and carriers need a space in front of them.
#RD 01, Phone needs space infront of it, No space for carriers. 

plotColumn(44, "eTCPDown2")
#50 on Spreadsheet -> eTCP_UP1 -> 37 on mydatafiltered
#51 on Spreadsheet -> eTCP_DOWN1 -> 38
#54 on Spreadsheet -> eTCP_UP2 -> 39
#55 on Spreadsheet -> eTCP_DOWN2 -> 40

#56 on Spreadsheet RD10 -> eTCPUp1 -> 41
#57 on Spreadsheet RD10 -> eTCPDown1 -> 42
#60 on Spreadsheet Rd10 -> eTCPUp2 -> 43
#61 on Spreadsheet Rd10 -> eTCPDown2 -> 44

