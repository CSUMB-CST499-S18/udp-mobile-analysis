library(ggplot2)
library(plot3D)
library(zoom)

mydata = read.csv("https://raw.githubusercontent.com/CSUMB-CST499-S18/udp-mobile-analysis/master/Cumulative_Round10_Results_Final.csv")
par(mfrow=c(1,15))

getavgWest<-function(x,sdata){
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
  #View(mydatafiltered)
  #Convert the errors to INF
  jitterCol="jitterCol"
  mydata = read.csv("https://raw.githubusercontent.com/CSUMB-CST499-S18/udp-mobile-analysis/master/Cumulative_Round10_Results_Final.csv")
  
  mydatafiltered = mydata[c(1:8, 12, 30:53)]
  
  names(mydatafiltered)[x]<-jitterCol
  mydatafiltered$jitterCol  <- gsub("^[a-z].*", -1, mydatafiltered$jitterCol)
  #mean(as.numeric(as.character(mydatafiltered$jitterCol)))
  mydatafiltered$jitterCol  <- gsub(-1,sum(as.numeric(as.character(mydatafiltered$jitterCol[mydatafiltered$jitterCol > 0])))/ length(as.numeric(as.character(mydatafiltered$jitterCol[mydatafiltered$jitterCol > 0]))), mydatafiltered$jitterCol)
  
  #convert char to numeric 
  mydatafiltered$jitterCol <- as.numeric(as.character(mydatafiltered$jitterCol))
  mydat<-mydatafiltered[mydatafiltered$DeviceType == "Phone",]
  
  
  
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
    ss<-c(ss,getavgWest(i,sprintsorted))
  }
  
  #Verizon
  sv =c()
  for(i in 1:100)
  {
    sv<-c(sv,getavgWest(i,verizonsorted))
  }
  #T-Mobile
  st =c()
  for(i in 1:100)
  {
    st<-c(st,getavgWest(i,tmobilesorted))
  }
  
  #ATT
  sa =c()
  for(i in 1:100)
  {
    sa<-c(sa,getavgWest(i,attsorted))
  }
  #####################################################################################
  percent = c()
  for(i in 1:100)
  {
    percent<-c(percent,i)
  }
  percent
  west=c()
  west<-cbind(percent,sv,sa,st,ss)
  
  west <- transform(west,  percent = as.numeric(percent), 
                    sv = as.numeric(sv), 
                    sa = as.numeric(sa), 
                    st = as.numeric(st), 
                    ss = as.numeric(ss))
  
  west
  
  
  #base code for making a 3D histogram
  # X,Y and Z values
  # x = c(1,2,3,4,5)
  # y = c(1,2,3,4,5)
  # 
  # zval = c(20.8, 22.3, 22.7, 11.1, 20.1,
  #          2.2,  6.7,  14.1, 6.6,  24.7,
  #          15.7, 15.1, 9.9,  9.3,  14.7,
  #          8.0,  14.3, 5.1,  6.5,  19.7,
  #          21.9, 11.2, 11.6, 3.9,  14.8 )
  # 
  # # Convert Z values into a matrix.
  # z = matrix(zval, nrow=5, ncol=5, byrow=TRUE)
  # 
  # 
  # hist3D(x,y,z, zlim=c(0,50), theta=40, phi=40, axes=TRUE,label=TRUE, nticks=5, 
  #        ticktype="detailed", space=0.5, lighting=TRUE, light="diffuse", shade=0.5)
  
  
  
  if(type == "eUDPLoss") {
    x = c(0, 0.1, 0.2, 0.3, 0.4)#geom_line(aes(y = sv, colour = "Verizon"), linetype = "solid", size = 1)
    y = c(0, 0.1, 0.2, 0.3, 0.4)#scale_y_continuous(name="East Average Jitter (Milliseconds)")
    
    zval = c(sv)#scale_size_continuous(name="Percentage (%)", limits=c(0,100))
    
    # Convert Z values into a matrix.
    z = matrix(zval, nrow=5, ncol=5, byrow=TRUE)
    
    
    hist3D(x,y,z, zlim=c(0,50), theta=40, phi=40, axes=TRUE,label=TRUE, nticks=5, 
         ticktype="detailed", space=0.5, lighting=TRUE, light="diffuse", shade=0.5)
  } else if(type == "eUDPJit") {
    ggplot(west, aes(percent)) + 
      geom_line(aes(y = sv, colour = "Verizon"), linetype = "solid", size = 1) +
      geom_line(aes(y = sa, colour = "AT&T"), linetype = "solid", size = .90) +
      geom_line(aes(y = st, colour = "T-Mobile"), linetype = "solid", size = .80) +
      geom_line(aes(y = ss, colour = "Sprint"), linetype = "solid", size = .70) +
      scale_x_continuous(name="Percentage (%)", limits=c(0, 100)) +
      scale_y_continuous(name="East Average Jitter (Milliseconds)") +
      ggtitle("East Phone")
  } else if(type == "wUDPJit") {
    ggplot(west, aes(percent)) + 
      geom_line(aes(y = sv, colour = "Verizon"), linetype = "solid", size = 1) +
      geom_line(aes(y = sa, colour = "AT&T"), linetype = "solid", size = .90) +
      geom_line(aes(y = st, colour = "T-Mobile"), linetype = "solid", size = .80) +
      geom_line(aes(y = ss, colour = "Sprint"), linetype = "solid", size = .70) +
      scale_x_continuous(name="Percentage (%)", limits=c(0, 100)) +
      scale_y_continuous(name="West Average Jitter (Milliseconds)") +
      ggtitle("West Phone")
  }
  else if(type == "wUDPLoss") {
    ggplot(west, aes(percent)) + 
      geom_line(aes(y = sv, colour = "Verizon"), linetype = "solid", size = 1) +
      geom_line(aes(y = sa, colour = "AT&T"), linetype = "solid", size = .90) +
      geom_line(aes(y = st, colour = "T-Mobile"), linetype = "solid", size = .80) +
      geom_line(aes(y = ss, colour = "Sprint"), linetype = "solid", size = .70) +
      scale_x_continuous(name="Percentage (%)", limits=c(0, 100)) +
      scale_y_continuous(name="West Average Loss (Milliseconds)") +
      ggtitle("West Phone")
  }
  
  
}
par(mfrow=c(4,1))
#Example: plotJitter(x)
# x = column number for column to plot
#west
# plotColumn(11, "wUDPLoss")
# plotColumn(13, "wUDPJit")
# plotColumn(16, "wUDPJit")
# plotColumn(28, "wUDPJit")
# #east
 plotColumn(19, "eUDPLoss")
# 
# #east lost
# plotColumn(20, "eUDPLoss")