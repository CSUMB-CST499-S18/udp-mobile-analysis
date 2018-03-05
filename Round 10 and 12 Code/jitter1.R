library(ggplot2)
mydata = read.csv("https://raw.githubusercontent.com/CSUMB-CST499-S18/udp-mobile-analysis/jitter/Cumulative_Round10_Results_Final.csv")
mydatafiltered = mydata[c(1:8, 12, 30:53)]
View(mydatafiltered)
#Convert the errors to INF
mydatafiltered$wUDPJit1 = as.character(mydatafiltered$wUDPJit1)
mydatafiltered$eUDPJit1 = as.character(mydatafiltered$eUDPJit1)
mydatafiltered$wUDPJit1 <- gsub("^[a-z].*", "Inf", mydatafiltered$wUDPJit1)
mydatafiltered$eUDPJit1 <- gsub("^[a-z].*", "Inf", mydatafiltered$eUDPJit1)


#convert char to numeric 
mydatafiltered$wUDPJit1 <- as.numeric(as.character(mydatafiltered$wUDPJit1))
mydatafiltered$eUDPJit1 <- as.numeric(as.character(mydatafiltered$eUDPJit1))

View(mydatafiltered)
mydat<-mydatafiltered[mydatafiltered$DeviceType == "Phone",]
getavgWest<-function(x,sdata){
  y<-nrow(sdata)*(x/100)
  print(y)
  y<-round(y, digits = 3)
  
  newdata<-sdata[1:y,]
  sumnewdata<-sum(newdata$wUDPJit1)
  print(sumnewdata)
  avgNewData<-sumnewdata/y
  avgNewData<-round(avgNewData, digits = 3)
  return(avgNewData)
}
getavgEast<-function(x,sdata){
  y<-nrow(sdata)*(x/100)
  y<-round(y, digits = 3)
  
  newdata<-sdata[1:y,]
  sumnewdata<-sum(newdata$eUDPJit1)
  avgNewData<-sumnewdata/y
  avgNewData<-round(avgNewData, digits = 3)
  return(avgNewData)
}

#Sprint
sprint<-mydat[mydat$Provider == "Sprint",]
sprintSorted<-sprint[order(sprint$wUDPJit1),]
sprintSortedEast<-sprint[order(sprint$eUDPJit1),]
nrow(sprintSortedEast)

#Verizon
verizon<-mydat[mydat$Provider == "Verizon",]
verizonSorted<-verizon[order(verizon$wUDPJit1),]
verizonSortedEast<-verizon[order(verizon$eUDPJit1),]

#T-Mobile
tmobile<-mydat[mydat$Provider == "T-Mobile",]
tmobileSorted<-tmobile[order(tmobile$wUDPJit1),]
tmobileSortedEast<-tmobile[order(tmobile$eUDPJit1),]

#AT&T
att<-mydat[mydat$Provider == "AT&T",]
attSorted<-att[order(att$wUDPJit1),]
attSortedEast<-att[order(att$eUDPJit1),]


#Sprint
ss = c()
ssE = c()
for(i in 1:100)
  #for(i in 1:10)
{
  ss<-c(ss,getavgWest(i,sprintSorted))
  ssE<-c(ssE,getavgEast(i,sprintSortedEast))
  
}

#Verizon
sv =c()
svE =c()
for(i in 1:100)
{
  sv<-c(sv,getavgWest(i,verizonSorted))
  svE<-c(svE,getavgEast(i,verizonSortedEast))
  
}
#T-Mobile
st =c()
stE =c()
for(i in 1:100)
{
  st<-c(st,getavgWest(i,tmobileSorted))
  stE<-c(stE,getavgEast(i,tmobileSortedEast))
  
}

#ATT
sa =c()
saE =c()
for(i in 1:100)
{
  sa<-c(sa,getavgWest(i,attSorted))
  saE<-c(saE,getavgEast(i,attSortedEast))
  
}





#####################################################################################
percent =c()
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


east=c()
east<-cbind(percent,svE,saE,stE,ssE)
east <- transform(east,  percent = as.numeric(percent), 
                  svE = as.numeric(svE), 
                  saE = as.numeric(saE), 
                  stE = as.numeric(stE), 
                  ssE = as.numeric(ssE))
east
#joined east west
eastWest = c()
eastWest<-cbind(percent,sv,sa,st,ss,svE,saE,stE,ssE)
eastWest <- transform(eastWest,  percent = as.numeric(percent), 
                      svE = as.numeric(svE), 
                      saE = as.numeric(saE), 
                      stE = as.numeric(stE), 
                      ssE = as.numeric(ssE),
                      sv = as.numeric(sv),
                      sa = as.numeric(sa),
                      st = as.numeric(st),
                      ss = as.numeric(ss))
eastWest
# plot(ss,type = "l",col = "yellow", xlab = "Percent", ylab = "avgRtt", ylim=c(40,120),
#      main = "Rtt Comparison Chart", lwd = 5)
# 
# lines(sv, type = "l", col = "red", lwd = 5)
# lines(st, type = "l", col = "pink", lwd = 5)
# lines(sa, type = "l", col = "orange", lwd = 5)


View(mydat)

##############################################
ggplot(west, aes(percent)) + 
  geom_line(aes(y = sv, colour = "Verizon"), linetype = "solid", size = 1) +
  geom_line(aes(y = sa, colour = "AT&T"), linetype = "solid", size = .90) +
  geom_line(aes(y = st, colour = "T-Mobile"), linetype = "solid", size = .80) +
  geom_line(aes(y = ss, colour = "Sprint"), linetype = "solid", size = .70) +
  scale_x_continuous(name="Percentage (%)", limits=c(0, 100)) +
  scale_y_continuous(name="West Average UDP Jitter 1 (Milliseconds)", limits=c(0,15)) +
  ggtitle("West Phone UDP Jitter 1 (Round 10)")
# geom_line(linetype = "dashed") + 
# geom_point()
ggplot(east, aes(percent)) + 
  geom_line(aes(y = svE, colour = "Verizon"), linetype = "solid", size = 1) +
  geom_line(aes(y = saE, colour = "AT&T"), linetype = "solid", size = .90) +
  geom_line(aes(y = stE, colour = "T-Mobile"), linetype = "solid", size = .80) +
  geom_line(aes(y = ssE, colour = "Sprint"), linetype = "solid", size = .70) +
  scale_x_continuous(name="Percentage (%)", limits=c(0, 100)) +
  scale_y_continuous(name="East Average UDP Jitter 1 (Milliseconds)", limits=c(0,15)) +
  ggtitle("East Phone UDP Jitter 1 (Round 10)")



###############################################
dfwest <- data.frame(Carrier=rep(c("Verizon", "AT&T","T-Mobile","Sprint"), each=7),
                     Percentage=rep(c("1", "10", "30","50","70","80", "90"),4),
                     AvgUDPJitter=c(sv[1], sv[10], sv[30], sv[50], sv[70], sv[80], sv[90],
                              sa[1], sa[10], sa[30], sa[50], sa[70], sa[80], sa[90],
                              st[1], st[10], st[30], st[50], st[70], st[80], st[90],
                              ss[1], ss[10], ss[30], ss[50], ss[70], ss[80], ss[90]))
dfeast <- data.frame(Carrier=rep(c("Verizon", "AT&T","T-Mobile","Sprint"), each=7),
                     Percentage=rep(c("1", "10", "30","50","70","80", "90"),4),
                     AvgUDPJitter=c(svE[1], svE[10], svE[30], svE[50], svE[70], svE[80], svE[90],
                              saE[1], saE[10], saE[30], saE[50], saE[70], saE[80], saE[90],
                              stE[1], stE[10], stE[30], stE[50], stE[70], stE[80], stE[90],
                              ssE[1], ssE[10], ssE[30], ssE[50], ssE[70], ssE[80], ssE[90]))

# x axis treated as continuous variable
# dfwest$Percentage <- as.numeric(as.vector(dfwest$Percentage))
# 
# ggplot(data=dfwest, aes(x=Percentage, y=AvgUDPJitter, fill=Carrier)) +
#   geom_bar(stat="identity", position=position_dodge())+
#   scale_fill_brewer(palette="Paired")+
#   ggtitle("West Phone") +
#   theme_minimal()
# Axis treated as discrete variable
dfwest$Percentage<-as.factor(dfwest$Percentage)
ggplot(data=dfwest, aes(x=Percentage, y=AvgUDPJitter, fill=Carrier)) +
  geom_bar(stat="identity", position=position_dodge())+
  ggtitle("West Phone") +
  scale_fill_manual("Carriers", values = c("Verizon" = "red", 
                                           "AT&T" = "orange", 
                                           "T-Mobile" = "black", 
                                           "Sprint" = "blue")) +
  theme_minimal()

# ggplot east
dfeast$Percentage<-as.factor(dfeast$Percentage)
ggplot(data=dfeast, aes(x=Percentage, y=AvgUDPJitter, fill=Carrier)) +
  geom_bar(stat="identity", position=position_dodge())+
  ggtitle("East Phone") +
  scale_fill_manual("Carriers", values = c("Verizon" = "red", 
                                           "AT&T" = "orange", 
                                           "T-Mobile" = "black", 
                                           "Sprint" = "blue")) +
  theme_minimal()






