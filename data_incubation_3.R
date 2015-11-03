########   data incubation
####  question 1

## make center is (0,0),  the position of the tourist is (ai,bi)
## i is the ith step, center is at 0th step, so (a0,b0)=(0,0)
## each time, there are 4 choice of new (a,b), according to the direction:
##   (a+1,b)  (a,b+1)  (a-1,b)  (a,b-1)

direction<-function(choice,ab){
  if(choice==1) ab<-ab+c(1,0)
  if(choice==2) ab<-ab+c(0,1)
  if(choice==3) ab<-ab+c(-1,0)
  if(choice==4) ab<-ab+c(0,-1)
  return(ab)
}

getout<-function(routine,block,move,type){
  success<-0
  ### try 1000 differenct routines, whitch means to make 1000 simulaitons
  for(r in 1:routine){
    ab<-c(0,0) ###(a0,b0)
    if(type=="after"){
      for(i in 1:move){
        choice<-sample(c(1:4),1,F) ## each step randomly choose a direction
        ab<-direction(choice,ab)  ## new(a,b) is new position for ith step
      }### after for loop ab is the position after m moves
      if(abs(ab[1])>=block | abs(ab[2]>=block)){
        success<-success+1
      }
    }
    if(type=="within"){
      for(i in 1: move){
        choice<-sample(c(1:4),1,F) 
        ab<-direction(choice,ab) 
        if((abs(ab[1])<block) & (abs(ab[2]<block))){
          next  
        }    ### if the tourist didn't get out, maintain current ab and continuew next iteration
        else{
          success<-success+1
          break
        }   ### if the tourist get out, it's a success and there is no need to continue to move
      }
    }
  }
  prob<-success/routine
  return(prob)
}
### for question 1 & 2
getout(routine=10000,block=3,move=10,type="after") 
getout(routine=10000,block=10,move=60,type="after") 

### for question 3 & 4
getout(routine=10000,block=5,move=10,type="within") 
getout(routine=10000,block=10,move=60,type="within") 

#### question 5 & 6
eastwest<-function(routine,move){
  success<-0
  for(r in 1:routine){
    ab<-c(0,0) ###(a0,b0)
    east<-0
      for(i in 1:move){
        choice<-sample(c(1:4),1,F) 
        ab<-direction(choice,ab) 
        if(ab[1]>1){
          east<-east+1  ### if ever east of East 1st St.
        }
      }
      if((east>0) && (ab[1]<(-1))){
        success<-success+1
      }
  }
  prob<-success/routine
  return(prob)
}

eastwest(routine=10000,move=10)##0.0055
eastwest(routine=10000,move=30)##0.0717

### question7 & 8
avgmove<-function(routine,block){
  m<-rep(1,routine)
  for(i in 1:routine){
    ab<-c(0,0)
    move<-0
    while((abs(ab[1])<block) & (abs(ab[2]<block))){
      choice<-sample(c(1:4),1,F) 
      ab<-direction(choice,ab)
      move<-move+1
    }
    m[i]<-move
  }
  return(mean(m))
}
avgmove(routine=10000,block=10)## 155.2096
avgmove(routine=10000,block=60)## 5616.533

#### question 2
all<-read.csv("file:///Users/**/Desktop/data-Oct /311_Service_Requests_from_2010_to_Present.csv")
attach(all)

### question 2.1
freq<-table(Agency)
facount<-function(n,freqfun){
  return(sapply(1:n,function(x) freqfun[[x]]))
}   ### this funciton can also be used later

count<-facount(25,freq)
sort(count,decreasing=T)[2]/sum(count)  ###  0.1897962

### question 2.2
sub<-as.data.frame(cbind(Borough,Complaint.Type))
contable<-as.data.frame(table(Complaint.Type,Borough)[,-1])
probtable<-data.frame()## create data frame to contain probability
for(b in 1:6){
  for(c in 1:181){
    conpro<-contable[c,b]/sum(contable[,b])
    unconpro<-sum(contable[c,])/sum(contable)
    ## get unconditional probability of this type of complain in all complains
    probtable[c,b]<-conpro/unconpro
  }
}
max(probtable)### 26.79822 is maximum

### question 2.3
### make a funciton to transform decimal to degree
lat<-quantile(Latitude,c(0.1,0.9),na.rm=T) ### get 10% is 40.62182 & 90% is 40.85257
trans.degree<-function(d){
  d1<-floor(d)
  d2<-floor((d-d1)*60)
  d3<-((d-d1)*60-d2)*60
  degree<-c(d1,d2,d3)
  names(degree)<-c("。","'","''")
  return(degree)
}
trans.degree(lat[2]-lat[1])##  0.00000 13.00000 50.67647 

### question 2.4
mu<-c(mean(Latitude,na.rm=T),mean(Longitude,na.rm=T))
sigma<-matrix(data=c(sd(Latitude,na.rm=T),0,0,sd(Longitude,na.rm=T)), nrow = 2)
### can get mean and sgma, calculate area based on simulaiton and enlarge certain proportion
require(mvtnorm)
simu<-rmvnorm(n=nrow(all),mean=mu,sigma=sigma)
plot(x=simu[,1],y=simu[,2])
a<-(41.75-39.75)/2
b<-((-73)-(-74.75))/2
area<-pi*(a*111.06)*(b*111.06) ## based online, 1 degree is 111.06 kilometers
area  ### 33905.74

### question 2.5
callnumber<-length(Created.Date)  
strdate<-as.character(Created.Date)
spl1<-strsplit(strdate," ") ##get a list of date, time and Am or PM

### make a data frame to caontian call time info
callinfo<-data.frame(x,y,z)
for(i in 1:callnumber){
  sp<-spl1[[i]]
  callinfo[i,1]<-sp[1]
  callinfo[i,2]<-as.numeric(strsplit(sp[2],":")[[1]][1])
  callinfo[i,3]<-sp[3]
} 
day<-callinfo[,1][!duplicated(callinfo[,1])]
###  each day, should have 12am+ 12pm
hour<-rep(0,24)
daydiffer<-function(date){
  for(tt in 1:12){
    hour[tt]<-nrow(callinfo[callinfo$x==date&callinfo$z=="AM"&callinfo$y==tt,])  ## day is para
    hour[tt+12]<-nrow(callinfo[callinfo$x==date&callinfo$z=="PM"&callinfo$y==tt,]) 
  } 
  differ<-max(hour)-min(hour)
  return(differ)
}  ## this function get call numbers for each hour during a day and return the difference between max & min
mean(sapply(day,daydiffer))  ## get expectation is 464.5

### question 2.6
## need to sort time and get sd of each deta between consecutive calls time
### swith time to decimal version(in seconds)   ### sort for each day ,am pm fenlei
seconds<-rep(0,callnumber)
for(i in 1:callnumber){
  spltime<-strsplit(spl1[[i]][2],":")
  seconds[i]<-as.numeric(spltime[[1]][1])*3600+as.numeric(spltime[[1]][2])*60+as.numeric(spltime[[1]][3])
}
timeinfo<-as.data.frame(cbind(callinfo[,c(1,3)],seconds))## get time info data frame
deta<-function(date){
  samday<-sort(timeinfo[timeinfo$x==date&timeinfo$z=="AM",3])
  spmday<-sort(timeinfo[timeinfo$x==date&timeinfo$z=="PM",3])
  deta.am<-samday[-1]-samday[-length(samday)] 
  deta.pm<-spmday[-1]-spmday[-length(spmday)] 
  deta.mid<-spmday[1]-samday[length(samday)]
  deta.day<-c(deta.am,deta.mid,deta.am) ### this is to get deta seconds between donsective calls for a day
  return(deta.day)
}
ss<-sapply(day,deta)
sd(c(ss[[1]],ss[[2]],ss[[3]],ss[[4]],ss[[5]],ss[[6]],ss[[7]],ss[[8]],ss[[9]],ss[[10]],ss[[11]],ss[[12]],
     ss[[13]],ss[[14]],ss[[15]],ss[[16]],ss[[17]],ss[[18]],ss[[19]],ss[[20]],ss[[21]],ss[[22]]),na.rm=T)
### 672.9751


### question 3 project
allstate<-paste(paste("'",state[1],"'"),paste("'",state[2],"'"),sep=",")
for(i in 3:length(state)){
  allstate<-paste(allstate,paste("'",state[i],"'"),sep=",")
}
allstate 
## this is what I need, to paste the result to python for further for loop to make more csv files 
### here I just ue MA as an example,these are sheets gotten by python
city<-read.csv("file:///Users/**/LocationInfogetCitiesInStateMA.csv")[,1:4]
zip<-read.csv("file:///Users/**/LocationInfogetZipCodesInStateMA.csv")[,1:3]
stateinfo<-read.csv("file:///Users/**/states.csv")[,1:4]
## these are sheets got from R
candidate<-read.csv("file:///Users/**/CandidateSummaryAction1.csv")
committe<-read.csv("file:///Users/**/CommitteeSummaryAction.csv")
expendure<-read.csv("file:///Users/**/independent-expenditure.csv")

state<-as.character(committe$sta[!duplicated(committe$sta)])
##need to use expendure sheet
avgcost<-rep(0,length(state))
for(i in 1:length(state)){
  substate<-expendure[which(expendure[,"can_off_sta"]==state[i]),]
  dd<-as.character(substate[,"agg_amo"])
  cost<-as.numeric(gsub(",","",sapply(strsplit(dd,split='$',fixed=TRUE),function(x) (x[2]))))
  spender<-substate[,"spe_id"][!duplicated(substate[,"spe_id"])]
  avgcost[i]<-sum(cost)/length(spender)
}
avgstate<-as.data.frame(cbind(state,avgcost))
## some states has no records that can be deleted later,51 records
## need to use stateinfo sheet
y<-rep(0,51)
x<-rep(0,51)
for (i in 1:51){
  stateinfo[,]
  y[i]<-stateinfo[which(stateinfo[,"stateCode"]==state[i]),][,"longitude"]
  x[i]<-stateinfo[which(stateinfo[,"stateCode"]==state[i]),][,"latitude"] 
}
lab<-round(as.numeric(as.character(avgstate[,2][1:51])),0)
plot(x,y,main="coordinate and average cost for commette",xlab="Latitude",ylab="Longitude"
     ,col="blue", pch=19,lty = "solid",lwd=2,ylim=c(-155,-60))
text(x,y,labels=lab,cex= 0.7,pos=3)

########  .........

