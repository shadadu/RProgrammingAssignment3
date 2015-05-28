best<-function(state, outcome){
  
  outcome_data<-read.csv("outcome-of-care-measures.csv",colClasses="character")
  s0="Invalid outcome"
  if(outcome=="heart attack"){
  s<-names(outcome_data[11])
  }else if(outcome=="heart failure"){
  s<-names(outcome_data[17])
  }else if(outcome=="pneumonia"){
  s<-names(outcome_data[23])
  }else{
  #s=s0
    stop("Invalid outcome")
  }
  
  if(!(state %in% outcome_data[,"State"])  ){
       stop("Invalid state")
  }#else if(s==s0){
    #stop("Invalid outcome")
  #}
  else{
    state_indexes<-which(outcome_data[,"State"] %in% state)
    df<-data.frame(NA,NA)
    colnames(df)<-c("Hospital.Name","Outcome")
    n=1
    for(i in state_indexes){
      df[n,1]=outcome_data[i,"Hospital.Name"]
      df[n,2]=outcome_data[i,s]
      n=n+1
    }
    df[,2]<-as.numeric(df[,2])
    df<-df[complete.cases(df),]
    m<-min(df[,2],na.rm=TRUE)
   
    ##y<-tapply(outcome_data[,s],outcome_data[,"State"],min)
    ##as.numeric(y[state])
    min_index_arr<-which(df[,2] %in% m)
         
    best_hospitals<-df[min_index_arr,1]  
    
    sort(best_hospitals)[1]
  }
  
}