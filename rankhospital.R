rankhospital<-function(state, outcome, num){
    
  outcome_data<-read.csv("outcome-of-care-measures.csv",colClasses="character")
  s0="Invalid outcome"
  if(outcome=="heart attack"){
    s<-names(outcome_data[11])
  }else if(outcome=="heart failure"){
    s<-names(outcome_data[17])
  }else if(outcome=="pneumonia"){
    s<-names(outcome_data[23])
  }else{
    stop("Invalid outcome")
  }
  
  if( !(state %in% outcome_data[,"State"])  ){
    stop("Invalid state")
  }else{
    
    matchings<-vector("character")
    
        
    outcome_data[,s]<-as.numeric(outcome_data[,s])
    
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
    if(num=="worst"){
      num=length(df[,2])
    }else if(num=="best"){
      num<-1
    }else if(num>length(df[,2])){
      return(NA)
    }
    
    
    df<-df[order(df[,2], df[,1] ),  ]
    
    df[num,1]
      
  } 
  
 
 
  
}