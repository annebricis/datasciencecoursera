complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        
        observations<-data.frame(nrow=length(id),ncol=2)
        obshdr<-c("id","nobs")
        colnames(observations)<-obshdr
        count<-0
        
        for(i in id){
                
                ## format datafile name using id provided by prefixing with 0 or 00 if necessary
                prefix<-if(i<10){
                        "00"
                }else if(i<100){
                        "0"
                }else{
                        ""
                }
                
                monitorFile<-paste(prefix,i,".csv",sep="")      ## sets up file name eg 002.csv
                
                ## read the file which is located in the directory provided eg specdata/002.csv
                data<-read.csv(file.path(directory,monitorFile))   
                
                count<-count+1
                
                ## calculate number of observations
                observations[count,1]<-i
                observations[count,2]<-length(data$sulfate[!is.na(data$sulfate) & !is.na(data$nitrate)])
                
        }
        observations
        
}