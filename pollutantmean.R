pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        ## NOTE: Do not round the result!
        
        
        
        ## store total sum and total non-na of each file in matrix and associate a pointer (count)
        totals<-matrix(nrow=length(id),ncol=2)
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
                
                ## calculate and store the relevant sum total and non-na from current datafile
                count<-count+1

                if(pollutant=="sulfate"){
                        totals[count,1]<-sum(data$sulfate,na.rm=TRUE)
                        totals[count,2]<-length(data$sulfate[!is.na(data$sulfate)])
                }else if(pollutant=="nitrate"){
                        totals[count,1]<-sum(data$nitrate,na.rm=TRUE)
                        totals[count,2]<-length(data$nitrate[!is.na(data$nitrate)])
                }
        }
        
        ## calculate the overall mean
        sum(totals[,1])/sum(totals[,2])
        
}