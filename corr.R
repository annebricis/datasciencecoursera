corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!
        
        numOfFiles<-length(dir('specdata'))
        
        correlations<-rep(NA,numOfFiles)
        
        for(i in 1:numOfFiles){
                
                ## format datafile name using id provided by prefixing with 0 or 00 if necessary
                prefix<-if(i<10){
                        "00"
                }else if(i<100){
                        "0"
                }else{
                        ""
                }
                
                monitorFile<-paste(prefix,i,".csv",sep="")      ## sets up file name eg 002.csv
                
                result<-complete(directory,i)
                observed <-result[1,2]
                
                if(observed > threshold){
                        data<-read.csv(file.path(directory,monitorFile))
                        
                        sulfate<-data$sulfate[!is.na(data$sulfate) & !is.na(data$nitrate)]
                        nitrate<-data$nitrate[!is.na(data$sulfate) & !is.na(data$nitrate)]
                        
                        correlations[i]<-cor(sulfate,nitrate)
                }
        }
        correlations[!is.na(correlations)]
}