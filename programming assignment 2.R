setwd("/Users/xuebao/Desktop/specdata")
getwd()
csvpath = "/Users/xuebao/Desktop/specdata"
csvfilesn = list.files(path = csvpath, pattern = "*.csv")
tmprt = function(rtcsv){
  read.csv(rtcsv, stringsAsFactors = FALSE)
}
lhudata = lapply(paste(csvpath,csvfilesn,sep = ""), tmprt)
csvfilesn
data1=lhudata[[1]]
pollutantmean<- function(directory, pollutant, id = 1:332){
    names <- list.files(directory)[id]
    read <- lapply(paste(directory, "/", names, sep = ""),read.csv)
    return(mean(unlist(lapply(read,function(x){x[,pollutant]})),na.rm = T))
}

makeCacheMatrix<-function(x=matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) m <<- inverse
  getinverse<-function() m 
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve<-function(x,...){
  m<-x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data<- x$get()
  m<-solve(data, ...)
  x$setinverse(m)
  m
}

B<-matrix(c(1:16),4,4)
b<-makeCacheMatrix(B)
cacheSolve(b)
