#The first function, makeCacheMatrix creates a "matrix"
#set the value of the vector
#get the value of the vector
#set the value of the mean
#get the value of the mean
makeCacheMatrix <- function(x = matrix()) {
m <- NULL 
set <- function(y) {
x <<- y
m <<- NULL
}
get <- function() x
setInverse  <- function(iVal) m <<- iVal
getInverse <- function() m
list(set = set,get = get , 
setInverse = setInverse,
getInverse = getInverse)
}

#The following function calculates the inverse of the "matrix"
#created with the above function. However, it first checks 
#to see if the mean has already been calculated. 
#If so, it gets the mean from the cache and skips the computation
#Otherwise, it calculates the inverse of the data and sets the value of
#the inverse in the cache via the setInverse function.
#it also checks for re-arranging a matrix in to square matrix 
#for processing inverse
# example  - a <- c(1,2,3,4) will stored as 2*2 matrix arranged by row
# sample run :
# a<- c(1,2,3,4)  
# m<-makeCacheMatrix(a)
# cacheSolve(m)
# cacheSolve(m) - matix inverse will be returned from cache at this step

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
    if(!is.null(m)) {
        message("getting matrix from cache")
        return(m)
    }
    data <- x$get()
    # If condition below decides if there is a need to convert
    # non-square matrix into a 
    # possible square matrix based on  two conditions :
    # condition 1 - if nrows = ncols of matrix
    # condition 2 - if  reminder of length of matrix divided by
    # squareroot(length)
    # is zero.
    # example - 4*1 matrix  - will be converted to 2*2 matrix
    tmpData <- matrix(data)
    rCount<- nrow(tmpData)       
    cCount<- ncol(tmpData)
    mLength <- length(tmpData)
    mLengthRoot<- sqrt(length(tmpData))
    if(!((rCount!= cCount) && !(mLength%%mLengthRoot==0)))
    data<- matrix(data, ncol=sqrt(length(data)), byrow=TRUE) 
    m <- solve(data)
    x$setInverse(m)
    m
}
