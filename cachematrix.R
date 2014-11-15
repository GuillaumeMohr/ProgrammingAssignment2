## These two functions build a special matrix object that caches its inverse
## so it does not have to compute it more than once.

## This function create a special matrix object that contains its inverse once it is
## computed for the first time. The object is accessible through a list of
## four functions : get, set, setinverse and getinverse.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL    #the variable that will contain the inverse matrix, at creation it is NULL  
    set <- function(y) {    #function that can change the matrix
        x <<- y     
        i <<- NULL       #if the matrix is modified, we set its inverse to NULL since we do not know it yet
    }
    get <- function() x    #return the matrix 
    setinverse <- function(inverse) i <<- inverse   #set the value of the inverse matrix
    getinverse <- function() i    #return the inverse matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)  #the special matrix object is actually a list of four functions
}


## This function returns the inverse of a special matrix either 
##  (i) by returning the inverse that is already cached in the special matrix
##      structure ; or
## (ii) by computing the inverse in case it has not been computed before. 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    #First, we check that the inverse is not already chached in the matrix object
    i <- x$getinverse()
    if(!is.null(i)) {   
        message("getting cached data") #the inverse has alread been computed, so we do not need to do it again
        return(i)
    }
    
    #if we arrive to this point, it means the inverse has not yet been computed
    data <- x$get()    #we get the matrix values
    i <- solve(data, ...)   #compute the inverse matrix
    x$setinverse(i)    #set the inverse in the special matrix object
    i    #return the inverse
}
