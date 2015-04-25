## R Programming Week 2 Assignment - Jason Goldwater
## This project is to learn about the concepts of lexical scoping with the idea that we can
## create a cache to perform functions on long data sets or vectors

## The disucssion group among fellow colleagues was very helpful for this, which I initially found
## very challenging.


## makeCacheMatrix will receive a matrix vairable, and set the variables and functions in memory,
## and will, hopefully, return a list of functions nested with the makeCacheMatrix function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The cacheSolve function will receive a variable that is a matrix defined by makeCacheMatrix()
## It will first check to see if there is a non-NULL value in m
## if it finds a non-NULL value for m in the cache already, it will return that value
## If it does not fina an existing non-NULL value in the cache, it will get the commandline values for m and return
#3 and inverse of that matrix .

cacheSolve <- function(x, ...) {
 inv <- x$getinverse()
 if(!is.null(inv)) {
   message("getting cached data.")
   return(inv)
 }
 data <- x$get()
 inv <- solve(data)
 x$setinverse(inv)
 inv
}

## Testing functions
## > x = rbind(c(1, 4), c(4, 1))
## > m = makeCacheMatrix(x)
## > m$get()
## [,1] [,2]
## [1,]    1    4
## [2,]    4    1
## > cacheSolve(m)
## [,1]        [,2]
## [1,] -0.06666667  0.26666667
## [2,]  0.26666667 -0.06666667
## > cacheSolve(m)
## getting cached data.
## [,1]        [,2]
## [1,] -0.06666667  0.26666667
## [2,]  0.26666667 -0.06666667
## > 
