## Matrix inversion is a tedious process especially when the order
## of the matrix is too large. Programming language like R do take
## certain amount of storage and time while doing this operation.

## The cache Matrix Inverse helps the user and the R save some time 
## by storing the values of a specific matrix as cache memory and 
## thereby accessing the values instantly.

## The makeCacheMatrix function is used to create a special object 
## stores a matrix and caches its inverse. The function is used to 
## create a matrix and kept it as list of functions that 

## 1. set the value of matrix
## 2. get the value of matrix
## 3. set the value of inverse
## 4. get the value of inverse

## The whole function is divided into two parts

## This function sets the matrix in the required form as in the 
## form of list of functions.

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse) i<<-inverse
  getinverse<-function() i
  list(set=set,get=get,setinverse=setinverse,
       getinverse=getinverse)
  
}


## The following function is used to calculate the inverse of the matrix if not already
## been calculated otherwise it uses the cached value to print the inverse.
## If the matrix is new it uses solve() to find the inverse and 
## stores the value as cache so that it can be used in the future.

cacheSolve <- function(x, ...) {
  i<-x$getinverse()
  if(!is.null(i)){
    message("Getting the cached data")
    return(i)
  }
  data<-x$get()
  i<-solve(data,...)
  x$setinverse(i)
  i
}
# WORKING OUTPUT

M<-matrix(c(4,5,6,7,8,9,3,5,9,5,2,1,4,3,2,6),4,4) # Assigning a matrix of 4x4 to variable M
M1<-makeCacheMatrix(M) # The matrix M is converted into special form using makeCacheMatrix
cacheSolve(M1) # Solving the matrix M for the first time i.e. inverse of M
cacheSolve(M1) # Executing it a second time, the value is this obtained from the cache data