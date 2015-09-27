## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#This function creates a list containing a function to
# 1. set the input matrix
# 2. get the input matrix
# 3. set the inverse matrix
# 4. get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL  
  set<-function(y) {
    x<<-y       #set the input matrix
    inv<<-NULL
  }
  get<-function() x #get the input matrix
  setinv<-function(inverse) inv<<-inverse #set the inverse matrix
  getinv<-function() inv   #get the inverse matrix
  list(set=set, get=get,  #create a list with the above values
       setinv=setinv,
       getinv=getinv)

}


## Write a short comment describing this function
# The function below calculates the inverse of the input matrix created by the above function. It first checks 
# to see whether the inverse has already been calculated. If so, it gets the inverse, displays a message and the inverse.
# Otherwise it calculates the inverse, sets the value of inverse in the cache via the setinv function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<-x$getinv()  #read the inv matrix from the list
  if(!is.null(inv)) {  #if the inv matrix is not empty display a message
    message('getting cached data')
    return(inv) #display the inverse matrix
  }
  #otherwise calculate the inverse
  data<-x$get() #retrieve the matrix
  inv<-solve(data,...) #calculate the inverse
  x$setinv(inv) #assign the inverse to the setinv element in the list
  inv #display the inverse
  }
  
  
  

