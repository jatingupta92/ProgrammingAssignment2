## These functions take a matrix and check if its inverse is already stored in
## the cache memory. If yes, the inverse is returned from the cache memory else
## inverse of the matrix is calculated and store in the memory.

## comments included with each line of code

makeCacheMatrix <- function(x = matrix()) {

  n_row<-nrow(x) 
  n_col<-ncol(x)
  temp<-matrix(nrow=n_row,ncol=n_col) #define a temporary matrix with the same dimensions
  # as the matrix that is given as an arguement  
  setmatrix<-function(y)              
  {                                    # sets the original matrix as y and fills the 
    x<<-y                              # temp matrix with NA
    temp[temp]<<-NA
  }
  
  getmatrix<-function() x              #get the value of original matrix
  setinverse<-function(z) temp<<-z     #set the inverse of matrix as z
  getinverse<-function() temp         # get the value of inverse   
  
  list(set=setmatrix,get=getmatrix,getinverse=getinverse,setinverse=setinverse)
  
}


## comments included with each line of code

cacheSolve <- function(x, ...) {
  inverse<-x$getinverse() #gets the value of inverse
  
  if(!is.na(inverse[1,1])) #if value of first element of inverse is not NA
  {                        # returns the inverse from cache memory
    print("Cached Inverse")
    return(inverse)
    
  }
  
  else
  {
    matrix<-x$get()                 #if value of first element of inverse is NA
    inverse<-solve(matrix,...)      #get the defined matrix, calculate its inverse
    x$setinverse(inverse)           #store the inverse and then return
    return(inverse)
  }
}
