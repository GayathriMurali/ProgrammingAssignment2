
#This function creates a special matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  #Assign to NULL initially before caching begins  
    special_matrix<-NULL
  #Function below sets the value of matrix x 
    set<-function(y=matrix())
    {
      x<-y
      special_matrix<-NULL
    }
  
  #Function below gets the cached inverse of the matrix
    get<-function()x
  
  #Function that sets the inversed  matrix to the current matrix
    setinverse<-function(inverse) 
    {
      special_matrix<-inverse
    }
  
  #Function to retrieve the special matrix
    getinverse<-function()special_matrix
  #Return a list containing all the functions defined within makeCacheMatrix
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Function to compute inverse of a matrix
cacheSolve <- function(x, ...) {
  #Get the inverse cached matrix 
    special_matrix<-x$getinverse()
  #If inverse has been calculated
    if(!is.null(special_matrix))
    {
        message("Retreiving matrix from cache")
        return(special_matrix)
    }
  #Calculate matrix inverse if cache is empty
    data<-x$get()
    special_matrix<-solve(data)
  #Set the special_matrix to inversed matrix
    x$setinverse(special_matrix)
  #Return the inversed matrix
    special_matrix
}
