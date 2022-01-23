## R Programming course: assignment week 3
## Code submitted by Rahul Shidhaye github username: rahulshidhaye
## 23 Jan 2022

makeCacheMatrix<-function(x=matrix ()){            ## a new function is created to cache the inverse of a special matrix. 'x' is the function argument. 
  m<-NULL                                          ## 'm' is an object with value set to NULL 
  set<-function (y){                               ## set function is defined in this step and 'y' is the argument for set function  
    x <<- y                                        ## assigns the value to matrix in paretn environment
    m <<- NULL                                     ## assigns the value of NULL to 'm'
  }
  get<-function () x                               ## get function is defined in this step and it returns value of the matrix argument 
  setinverse<- function (inverse) m <<- inverse    ## in this step value is assigned to 'm' in parent environment 
  getinverse<- function () m                       ## gets the value of 'm'
  list(set = set, get = get,                       ## the functions defined above are assigned as elements in a list in parent environment 
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve<-function (x, ...){                     ## cachesolve is a function which returns inverse of 'x'
  m<-x$getinverse ()                               ## in this step getinverse function is called on input matrix  
  if (!is.null(m)){
    message ("getting cached data")                ## inverse of the matrix is retrived from cache if the inverse has been calculted already and matrix has not changed. 
    return (m)
  }
  data<-x$get()
  m<-solve (data, ...)
  x$setinverse(m)
  m                                                ## prints the inverse of the matrix 
}