## Put comments here that give an overall description of what your functions do
## Regards! Hector
## Define the local environment variables and functions necesarry for the Cache Matrix

makeCacheMatrix <- function ( x = matrix() ) {
      
  m <- NULL ;
  
  set <- function( y ) {
    x <<- y ;
    m <<- NULL ;
  }
  
  get <- function() x ;
  
  setinverse <- function(mean) m <<- mean ;
  getinverse <- function() m ;
  
  list( set = set, get = get, setinverse = setinverse, getinverse = getinverse ) ;
  
}

## As the assignment commands, this fnctionfunction retursn a matrix that is the inverse of 'x' and caches it as possible

cacheSolve <- function ( x = makeCacheMatrix(), ... ) {
  
  m <- x$getinverse() ;
  if( !is.null( m ) ) 
  {
    message( "getting cached data" ) ;
    return( m ) ;
  }
  else    
  {  
    data <- x$get() ;
    m <- solve( data, ... ) ;
    x$setinverse( m ) ;
    message( "normally calculated" ) ;
    return( m ) ;
  }
  
}
