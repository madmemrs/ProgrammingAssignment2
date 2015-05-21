makeCacheMatrix <- function(x=matrix()){

	## makeCacheMatrix takes a square invertable matrix
	## The function will create and cache the inverse 
	## If the inverse is already cached and the original matrix
	## is unchanged, the cached inverse is returned.


	## check if x is a matrix
	booli <- is.matrix(x)
	if(!booli){
		stop("makeCacheMatrix only accepts a square and invertable matrix")
		
	}

	## check if matrix is square
	nr <- nrow(x)
	nc <- ncol(x)

	if(!(nr == nc)){
		stop("makeCacheMatrix only accepts a square and invertable matrix")
		

	}
	
	invs <- NULL
	set <- function(y){

		x <<- y  ## create x outside of this environment
		invs <<- NULL

	}## end function
	
	get <- function() {
		x  ## returns x from above 
	} ## end function

	setinvs <- function(invs){
		invs <<- invs
	} ## end function

	getinvs <- function() invs  ##inline function that returns the inverse

	## makeCacheMatrix returns a list 
	list(set=set, get=get,setinvs=setinvs,getinvs=getinvs)


} ## end of makeCacheMatrix

cacheSolve <- function(x, ...){
	
	invs <- x$getinvs()
	if(!is.null(invs)){
		message("getting cached inverse")
		return(invs)
	} ## end if

	data <- x$get()
	invs <- solve(data, ...)
	x$setinvs(invs)

	invs


} ## end of cacheSolve
