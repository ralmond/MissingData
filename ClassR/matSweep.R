### This is the matrix sweep operator:  Beaton (1964), Dempster (1969), Goodnight(1979).

### Version 0.1  RGA 2016-03-16


### Function matSweep
### Args:
###  A -- matrix to be swept
### ploc -- sequence of pivot locations.
### tol -- Check for zero pivot values (to avoid singular inversions)
### Output:  Matrix A with rows indicated by ploc swept out.
matSweep <- function (A, ploc= 1L:nrow(A), tol=.Machine$double.eps^0.5) {
  if (!is.matrix(A) || nrow(A) != ncol(A))
    stop("The first argument to matSweep must be a symmetric matrix")
  if (any(ploc < 1L) || any(ploc > nrow(A)) ||
      any( (ploc - round(ploc)) > tol))
    stop("Pivot locations must be integers referencing array rows.")
  ### Uses tail recursion to loop through
  matSweepAux(A,ploc,tol)
}

matSweepAux <- function (A, ploc, tol) {
  ## Base Case
  if (length(ploc) == 0L) return (A)
  ## Pivot first index
  k <- ploc[1]                          #Goodnight SWEEP(k) operator
  D <- A[k,k]                           #Step 1
  if (abs(D) < tol) {
    stop("Singular pivot at location",k)
  }
  A[k,] <- A[k,]/D                      #Step 2
  for (i in 1L:nrow(A)) {               #Step 3
    if (i != k ) {
      B <- A[i,k]
      A[i,] <- A[i,] - B*A[k,]
      A[i,k] <- - B/D
    }
  }
  A[k,k] <- 1/D                         #Step 4
  ## Recurse
  matSweepAux(A,ploc[-1],tol)
}


### Function revSweep
### Args:
###  A -- matrix to be unswept
### ploc -- sequence of pivot locations.
### tol -- Check for zero pivot values (to avoid singular inversions)
### Output: Matrix of the same size as A with the indicated rows unswept
revSweep <- function (A, ploc= 1L:nrow(A), tol=.Machine$double.eps^0.5) {
  if (!is.matrix(A) || nrow(A) != ncol(A))
    stop("The first argument to revSweep must be a symmetric matrix")
  if (any(ploc < 1L) || any(ploc > nrow(A)) ||
      any( (ploc - round(ploc)) > tol))
    stop("Pivot locations must be integers referencing array rows.")
  ### Uses tail recursion to loop through
  revSweepAux(A,ploc,tol)
}

###
## I'm having some issues here.  I'm taking the formula from Little &
## Rubin (2002), p 151, and adapting it to fit into the Algorithm
## version of Goodnight.  Goodnight does not give a reverse sweep.  
## Little and Rubin seem to imply that revSweep(matSweep(A,k),k)==A
## But this is not true.  However, in the cases I've tested
## matSweep(matSweep(A,k),k) == A
## Need more investigation.


revSweepAux <- function (A, ploc, tol) {
  ## Base Case
  if (length(ploc) == 0L) return (A)
  ## Pivot first index
  k <- ploc[1]                          #Goodnight SWEEP(k) operator
  D <- A[k,k]                           #Step 1
  if (abs(D) < tol) {
    stop("Singular pivot at location",k)
  }
  A[k,] <- -A[k,]/D                      #Step 2
  for (i in 1L:nrow(A)) {               #Step 3
    if (i != k ) {
      B <- A[i,k]
      A[i,] <- - B*A[k,]
      A[i,k] <- - B/D
    }
  }
  A[k,k] <- -1/D                         #Step 4
  ## Recurse
  revSweepAux(A,ploc[-1],tol)
}


### Test taken from http://www-rci.rutgers.edu/~dhjones/APPLIED_LINEAR_STATISTICAL_MODELS%28PHD%29/LECTURES/LECTURE06/3-The%20sweep%20operator.pdf
##
A11 <- matrix(c(2,3, 4,1),2,2)
A12 <- matrix(c(5,6),2,1)
A22 <- 70

A0 <- rbind(cbind(A11,A12),c(t(A12),70))

## Expected results
A1 <- matrix(c(.5,-1.5,-2.5, 2,-5,-4, 2.5,-1.5,57.5),3,3)
A2 <- matrix(c(-.1,.3,-1.3, .4,-.2,-.8, 1.9,.3,58.7),3,3)

A0.1 <- matSweep(A0,1)
stopifnot(all(abs(A1-A0.1)<.Machine$double.eps^.5))

A0.12 <- matSweep(A0,1:2)
stopifnot(all(abs(A2-A0.12)<.Machine$double.eps^.5))
