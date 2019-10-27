##### code 004 #####
#previous: dispersion_estimator

#### minimum-variance optimization - FRAPO 10.4

#creates a constrained function for minimum-variance portfolio
PortMinVar <- function(x){
  k <- ncol(x) 
  nnol <- nnoc(G = -diag(k), h = rep(0, k)) #long positions only
  A1 <- matrix(rep(1, k), nrow = 1) #constrain of fully invested
  b1 <- 1.0                         #constrain of fully invested
  opt <- cccp(P = x, q = rep(0, k), A = A1, b = b1, #function for solving "cone constrained convex programs"
              cList = list(nnol),
              optctrl = ctrl(trace = F))
  getx(opt)
}
#this funtion returns the weight vector for x

#### optimization ####
portnames <- NULL
idx <- 1:1000
for (i in objnames) {
  objname <- paste('Port', i, sep = '') 
  portnames <- c(portnames, objname) #name de DGP/sample combination
  obj <- eval(as.name(i))
  weights <- lapply(obj, PortMinVar) #performs the function for portfolio weights
  assign(objname, sapply(idx, function(x)
    sqrt(crossprod(weights[[x]], obj[[x]]) %*% weights[[x]]))) #gives portfolio returns (?)
}
#gives the min-var optimum portfolio for each combination of DGP and sample

#calculate median and IQR of portfolio risks
mednames <- NULL
iqrnames <- NULL
for (i in portnames) {
  objname1 <- paste('Med', i, sep = '') 
  objname2 <- paste('IQR', i, sep = '')
  mednames <- c(mednames, objname1) #setting names for each median
  iqrnames <- c(iqrnames, objname2) #setting names for each interquartile range 
  assign(objname1, median(eval(as.name(i)))) #calculates median
  assign(objname2, IQR(eval(as.name(i))))    #calculates iqr
}
#returns the median and iqr for each portfolio created

### JOB TO BE DONE: CREATE A TABLE