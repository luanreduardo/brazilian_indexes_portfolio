##### code 003 #####
#previous: mom_estimator_function

#### estimates for data processes - FRAPO 10.3

#dimensions of simulation
#lists of data generated in data_generation_copulas.R
DGP <- c('rNcopMargN', 'rNcopMargT', 'rTcopMargT')
#names of the estimation methods function (non-robust and robust)
EST <- c('CovClassic', 'CovMcd', 'CovMest',
         'CovMMest', 'CovMve', 'CovOgk',
         'CovSde', 'CovSest')
#defining vector for 5, 10 and 15yr monthly sample
SAMPLE <- c(60, 120, 240)

#creates lists for combinations of DGP and sample sizes
datnames <- NULL
for (i in DGP) {
  for (j in SAMPLE) {
    objname <- paste(i, j, sep = '') #defines object name
    datnames <- c(datnames, objname)
    cat(paste('Creating list object', objname, '\n')) #shows what object name was defined
    assign(objname, lapply(eval(as.name(i)),       #extracts from each list elementand each DGP
                           function(x) x[1:j, ]))  #the number of rows in sample
  }
}
#nine new lists objects have been created by the process above

#creates lists with estimates of location and dispersion
#for combinations of DGP, sample sizes and estimators
objnames <- NULL
for (i in datnames) {
  for (j in EST) {
    objname <- paste(j, i, sep = '')
    objnames <- c(objnames, objname) #names the list objects that will store mean and cov
    cat(paste('Creating list object', objname, "\n")) #displays the names created
    assign(objname, lapply(eval(as.name(i)),#applies function for each list element in each DGP and sample size
                           Moments, method = j)) #function defined in mom_estimador_function.R
  }
}
#seventy-two list objects have been created by the process above
#each containing a dispersion estimate