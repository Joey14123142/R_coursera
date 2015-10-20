pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  path <- directory
  files <- list.files(path)
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  num <- as.numeric(sub('\\.csv','',files))
  target <- files[match(id,num)]
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  dt <- lapply(file.path(path, target), read.csv)
  total <- do.call(rbind.data.frame, dt)
  mean(total[,pollutant], na.rm = T)
}

complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files

  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  nobs <- function(id) {
    path <- file.path(directory, paste(sprintf("%03d", as.numeric(id)), ".csv", sep=""))
    return (sum(complete.cases(read.csv(path))))
  }
  return (data.frame(id=id, nobs=sapply(id, nobs)))
}

corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  files <- list.files(directory)
  result <- c()
  for (file in files) {
    dt <- read.csv(file.path(directory, file))
    nob <- sum(complete.cases(dt))
    if (nob > threshold){
      result <- c(result,cor(dt$sulfate, dt$nitrate, use = "complete.obs"))
    }
  }
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  return (result)
}

