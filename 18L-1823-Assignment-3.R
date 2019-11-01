datAll=read.table("assign3GaussTrain.txt") 	#replace … with filename
datAllTest = read.table("assign3GaussTest.txt")

datAll = data.matrix(datAll)	#this will convert to a matrix data structure
datAllTest = data.matrix(datAllTest)

# above is a built in function for reading text files.  The entire dataset will be stored in datAll


ncol(datAll)		# this returns total cols in datAll

labels = datAll[,ncol(datAll)]  		#this will store last column of datAll in labels
trainX = datAll[,-ncol(datAll)]		#this will store all features except label in dat

oneClass = labels==1			#this will give you all indices of class = 1
zeroClass = labels==0		#this will give you all indices of class = 0

oneDat = trainX[oneClass,]		#this will give you the data matrix for class = 1
zeroDat = trainX[zeroClass,]		#this will give you the data matrix for class = 0


oneMean = colMeans(oneDat)	#this will give the mean of class = 1

oneCov    = cov(oneDat)		#this will give the covariance matrix of class = 1	
invOneCov = solve(oneCov)		#this will give the inverse of matrix

zeroMean = colMeans(zeroDat)
zeroCov = cov(zeroDat)	
print(zeroCov)

overallCovM = cov(trainX)

invZeroCov = solve(zeroCov)	

find_map_one <- function(point,case) {
 

  len_found = length(point)
  
  covM = diag(len_found)
  zeroCovM = diag(len_found)
  oneCovM =  diag(len_found)
  
  if(case == 1) {
    covM = diag(len_found)
    zeroCovM = diag(len_found)
    oneCovM =  diag(len_found)
  }
  else if(case == 2){
    covM = overallCovM
    zeroCovM = overallCovM
    oneCovM = overallCovM
  }else if(case == 3){
    covM = oneCov
    zeroCovM = zeroCov
    oneCovM = oneCov
  }
  
  prob_c_one = mean(oneClass)
  prob_c_zero = 1 - prob_c_one
  
  like_lihood = gauss(point,oneMean,covM)
  
  p_of_x = (gauss(point,zeroMean,zeroCovM)*prob_c_zero) + (gauss(point,oneMean,oneCovM)*prob_c_one)
  
  posterior = ((like_lihood * prob_c_one)/p_of_x)
  

  return(posterior)
  
}

find_map_zero <- function(point,case) {
  
  
  len_found = length(point)
  
  covM = diag(len_found)
  zeroCovM = diag(len_found)
  oneCovM =  diag(len_found)
  
  if(case == 1) {
    covM = diag(len_found)
    zeroCovM = diag(len_found)
    oneCovM =  diag(len_found)
  }
  else if(case == 2){
    covM = overallCovM
    zeroCovM = overallCovM
    oneCovM = overallCovM
  }else if(case == 3){
    covM = zeroCov
    zeroCovM = zeroCov
    oneCovM = oneCov
  }
  
  
  print(covM)
  
  prob_c_one = mean(oneClass)
  prob_c_zero = 1 - prob_c_one
  
  like_lihood = gauss(point,zeroMean,covM)
  
  p_of_x = (gauss(point,zeroMean,zeroCovM)*prob_c_zero) + (gauss(point,oneMean,oneCovM)*prob_c_one)
  
  posterior = ((like_lihood * prob_c_zero)/p_of_x)
  
  
  return(posterior)
  
}


  

gauss <- function(x, meanVe, covM) { 
  
  x_mins_u = x - meanVe
  inv_covM = solve(covM)
  x_minus_u_transpose = t(x_mins_u)
  exp_upper = exp((-1/2)*((x_minus_u_transpose)%*%(inv_covM)%*%(x_mins_u)))
  n = length(x)
  mod_sigma_squared = sqrt(det(covM))
  lower = ((2*pi)^(n/2)) * mod_sigma_squared
  final = exp_upper/lower
  return(final)
  
}


get_all_maps_for_mistakes <- function(data_of_interest, case){
  
  
  for (value in 1:nrow(data_of_interest)){
    
    point = data_of_interest[value,]
    map_zero = find_map_zero(point,case)
    map_one = find_map_one(point,case)
    print(map_zero)
    print(map_one)
    predicted_value = 0
    if(map_zero < map_one){
      predicted_value = 1
    }
    print(predicted_value)
    perdicted_labels <<- append(perdicted_labels,predicted_value)
    
  }
  
}

perdicted_labels <- vector()

perdicted_matrix <- matrix(nrow = 0, ncol=3)


get_all_maps_for_decision_boundary <- function(data_of_interest, case){
  
  
  for (value in 1:nrow(data_of_interest)){
    
    point = data_of_interest[value,]
    map_zero = find_map_zero(point,case)
    map_one = find_map_one(point,case)
    print(map_zero)
    print(map_one)
    predicted_value = 0
    if(map_zero < map_one){
      predicted_value = 1
    }
    print(predicted_value)
    perdicted_matrix <<- rbind( perdicted_matrix,c(point[1],point[2], predicted_value))
    
  }
  
}

for (value in 1:3) {
  
  perdicted_labels <<- vector()
  
  get_all_maps_for_mistakes(trainX, value)
  
  print(perdicted_labels)
  print(length(perdicted_labels))
  
  
  plot(oneDat[,1],oneDat[,2],col='yellow')  	#this will plot data points for class one in yellow color
  
  points(zeroDat[,1],zeroDat[,2],col='green') #this will plot data points for class zero in green color 
  
  mistake = perdicted_labels!=labels
  
  new_labels = data.matrix(perdicted_labels)
  
  points(trainX[mistake,1],trainX[mistake,2],col='red')
  
}

for (value in 1:3) {
  
  perdicted_matrix <- matrix(nrow = 0, ncol=3)
  
  get_all_maps_for_decision_boundary(datAllTest , value)
  
  
  labels_cur = perdicted_matrix[,ncol(perdicted_matrix)]  		#this will store last column of datAll in labels
  testX_cur = perdicted_matrix[,-ncol(perdicted_matrix)]		#this will store all features except label in dat
  
  oneClass_cur = labels_cur == 1			#this will give you all indices of class = 1
  zeroClass_cur = labels_cur == 0		#this will give you all indices of class = 0
  
  oneDat_cur = testX_cur[oneClass_cur,]		#this will give you the data matrix for class = 1
  zeroDat_cur = testX_cur[zeroClass_cur,]		#this will give you the data matrix for class = 0
  
  
  plot(oneDat_cur[,1],oneDat_cur[,2],col='yellow')  	#this will plot data points for class one in yellow color
  
  points(zeroDat_cur[,1],zeroDat_cur[,2],col='green') #this will plot data points for class zero in green color 
  
  
}

