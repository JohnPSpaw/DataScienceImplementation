#KNN Implementation
#John Spaw

#Implemented for Iris dataset.... wish to generalize....

#Euclidean Distance Calculation
#Input: Two vectors of equal length
#Output: Scalar with euclidean distance between two vectors
euclideanDist.func <- function(a,b)
{
  d <- 0 #initialize distance at zero... will add to d for each index of the vectors
  for(i in c(1:(length(a)-1))) #For each variable (ignoring the label)
  { 
    sq_dist <- (a[[i]] - b[[i]])^2 #take squared distance between vectors at index i
    d <- d + sq_dist
  } 
  d <- sqrt(d)
  return(d)
}

#KNN prediction function
#Input: Test set, training set, and knum=number of neighbors to compare
#Output: Vector of prediction labels for test set
knn_predict.func <- function(test_set, train_set, k_num)
{

  train_labels <- train_set[,5] #vector of labels for training set
  test_labels <- rep(NA,nrow(test_set))

  #Loop through each point in the test set:
  for(i in c(1:nrow(test_set)))
  {
    test_point <- test_set[i,]            #Test vector
    dist <- rep(NA, nrow(train_set))      #vector of distances between individual point and all points in training set
    
    #Loop through each point in the training set (for each in the test set) and compute EU distance
    for(j in c(1:nrow(train_set)))
    {
      train_point <- train_set[j,]
      dist[j] <- euclideanDist.func(train_point, test_point)
    }
    
    #Create, sort, and reduce dataframe of distances/labels
    dist_label.df <- data.frame(dist,train_labels)                
    dist_label.df <- dist_label.df[order(dist_label.df$dist),]     #Sort dataframe by distance
    dist_label.df <- dist_label.df[1:k_num,]                       #Subset to k nearest neighbors
    
    #print(dist_label.df)
    #Count train label votes from distance dataframe
    
    setosa <- 0
    versicolor <- 0
    virginica <- 0
    
    for(k in 1:nrow(dist_label.df))
    {
      if(as.character(dist_label.df[k,"train_labels"]) == "setosa")
      {
        setosa <- setosa + 1  
      }
      else if(as.character(dist_label.df[k,"train_labels"]) == "versicolor")
      {
        versicolor <- versicolor + 1  
      }
      else
      {
        virginica <- virginica + 1
      }
    }
    
    #Decide label from max votes
    votes <- data.frame(setosa, versicolor, virginica)
    #print(votes)
    winner_index <- which(votes == max(votes))
    test_labels[i] <- names(votes)[winner_index]
  }
  return(test_labels)
}  

#Accuracy calculation
accuracy <- function(test_data){
  correct = 0
  for(i in c(1:nrow(test_data))){
    if(test_data[i,5] == test_data[i,6]){ 
      correct = correct+1
    }
  }
  accu = correct/nrow(test_data) * 100  
  return(accu)
}






