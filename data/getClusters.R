getClusters<-function(data) {
  mydata  = data
  mydata = as.data.frame(unclass(mydata))
  summary(mydata)
  dim(mydata)
  myDataClean = na.omit(mydata)
  dim(myDataClean)
  summary(myDataClean)
  library(readr)
  
  #Only include numeric columns
  myDataClean<-myDataClean %>% select_if(., is.numeric)
  if(ncol(myDataClean) > 0) {
      scaled_data = as.matrix(scale(myDataClean))
      data <- scaled_data
      
      set.seed(123)
      wss <- sapply(1:15, 
                        function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
      set.seed(123)
      clusters <- sapply(1:15, 
                    function(k){kmeans(data, k, nstart=50,iter.max = 15 )$cluster})
      colnames(clusters)<-paste0("Clusters_", seq(1:15))
      clusters<-cbind(mydata, clusters)
  } else {
    print("Error.  Numeric columns not found")
  }
  return(list(clusters, wss))
} 

#data<-getClusters(iris)
#clusters<-data.frame(data[1])
#wss<-data[[2]]

#plot(1:15, wss,
#        type="b", pch = 19, frame = FALSE, 
#        xlab="Number of clusters K",
#        ylab="Total within-clusters sum of squares")

