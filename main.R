# Title     : TODO
# Objective : TODO
# Created by: Sierra Kilo
# Created on: 07-May-20

binarize <-function(dataColumns, extraColumns=NULL){
  # unlist() combines the factor (list) representing each
  # #item column into a unified list.
  # levels() returns the distinct names of products found
  # in the unified list containing the combined products
  # of all transactions
  columnNames <- levels(unlist(dataColumns))

  # Remove the "" element from (if it exists) from column names
  blank <- which(columnNames == "")
  if (length(blank) !=0)
    columnNames <- columnNames[-c(blank)]

  # Apply anonymous function(x) to every row of dataColumns (MARGIN: 1).
  # For every row, combine the elements of each column into a unified list using unlist().
  # For every name in columnNames, check if the name appears inside
  # the unified list (row) returned by unlist(), returning TRUE or FALSE.
  # As a result, a list containing TRUE and FALSE values in the order of the given
  # names in columnNames is created and returned, with the ith element in
  # this list corresponding to the element of in the ith index of columnNames.
  binaryResult <- as.data.frame(t(apply(dataColumns, 1
    , function(x) (columnNames) %in% as.character(unlist(x)))))

  # Set the names of productsBinary's elements to productNames' names.
  names(binaryResult) <- columnNames

  # Combine data columns and extra columns into a unified data frame.
  if (is.null(extraColumns)==FALSE)
    binaryResult<- cbind(extraColumns, binaryResult)

  return(binaryResult)
}

# Perform the actions described by exercise 1.
prepareData <- function() {
  #options(stringsAsFactors=T)
  groceries <- read.csv("GroceriesInitial.csv",header=TRUE,sep=",", stringsAsFactors=TRUE)
  #str(groceries)

  # Apply anonymous function(x) to every row of columns 4->35 of groceries (MARGIN: 1).
  # For every row, combine the elements of each column into a unified list using unlist().
  # For every product name in productNames, check if the product name appears inside
  # the unified list (row) returned by unlist(), returning TRUE or FALSE.
  # As a result, a list containing TRUE and FALSE values in the order of the given
  # product names in productNames is created and returned, with the ith element in
  # this list corresponding to the element of in the ith index of productNames.
  #productsBinary <- as.data.frame(t(apply(groceries[,4:35],1, function(x)
  #(productNames) %in% as.character(unlist(x)))))
  productsBinary <- binarize(groceries[,4:35], groceries[,1:3])

  # Keep only the 13 columns corresponding to the 13 products we need for our analysis
  # as stated by the assignment's description.
  filteredProductsBinary <- productsBinary[, c("citrus fruit", "tropical fruit", "whole milk", "other vegetables",
  "rolls/buns", "chocolate", "bottled water", "yogurt", "sausage", "root vegetables", "pastry", "soda", "cream")]

  # Combine groceries data frame's columns 1->3 with filteredProductsBinary 13 product columns
  # into a unified data frame.
  groceriesBinary <- cbind(groceries[,1:3], filteredProductsBinary)

  # Quantile the basket_value column values into three discreet (and nearly equal) categories
  groceriesDiscrete <- groceriesBinary
  cutPoints <- quantile(groceriesDiscrete$basket_value, probs = seq(0, 1, 1/3), na.rm = TRUE, names = FALSE)

  # Divide the range of groceriesDiscrete into intervals and code the values in groceriesDiscrete according
  # to which interval they fall into. For this purpose, a "basket_value_dis" column is added to the data frame,
  # with the labels "Low", "Medium" and "High" used for the resulting category.
  groceriesDiscrete$basket_value_dis <- cut(groceriesDiscrete$basket_value, breaks = cutPoints
    ,labels=c("low_value_basket","medium_value_basket","high_value_basket"), include.lowest = TRUE)

  groceriesDiscrete <- binarize(as.data.frame(groceriesDiscrete$basket_value_dis), groceriesDiscrete)
  groceriesDiscrete <- groceriesDiscrete[, -c(which(colnames(groceriesDiscrete)=="basket_value_dis"))]
  #str(groceriesDiscrete)

  return(groceriesDiscrete)
}

########################################################################################################################

# Perform the actions described by exercise 2.
testAssociationRules <- function(groceriesDiscrete) {
  library(arules)

  # ========== 2a) ==========
  # Apply apriori method to groceries discrete data with minimum support = 0.001
  print("rules test 1: ")
  rulesTest1 <- apriori(groceriesDiscrete[,4:ncol(groceriesDiscrete)], parameter = list(minlen=2, supp=0.001)
    ,control = list(verbose=FALSE))
  # Check the test results
  inspect(head(rulesTest1, n=20))

  # Apply apriori method to groceries discrete data with minimum support = 0.02
  print("rules test 2: ")
  rulesTest2 <- apriori(groceriesDiscrete[,4:ncol(groceriesDiscrete)], parameter = list(minlen=2, supp=0.02)
    ,control = list(verbose=FALSE))
  # Check the test results
  inspect(head(rulesTest2, n=20))

  # Apply apriori method to groceries discrete data with minimum support = 0.03
  print("rules test 3: ")
  rulesTest3 <- apriori(groceriesDiscrete[,4:ncol(groceriesDiscrete)], parameter = list(minlen=2, supp=0.03)
    ,control = list(verbose=FALSE))
  # Check the test results
  inspect(head(rulesTest3, n=20))

  # Apply apriori method to groceries discrete data with minimum support = 0.04
  print("rules test 4: ")
  rulesTest4 <- apriori(groceriesDiscrete[,4:ncol(groceriesDiscrete)], parameter = list(minlen=2, supp=0.04)
    ,control = list(verbose=FALSE))
  # Check the test results
  inspect(head(rulesTest4, n=20))
}


generateAssociationRules <- function(groceriesDiscrete) {
  library(arules)

  # ========== 2b) ==========
  productRules <- apriori(groceriesDiscrete[,4:(ncol(groceriesDiscrete)-3)], parameter = list(minlen=2, supp=0.001)
    ,control = list(verbose=FALSE))

  productRulesByConfidence <- sort(productRules, by="confidence")

  print("Top 20 product rules by Confidence: ")
  inspect(head(productRulesByConfidence, n=20))


  # ========== 2c) ==========
  # 0.018
  productAndValueRules <- apriori(groceriesDiscrete[,4:ncol(groceriesDiscrete)], parameter = list(minlen=2, supp=0.018)
    ,control = list(verbose=FALSE))

  productAndValueRulesByConfidence <- sort(productAndValueRules, by="confidence")

  print("Top 20 product and value category rules by Confidence: ")
  inspect(head(productAndValueRulesByConfidence, n=20))

}

########################################################################################################################
# Perform the actions described by exercise 3.

filterNormalizeCostRecency <- function(groceriesDiscrete) {
  # Only keep the properties (filter the properties) "basket_value" and "recency_days" from groceriesDiscrete.
  costAndRecency <- groceriesDiscrete[,c("basket_value", "recency_days")]

  # Normalize the values of "basket_value" and "recency_days".
  normalizedCostAndRecency <- scale(costAndRecency)

  return(normalizedCostAndRecency)
}

performClustering <- function(normalizedCostAndRecency) {
  # Execute the k-means algorithm.
  set.seed(1234)
  kmeansFit <- kmeans(normalizedCostAndRecency, 5, nstart = 1000, iter.max = 1000)

  return(kmeansFit)
}

printClusteringCharts <- function(groceriesDiscrete) {
  # ========== 3a) ==========

  # Only keep the properties (filter the properties) "basket_value" and "recency_days" from groceriesDiscrete.
  costAndRecency <- groceriesDiscrete[,c("basket_value", "recency_days")]

  # Only keep the properties (filter the properties) "basket_value" and "recency_days" from groceriesDiscrete.
  # Normalization is essential in order to get accurate clustering results from k-means.
  normalizedCostAndRecency <- filterNormalizeCostRecency(groceriesDiscrete)

  kmeansFit <- performClustering(normalizedCostAndRecency)
  print("k-means raw result: ")
  print(str(kmeansFit))

  # Visualize
  #library("factoextra")
  library(ggplot2)

  # Plot the clustered data returned from kmeans (using normalized axis values)
  print(ggplot() + ggtitle("Normalized clusters") +
    geom_point(data = as.data.frame(normalizedCostAndRecency), mapping = aes(x=recency_days, y=basket_value,
    colour = kmeansFit$cluster)) + scale_color_gradient(low="blue", high="red"))

  # Plot the clustered data using denormalized axis values
  print(ggplot() + ggtitle("Denormalized clusters") +
    geom_point(data = costAndRecency, mapping = aes(x=recency_days, y=basket_value,
    colour = kmeansFit$cluster)) + scale_color_gradient(low="blue", high="red"))


  # ========== 3b) ==========
  # Get the denormalized cluster centers
  denormalizedCenters <- t(apply(kmeansFit$centers, 1, function(r)
  r * attr(normalizedCostAndRecency, 'scaled:scale') + attr(normalizedCostAndRecency, 'scaled:center')))

  # Calculate the denormalized centers' mean
  centersMean <- cbind(mean(denormalizedCenters[,'recency_days']), mean(denormalizedCenters[,'basket_value']))
  print("The mean of the denormalized centers is: ")
  print(centersMean)

  # Calculate the denormalized centers' standard deviation
  centersStdev<- cbind(sd(denormalizedCenters[,'recency_days']), sd(denormalizedCenters[,'basket_value']))
  print("The standard deviation of the denormalized centers is: ")
  print(centersStdev)

  # Plot denormalized data + denormalized cluster centers
  print(ggplot() + ggtitle("Denormalized clusters and denormalized cluster centers") +

    geom_point(data = costAndRecency, mapping = aes(x=recency_days, y=basket_value,
    colour = kmeansFit$cluster)) + scale_color_gradient(low="blue", high="red") +

    geom_point(mapping = aes_string(x = denormalizedCenters[,'recency_days'],
                                    y = denormalizedCenters[,'basket_value']),
                                    color = "red", size = 4))

  # Plot denormalized data + denormalized cluster centers + mean and standard deviation of denormalized cluster centers
  print(ggplot() + ggtitle("Denormalized clusters + cluster centers + mean of centers + std of centers") +

    geom_point(data = costAndRecency, mapping = aes(x=recency_days, y=basket_value,
    color = kmeansFit$cluster)) + scale_color_gradient(low="blue", high="red") +

    geom_point(mapping = aes_string(x = denormalizedCenters[,'recency_days'],
                                    y = denormalizedCenters[,'basket_value']),
                                    color = "red", size = 4) +

    geom_point(mapping = aes_string(x = centersMean[, 1],
                                    y = centersMean[, 2]),
                                    color = "green", size = 6) +

    geom_point(mapping = aes_string(x = centersStdev[, 1],
                                    y = centersStdev[, 2]),
                                    color = "yellow", size = 6))


  # Visualize the size of each cluster using a pie chart
  pieRecencyValueData<- table(kmeansFit$cluster)
  pieRecencyValueData <- pieRecencyValueData/sum(pieRecencyValueData)*100
  pie(pieRecencyValueData, labels = paste(names(pieRecencyValueData), "\n", pieRecencyValueData, sep=""),
      main = "Size of clusters (%)")
}

generateGroceriesWithBinaryClusterData <- function(groceriesDiscrete, kmeansFit) {
  # ========== 3c ==========
  # Convert cluster data to a binary form.
  clusterBinary <- as.data.frame(t(sapply(kmeansFit$cluster, FUN=function(x)
  seq(1, nrow(kmeansFit$centers)) %in% x  )))

  # Set cluster names in clusterBinary
  names(clusterBinary) <- c("cluster1", "cluster2", "cluster3", "cluster4", "cluster5")

  groceriesWithClusters <- cbind(groceriesDiscrete, clusterBinary)

  return(groceriesWithClusters)
}

########################################################################################################################

# Perform the actions described by exercise 4.
clusterProductProfile <- function(groceriesWithClusters) {
  library(arules)

  #summary(groceriesWithClusters)
  #
  #str(groceriesWithClusters)
  ## supp=0.01, conf=0.4
  ## supp=0.0248, conf=0.4
  #supp=0.0095, conf=0.7

  productClusterRules <- apriori(groceriesWithClusters[,c(4:16, 20:24)], parameter = list(minlen=2, supp=0.01, conf=0.4),
     appearance = list (default="lhs",rhs=c("cluster1", "cluster2", "cluster3", "cluster4", "cluster5")),
                                 control = list(verbose=FALSE))


  productClusterRulesByConfidence <- sort(productClusterRules, by="confidence")

  #inspect(productClusterRulesByConfidence)


  print("Top 20 product/cluster rules by Confidence: ")
  inspect(head(productClusterRulesByConfidence, n=20))


  # Generate the top 20 product association rules for cluster1
  cluster1Rules <- apriori(groceriesWithClusters[,c(4:16, 20:24)], parameter = list(minlen=2, supp=0.001),
     appearance = list (default="lhs",rhs="cluster1"), control = list(verbose=FALSE))
  cluster1Rules <- sort(cluster1Rules, by="confidence")

  print("Top 20 rules by Confidence for *** Cluster 1 ***: ")
  inspect(head(cluster1Rules, n=20))


  # Generate the top 20 product association rules for cluster2
  # 0.0055
  cluster2Rules <- apriori(groceriesWithClusters[,c(4:16, 20:24)], parameter = list(minlen=2, supp=0.0055),
    appearance = list (default="lhs",rhs="cluster2"), control = list(verbose=FALSE))
  cluster2Rules <- sort(cluster2Rules, by="confidence")

  print("Top 20 rules by Confidence for *** Cluster 2 ***: ")
  inspect(head(cluster2Rules, n=20))


  # Generate the top 20 product association rules for cluster3
  cluster3Rules <- apriori(groceriesWithClusters[,c(4:16, 20:24)], parameter = list(minlen=2, supp=0.005, conf=0.1),
    appearance = list (default="lhs",rhs="cluster3"), control = list(verbose=FALSE))
  cluster3Rules <- sort(cluster3Rules, by="confidence")

  print("Top 20 rules by Confidence for *** Cluster 3 ***: ")
  inspect(head(cluster3Rules, n=20))

  # Generate the top 20 product association rules for cluster4
  #supp=0.012, conf=0.1
  cluster4Rules <- apriori(groceriesWithClusters[,c(4:16, 20:24)], parameter = list(minlen=2, supp=0.01, conf=0.2),
    appearance = list (default="lhs",rhs="cluster4"), control = list(verbose=FALSE))
  #
  cluster4Rules <- sort(cluster4Rules, by="confidence")

  print("Top 20 rules by Confidence for *** Cluster 4 ***: ")
  inspect(head(cluster4Rules, n=20))


  # Generate the top 20 product association rules for cluster5
  cluster5Rules <- apriori(groceriesWithClusters[,c(4:16, 18:24)], parameter = list(minlen=2, supp=0.001, conf=0.1),
    appearance = list (default="lhs",rhs="cluster5"), control = list(verbose=FALSE))

  cluster5Rules <- sort(cluster5Rules, by="confidence")

  print("Top 20 rules by Confidence for *** Cluster 5 ***: ")
  inspect(head(cluster5Rules, n=20))

}

execute <- function() {
  groceriesDiscrete <- prepareData()
  #str(groceriesDiscrete)

  # ============================================== Exercise 1 ==============================================
  #str(groceriesDiscrete)


  # ============================================== Exercise 2 ==============================================
  #testAssociationRules(groceriesDiscrete)
  #generateAssociationRules(groceriesDiscrete)


  # ============================================== Exercise 3 ==============================================
  printClusteringCharts(groceriesDiscrete)
  #test <- generateGroceriesWithBinaryClusterData(groceriesDiscrete, performClustering(filterNormalizeCostRecency(groceriesDiscrete)))

  # ============================================== Exercise 4 ==============================================
  #clusterProductProfile(generateGroceriesWithBinaryClusterData(groceriesDiscrete, performClustering(filterNormalizeCostRecency(groceriesDiscrete))))
}

test <- function() {
  groceriesDiscrete <- prepareData()
  groceriesClustered <- generateGroceriesWithBinaryClusterData(groceriesDiscrete, performClustering(filterNormalizeCostRecency(groceriesDiscrete)))
  #print(groceriesClustered$cluster1)
  #str(groceriesClustered)


  clusterNumbers <- performClustering(filterNormalizeCostRecency(groceriesDiscrete))$cluster

  groceriesClustered <- cbind(groceriesClustered, clusterNumbers)

  #library("writexl")
  #write_xlsx(groceriesClustered,"testOutput2.csv")
  #
  #groceries <- read.csv("GroceriesClustered.csv",header=TRUE,sep=",", stringsAsFactors=TRUE)
  #print(groceries$cluster0)
  #str(groceries)

}

#test()
execute()
