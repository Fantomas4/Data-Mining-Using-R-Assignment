# Title     : TODO
# Objective : TODO
# Created by: Sierra Kilo
# Created on: 07-May-20

#options(stringsAsFactors=T)
groceries <- read.csv("GroceriesInitial.csv",header=TRUE,sep=",", stringsAsFactors=TRUE)
#str(groceries)

# unlist() combines the factor (list) representing each
# column into a unified list.
# levels() returns the distinct names of products found
# in the unified list containing the combined products
# of all transactions
productNames <- levels(unlist(groceries[,4:35]))
#print(productNames)

# Remove the "" element from productNames list
blank <- which(productNames == "") #row 102
productNames <- productNames[-blank]

# Apply anonymous function(x) to every row of columns 4->35 of groceries (MARGIN: 1).
# For every row, combine the elements of each column into a unified list using unlist().
# For every product name in productNames, check if the product name appears inside
# the unified list (row) returned by unlist(), returning TRUE or FALSE.
# As a result, a list containing TRUE and FALSE values in the order of the given
# product names in productNames is created and return, with the ith element in
# this list corresponding to the element of in the ith index of productNames.
productsBinary <- as.data.frame(t(apply(groceries[,4:35],1, function(x)
(productNames) %in% as.character(unlist(x)))))

# Set the names of productsBinary's elements to productNames' names.
names(productsBinary) <- productNames

# Keep only the 13 columns corresponding to the 13 products we need for our analysis
# as stated by the assignment's description.
filteredProductsBinary <- productsBinary[, c("citrus fruit", "tropical fruit", "whole milk", "other vegetables",
"rolls/buns", "chocolate", "bottled water", "yogurt", "sausage", "root vegetables", "pastry", "soda", "cream")]

# Combine groceries data frame's columns 1->3 with filteredProductsBinary 13 product columns
# into a unified data frame.
groceriesBinary <- cbind(groceries[,1:3], filteredProductsBinary)

#str(groceriesBinary)

# Quantile the basket_value column values into three discreet (and nearly equal) categories
groceriesDiscrete <- groceriesBinary
cutPoints <- quantile(groceriesDiscrete$basket_value, probs = seq(0, 1, 1/3), na.rm = TRUE, names = FALSE)

# Divide the range of groceriesDiscrete into intervals and code the values in groceriesDiscrete according
# to which interval they fall into. For this purpose, a "basket_value_bin" column is added to the data frame,
# with the labels "Low", "Medium" and "High" used for the resulting category.
groceriesDiscrete$basket_value_bin <- cut(groceriesDiscrete$basket_value, breaks = cutPoints,
                                          labels=c("Low","Medium","High"), include.lowest = TRUE)

#table(groceriesDiscrete$basket_value_bin)
#str(groceriesDiscrete)