# Title     : TODO
# Objective : TODO
# Created by: Sierra Kilo
# Created on: 07-May-20

options(stringsAsFactors=T)
groceries <- read.csv("GroceriesInitial.csv",header=TRUE,sep=",")
#str(groceries)

# unlist() combines the factor (list) representing each
# column into a unified list.
# levels() returns the distinct names of products found
# in the unified list containing the combined products
# of all transactions
productNames <- levels(unlist(groceries[,4:35]))
#print(productNames)

# remove the "" element from productNames list
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

#print(productNames)


## set the names of productsBinary's elements to productNames' names.
names(productsBinary) <- productNames


groceriesBinary <- cbind(groceries[,1:3],productsBinary)
