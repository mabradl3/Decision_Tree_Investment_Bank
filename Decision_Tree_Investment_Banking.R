# Marshall Bradley
# Decision Tree Example for an Investment Bank

# Open the required packages
library("rpart")
library('lattice')
library("rattle") # Fancy tree plot
library("rpart.plot") # Enhanced tree plots
library("RColorBrewer") # Color selection for fancy tree plot
library("party") # Alternative decision tree algorithm
library("partykit")

# Load in the dataset and create the training/test split
load("bankData.rdata")
options(digits=2)
set.seed(336)
perm=sample(1:41188)
bank_random = bank[perm,]
train = bank_random[1:floor(.75*41188),]
test = bank_random[(floor(.75*41188)+1):41188,]

# Create the tree
tree = rpart(next.product ~ . -next.product, data=train, method="class", parms = list(split="entropy"))

# Make adaptations to the tree/plot it -- note that this is a simple plot & more complex trees are below
.pardefault = par()
plot(tree, uniform = T)
text(tree, use.n=T)

# Plot the variable importance
tree$variable.importance
barchart(tree$variable.importance[order(tree$variable.importance)], xlab='Importance', horiz=T, ylab='Variable', main = 'Variable Importance')

# Score the data
trainscores = predict(tree, type='class')
bankscores = predict(tree, test, type='class')
cat('Training Misclassification Rate:', sum(trainscores!=train$next.product)/nrow(train))
cat('Testing Misclassification Rate:', sum(bankscores!=train$next.product)/nrow(train))


# Create new tree!
fancyRpartPlot(tree)
prp(tree)
prp(tree, type =3, extra=100) # label branches, label nodes with % of obs
prp(tree, type =3, extra=2) # label branches, label nodes with misclass rate
prp(tree, type =3, extra=8) # label branches, label nodes with pred prob of class
# BEWARE WITH BINARY TREES WHERE WE TYPICALLY WANT TO SHOW PROB OF SUCCESS/FAILURE
# FOR EVERY NODE IN THE TREE!
prp(tree, type =2, extra=8, leaf.round=1, border.col=1, box.col = brewer.pal(10,"Set3")[tree$frame$yval], )

