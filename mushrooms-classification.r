# read https://rstudio-pubs-static.s3.amazonaws.com/1776_dbaebbdbde8d46e693e5cb60c768ba92.html
# if this install not works install java jdk 8 and run "sudo R CMD javareconf"
install.packages("xlsx", dep = T)
library(xlsx)

mushrooms <- read.csv("mushrooms.csv", header = TRUE, sep = ',')

summary(mushrooms)

# delete veil.type (only one value)
mushrooms$veil.type <- NULL

# check that the dataset is reordened
head( mushrooms[1], 16 )

# create dataset to test and training
X <- mushrooms[,2:22]
y <- mushrooms[,1]

# calculate split 1/3 to test. This number is where the dataset must split the values
split <- length(mushrooms$class) - round(length(mushrooms$class)/3) 

trainInputs <- X[1:split,]
trainOutput <- y[1:split]
testInputs <- X[ (split + 1):length(mushrooms$class),]
testOutput <- y[(split + 1):length(mushrooms$class)]


install.packages("C50", dep = T)
library(C50)

# I create the model
model <- C50::C5.0( trainInputs, trainOutput )
summary( model )
plot( model )

# Launch the predict with the test data
prediction <- predict( model, testInputs, type="class" )

# Check the accuracy of the model
sum( prediction == testOutput ) / length( prediction )
# [1] 0.9302068

# Confusion matrix
table(prediction, testOutput)
#           testOutput
# prediction    e    p
#          e  537   45
#          p  144 1982


####################
# cross validation #
####################

Folds <- 10

# MODELS
# -------------------------------------------------------------------------------- 
Iter   <- data.frame(iteracion = NULL, percentHits = NULL)
for (i in 1:Folds)
{
  # Reorder dataset random
  mushroomsFolds <- mushrooms[ sample( nrow( mushrooms )), ]

  # create dataset to test and training
  X <- mushroomsFolds[,2:22]
  y <- mushroomsFolds[,1]
  
  # calculate split 1/3 to test. This number is where the dataset must split the values
  split <- length(mushroomsFolds$class) - round(length(mushroomsFolds$class)/3) 
  
  trainInputs <- X[1:split,]
  trainOutput <- y[1:split]
  testInputs <- X[ (split + 1):length(mushroomsFolds$class),]
  testOutput <- y[(split + 1):length(mushroomsFolds$class)]

  # Create the model  
  model <- C50::C5.0( trainInputs, trainOutput )
  # Launch the predict with the test data
  prediction <- predict( model, testInputs, type="class" )
  
  # Check the accuracy of the model
  percentHits <- sum( prediction == testOutput ) / length( prediction )
  
  Iter <- rbind(Iter, data.frame(Iter = i, percentHits = percentHits))  
}

# Plot
# -------------------------------------------------------------------------------- 
average  <- format(mean(Iter$percentHits, na.rm=TRUE)*100,digits = 4)
plot(Iter,type = "b", main = "% Prediction in each Iteration",  
     cex.axis = .7,cex.lab = .7,cex.main = .8, 
     xlab ="No. Iteration", ylab="% Prediction")
abline(h = mean(Iter$percentHits), col = "blue", lty = 2)
legend("topright", legend = paste("Effectivity Prediction =", average, "%"),
       col = "blue", lty = 2, lwd = 1, cex=.7, bg=NULL)

