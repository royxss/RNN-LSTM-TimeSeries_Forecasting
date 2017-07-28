# Read file
setwd("C:\\Users\\SROY\\Desktop\\Courses\\Practicum\\IIT Predictive Analytics")
#load("PAT Method2.RData")
library("keras")
library("ggplot2")
library("reshape2")

# Define parameters
options(digits=6)

# Parameters
filename = "C:\\Users\\SROY\\Documents\\CodeBase\\lstm ts\\y17.csv"
seq_len = 30

# Load data
df <- read.csv2(filename, header=FALSE)
names(df) <- "price"

# Divide into window sizes.
# While splitting into windows, to ensure window sizes are of equal length
# We will drop initial number of records.
rows_to_skip = nrow(df) %% seq_len 
data <- data.frame(df[(rows_to_skip+1):nrow(df),"price"])

# Create dynamic empty dataframe
# Create dataframe column names 
temp <- list()
for (i in 1:seq_len){
  temp <- c(temp,paste0("t", i , "=numeric()" ))
  if (i != seq_len) {temp <- c(temp,",")}
}
temp <- unlist(temp)
cname <- paste(temp, collapse = '')
colformula <- paste0("ddf <- data.frame(",cname,")")
eval(parse(text=colformula))

# Split into seq_len windows
seq_chunks <- nrow(data) - (seq_len - 1) 
result <- list()

for (index in 1:seq_chunks){
  row <- data[index: (index + seq_len -1),]
  ddf[index,] <- row
}
ddf <- sapply(ddf, as.numeric)

# Manually split data into train test
splitPct <- 0.8
trainCount <- ceiling(nrow(ddf) * splitPct)
train <- ddf[1:trainCount,]
test <- ddf[(trainCount+1) : nrow(ddf),]
stopifnot(nrow(train) + nrow(test) == nrow(ddf))


# Shuffle train data
set.seed(13843)
#train <- train[sample(nrow(train)),]

# Normalize each row so that the first element is zero
normalize <- function(x) {
  return ((x / x[1]) - 1)
}
# Denormalize each row
denormalize <- function(nx, x1) {
  return ((nx + 1) * x1)
}

# Normalize data set for each row for train
colformulaTrain <- paste0("trainndf <- data.frame(",cname,")")
eval(parse(text=colformulaTrain))
for (index in 1:nrow(train)){
  row <- train[index,]
  row <- (row/row[1]) - 1
  trainndf[index,] <- row
}
trainndf <- sapply(trainndf, as.numeric)


# Normalize data set for each row for test
colformulaTest <- paste0("testndf <- data.frame(",cname,")")
eval(parse(text=colformulaTest))
for (index in 1:nrow(test)){
  row <- test[index,]
  row <- (row/row[1]) - 1
  testndf[index,] <- row
}
testndf <- sapply(testndf, as.numeric)


# Split up x and y in test train
trainX <- trainndf[,-ncol(trainndf)]
trainY <- trainndf[,ncol(trainndf)]
testX <- testndf[,-ncol(testndf)]
testY <- testndf[,ncol(testndf)]

# Create dimesnions for RNN
trainX_Matrix <- array(as.matrix(trainX), dim=c(NROW(trainX), NCOL(trainX), 1))
testX_Matrix <- array(as.matrix(testX), dim=c(NROW(testX), NCOL(testX), 1))

# Plot between true and predicted values
getPlot <- function(testY,pred,title){
  dplot <- data.frame(TestVal=testY,PredVal=pred)
  days <- seq(length(testY))
  dplot <- cbind(dplot,days)
  ggplot() + 
    geom_line(data = dplot, aes(x = days, y = TestVal), color = "red") +
    geom_line(data = dplot, aes(x = days, y = PredVal), color = "blue") +
    xlab('Days') +  ylab('Price') + labs(title = title)
}

getPredictedValTest <- function(){
  # Initialize list
  npredCont <- list()
  dpredCont <- list()
  temp <- list()
  
  for (index in (1:nrow(test))){ 
    # Get raw test features
    rowWindow <- test[index,-ncol(test)]
    
    # Start sliding window only after first iteration. Replace from tail with new predicted values
    if (index > 1) {
      if (index-1 <= length(rowWindow)) {
        cutTail <- length(rowWindow) - index + 2
        addTailStart = 1
        addTailEnd <- index-1 }
      
      if (index-1 > length(rowWindow)){
        cutTail = 1
        addTailStart <- index - length(rowWindow)
        addTailEnd <- index-1 }  
      
      rowWindow <- c(rowWindow[-(cutTail:length(rowWindow))], dpredCont[addTailStart:addTailEnd])
    }
    #print("t1       t2       tn-1       tn")
    #print(paste(rowWindow[1],rowWindow[2], rowWindow[length(rowWindow)-1],rowWindow[length(rowWindow)]))
    # Normalize
    nrowWindow <- normalize(rowWindow)
    # Convert into shape
    nrowMatrix <- array(as.matrix(nrowWindow), dim=c(1, seq_len-1, 1))
    # Predict value
    predWindow <- model %>% predict(nrowMatrix)
    # Store normalized predicted value
    npredCont <- unlist(c(npredCont, predWindow[1]))
    # Store denormalized predicted value
    dpredCont <- as.vector(unlist(c(dpredCont, ( (npredCont[index]  + 1) * rowWindow[1]))))
  }
  temp[[1]] <- npredCont
  temp[[2]] <- dpredCont
  return(temp)
}

# Set Keras params for RNN
epochs  = 100
dropoutRate = 0.2
learningRate = 0.04
batch_size = 10
validation_split = 0.2

# Using Keras sequential model
model <- keras_model_sequential()

model %>%
  layer_lstm(units = 30,
             input_shape = c(NCOL(trainX_Matrix), 1), 
             activation = "linear") %>%
  layer_dropout(rate=dropoutRate) %>%
  
  layer_dense(1) %>%
  layer_activation("linear")

optimizer <- optimizer_rmsprop(lr = learningRate)

model %>% compile(
  loss = 'mse',
  optimizer = 'adam')

# fitting the model on the training dataset
model %>% fit(trainX_Matrix, trainY,
              batch_size = batch_size, 
              validation_split = validation_split,
              epochs = epochs)

## Forecast Series
# Evaluating model on the dataset
# predSeq <- model %>% predict(testX_Matrix)
# head(predSeq)
# getPlot(testY, predSeq, "Continuous Forecast")

PredictedValTestResult <- getPredictedValTest()

# Plot normalized predicted vs true on test
# getPlot(testY, PredictedValTestResult[[1]], "Sliding Forecast on normalized price")

# Plot actual predicted vs on true test
getPlot(test[,ncol(test)], PredictedValTestResult[[2]], "Sliding Forecast on actual price")

# save.image("method2.RData")
