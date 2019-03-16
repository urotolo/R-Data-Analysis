library(ggplot2)
library(caret)
library(deSolve)
library(lattice)

# Ulisse Rotolo

trainingData <- read.table("/root/Desktop/DataAnalysisTest/training_data.txt", header=TRUE) # Read in data for Part 1
testData.txt <- read.table("/root/Desktop/DataAnalysisTest/test_data.txt") # Read in data for Part 1
testData.txt

# PART 1

#Part 1. a

ggplot() + geom_point(trainingData, mapping=(aes(e, target))) + ylab("Target") + xlab("E")   # e vs target column
ggplot() + geom_point(trainingData, mapping=(aes(i, target))) + ylab("Target") + xlab("E")   # i vs target column


linearmod = lm(as.numeric(as.character(V14))~as.numeric(as.character(V5))+as.numeric(as.character(V9)), data=trainingData.txt) # Linear regression model

interc=linearmod$coefficients
slop=linearmod$coefficients

#
# PART 2

exam1_golf_data.csv <- read.csv("/root/Desktop/DataAnalysisTest/exam1_golf_data.csv")
head(exam1_golf_data.csv)

#Part 2. a

is.na_replace_0 <- exam1_golf_data.csv$hole                       # I replace all the non NA's with 1
is.na_replace_0[!is.na(is.na_replace_0)] <- 1
is.na_replace_0
is.na_replace_0[is.na(is.na_replace_0)] <- 0                      # All the NA's are replaced with 0
is.na_replace_0                                                   # This more clearly tells me when the putt goes in or misses.

rowHasG = subset(exam1_golf_data.csv, From =="G")                  # creates a partial matrix that only includes rows with the G.
rowHasG                                                           # This allows my measurements to be in only feet.
ace = c()

for(i in 2:nrow(rowHasG)){                                        # Set for loop to length of matrix
  if(rowHasG[[5]][i] > rowHasG[[5]][i-1]){                        # Checks distance column. I noticed that when
    ace[i-1] = 1                                                  # the succeding value for a cell in distance is
  }else{                                                          # greater then the previous cell, that means a new golf.
    ace[i-1] = 0                                                  # round begins. I store this data in a vector, then add to my matrix of G rows. 
  }                                                               # Now I know when a golf game iteration begins and ends in terms of G.
}

ace[328] = 0
ace
rowHasG$Ace <- ace
rowHasG$Ace

logimodel <- glm(rowHasG$Ace~rowHasG$dist, family = binomial(link = "logit")) # Logistic model


B0=logimodel$coefficients[1] 
B1=logimodel$coefficients[2]

B0
B1

#Part 2. b && part 2. c

# For 2. b: According to my model probabilty is .50 for distance 6 feet
# For 2. c: According to my model at distance 20 feet the probability is .0007209147

for(i in 100:1){                # Gives the distance and probability of scoring at that distance,
  dist = i                      # distance starts at 1 foot and ascends upwards until distance is 100 feet.
  cat("Distance: ", i)
  print("")
  print("probability: ")
  print(1/(1+exp(-B0-B1*dist))) # Gives probability of scoring a putt at i distance
}

# PART 3

#Part 3. a

testData.txt <- read.table("/root/Desktop/DataAnalysisTest/logisticData.txt")
head(testData.txt)
testData.txt

# TestData keeps including the string "time" and "amount" in the actual calculation,
# that throws everything off, since I didnt know how to remove "time" or "amount",
# I just created another data frame with only the numeric values and not including "time" or "amount".
# Times and Amount are the names of the column now, instead of how when you first import logisticData
# "time" and "amount" are the actual first value of column V1 and V2. Also did the same thing for my differential
# equation data, for some reason explicitely redeclaring them as data frames made my calculations run smoother.

times = c(0,350,700,1050,1400,1750,2100,2450,2800,3150,3500)
amounts = c(86.9,130.8,401.0,596.5,1288.3,2072.2,2346.8,3495.3,4097.2,3936.4,4220.8)

mimicTestDataDf = data.frame(times, amounts)

y0 = 200
r = .0015                         # Initial values given to me
c = 5000

diffFunc <- function(r,y,c){      # Programs the equation dy/dx = ry(1-(y/c))
  return(list(r*y0*(1-y0/c)))
}

output_times = seq(0,3500, by=291.666)
solveDiff = ode(y=y0, times = output_times , func=diffFunc)   # Solves differential equation
head(solveDiff)
class(solveDiff)
solveDiff     # Results given initial parameters, compare to testData.txt for accuracy of solution

deSolveAmounts = c(200.000,329.0052,531.9992,837.7755,1269.3723,1825,7823,2464.9363,3108.7039,3676.7529,4122.3597)
mimicSolveDiffDataDf = data.frame(times, deSolveAmounts)
mimicSolveDiffDataDf 

ggplot()+geom_point(aes(x=mimicTestDataDf$times, y=mimicTestDataDf$amounts)) + geom_line(aes(solveDiff[,1],solveDiff[,2])) +
  ylim(c(0,5000)) + ylab("Amounts") + xlab("Times")

#Part 3. b

sumSquareResidualData = 0
sumSquareResidualModel = 0

calculateError <- function(x,y){
  for(i in 1:nrow(x)){
    squaredValue = x[[2]][i] * x[[2]][i]
    sumSquareResidualData = sumSquareResidualData + squaredValue
  }
  for(i in 1:nrow(y)){
    squaredValue = y[[2]][i] * y[[2]][i]
    sumSquareResidualModel = sumSquareResidualModel + squaredValue
  }
  return(list(sqrt(sumSquareResidualData), sqrt(sumSquareResidualModel)))
}

print(calculateError(mimicTestDataDf, mimicSolveDiffDataDf))   # Compares the sqrt of the squared residual sum
                                                               # of logisticData and the differential eq solution.

#Part 3. c

#optim_results = optim(c(5000,200,.0015), calculateError(mimicTestDataDf, mimicSolveDiffDataDf))
#optim_results

logistic_error = function(params) {
  fun_values = diffFunc(params[1],params[2],params[3])
  return(sqrt(sum( (as.numeric(testData.txt$amount)-as.numeric(fun_values))^2 )))
}

optim_results = optim(c(r, y0, 5000), logistic_error)
optim_results

