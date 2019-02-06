#' RootClass
#'
#' @author Rappster \url{https://stackoverflow.com/questions/13517007/defining-default-field-values-for-instances-of-s4-reference-classes}
#'
#'
#' A generic Class to help create default values to class elements
#' 
gen <- setRefClass("RootClass",
                   methods=list(
                     ensureDefaultValues=function(values, ...) {
                       "function to insert default value to each param from the class"
                       if (!missing(...)) {
                         arguments   <- list(...)             
                         specified   <- names(arguments)
                         idx <- which(specified %in% names(values))
                         if (length(idx)) {
                           for (ii in specified[idx]) {
                             values[[ii]] <- arguments[[ii]]
                           }
                         }
                       }    
                       temp <- paste(paste0(names(values), "=values$", 
                                            names(values)), collapse=", ")
                       eval(parse(text=paste0(".self$initFields(", temp, ", ...)")))
                       return(TRUE)            
                     }
                   )
)

#' dataset
#'
#' @author JulianaGuama \url{https://github.com/JulianaGuama}
#'
#' A generic Class to help with dataset to be trained in artificial neural networks
#' 
#' @inheritParams RootClass for default values to class params
#' 
#' @field xTrain matrix rate of x data to be sent to train
#' @field xTest matrix rate of x data to be sent to test (validation)
#' @field yTrain matrix rate of y data to be sent to train
#' @field yTest matrix rate of y data to be sent to test (validation)
#' @field x matrix complete x data
#' @field y matrix complete y data
#' @field idx integer index of data splitted between train and test
#' @field bias logical indicate if a bias must be put on xTrain while split process
#' @field randomize logical indicate if the data should be randomized on spliting
#' @field splitRate numeric rate of data to train
#' 
#' @param value on @describeIn insert_Bias can be c("custom", "output mean", '') any other will be treat as default (insert vector of 1 with NROW(x) size in the first column in xTrain)
#'
#' @example data <- dataset$new(x=X, y=Y, bias=TRUE)
#' @example data$splitData()
#' 
dataset <- setRefClass("dataset",
                       contains="RootClass",
                       fields = list(
                         xTrain = "matrix",
                         xTest = "matrix",
                         yTrain = "matrix",
                         yTest = "matrix",
                         x="matrix",
                         y="matrix",
                         idx="integer",
                         bias="logical",
                         randomize="logical",
                         splitRate = "numeric"),
                       methods = list(
                         initialize=function(...){
                           .self$ensureDefaultValues(
                             values=list(
                               xTrain = matrix(nrow=0, ncol=0),
                               xTest = matrix(nrow=0, ncol=0),
                               yTrain = matrix(nrow=0, ncol=0),
                               yTest = matrix(nrow=0, ncol=0),
                               x=matrix(nrow=0, ncol=0),
                               y=matrix(nrow=0, ncol=0),
                               idx= as.integer(0),
                               bias= FALSE,
                               randomize= TRUE,
                               splitRate = 0.7 
                             ),
                             ...
                           )
                           return(.self)
                         },
                         remove_Bias = function(){
                           "remove_Bias if NCOL on xTrain is bigger than on xTest, then there is bias to remove on fisrt column"
                           
                           #bias is always in first column
                           if(NCOL(xTrain) == NCOL(xTest) | NCOL(xTrain) < 2) stop("No bias to remove") 
                           
                           xTrain <<- xTrain[,2:NCOL(xTrain)]
                           bias <<- FALSE
                           
                         },
                         insert_Bias = function(value){
                           "insert_Bias if NCOL on xTrain is equal than on xTest, then a bias can be put on xTrain"
                           
                           if(!NROW(xTrain)) stop("Please, first split the data to insert Bias")
                           if(NCOL(xTrain) > NCOL(yTrain)) stop("xTrain already has bias")
                           
                           if(bias){
                             if(NROW(value) == 2 & value[1] == "custom") xTrain <<- cbind(as.numeric(value[2]), xTrain)
                             else if(value == "output mean") xTrain <<- cbind(mean(y), xTrain)
                             else xTrain <<- cbind(1, xTrain) 				
                             
                             bias <<- TRUE
                           }
                         },
                         splitData = function(){
                           "split (x,y) data into train and test data"
                           
                           sizeData <- NROW(x)
                           if(!sizeData | NROW(xTrain) > 1) stop("No data to split")
                           
                           split <- round(sizeData*splitRate)
                           idx <<- sample(1:NROW(x))
                           
                           #split (x,y) into xTrain and yTrain
                           if(NCOL(x) == 1) xTrain <<- x[idx[1:split]]
                           else xTrain <<- x[idx[1:split],]
                           
                           if(bias) xTrain <<- insert_Bias()
                           
                           if(NCOL(y) == 1) yTrain <<- as.matrix(y[idx[1:split]])
                           else yTrain <<- as.matrix(y[idx[1:split], ])
                           
                           #get next position to split from
                           split <- split + 1
                           
                           #split (x,y) into xTest and yTest
                           if(NCOL(x) == 1) xTest  <<- x[idx[split:sizeData]]
                           else xTest  <<- x[idx[split:sizeData],]
                           
                           if(NCOL(y) == 1)
                             yTest  <<- as.matrix(y[idx[split:sizeData]])
                           else yTest  <<- as.matrix(y[idx[split:sizeData], ])
                           
                           
                         },
                         joinData = function(){
                           "Join train and test data into (x,y) data"
                           
                           if(NROW(x)) stop("Data already joined")
                           if(!NROW(xTrain)) stop("No splited data to be joined")
                           if(!NROW(idx)) warning("No index file! Data is joined by binding(train, test)")
                           
                           len <- NROW(xTrain) + NROW(xTest)
                           split <- NROW(xTrain)
                           
                           #create matrix with right size to insert data organized
                           x <<- matrix(nrow=len, ncol=NCOL(xTrain))
                           y <<- matrix(nrow=len, ncol=NCOL(yTrain))
                           
                           if(NROW(idx) == len){
                             order <- order(idx)
                             
                             #Initiate with training data on (x,y)
                             if(NCOL(x) == 1) x <<- xTrain[order[1:split]]
                             else x <<- xTrain[order[1:split], ]
                             
                             if(NCOL(y) == 1)y <<- yTrain[order[1:split]]
                             else y <<- yTrain[order[1:split],]
                             
                             split <- split + 1
                             #Now get test data on (x,y)
                             if(NCOL(x) == 1) x <<- rbind(x, xTest[order[split:len]])
                             else x <<- rbind(x, xTest[order[split:len], ])
                             
                             if(NCOL(y) == 1) y <<- rbind(y, yTest[order[split:len]])
                             else y <<- rbind(y, yTest[order[split:len], ])
                             
                           } else{
                             #Initiate with training data on (x,y)
                             if(NCOL(x) == 1) x <<- xTrain[1:split]
                             else x <<- xTrain[1:split, ]
                             
                             if(NCOL(y) == 1) y <<- yTrain[1:split]
                             else y <<- yTrain[1:split,]
                             
                             split <- split + 1
                             #Now get test data on (x,y)
                             if(NCOL(x) == 1) x <<- rbind(x, xTest[split:len])
                             else x <<- rbind(x, xTest[split:len, ])
                             
                             if(NCOL(y) == 1) y <<- rbind(y, yTest[split:len])
                             else y <<- rbind(y, yTest[split:len, ])
                             
                           }#if-else (NROW(idx) == len)
                         }#joinData
                       )#methods
)#class
