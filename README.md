# Dataset Class

This class was created to help during machine learning experiments with raw datasets.

## Getting Started

Copy this file to your project and run source on it.


```
#Some sample to use the class

#for empty variable
var <- dataset$new()

#if you have an file with x and y columns
file <- read.csv("myFile.csv")
var <- dataset$new(x=file$x, y=file$y)
var$splitData() #for data split

#if you have an file with x and y columns
var <- dataset$new(x=file$x, y=file$y, bias=TRUE)
var$splitData() #for data split and bias add on xTrain

```


## Author

* **Juliana Guamá** -- [Follow me](https://github.com/JulianaGuama)

## License

This project is licensed under the cc-by-sa-4.0 - see the [LICENSE.txt](LICENSE.txt) file for details

## Acknowledgments

* Observe that the first class (RootClass) is under stackoverflow authorship (user Rappster)
I had some issue from determining default values to generic class and found this solution on stackoverflow. :D

