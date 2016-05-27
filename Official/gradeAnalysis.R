# make the numbers numberic rather than integer for the calculation. 
da <- read.csv("./Official/EC381.csv", stringsAsFactors = FALSE, colClasses = c(rep("character", 3), rep("numeric", 9)))
da$Exam <-  rowMeans(da[6:12], na.rm = TRUE)
da$Total <- da$Assignment*0.5 + da$Exam*0.5
str(da)
head(da)
  myStats <- function(x, na.omit=FALSE){
    if (na.omit)
      x <- x[!is.na(x)]
    m <- mean(x)
    md <- median(x)
    s <- sd(x) 
    max <- max(x)
    min <- min(x)
    n <- length(x)
    return(c(n=n, mean=m, median = md, stdev=s, max = max, min = min))
  }
  Stats <- apply(da[,c(5:14)], 2, FUN = myStats, na.omit = TRUE)
  round(Stats, 2)
#-----------------------------------------------------
boxplot(da[,c(5, 13, 14)], main = "Boxplot of assignment results", col = "lightblue") 
boxplot(da[,c(6:12)], main = "Boxplot of exam question results", names = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7"), 
        col = "lightblue")
