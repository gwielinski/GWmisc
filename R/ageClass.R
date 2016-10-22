#' ageClass
#'
#' Function to create variables with different age class
#' @param ageVector numeric vector including the age to be proccess
#' @param classType charactor vector of length 1 used to specify which age class cut to apply
#' @examples
#' y <- data.frame(id=1:10, age=sample(18:60, 10))
#' y <- cbind(y, ageClass(ageVector = y$age, classType = "class10"))  # classType %in% c("classAMT", "class10", "class5", "classDecennie")

ageClass <- function(ageVector, classType = "class10") {

# Valider le paramètre classType
#  if(classType %in% c("classAMT", "class10", "class5", "classDecennie")){}

switch(classType,                 # Switch statement depending on the classType parameter http://stackoverflow.com/questions/10393508/how-to-use-the-switch-statement-in-r-functions
        classAMT={
## age class id
# classAMT
  # id
    x <- data.frame(classAMTid = cut(ageVector, breaks=c(0,14,24,39,64,100), include.lowest = FALSE, labels = FALSE), row.names = NULL)
  # label
    classIds <- 1:5
    classNames <- c("0-14","15-24","25-39","40-64","65+")
    dfclass <- data.frame(id=classIds, classAMTNames = classNames)
    x <- left_join(x,dfclass,by = c("classAMTid"="id"))	#Rajouter le label

  # Retourner le nouveau data frame à être cbind
    return(x)
},
class10={
# class10
  # id
    x <- data.frame(class10id = cut(ageVector, breaks=c(0,15,24,34,44,54,64,74,100), include.lowest = FALSE, labels = FALSE), row.names = NULL)
  # label
    classIds <- 1:8
    classNames <- c("0-15","16-24","25-34","35-44","45-54","55-64","65-74","75+")
    dfclass <- data.frame(id=classIds, class10Names = classNames)
    x <- left_join(x,dfclass,by = c("class10id"="id"))	#Rajouter le label

  # Retourner le nouveau data frame à être cbind
    return(x)
},
class5={
# class5
  # id
    x <- data.frame(class5id = cut(ageVector, breaks=c(0,15,19,24,29,34,39,44,49,54,59,64,69,74,100), include.lowest = FALSE, labels = FALSE), row.names = NULL)
  # label
    classIds <- 1:13
    classNames <- c("0-15","16-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","75+")
    dfclass <- data.frame(id=classIds, class5Names = classNames)
    x <- left_join(x,dfclass,by = c("class5id"="id"))	#Rajouter le label

  # Retourner le nouveau data frame à être cbind
    return(x)
},
classDecennie={
# classDecennie
  # id
    x <- data.frame(classDecennieid = cut(ageVector, breaks=c(0,19,29,39,49,59,69,79,89,100), include.lowest = FALSE, labels = FALSE), row.names = NULL)
  # label
    classIds <- 1:9
    classNames <- c("0-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90-99")
    dfclass <- data.frame(id=classIds, classDecennieNames = classNames)
    x <- left_join(x,dfclass,by = c("classDecennieid"="id"))	#Rajouter le label

  # Retourner le nouveau data frame à être cbind
    return(x)
}
) # End of switch

  # À faire
  ## Validation du paramètre classType

} # End of function
