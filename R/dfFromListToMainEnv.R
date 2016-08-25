#' dfFromListToMainEnv
#' 
#' Function to convert data frames contained in a list object into data frame objects in the main environment
#' @param dfList A list object containing data frames
#' @param dfNames Optionnal argument to be specified if dfList doesn't have proper names assigned to its data frames
#' @export
#' @examples
#' dfFromListToMainEnv(dfList = list(data.frame(x1=1:10, x2=11:20), data.frame(x3=1:3)), dfNames = c("foo", "bar"))

dfFromListToMainEnv <- function(dfList, dfNames = NULL){

  if(!missing(dfNames)){
    names(dfList) <- dfNames # Assign names to data frames prior to running the function if dfNames is specified
  }

  list2env(x=dfList, envir = .GlobalEnv) # Create objects in Global Env.
}

# Créer une gestion d'erreur lorsque le nom des df n'est pas spécifié dans l'objet list et
# qu'il n'est pas inclut dans le call de la fonction
