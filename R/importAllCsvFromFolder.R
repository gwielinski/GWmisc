#' importAllCsvFromFolder
#'
#' Function to import all csv files contained inside a specified folder
#' @param folder folder containing csv files by default it is set to the working directory
#' @export
#' @examples
#' importAllCsvFromFolder(folder = "Data")

importAllCsvFromFolder <- function(folder = getwd()){

  temp = list.files(path=folder, pattern="*.csv")

  dfFromListToMainEnv(
    lapply(
      setNames(paste0(folder, "/", temp), make.names(gsub("*.csv$", "", temp))), read.csv
    )
  )

}

# Inclure une procédure pour enlever un caractère "/" ou "\\" à la fin de la variable folder
