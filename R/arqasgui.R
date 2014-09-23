#' Stores a file route in a ini file, for loading when arqas starts.
#' 
#' @param file Route to the file
#' @return TRUE if everything goes well
#' @export
addFile<- function(file) {
  userfile <- file(system.file("userRoutes.ini", package="arqasgui"), "a")
  writeLines(c(file), userfile)
  close(userfile)
  return (TRUE)
}

#' Stores all the files in the folder route in a ini file, for loading when arqas starts.
#' 
#' @param folder Route to the folder
#' @return TRUE if everything goes well
#' @export
addFolder <- function(folder) {
  files <- dir(folder)
  #if lenght is 0 folder is a file
  if (length(files) == 0)
    addFile(folder)
  else
    for (i in files)
      addFolder(paste(folder, i, sep="/"))
  return(TRUE)
}

#' Removes all the paths in the ini file.
#' @return TRUE if everything goes well
#' @export
resetIni <- function() {
 path <- system.file("userRoutes.ini", package="arqasgui")
 file.remove(path)
 file.create(path)
}

#' List of export models to the UI
#' @export
#' @keywords internal
uiList <- list()

#' List of registered distributions to the UI
#' @export
#' @keywords internal
distrList <- list()

# The empty distribution gonna be always the last one.
distrList[[1]] <- list(id=1, name="No Arrivals", fun=function(){})

#' Counter of exported funtions
#' @keywords internal
exportedFunctions <- 0

#' Counter of registered distributions
#' @keywords internal
registeredDistributions <- 1

#' Exports a function to the UI
#' 
#' @param fun Function of the model
#' @param name Name of the model
#' @param args Type of each parameter of the function (numerical, character, vector, matrix)
#' @param class A string to agrupate funtions in the same menu option
#' @export
exportToUI <- function(fun, name, args, class) {
  exportedFunctions <<- exportedFunctions + 1
  
  el <- list(id=exportedFunctions, name=name, fun=fun, args=args)
  oldClass(el) <- class
  uiList[[exportedFunctions]] <<- el
}

#' Register a distribution to the UI
#' 
#' @param fun Function of the distribution
#' @param name Name of the distribution
#' @param index Create a label in the list with the name of the distribution
#' @export
registerDistribution <- function(fun, name, index=FALSE) {
  registeredDistributions <<- registeredDistributions + 1
  
  emptyDistr <- distrList[[length(distrList)]]
  el <- list(id=registeredDistributions-1, name=name, fun=fun)
  
  auxList <- distrList[-length(distrList)]
  
  if (index) {
    auxList[[class(fun())[1]]] <- el
  } else {
    auxList[[registeredDistributions-1]] <- el
  }
  
  distrList <<- c(auxList, list(list(id=registeredDistributions, name=emptyDistr$name, fun=emptyDistr$fun)))
}

#Register in the UI the default distributions
registerDistribution(Exp, "Exponential", TRUE)
registerDistribution(Norm, "Normal", TRUE) #sd > 0
registerDistribution(Weibull, "Weibull", TRUE) 
registerDistribution(Unif, "Uniform", TRUE)
registerDistribution(Chisq, "Chi Square", TRUE)
registerDistribution(Lnorm, "Log-Normal", TRUE)
registerDistribution(Gammad, "Gamma", TRUE)
registerDistribution(Dirac, "Dirac", TRUE)

#Here you can register your own distributions
#registerDistribution(function(mate=30){Exp(mate)}, "My Exponential")




#' Create a list with the label and the type of the argument of the model
#' 
#' @param label Description of the argument
#' @param type Type of the argument (numeric, boolean, vector, matrix, distr, vdistr)
#' @export
modelarg <- function(label, type) {list(label=label, type=type)}

#Register in the UI the default queue models
exportToUI(M_M_1, "M/M/1",
           list(modelarg("Arrival rate", "numeric"), modelarg("Service rate", "numeric")),
           c("M_M_1", "M_M_S", "MarkovianModel"))
exportToUI(M_M_S, "M/M/s", 
           list(modelarg("Arrival rate", "numeric"), modelarg("Service rate", "numeric"), modelarg("Servers", "numeric")),
           c("M_M_S", "MarkovianModel"))
exportToUI(M_M_1_K, "M/M/1/K",
           list(modelarg("Arrival rate", "numeric"), modelarg("Service rate", "numeric"), modelarg("Queue size", "numeric")),
           c("M_M_1_K", "M_M_S_K", "MarkovianModel"))
exportToUI(M_M_S_K, "M/M/s/K",
           list(modelarg("Arrival rate", "numeric"), modelarg("Service rate", "numeric"), modelarg("Servers", "numeric"), modelarg("Queue size", "numeric")),
           c("M_M_S_K", "MarkovianModel"))
exportToUI(M_M_1_INF_H, "M/M/1/INF/H",
           list(modelarg("Arrival rate", "numeric"), modelarg("Service rate", "numeric"), modelarg("Potential population", "numeric")),
           c("M_M_1_INF_H", "M_M_S_INF_H", "MarkovianModel"))
exportToUI(M_M_S_INF_H, "M/M/s/INF/H",
           list(modelarg("Arrival rate", "numeric"), modelarg("Service rate", "numeric"), modelarg("Servers", "numeric"), modelarg("Potential population", "numeric")),
           c("M_M_S_INF_H", "MarkovianModel"))
exportToUI(M_M_S_INF_H_Y, "M/M/s/INF/H with Y replacements",
           list(modelarg("Arrival rate", "numeric"), modelarg("Service rate", "numeric"), modelarg("Servers", "numeric"), modelarg("Potential population", "numeric"), modelarg("Replacements", "numeric")),
           c("M_M_S_INF_H_Y", "M_M_S_INF_H", "MarkovianModel"))
exportToUI(M_M_INF, "M/M/INF", 
           list(modelarg("Arrival rate", "numeric"), modelarg("Service rate", "numeric")),  c("M_M_INF", "MarkovianModel"))
exportToUI(OpenJacksonNetwork, "Open Jackson Network", 
           list(modelarg("Arrival rate at each node", "vector"), modelarg("Service rate at each node", "vector"), modelarg("Servers at each node", "vector"), modelarg("Transition matrix", "matrix")),
           c("OpenJackson", "Network", "MarkovianModel"))
exportToUI(ClosedJacksonNetwork, "Closed Jackson Network",
           list(modelarg("Service rate at each node", "vector"), modelarg("Servers at each node", "vector"), modelarg("Transition matrix", "matrix"), modelarg("Number of customers", "numeric")),
           c("ClosedJackson", "Network", "MarkovianModel"))

exportToUI(G_G_1, "G/G/1",
           list(modelarg("Arrival distribution", "distr"), modelarg("Service distribution", "distr"), modelarg("Stabilization parameter", "numeric"), modelarg("Simulation parameter", "numeric"), modelarg("Generate historic?", "boolean"), modelarg("Number of simulations", "numeric"), modelarg("Number of subprocesses", "numeric")),
           c("G_G_1", "SimulatedModel"))
exportToUI(G_G_S, "G/G/s",
           list(modelarg("Arrival distribution", "distr"), modelarg("Service distribution", "distr"), modelarg("Servers", "numeric"), modelarg("Stabilization parameter", "numeric"), modelarg("Simulation parameter", "numeric"), modelarg("Generate historic?", "boolean"), modelarg("Number of simulations", "numeric"), modelarg("Number of subprocesses", "numeric")),
           c("G_G_S", "SimulatedModel"))
exportToUI(G_G_1_K, "G/G/1/K",
           list(modelarg("Arrival distribution", "distr"), modelarg("Service distribution", "distr"), modelarg("Queue size", "numeric"), modelarg("Stabilization parameter", "numeric"), modelarg("Simulation parameter", "numeric"), modelarg("Generate historic?", "boolean"), modelarg("Number of simulations", "numeric"), modelarg("Number of subprocesses", "numeric")),
           c("G_G_1_K", "SimulatedModel"))
exportToUI(G_G_S_K, "G/G/s/K",
           list(modelarg("Arrival distribution", "distr"), modelarg("Service distribution", "distr"), modelarg("Servers", "numeric"), modelarg("Queue size", "numeric"), modelarg("Stabilization parameter", "numeric"), modelarg("Simulation parameter", "numeric"), modelarg("Generate historic?", "boolean"), modelarg("Number of simulations", "numeric"), modelarg("Number of subprocesses", "numeric")),
           c("G_G_S_K", "SimulatedModel"))
exportToUI(G_G_1_INF_H, "G/G/1/INF/H",
           list(modelarg("Arrival distribution", "distr"), modelarg("Service distribution", "distr"), modelarg("Potential customers", "numeric"), modelarg("Stabilization parameter", "numeric"), modelarg("Simulation parameter", "numeric"), modelarg("Generate historic?", "boolean"), modelarg("Number of simulations", "numeric"), modelarg("Number of subprocesses", "numeric")),
           c("G_G_1_INF_H", "SimulatedModel"))
exportToUI(G_G_S_INF_H, "G/G/s/INF/H",
           list(modelarg("Arrival distribution", "distr"), modelarg("Service distribution", "distr"), modelarg("Servers", "numeric"), modelarg("Potential customers", "numeric"), modelarg("Stabilization parameter", "numeric"), modelarg("Simulation parameter", "numeric"), modelarg("Generate historic?", "boolean"), modelarg("Number of simulations", "numeric"), modelarg("Number of subprocesses", "numeric")),
           c("G_G_S_INF_H", "SimulatedModel"))
exportToUI(G_G_S_INF_H_Y, "G/G/s/INF/H with Y replacements",
           list(modelarg("Arrival distribution", "distr"), modelarg("Service distribution", "distr"), modelarg("Servers", "numeric"), modelarg("Potential customers", "numeric"), modelarg("Replacements", "numeric"), modelarg("Stabilization parameter", "numeric"), modelarg("Simulation parameter", "numeric"), modelarg("Generate historic?", "boolean"), modelarg("Number of simulations", "numeric"), modelarg("Number of subprocesses", "numeric")),
           c("G_G_S_INF_H_Y", "SimulatedModel"))
exportToUI(G_G_INF, "G/G/INF",
           list(modelarg("Arrival distribution", "distr"), modelarg("Service distribution", "distr"), modelarg("Stabilization parameter", "numeric"), modelarg("Simulation parameter", "numeric"), modelarg("Generate historic?", "boolean"), modelarg("Number of simulations", "numeric"), modelarg("Number of subprocesses", "numeric")),
           c("G_G_INF", "SimulatedModel"))
exportToUI(OpenNetwork, "Open Network",
           list(modelarg("Arrival distribution at each node", "vdistr"), modelarg("Service distribution at each node", "vdistr"), modelarg("Servers at each node", "vector"), modelarg("Transition matrix", "matrix"), modelarg("Stabilization parameter", "numeric"), modelarg("Simulation parameter", "numeric"), modelarg("Generate historic?", "boolean"), modelarg("Number of simulations", "numeric"), modelarg("Number of subprocesses", "numeric")),
           c("Open", "SimulatedNetwork", "SimulatedModel"))
exportToUI(ClosedNetwork, "Closed Network",
           list(modelarg("Service distribution at each node", "vdistr"), modelarg("Servers at each node", "vector"), modelarg("Transition matrix", "matrix"), modelarg("Number of customers", "numeric"), modelarg("Stabilization parameter", "numeric"), modelarg("Simulation parameter", "numeric"), modelarg("Generate historic?", "boolean"), modelarg("Number of simulations", "numeric"), modelarg("Number of subprocesses", "numeric")),
           c("Closed", "SimulatedNetwork", "SimulatedModel"))

#' Run the interface
#' @export
initArqasgui <- function(host=NULL) {
  runApp(system.file("shinyfiles", package="arqasgui"), host=host)
}