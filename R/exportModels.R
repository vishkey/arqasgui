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

#'Here you can register your own models
#'exportToUI(<model fuction>, <name [String]>, <list of modelarg()>, <list of the classes>)
#'The modelarg have 2 params: modelarg(<String that describes the parameter>, <type of the parameter>)
#'The type can be: "vdistr": vector of Distr objects
#'                 "distr" : Distr object
#'                 "numeric": number
#'                 "vector" : vector of numbers
#'                 "matrix":  matrix of numbers
#'                 "boolean": TRUE/FALSE value