options(shiny.deprecation.messages=FALSE)

toDatatable <- function(qm, ...) {UseMethod("toDatatable", qm)}

toDatatable.MarkovianModel <- function(qm) {
  with(qm$out,
     data.frame("L"=sprintf("%5.4g", l), "Lq"=sprintf("%5.4g", lq), 
                "W"=sprintf("%5.4g", w), "Wq"=sprintf("%5.4g", wq), 
                "Intensity"=sprintf("%5.4g", rho), "Efficiency"=sprintf("%5.4g", eff)))
}

toDatatable.OpenJackson <- function(qm) {
  res <- with(qm$out, data.frame("Node"=1, "L"=sprintf("%5.4g", l[1]), "Lq"=sprintf("%5.4g", lq[1]), "W"=sprintf("%5.4g", w[1]), "Wq"=sprintf("%5.4g", wq[1])))
  for(i in 2:length(qm$out$l)) {
    res <- with(qm$out, rbind(res, data.frame("Node"=i, "L"=sprintf("%5.4g", l[i]), "Lq"=sprintf("%5.4g", lq[i]), "W"=sprintf("%5.4g", w[i]), "Wq"=sprintf("%5.4g", wq[i]))))
  }
  res <- with(qm$out, rbind(res, data.frame("Node"="Total", "L"=sprintf("%5.4g", lt), "Lq"=sprintf("%5.4g", lqt), "W"=sprintf("%5.4g", wt), "Wq"=sprintf("%5.4g", wqt))))
  return(res)
}

toDatatable.ClosedJackson <- function(qm) {
  res <- with(qm$out, data.frame("Node"=1, "L"=sprintf("%5.4g", l[1]), "Lq"=sprintf("%5.4g", lq[1]), "W"=sprintf("%5.4g", w[1]), "Wq"=sprintf("%5.4g", wq[1])))
  for(i in 2:length(qm$out$l)) {
    res <- with(qm$out, rbind(res, data.frame("Node"=i, "L"=sprintf("%5.4g", l[i]), "Lq"=sprintf("%5.4g", lq[i]), "W"=sprintf("%5.4g", w[i]), "Wq"=sprintf("%5.4g", wq[i]))))
  }
  return(res)
}

toDatatable.SimulatedModel <- function(qm) {
  if (!is.list(qm$out$l)) {
    res <- with(qm$out,
               data.frame("L"=sprintf("%5.4g", l), "Lq"=sprintf("%5.4g", lq), 
                          "W"=sprintf("%5.4g", w), "Wq"=sprintf("%5.4g", wq), 
                          "Intensity"=sprintf("%5.4g", rho), "Efficiency"=sprintf("%5.4g", eff)))
  } else {
    res <- with(qm$out, 
                data.frame("_empty"="Mean",
                      "L"=sprintf("%5.4g", l$mean), "Lq"=sprintf("%5.4g", lq$mean), 
                      "W"=sprintf("%5.4g", w$mean), "Wq"=sprintf("%5.4g", wq$mean), 
                      "Intensity"=sprintf("%5.4g", rho$mean), "Efficiency"=sprintf("%5.4g", eff$mean)))
    res <- rbind(res, with(qm$out,
                           data.frame("_empty"="Sd",
                                      "L"=sprintf("%5.4g", l$sd), "Lq"=sprintf("%5.4g", lq$sd), 
                                      "W"=sprintf("%5.4g", w$sd), "Wq"=sprintf("%5.4g", wq$sd), 
                                      "Intensity"=sprintf("%5.4g", rho$sd), "Efficiency"=sprintf("%5.4g", eff$sd))))
    res <- rbind(res, with(qm$out,
                           data.frame("_empty"="Median",
                                      "L"=sprintf("%5.4g", l$summary[3]), "Lq"=sprintf("%5.4g", lq$summary[3]), 
                                      "W"=sprintf("%5.4g", w$summary[3]), "Wq"=sprintf("%5.4g", wq$summary[3]), 
                                      "Intensity"=sprintf("%5.4g", rho$summary[3]), "Efficiency"=sprintf("%5.4g", eff$summary[3]))))
    res <- rbind(res, with(qm$out,
                           data.frame("_empty"="Min",
                                      "L"=sprintf("%5.4g", l$summary[1]), "Lq"=sprintf("%5.4g", lq$summary[1]), 
                                      "W"=sprintf("%5.4g", w$summary[1]), "Wq"=sprintf("%5.4g", wq$summary[1]), 
                                      "Intensity"=sprintf("%5.4g", rho$summary[1]), "Efficiency"=sprintf("%5.4g", eff$summary[1]))))
    res <- rbind(res, with(qm$out,
                           data.frame("_empty"="Max",
                                      "L"=sprintf("%5.4g", l$summary[6]), "Lq"=sprintf("%5.4g", lq$summary[6]), 
                                      "W"=sprintf("%5.4g", w$summary[6]), "Wq"=sprintf("%5.4g", wq$summary[6]), 
                                      "Intensity"=sprintf("%5.4g", rho$summary[6]), "Efficiency"=sprintf("%5.4g", eff$summary[6]))))
    res <- rbind(res, with(qm$out,
                           data.frame("_empty"="1st Quartile",
                                      "L"=sprintf("%5.4g", l$summary[2]), "Lq"=sprintf("%5.4g", lq$summary[2]), 
                                      "W"=sprintf("%5.4g", w$summary[2]), "Wq"=sprintf("%5.4g", wq$summary[2]), 
                                      "Intensity"=sprintf("%5.4g", rho$summary[2]), "Efficiency"=sprintf("%5.4g", eff$summary[2]))))
    res <- rbind(res, with(qm$out,
                           data.frame("_empty"="3rd Quartile",
                                      "L"=sprintf("%5.4g", l$summary[5]), "Lq"=sprintf("%5.4g", lq$summary[5]), 
                                      "W"=sprintf("%5.4g", w$summary[5]), "Wq"=sprintf("%5.4g", wq$summary[5]), 
                                      "Intensity"=sprintf("%5.4g", rho$summary[5]), "Efficiency"=sprintf("%5.4g", eff$summary[5]))))    
    
    
  }
  return(res)
}

toDatatable.Open <- function(qm) {
  if (!is.list(qm$out$l)) {
    res <- with(qm$out,
                data.frame("Node"=1:length(l), "L"=sprintf("%5.4g", l), "Lq"=sprintf("%5.4g", lq), 
                           "W"=sprintf("%5.4g", w), "Wq"=sprintf("%5.4g", wq)))
  } else {
    res <- with(qm$out, 
                data.frame("Info"= c("Mean", 1:length(l$mean)),
                           "L"=c("", sprintf("%5.4g", l$mean)), "Lq"=c("", sprintf("%5.4g", lq$mean)),
                           "W"=c("", sprintf("%5.4g", w$mean)), "Wq"=c("",sprintf("%5.4g", wq$mean))))
    res <- rbind(res, with(qm$out,
                           data.frame("Info"=c("Sd", 1:length(l$mean)),
                                      "L"=c("", sprintf("%5.4g", l$sd)), "Lq"=c("", sprintf("%5.4g", lq$sd)),
                                      "W"=c("", sprintf("%5.4g", w$sd)), "Wq"=c("", sprintf("%5.4g", wq$sd)))))
    res <- rbind(res, with(qm$out,
                           data.frame("Info"=c("Median", 1:length(l$mean)),
                                      "L"=c("", sprintf("%5.4g", l$summary[3,])), "Lq"=c("", sprintf("%5.4g", lq$summary[3,])),
                                      "W"=c("", sprintf("%5.4g", w$summary[3,])), "Wq"=c("", sprintf("%5.4g", wq$summary[3,])))))
    res <- rbind(res, with(qm$out,
                           data.frame("Info"=c("Min", 1:length(l$mean)),
                                      "L"=c("", sprintf("%5.4g", l$summary[1,])), "Lq"=c("", sprintf("%5.4g", lq$summary[1,])),
                                      "W"=c("", sprintf("%5.4g", w$summary[1,])), "Wq"=c("", sprintf("%5.4g", wq$summary[1,])))))
    res <- rbind(res, with(qm$out,
                           data.frame("Info"=c("Max", 1:length(l$mean)),
                                      "L"=c("", sprintf("%5.4g", l$summary[6,])), "Lq"=c("", sprintf("%5.4g", lq$summary[6,])),
                                      "W"=c("", sprintf("%5.4g", w$summary[6,])), "Wq"=c("", sprintf("%5.4g", wq$summary[6,])))))
    res <- rbind(res, with(qm$out,
                           data.frame("Info"=c("1st Quartile", 1:length(l$mean)),
                                      "L"=c("", sprintf("%5.4g", l$summary[2,])), "Lq"=c("", sprintf("%5.4g", lq$summary[2,])),
                                      "W"=c("", sprintf("%5.4g", w$summary[2,])), "Wq"=c("", sprintf("%5.4g", wq$summary[2,])))))
    res <- rbind(res, with(qm$out,
                           data.frame("Info"=c("3rd Quartile", 1:length(l$mean)),
                                      "L"=c("", sprintf("%5.4g", l$summary[5,])), "Lq"=c("", sprintf("%5.4g", lq$summary[5,])),
                                      "W"=c("", sprintf("%5.4g", w$summary[5,])), "Wq"=c("", sprintf("%5.4g", wq$summary[5,])))))
  }
  return(res)
}

toDatatable.Closed <- function(qm) {toDatatable.Open(qm)}

toDatatable.FitList <- function(fitlist, tab) {
  res <- data.frame(Distributions=NULL, ButtonData=NULL)
  index <- 1
  for(i in fitlist) {
    argnames <- names(i$estimate)
    aux <- ""
    j <- 0
    if (length(argnames) > 1) {
      switch(i$distname,
        gamma = {
          for(j in 1:(length(i$estimate)-1)) {
            if (argnames[j] == "rate") {
              aux <- paste(aux, "scale=", eval(parse(text=paste("1/", i$estimate[j], sep=""))), ", ", sep="")
            }else {
              aux <- paste(aux, argnames[j], "=", i$estimate[j], ", ", sep="")
            }
          }
          
          if (argnames[j+1] == "rate") {
            aux <- paste(aux, "scale=", eval(parse(text=paste("1/", i$estimate[j+1], sep=""))), sep="")
          }else {
            aux <- paste(aux, argnames[j+1], "=", i$estimate[j+1], sep="")
          }
          res <- rbind(res, data.frame(Distributions=paste("<center><b>", i$distname, "(</b>", aux, "<b>)</b></center>", sep=""), ButtonData=paste("<button id='storeButton_", tab, "_", index, "' class='btn btn-primary shiny-jquerybutton-input' once dialog-input data-distribution='gammad(", aux ,")'>Save</button>", sep="")))
        },
        {
          for(j in 1:(length(i$estimate)-1)) {
              aux <- paste(aux, argnames[j], "=", i$estimate[j], ", ", sep="")
          }
          aux <- paste(aux, argnames[j+1], "=", i$estimate[j+1], sep="")
          res <- rbind(res, data.frame(Distributions=paste("<center><b>", i$distname, "(</b>", aux, "<b>)</b></center>", sep=""), ButtonData=paste("<button id='storeButton_", tab, "_", index, "' class='btn btn-primary shiny-jquerybutton-input' once dialog-input data-distribution='", i$distname,"(", aux ,")'>Save</button>", sep="")))
        }
      )
    } else {
      res <- rbind(res, data.frame(Distributions=paste("<center><b>", i$distname, "(</b>", argnames[j+1], "=", i$estimate[j+1], "<b>)</b></center>", sep=""), ButtonData=paste("<button id='storeButton_", tab, "_", index, "' class='btn btn-primary shiny-jquerybutton-input' once dialog-input data-distribution='", i$distname,"(", argnames[j+1], "=", i$estimate[j+1] ,")'>Save</button>", sep="")))
    }
    index <- index+1
  }
  return(res)
}

#' Transforms two list of names and values into a unique array
#' @param l1 List of names
#' @param l2 List of values
getOptions <- function (l1, l2) {
  res <- c()
  for(i in 1:length(l1)) {
    res <- eval(parse(text=paste("c(res, '", l1[i], "' = ", l2[i], ")")))
  }
  return(res)
}

#' Capitalize the first character of a string
#' @param x String
simpleCap <- function(x) {
  paste(toupper(substring(x, 1,1)), substring(x, 2),
        sep="", collapse=" ")
}

#' Lower the first character of a string
#' @param x String
simpleLow <- function(x) {
  paste(tolower(substring(x, 1,1)), substring(x, 2),
        sep="", collapse=" ")
}

#' Transforms a matrix into a string
#' @param m Matrix
matrixtostring <- function(m) {
  if (is.null(m)) return("")
  rows <- nrow(m)
  res <- ""
  for(i in 1:rows) {
    res <- paste(res, "[", sep="")
    for(j in 1:rows)
      res <- paste(res, m[i,j], ifelse(j==rows, "", ","), sep="")
    res <- paste(res, ifelse(i==rows, "]", "],"), sep="")
  }
  return(res)
}

#' Transforms a vector into a string
#' @param v Vector
vectortostring <- function(v) {
  res <- "";
  for(i in 1:(length(v)-1)) {
    res <- paste(res, v[i], ";", sep="")
  }
  res <- paste(res, last(v), sep="")
  return(res)
}

#' Obtains the avaiable distributions and return a list of choices
selectDistr <- function(WithNoArrivals=TRUE) {
  aux  <- function(n, v) {list(name=n, value=eval(parse(text=paste(v, sep=""))))}
  choicelist <- list()
  cond <-  ifelse(WithNoArrivals, 0, 1)
  for(i in 1:(length(distrList$get())-cond)) {
     choicelist[[i]] <- list(name=distrList$get()[[i]]$name, params=mapply(aux, as.list(names(formals(distrList$get()[[i]]$fun))), formals(distrList$get()[[i]]$fun), SIMPLIFY=FALSE))
  }
  return(choicelist)
}


distrToText <- function(d) {
      if (class(d)[1] == "no_distr") return("-")
      paramNames <- slotNames(param(d))
      #paramNames <- rep("a", 6)
      paramNames <- paramNames[-length(paramNames)]
      values <- sapply(paramNames, function(x){slot(param(d), x)})
      
      if (length(values) > 1) {
        str <- paste("(", values[1], ", ", sep="")
        for(i in 2:length(values)) {
          if (i != length(values))
            str <- paste(str, values[i], ", ", sep="")
          else
            str <- paste(str, values[i], ")", sep="")
        }
      }
      else {
        str <- paste("(", values[1], ")", sep="")
      }
      return(paste(class(d)[1], str, sep=""))
}

distrDefaultValues <- function(distrModel) {
   if (class(distrModel)[1] =="no_distr")
     return("{id: 0}")
   
   aux  <- function(n, v) {list(name=n, value=v)}
   parameters <- param(distrModel)
   paramNames <- slotNames(parameters)

   res <- paste("{id:", distrList$get()[[class(distrModel)[1]]]$id, ", params:[", sep="")
   for(i in 1:(length(paramNames)-1)) {
    res <- paste(res, "{name:'", paramNames[i], "' , value:", slot(parameters, paramNames[i]), "}", sep="")
    if (i != length(paramNames)-1)
      res <- paste(res, ", ", sep="")
   }
   res <- paste(res, "]}", sep="")
   return(res)
}

vdistrDefaultValues <- function(distrsModels) {
  res <- "["
  for (i in 1:length(distrsModels)) {
    res <- paste(res, distrDefaultValues(distrsModels[[i]]), sep="")
    if (i != length(distrsModels))
      res <- paste(res, ", ", sep="")
  }
  res <- paste(res, "]", sep="")
  return(res)
}
#' Generate the necesary HTML for each type of argument into a model
#' @param input The list of shiny inputs avaiable in the HTML
#' @param model The queue model to generate the inputs
#' @param parameters Optional inital values for the inputs.
generateInputs <- function(input, model, parameters) {UseMethod("generateInputs", model)}
  
generateInputs.default <- function(input, model, parameters) {
  if (!is.null(parameters))
    args <- parameters
  else
    args <- formals(model$fun)
  mclass <- class(model)
  argsnames <- names(args)
  inputs <- "<form class='pure-form-stacked arqas-form'><fieldset>"
  if (any(mclass == "SimulatedNetwork" | mclass == "Network")) {
    inputs <- paste(inputs, "<label for='numNodes", input$results$total, "'>Number of nodes:</label>\n<input id='numNodes", input$results$total ,"' type='number' min=1 value=", length(eval(args[[1]])), " />\n", sep="")
  }
  for(i in 1:length(model$args)) {
    label <- ifelse(is.null(l <- model$args[[i]]$label), argsnames[i], l)
    switch(model$args[[i]]$type,
        "numeric" = inputs <- paste(inputs, "<label for='", argsnames[i], input$results$total , "'>", simpleCap(label),":</label>\n<input id='", argsnames[i], input$results$total , "' type='number' min=0 value=", eval(args[[i]]) ," />\n", sep=""),
        "matrix" = inputs <- paste(inputs, "<label for='", argsnames[i], input$results$total, "'>", simpleCap(label),":</label><span id='", argsnames[i], input$results$total, "' ini-value='", matrixtostring(eval(args[[i]])) ,"' class='shiny-matrix-input' ", if(any(mclass=="SimulatedNetwork" | mclass=="Network")) {"data-withoutnodes=true"} ," /><br>", sep=""),
        "vector" = inputs <- paste(inputs, "<label for='", argsnames[i], input$results$total, "'>", simpleCap(label), ":</label><input id='", argsnames[i], input$results$total, "' value='", vectortostring(eval(args[[i]])),"' class='shiny-vector-input'  /><br>", sep=""),
        "boolean" = inputs <- paste(inputs, "<label for='", argsnames[i], input$results$total, "'></label><input id='", argsnames[i], input$results$total, "' type='checkbox' ", ifelse(eval(args[[i]]), "checked", ""), ">  ", simpleCap(argsnames[i]), "</input><br>", sep=""),
        "distr" = inputs <- paste(inputs, "<label for='", argsnames[i], input$results$total, "'>", simpleCap(label), ":</label><div id='", argsnames[i], input$results$total, "' class='shiny-distr-input' defaultvalue=\"", distrDefaultValues(eval(args[[i]])), "\"></div><br>", sep=""),
        "vdistr" = inputs <- paste(inputs, "<label for='", argsnames[i], input$results$total, "'>", simpleCap(label), ":</label><div id='", argsnames[i], input$results$total, "' class='shiny-vdistr-input' defaultvalue=\"", vdistrDefaultValues(eval(args[[i]])), "\"></div><br>", sep="")
    )       
  }
  inputs <- paste(inputs, "<br></fieldset>", sep="")
  if(any(mclass == "Network")) {
    #inputs <- paste(inputs, tagList(tags$label("for"=paste("fileInput", input$results$total, sep=""), "File"), tags$input(id=paste("fileInput", input$results$total, sep=""), type="file", multiple=FALSE, title=" "), tags$br(), tags$br()), sep="")
    inputs <- paste(inputs, tagList(tags$input(id=paste("fileInput", input$results$total, sep=""), type="file", multiple=FALSE, title=" ", style="display:none;"),
                                     tags$input(id=paste("fileInput", input$results$total, "button", sep=""), type="button", class="btn btn-info", value="Upload data file", onclick=paste("document.getElementById('fileInput",input$results$total,"').click();", sep="")), tags$br(), tags$br()))
  }
  if(any(mclass == "SimulatedModel") || any(mclass == "Network")) {
    inputs <- paste(inputs, "<button id='compute", input$results$total, "' class='btn btn-primary shiny-jquerybutton-input' once>Compute</button>", sep="")
  }
  inputs <- paste(inputs, "</form>", sep="");
  return(inputs)
}

generateInputs.DataAnalysis <- function(input, model, parameters) {
  inputs <- "<form class='pure-form-stacked arqas-form'><fieldset>"
  inputs <- paste(inputs, tags$label("for"=paste("fileInput", input$results$total, sep=""), "Arrival Data:"))
  inputs <- paste(inputs, tagList(tags$input(id=paste("fileInput", input$results$total, sep=""), type="file", multiple=FALSE, title=" ", style="display:none;"),
                                  tags$input(id=paste("fileInput", input$results$total, "button", sep=""), type="button", class="btn btn-info", value="Upload data file", onclick=paste("document.getElementById('fileInput",input$results$total,"').click();", sep="")), tags$br(), tags$br()))
  inputs <- paste(inputs, tags$label("for"=paste("checkboxDistr", input$results$total, sep=""), "Distributions:"))
  inputs <- paste(inputs, "<div id='checkboxDistr", input$results$total, "' class='shiny-checkboxvdistr-input'></div>", sep="")
  inputs <- paste(inputs, "</form>", sep="");
  return(inputs)
}
#' Generate a menu, from a list.
#' @param l List
generateMenu <- function(l) {
  res <- vector("list", length(l))
  for(i in 1:length(l)) {
    res[[i]] <- list(title=l[[i]]$name, submenu=list(), id=l[[i]]$id)
  }
  return(res)
}

#' Creates a new Tab for a Tab Panel
#' @param title Title of the tab
#' @param content Content of the tab
newTab <- function(title, content) {
  list(title=title, content=content)
}

#' Sends a message to the HTML Shiny Tab Input to update its fields
#' @param session Session in the server
#' @param inputId Id of the input to update
#' @param action The action to execute. Add/Remove/Update
#' @param value The value to send. Depends of the action.
#' @param removeButton Logical to indicate if its necesary to show the "close" icon in the tab.
updateTabInput <- function(session, inputId, action=NULL, value=NULL, removeButton=FALSE, updating=FALSE) {
  message <- list(action=action, value=value, removeButton=removeButton, updating=updating)
  session$sendInputMessage(inputId, message)
}

#' Sends a message to the HTML Shiny Menu Input to update its fields
#' @param session Session in the server
#' @param inputId Id of the input to update
#' @param action The action to execute. Add/Remove/Update
#' @param menu The new value of the menu
updateMenuInput <- function(session, inputId, action=NULL, menu=NULL) {
  message <- list(action=action, menu=menu)
  session$sendInputMessage(inputId, message)
}

#' Sends a message to the HTML Shiny Vector Input to update its fields
#' @param session Session in the server
#' @param inputId Id of the input to update
#' @param value Value of the vector
updateVectorInput <- function(session, inputId, label=NULL, value=NULL) {
  message <- list(label=label, value=value)
  session$sendInputMessage(inputId, message)
}

#' Sends a message to the HTML Shiny Matrix Input to update its fields
#' @param session Session in the server
#' @param inputId Id of the input to update
#' @param min Minimum value permited
#' @param max Maximum value permited
#' @param value Actual value of the input
#' @param step adfasdf
#' @param attr Atribute to modify
#' @param attrValue Value for the attribute to modify
updateMyNumberInput <- function(session, inputId, min=NULL, max=NULL, value=NULL, step=NULL, attr=NULL, attrValue=NULL) {
  message <- list(min=min, max=max, value=value, step=step, attr=attr, attrValue=attrValue)
  session$sendInputMessage(inputId, message)
}

#' Sends a message to the HTML Shiny Matrix Input to update its fields
#' @param session Session in the server
#' @param inputId Id of the input to update
#' @param value Value of the matrix
#' @param size Number of colums or rows. Expect square matrix.
updateMatrixInput <- function(session, inputId, value=NULL, size=NULL) {
  message <- list(value=value, size=size)
  session$sendInputMessage(inputId, message)
}

#' Sends a message to the HTML Shiny Matrix Input to update the number of nodes
#' @param session Session in the server
#' @param inputId Id of the input to update
#' @param numNodes Number of tnodes
updateNodesMatrixInput <- function(session, inputId, numNodes=NULL) {
  message <- list(numNodes=numNodes)
  session$sendInputMessage(inputId, message)
}


#' Sends a message to the HTML Shiny JQueryUISlider Input to update its fields
#' @param session Session in the server
#' @param inputId Id of the input to update
#' @param values Values of the slider
#' @param step Step of the slider
#' @param min Minimun value of the slider
#' @param max Maximun value of the slider
updateJQueryUISliderInput <- function(session, inputId, values=NULL, step=NULL, min=NULL, max=NULL) {
  message <- list(values=values, step=step, min=min, max=max)
  session$sendInputMessage(inputId, message)
}

#' Sends a message to the HTML Shiny SelectDistr Input to update its fields
#' @param session Session in the server
#' @param inputId Id of the input to update
#' @param distributions List of avaiable distributions
updateSelectDistrInput <- function(session, inputId, distributions=NULL) {
  message <- list(distributions=distributions)
  session$sendInputMessage(inputId, message)
}

#' Sends a message to the HTML Shiny VectorSelectDistr Input to update its fields
#' @param session Session in the server
#' @param inputId Id of the input to update
#' @param distributions List of avaiable distributions
updateVectorSelectDistrInput <- function(session, inputId, distributions=NULL) {
  message <- list(distributions=distributions)
  session$sendInputMessage(inputId, message)
}

#' Sends a message to the HTML Shiny VectorSelectDistr Input increase the number of nodes
#' @param session Session in the server
#' @param inputId Id of the input to update
#' @param nodes The new number of nodes
updateNodesVectorSelectDistrInput <- function(session, inputId, numNodes=NULL) {
  message <- list(numNodes=numNodes)
  session$sendInputMessage(inputId, message)
}

updateCheckboxDistrInput <- function(session, inputId, distributions=NULL) {
  message <- list(distributions=distributions)
  session$sendInputMessage(inputId, message)
}

updateButtonInput <- function(session, inputId, disabled=NULL) {
  message <- list(disabled=disabled)
  session$sendInputMessage(inputId, message)
}

updateSaveReportInput <- function(session, inputId, action=NULL, sections=NULL, html=NULL, disabled=FALSE) {
  message <- list(action=action, sections=sections, html=html, disabled=disabled)
  session$sendInputMessage(inputId, message)
}

#' Returns a HTML string to build the tools to manipulate the model.
#' @param input The list of shiny inputs avaiable in the HTML
#' @param model The queue model to generete the tools
generateToolbox <- function(input, model) {UseMethod("generateToolbox", model)}

#' @rdname generateToolbox
#' @method generateToolbox MarkovianModel
#' @details
#' \code{generateToolbox.MarkovianModel} implements the method for a Markovian model
generateToolbox.MarkovianModel <- function(input, model) {
  res <- "<form class='pure-form-aligned'><fieldset>"
  defaultModel <- model$fun()
  tryCatch({Qn(defaultModel, 0)
            res <- paste(res, "<label>Pn and Qn:</label><br>", sep="")
           },
           error=function(e) {
             res <<- paste(res, "<label>Pn:</label><br>", sep="")             
  
          })
 
  maxSliderPn <- ifelse(is.infinite(mc <- maxCustomers(defaultModel)), 20, mc)
  return(paste(res, "<div class='pure-control-group'>",
                        "<label for='PnQnMin", input$results$total, "'>n from </label>",
                        "<input type='number' class='arqas-number-input' value='0' min='0' id='PnQnMin", input$results$total, "'></input>",
                    "</div>",
                    "<div class='pure-control-group'>",
                        "<label for='PnQnMax", input$results$total,"'>to </label><input type='number' class='arqas-number-input'  value='", maxSliderPn ,"' min='0' id='PnQnMax", input$results$total,"'></input><br><br>",
                        "<div id='PnQnSlider", input$results$total, "' class='shiny-slider-input' range='true' min=0 max=", maxSliderPn ," step=1 values='[0, ", maxSliderPn ,"]'></div><br>",
                    "</div>",
                    "<label for='WtWqtSlider", input$results$total, "'><b>W(t) and Wq(t):</b></label><br>",
                    "<div class='pure-control-group'>",
                        "<label for='WtWqtMin", input$results$total, "'>t from </label>",
                        "<input type='number' min='0' step='0.01' class='arqas-number-input'  id='WtWqtMin", input$results$total, "' value='0'/><br>",
                    "</div>",
                    "<div class='pure-control-group'>",
                        "<label for='WtWqtMax", input$results$total,"'>to </label>",
                        "<input type='number' min='0' step='0.01' class='arqas-number-input' id='WtWqtMax", input$results$total,"' value='0.5'/><br>",
                    "</div>",
                    "<div class='pure-control-group'>",
                        "<label for='WtWqtStep", input$results$total,"'>with step </label>",
                        "<input type='number' min='0' step='0.01' class='arqas-number-input' id='WtWqtStep", input$results$total,"' value='0.05'/><br>",
                    "</div>",
                   "<div id='WtWqtSlider", input$results$total, "' class='shiny-slider-input' range='true' min=0 max=2 step=0.05 values='[0, 0.5]'></div><br>",
                   "<button id='CalculateButton", input$results$total, "' type='button' class='btn btn-primary shiny-jquerybutton-input'>Update</button>",
                   "<hr><br>",
                   "<div id='saveReport", input$results$total, "' class='shiny-savereport-input'></div>",
               "</fieldset></form>", sep=""))
}

#' @rdname generateToolbox
#' @method generateToolbox Network
#' @details
#' \code{generateToolbox.Network} implements the method for Networks models
generateToolbox.Network <- function(input, model) {
  defaultModel <- model$fun()
  maxSliderPn <- ifelse(is.infinite(mc <- maxCustomers(defaultModel)), 20, mc)
  res <- paste("<label for='pn1nk", input$results$total, "'><b>Pn<sub>1</sub>..n<sub>", length(defaultModel$servers), "</sub></b></label><br>\n",
               "<input id='pn1nk", input$results$total, "' value=", vectortostring(1:length(defaultModel$servers)), " class='shiny-vector-input'/><br><br>\n",
               "<label for='PnSlider", input$results$total, "'><b> Pn</b><br><br> n from <input type='number' class='arqas-number-input'value='0' min='0' id='PnMin", input$results$total, "'></input> to <input type='number' class='arqas-number-input' value='5' min='0' id='PnMax", input$results$total,"'></input>:</label><br>",
               "<div id='PnSlider", input$results$total, "' class='shiny-slider-input' range='true' min=0 max=", maxSliderPn ," step=1 values='[0, 5]'></div><br>",
               "<button id='CalculateButton", input$results$total, "' type='button' class='btn btn-primary shiny-jquerybutton-input'>Update</button><br>",
               "<hr><br>",
               "<div id='saveReport", input$results$total, "' class='shiny-savereport-input'></div>",
               sep="")
  return(res)
}

#' @rdname generateToolbox
#' @method generateToolbox OpenJackson
#' @details
#' \code{generateToolbox.OpenJackson} implements the method for OpenJackson model
generateToolbox.OpenJackson <- function(input, model) {
  defaultModel <- model$fun()
  return(tagList(selectInput(inputId=paste("nodeSelector", input$results$total, sep=""), label=tags$b("Show node:"), choices=c("-----", as.character(1:length(defaultModel$servers))), multiple=FALSE), tags$br(), 
                 tags$label("for"=paste("pn1nk", input$results$total, sep=""), paste("Pn<sub>1</sub>...n<sub>", length(defaultModel$servers), "</sub>:",  sep="")), 
                 tags$input(id=paste("pn1nk", input$results$total, sep=""), value=vectortostring(1:length(defaultModel$servers)) ,class="shiny-vector-input"),
                 tags$hr(), tags$br(),
                 tags$div(id=paste("saveReport", input$results$total, sep=""), class='shiny-savereport-input')
  ))
}

#' @rdname generateToolbox
#' @method generateToolbox SimulatedModel
#' @details
#' \code{generateToolbox.SimulatedModel} implements the method for a Simulated model
generateToolbox.SimulatedModel <- function(input, model) {
  return(tagList(tags$label("for"=paste("depth", input$results$total, sep=""), "Number of points (depth): "), 
                 tags$input(id=paste("depth", input$results$total, sep=""), value=100, class="shiny-numeric-input", type="number"),
                 tags$br(), tags$br(),
                 tags$button(id=paste("CalculateButton", input$results$total, sep=""), class="btn btn-primary shiny-jquerybutton-input", "Update"),
                 tags$hr(), tags$br(),
                 tags$div(id=paste("saveReport", input$results$total, sep=""), class='shiny-savereport-input')
         ))
}

generateToolbox.DataAnalysis <- function(input, model) {
  return()
}

# #' @rdname generateToolbox
# #' @method generateToolbox SimulatedNetwork
# #' @details
# #' \code{generateToolbox.SimulatedNetwork} implements the method for a SimulatedNetwork model.
# generateToolbox.SimulatedNetwork <- function(input, model) {
#   
# }

#' Generates the divs of the tree main panels. The inputs panel, the output panel and the tools panel.
#' @param session Session in the server
#' @param input The list of shiny inputs avaiable in the HTML
#' @param model The model to generate the different panels.
#' @param parameters Optional inital values for the inputs.
generatePanel <- function(session, input, model, parameters) {UseMethod("generatePanel", model)}

generatePanel.default <- function(session, input, model, parameters) {
  list(
    paste("<div class='container-fluid'>",
              "<div id='RowToolsBox' class='row-fluid'>",
                "<div class='col-md-3'>",
                    "<div class='InputToolsBox'>",
                        "<div class='InputDataBox ui-widget-content ui-corner-all'>",
                            "<h4>Input data:</h4><hr><br>", generateInputs(input, model, parameters) ,
                        "</div>",
                        "<div class='ToolsBox ui-widget-content ui-corner-all' ", if(class(model)[1] == "DataAnalysis") "style='display:none'" else "", ">",
                            "<h4>Tools:</h4><hr><br>", generateToolbox(input, model),
                        "</div>",
                    "</div>",
                "</div>",
                "<div class='col-md-9 main'>",
                    "<div class='ModelOutputBox ui-widget-content ui-corner-all'>",
                        "<h4>Output:</h4><hr><br>",
                        "<div id='ModelOutputTabs", input$results$total ,"' class='shiny-tabs-input ModelOutputTabs'><ul></ul>",
                        "</div>",
                    "</div>",
                "</div>",
              "</div>",
          "</div>",
          sep="")
  )
}

generatePanel.DataAnalysis <- function(session, input, model, parameters) {
  list(
    paste("<div class='container-fluid'>",
            "<div id='RowToolsBox' class='row-fluid'>",
              "<div class='col-md-3'>",
                "<div class='InputToolsBox'>",
                  "<div class='InputDataBox ui-widget-content ui-corner-all' alone>",
                    "<h4>Input data:</h4><hr><br>", generateInputs(input, model, parameters) ,
                  "</div>",
                "</div>",
              "</div>",
              "<div class='col-md-9 main'>",
                "<div class='ModelOutputBox ui-widget-content ui-corner-all'>",
                  "<h4>Output:</h4><hr><br>",
                  "<div id='ModelOutputTabs", input$results$total ,"' class='shiny-tabs-input ModelOutputTabs'><ul></ul>",
                  "</div>",
                "</div>",
              "</div>",
            "</div>",
          "</div>",
          sep="")
  )
}

generatePanel.Report <- function(session, input, model, parameters) {
  list(
    paste("<div class='container-fluid'>",
          "<div id='RowToolsBox' class='row-fluid'>",
          "<div class='col-md-3'>",
          "<div class='InputToolsBox'>",
          "<div class='InputDataBox ui-widget-content ui-corner-all' alone>",
          "<center><button id='refresh", input$results$total, "' class='btn btn-primary shiny-jquerybutton-input'>Refresh</button><a id='printAllOutput", input$results$total, "' class='shiny-printall-output'><button id='printAll", input$results$total, "' class='btn btn-primary shiny-jquerybutton-input'>Print All</button></a></center><hr><br>",
          "<span id='reportMenuOutput", input$results$total, "' class='shiny-reportmenu-output'></span>",
          "</div>",
          "</div>",
          "</div>",
          "<div class='col-md-9 main'>",
          "<div id='ModelOutputBox", input$results$total, "' class='ModelOutputBox ui-widget-content ui-corner-all' style='height:90%; overflow:scroll;'>",
          "<span id='reportBodyOutput", input$results$total, "' class='shiny-reportbody-output'></span>",
          "</div>",
          "</div>",
          "</div>",
          "</div>",
          sep="")
  )
}

#' Generates a Distr object from the distrList.
#' @param distrInput The selected distribution
generateSimpleDistr <- function(distrInput) {
  lastid <- length(distrList$get())
  name <- distrList$get()[[distrInput$distribution]]$name
  if (distrInput$distribution == lastid)
    stop(simpleError("You must select a Distribution."))
  res <- "("
  for(i in 1:length(distrInput$params)) {
      res <- paste(res, distrInput$params[[i]]$name, "=", distrInput$params[[i]]$value, sep="")
      
    if (i == length(distrInput$params))
      res <- paste(res, ")", sep="")
    else
      res <- paste(res, ", ", sep="")
  }
  return(paste("distrList$get()[[", distrInput$distribution, "]]$fun", res, sep=""))
}

generateDistr <- function(distrInput) {
  if (!is.null(distrInput$distribution))
    res <- generateSimpleDistr(distrInput)
  else {
    res <- "c("
    for(i in 1:length(distrInput)) {
      if (distrInput[[i]]$distribution == 9)
        res <- paste(res, "no_distr()", sep="")
      else
        res <- paste(res, generateSimpleDistr(distrInput[[i]]), sep="")
      
      if (i != length(distrInput))
        res <- paste(res, ", ", sep="")
    }
    res <- paste(res, ")", sep="");
  }
  return(eval(parse(text=res)))
}

#' Generates the diferent arguments for a model function
#' @param fun The model function
#' @param id The number of the tab where the model is executed
generateArguments<- function(fun, id) {
  args <- names(formals(fun))
  res <- ""
  for(i in 1:length(args)) {
      switch(args[i],
             "p" = {res <- paste(res, "t(matrix(unlist(input$", args[i], id, "$matrix), nrow=length(input$", args[i], id, "$matrix)))", sep="")},
             "arrivalDistribution" = ,
             "serviceDistribution" = {res <- paste(res, "generateDistr(input$", args[i], id, ")", sep="")},
              res <- paste(res, "input$", args[i], id, sep="")
      )
    
      if (i != length(args))
        res <- paste(res, ", ", sep="")
  }
  return(res)
}

#' Generates a Datatable from the Pn and Qn values for the model
#' @param model Queue model
#' @param rangePnQn List of length 2 with the start value and the end value.
datatablePnQn <- function(model , rangePnQn) {
  ranges <- seq(rangePnQn[[1]], rangePnQn[[2]], 1)
  pns <- Pn(model, ranges)
  tryCatch ({
    qns <-Qn(model, ranges)
    return(data.frame("n"= ranges, "P"=pns, "Q"=qns))
  }, error = function(e) {
    return(data.frame("n"= ranges, "P"=pns))
  })
}

#' Generates a Datatable from the Wt and Wqt values for the model
#' @param model Queue model
#' @param rangeWtWqt List of length 2 with the start value and the end value.
#' @param step The desired step for the range.
datatableWtWqt <- function(model, rangeWtWqt, step) {
  ranges <- seq(rangeWtWqt[[1]], rangeWtWqt[[2]], by=step)
  wts <-  FW(model, ranges)
  wqs <- FWq(model, ranges)
  wqs <- ifelse(is.na(wqs) | is.infinite(wqs), 0, wqs)
  return(data.frame("t"=ranges, "W"=wts, "Wq"= wqs))
}

HTMLModelBasicData <- function(qm) {UseMethod("HTMLModelBasicData", qm)}

HTMLModelBasicData.MarkovianModel <- function(qm) {
  fancyName <- function(t) {
    return(switch(t, "arrivalDistribution"="Arrival Distribution", 
                     "serviceDistribution"="Service Distribution",
                     "servers"="Servers",
                      "h"="H", "k"="K", "y"="Y", t))
  }
  numElem <- length(qm)-1
  nameElem <- names(qm)[1:numElem]
  res <- paste("<table border=1 style='border-collapse:collapse;table-layout: fixed;text-align:center;width:auto;'><tr style='border: 1px solid #77d5f7;  font-weight: normal; color: #ffffff; background-color: #41a1c9;'><td colspan=", numElem, "><b>", gsub(pattern = "_", replacement = "/", x = class(qm)[1]), "</b></td></tr>", collapse="")
  
  res <- paste(res, "<tr style='border: 1px solid #77d5f7;  font-weight: normal; color: #ffffff; background-color: #41a1c9;'>", collapse="")
  for(i in nameElem) 
    res <- paste(res, "<td><b>", fancyName(i), "</b></td>", collapse="")
  res <- paste(res, "</tr><tr>", collapse="")
  for(j in 1:numElem)
    res <- paste(res, "<td>", if (!is.null(attribute <- attr(class(qm[[j]]), "package")) && attribute == "distr") distrToText(qm[[j]]) else qm[[j]], "</td>", collapse="")
  
  res <- paste(res, "</tr></table>", collapse="")

  return(res)
}

HTMLModelBasicData.ClosedJackson <- function(qm) {
  res <- paste("<table border=1 style='border-collapse:collapse;table-layout: fixed;text-align:center;width:auto;'><tr style='border: 1px solid #77d5f7;  font-weight: normal; color: #ffffff; background-color: #41a1c9;'><td colspan=5><b>", class(qm)[1], "</b></td></tr>", collapse="")
  res <- paste(res, "<tr style='border: 1px solid #77d5f7;  font-weight: normal; color: #ffffff; background-color: #41a1c9;'><td><b>Node</b></td><td><b>Arrival Distribution</b></td><td><b>Service Distribution</b></td><td><b>Servers</b></td><td><b>Number of customers</b></td></tr>", collapse="")
  for (i in 1:(qm$k)) {
    res <- paste(res, "<tr><td>", i, "</td>", "<td>Exp(", round(qm$lambda[i], 6), ")</td>", "<td>Exp(", qm$mu[i], ")</td>", "<td>", qm$servers[i], "</td>", collapse="")
    if (i == 1)
      res <- paste(res, "<td>", qm$n, "</td></tr>", collapse="")
    else
      res <- paste(res, "</tr>", collapse="")
  }
  res <- paste(res, "</table>", collapse="")
  return(res)
}

HTMLModelBasicData.Network <- function(qm) {
  fancyName <- function(t) {
    return(switch(t, "arrivalDistribution"="Arrival Distribution", 
                  "serviceDistribution"="Service Distribution",
                  "servers"="Servers",
                  t))
  }
  numElem <- length(qm$nodes[[1]])
  nameElem <- names(qm$nodes[[1]])
  res <- paste("<table border=1 style='border-collapse:collapse;table-layout: fixed;text-align:center;width:auto;'><tr style='border: 1px solid #77d5f7;  font-weight: normal; color: #ffffff; background-color: #41a1c9;'><td colspan=", numElem, "><b>", class(qm)[1], "</b></td></tr>", collapse="")
  
  res <- paste(res, "<tr style='border: 1px solid #77d5f7;  font-weight: normal; color: #ffffff; background-color: #41a1c9;'><td><b>Node</b></td>", collapse="")
  for (i in nameElem[-length(nameElem)]) 
    res <- paste(res, "<td><b>", fancyName(i), "</b></td>", collapse="")
  
  res <- paste(res, "</tr>", collapse="")
  for (i in 1:length(qm$nodes)) {
    res <- paste(res, "<tr><td>", i, "</td>", collapse="")
    for(j in 1:(length(nameElem)-1))
      res <- paste(res, "<td>", if (!is.null(attribute <- attr(class(qm$nodes[[i]][[j]]), "package")) && attribute == "distr") distrToText(qm$nodes[[i]][[j]]) else qm$nodes[[i]][[j]], "</td>", collapse="")  
    res <- paste(res, "</tr>")
  }
  res <- paste(res, "</table>", collapse="") 
  return(res)  
}

HTMLModelBasicData.SimulatedModel <- function(qm) {
  fancyName <- function(t) {
    return(switch(t, "arrivalDistribution"="Arrival Distribution", 
                  "serviceDistribution"="Service Distribution",
                  "s"="Servers",
                  "staclients"="Num. Customers for Stabilitation",
                  "nclients"="Num. Customers for Simulation",
                   "h"="Potencial Costumers",
                  "y"="Replacements",
                  "k"="Queue Size",
                  t))
  }
  numElem <- length(qm)-1
  nameElem <- names(qm)[1:numElem]
  res <- paste("<table border=1 style='border-collapse:collapse;table-layout: fixed;text-align:center;width:auto;'><tr style='border: 1px solid #77d5f7;  font-weight: normal; color: #ffffff; background-color: #41a1c9;'><td colspan=", numElem, "><b>", gsub(pattern = "_", replacement = "/", x = class(qm)[1]), "</b></td></tr>", collapse="")
  
  res <- paste(res, "<tr style='border: 1px solid #77d5f7;  font-weight: normal; color: #ffffff; background-color: #41a1c9;'>", collapse="")
  for(i in nameElem) 
    res <- paste(res, "<td><b>", fancyName(i), "</b></td>", collapse="")
  res <- paste(res, "</tr><tr>", collapse="")
  for(j in 1:numElem)
    res <- paste(res, "<td>", if (!is.null(attribute <- attr(class(qm[[j]]), "package")) && attribute == "distr") distrToText(qm[[j]]) else qm[[j]], "</td>", collapse="")
  
  res <- paste(res, "</tr></table>", collapse="")
  return(res)
}

HTMLModelBasicData.Open <- function(qm) {
  fancyName <- function(t) {
    return(switch(t, "arrivalDistribution"="Arrival Distribution", 
                  "serviceDistribution"="Service Distribution",
                  "s"="Servers",
                  "staclients"="Num. Customers for Stabilitation",
                  "transitions" = "Transitions",
                  t))
  }
  qmAux <- qm[-4]
  numElem <- length(qmAux)-1
  nameElem <- names(qmAux)[1:numElem]
  res <- paste("<table border=1 style='border-collapse:collapse;table-layout: fixed;text-align:center;width:auto;'><tr style='border: 1px solid #77d5f7;  font-weight: normal; color: #ffffff; background-color: #41a1c9;'><td colspan=", numElem+1, "><b>", class(qm)[1], "</b></td></tr>", collapse="")
  
  res <- paste(res, "<tr style='border: 1px solid #77d5f7;  font-weight: normal; color: #ffffff; background-color: #41a1c9;'><td><b>Node</b></td>", collapse="")
  for(i in nameElem) 
    res <- paste(res, "<td><b>", fancyName(i), "</b></td>", collapse="")
  res <- paste(res, "</tr>", collapse="")
  for (i in 1:length(qm$s)) {
    res <- paste(res, "<tr><td>", i, "</td><td>",
                                  distrToText(qm$arrivalDistribution[[i]]), "</td><td>",
                                  distrToText(qm$serviceDistribution[[i]]), "</td><td>",
                                  qm$s[i], "</td>", sep="")
    if (i == 1)
      res <- paste(res, "<td>", qm$staclients, "</td><td>", qm$transitions, "</td>", sep="")
    res <- paste(res, "</tr>", sep="")
  }
  res <- paste(res, "</tr></table>", collapse="")
  return(res)
  
}

HTMLModelBasicData.Closed <- function(qm) {
  fancyName <- function(t) {
    return(switch(t,
                  "serviceDistribution"="Service Distribution",
                  "s"="Servers",
                  "staclients"="Num. Customers for Stabilitation",
                  "transitions" = "Transitions",
                  "nclients" = "Num. of Customers in the network",
                  t))
  }
  qmAux <- qm[-3]
  numElem <- length(qmAux)-1
  nameElem <- names(qmAux)[1:numElem]
  res <- paste("<table border=1 style='border-collapse:collapse;table-layout: fixed;text-align:center;width:auto;'><tr style='border: 1px solid #77d5f7;  font-weight: normal; color: #ffffff; background-color: #41a1c9;'><td colspan=", numElem+1, "><b>", class(qm)[1], "</b></td></tr>", collapse="")
  
  res <- paste(res, "<tr style='border: 1px solid #77d5f7;  font-weight: normal; color: #ffffff; background-color: #41a1c9;'><td><b>Node</b></td>", collapse="")
  for(i in nameElem) 
    res <- paste(res, "<td><b>", fancyName(i), "</b></td>", collapse="")
  res <- paste(res, "</tr>", collapse="")
  for (i in 1:length(qm$s)) {
    res <- paste(res, "<tr><td>", i, "</td><td>",
                 distrToText(qm$serviceDistribution[[i]]), "</td><td>",
                 qm$s[i], "</td>", sep="")
    if (i == 1)
      res <- paste(res, "<td>", qm$staclients, "</td><td>", qm$transitions, "</td><td>", qm$nclients, "</td>", sep="")
    res <- paste(res, "</tr>", sep="")
  }
  res <- paste(res, "</tr></table>", collapse="")
  return(res)
  
}

HTMLModelBasicData.list <- function(qm) {
  HTMLModelBasicData(combineSimulations(qm))
}

HTMLlist <- function(l) {
   aux <- paste("<ul>", paste("<li><label>", names(l), ":</label> ", lapply(l, function(v){if (!is.null(attribute <- attr(class(v), "package")) && attribute == "distr") distrToText(v) else v}), "</li>", collapse=""), "</ul>", collapse="")
   return(aux)
}

HTMLdatatable  <- function(d) {
  aux <- "<table border=1 style='border-collapse:collapse;table-layout: fixed;text-align:center;width:auto;'><tr style='border: 1px solid #77d5f7;	font-weight: normal; background-color: #41a1c9; color: #ffffff;'>"
  columnnames <- names(d)
  columnnames[columnnames == "X_empty"] <- ""
  aux <- paste(aux, paste("<td><b>", columnnames, "</b></td>", collapse=""), sep="")
  for(i in 1:nrow(d)) {
    aux <- paste(aux, "<tr>", sep="")
    for (j in 1:ncol(d))
      aux <- paste(aux, "<td>", d[i, j], "</td>", sep="")
    aux <- paste(aux, "</tr>", sep="")
  }
  return(paste(aux, "</table>", sep=""))
}

HTMLmatrix  <- function(m, rownames=NULL, colnames=NULL, corner=NULL) {
  res <- "<table border=1 style='border-collapse:collapse;table-layout: fixed;text-align:center;width:auto;'>"
  if (!is.null(colnames) && length(colnames) == ncol(m))
    res <- paste(res, "<tr style='border: 1px solid #77d5f7;  font-weight: normal; color: #ffffff; background-color: #41a1c9;'><td><b>",if(!is.null(corner)) {corner},"</b></td>", paste("<td><b>", colnames, "</b></td>", collapse=""), "</tr>", collapse="")
  for(i in 1:nrow(m)) {
    res <- paste(res, "<tr>", if(!is.null(rownames) && length(rownames)==nrow(m)) {paste("<td><b>", rownames[i], "</b></td>", collapse="")}, paste("<td>", m[i,], "</td>", collapse=""), "</tr>", collapse="")
  }
  res <- paste(res, "</table>")
  return(res)
}

#'Generates a report with the result of an analysis or simulation with the desired sections.
generateReport <- function(reportData, qm, numTab) {
#     nReports <- 0
#     if (!file.exists(file.path("./www/report", "report_main.html"))) {
#       #target <- HTMLInitFile(file.path("./www/report"), "report", HTMLframe = TRUE, CSSFile = NULL, Title="Arqas Report", useLaTeX = FALSE, useGrid=FALSE)
#       #cat("<script src='../shared/jquery-ui/js/jquery-1.9.1.js' type='text/javascript'></script>", file = target, append = TRUE, sep = " ")
#       cat("<script src='./report.js' type='text/javascript'></script>", file = target, append = TRUE, sep = " ")
#       menu <- HTMLSetFile(file.path("./www/report", "report_menu.html"))
#     }
#     else {
#       target <- HTMLSetFile(file.path("./www/report", "report_main.html"))
#       menu <- HTMLSetFile(file.path("./www/report", "report_menu.html"))
#       menuContent <- unique(readLines(menu,skipNul = TRUE))
#       for (i in menuContent) {
#         if(!is.null(aux <- grep("class='reportEntry'", i)) & length(aux) > 0)
#           nReports <- nReports+1 
#       }
#     }

    if (is.null(reportData$title) | reportData$title == "") {
      reportData$title <- paste("ArqasReport", reportData$nReports+1, sep="")
    }
    
    rep.body <- paste("<div id='ArqasReport", reportData$nReports, "'>", sep="")
#     R2HTML::HTML(paste("<div id='ArqasReport", nReports, "'>", sep=""), file=target)
    rep.body <- paste(rep.body, "<center><u><a name='", reportData$title, "-", reportData$nReports, "'></a><h3>",reportData$title,"</h3></u></center>", sep="")
#     R2HTML::HTML(paste("<h3><a name='", reportData$title, "'>",reportData$title,"</a></h3>", sep=""), file=target)
    rep.menu <- paste("<li><b><a href='javascript:void(0);' name='",reportData$title,"-", reportData$nReports, "' >", reportData$title, "</a></b>  <a id='", reportData$title, "-", reportData$nReports, "-hide' mytarget='ArqasReport", reportData$nReports, "' href='javascript:void(0);'>[Hide]</a>  <a id='", reportData$title, "-", reportData$nReports, "-print' href='javascript:void(0);' mytarget='ArqasReport", reportData$nReports, "'>[Print]</a></li><ul>", sep="")
#     R2HTML::HTML(paste("<a href='report_main.html#", reportData$title, "' target='main' class='reportEntry'>", reportData$title, "</a>", sep=""), file=menu)
#     R2HTML::HTML("<ul>", file=menu)
    rep.body <- paste(rep.body,"<h4>Model info: </h4><br>", sep="" )
#     R2HTML::HTML(paste("<h4>Model info: </h4><br>", sep=""), file=target);
    rep.body <- paste(rep.body,HTMLModelBasicData(qm))
#     R2HTML::HTML(HTMLModelBasicData(qm), file=target)
    for(section in reportData$checkboxes)
      switch(section$val,
                 "summary"= {
#                              R2HTML::HTML(paste("<h4><a name='", reportData$title, "-summary'></a>Summary: </h4><br>", sep=""), file=target)
                               rep.body <- paste(rep.body, "<h4><a name='", reportData$title, "-", reportData$nReports, "-summary'></a>Summary: </h4><br>", HTMLdatatable(toDatatable(if(any(class(qm)=="list")) combineSimulations(qm) else qm)), sep="")
                               rep.menu <- paste(rep.menu, "<li><a href='javascript:void(0);' name='", reportData$title, "-", reportData$nReports, "-summary'>Summary</a></li>", sep="")
#                              R2HTML::HTML(paste("<li><a href='report_main.html#", reportData$title, "-summary' target='main'>Summary</a></li>", sep=""), file=menu)
#                              R2HTML::HTML(HTMLdatatable(toDatatable(qm)), file=target)
                   },
                 "probabilities" = {
#                                     R2HTML::HTML(paste("<h4><a name='", reportData$title, "-probabilities'></a>Probabilities: </h4><br>", sep=""), file=target)
                                      rep.body <- paste(rep.body, "<h4><a name='", reportData$title, "-", reportData$nReports, "-probabilities'></a>Probabilities: </h4><br>", sep="")
                                      rep.menu <- paste(rep.menu, "<li><a href='javascript:void(0);' name='", reportData$title, "-", reportData$nReports, "-probabilities'>Probabilities</a></li>", sep="")
#                                     R2HTML::HTML(paste("<li><a href='report_main.html#", reportData$title, "-probabilities' target='main'>Probabilities</a></li>", sep=""), file=menu)
                                    if (any(class(qm) == "ClosedJackson")) {
                                         aux <- data.frame()
                                        rangePn <- section$data$from:section$data$to
                                        for(i in 1:length(qm$s)) {
                                          aux <- rbind(aux, data.frame(list(n=rangePn, Pn=Pi(qm, rangePn, i), node=rep(i, length(rangePn)))))
                                        }
                                        rep.body <- paste(rep.body, HTMLdatatable(aux), sep="")
#                                         R2HTML::HTML(HTMLdatatable(rep.body), file=target)
                                    }
                                    else {
                                        rep.body <- paste(rep.body, HTMLdatatable(datatablePnQn(qm, range(section$data$from, section$data$to))), sep="")
#                                         R2HTML::HTML(HTMLdatatable(datatablePnQn(qm, range(section$data$from, section$data$to))), file=target)
                                    }},
                 "waitingtimes"  = {
                                      rep.body <- paste(rep.body, "<h4><a name ='", reportData$title, "-",reportData$nReports, "-waitingtimes'></a>Waiting Times: </h4><br>",
                                                   HTMLdatatable(datatableWtWqt(qm, range(section$data$from, section$data$to), section$data$step)), sep="")
                                      rep.menu <- paste(rep.menu, "<li><a href='javascript:void(0);' name ='", reportData$title, "-",reportData$nReports, "-waitingtimes'>Waiting times</a></li>", sep="" )
#                                     R2HTML::HTML(paste("<h4><a name ='", reportData$title, "-waitingtimes'></a>Waiting Times: </h4><br>", sep=""), file=target)
#                                     R2HTML::HTML(paste("<li><a href='report_main.html#", reportData$title, "-waitingtimes' target='main'>Waiting times</a></li>", sep=""), file=menu)
#                                     R2HTML::HTML(HTMLdatatable(datatableWtWqt(qm, range(section$data$from, section$data$to), section$data$step)), file=target)
                                    },
                 "waitingplots"   = {
                                      rep.body <- paste(rep.body, "<h4><a name='", reportData$title, "-", reportData$nReports, "-waitingplots'></a>Waiting Plots: </h4><br>", sep="")
#                                     R2HTML::HTML(paste("<h4><a name='", reportData$title, "-waitingplots'></a>Waiting Plots: </h4><br>", sep=""), file=target)
                                      rep.menu <- paste(rep.menu, "<li><a href='javascript:void(0);' name='", reportData$title, "-", reportData$nReports, "-waitingplots'>Waiting Plots</a></li>", sep="")
#                                      R2HTML::HTML(paste("<li><a href='report_main.html#", reportData$title, "-waitingplots' target='main'>Waiting Plots</a></li>", sep=""), file=menu)
                                      outfile <- tempfile(fileext='.png')
                                      plot(qm, seq(section$data$from, section$data$to, by=section$data$step), only="t")
                                      ggplot2::ggsave(outfile, width=11, height=6.5, dpi=75)
                                      rep.body <- paste(rep.body, base64::img(outfile), sep="")
#                                     R2HTML::HTML(paste("<center><img src='./plots/", basename(outfile), "'></img></center>", sep=""), file=target)
                                    },
                 "probabilitiesplots" = {
                                          rep.body <- paste(rep.body, "<h4><a name='", reportData$title, "-",reportData$nReports, "-probabilitiesplot'></a>Probabilities Plots: </h4><br>", sep="" )
#                                         R2HTML::HTML(paste("<h4><a name='", reportData$title, "-probabilitiesplot'></a>Probabilities Plots: </h4><br>", sep=""), file=target)
                                          rep.menu <- paste(rep.menu, "<li><a href='javascript:void(0);' name='", reportData$title, "-",reportData$nReports, "-probabilitiesplot'>Probabilities Plot</a></li>", sep="")
#                                          R2HTML::HTML(paste("<li><a href='report_main.html#", reportData$title, "-probabilitiesplot' target='main'>Probabilities Plot</a></li>", sep=""), file=menu)
                                         outfile <- tempfile(fileext='.png')
                                         plot(qm, seq(section$data$from, section$data$to, 1), only="n")
                                         ggplot2::ggsave(outfile, width=11, height=6.5, dpi=75)
                                         rep.body <- paste(rep.body, base64::img(outfile), sep="")
#                                         R2HTML::HTML(paste("<center><img src='./plots/", basename(outfile), "' alt='test'></img></center>", sep=""), file=target)
                                        },
                "networkgraph" = {
                                    rep.body <- paste(rep.body, "<h4><a name='", reportData$title, "-", reportData$nReports, "-networkgraph'></a>Network Graph: </h4><br>" , sep="")
#                                   R2HTML::HTML(paste("<h4><a name='", reportData$title, "-networkgraph'></a>Network Graph: </h4><br>", sep=""), file=target)
                                    rep.menu <- paste(rep.menu, "<li><a href='javascript:void(0);' name='", reportData$title, "-", reportData$nReports, "-networkgraph'>Network graph</a></li>", sep="" )
#                                   R2HTML::HTML(paste("<li><a href='report_main.html#", reportData$title, "-networkgraph' target='main'>Network graph</a></li>", sep=""), file=menu)
                                    rep.body <- paste(rep.body, "<img src='", section$data$image,"'></img>", sep="")
#                                   R2HTML::HTML(paste("<center><img src='", section$data$image,"'></img></center>", sep=""), file=target)
                                    rep.body <- paste(rep.body, "<h5>Routing Matrix</h5><br>", sep="")
                                    if (any(class(qm)=="list")) {
                                        combined <- combineSimulations(qm)
                                        rep.body <- paste(rep.body, HTMLmatrix(combined$prob, 1:nrow(combined$prob), 1:ncol(combined$prob), "from\\to"), sep="")
                                    } else
                                      rep.body <- paste(rep.body, HTMLmatrix(qm$prob, 1:nrow(qm$prob), 1:ncol(qm$prob), "from\\to"), sep="")
#                                   R2HTML::HTML("<center><h5>Routing Matrix</h5></center><br>", file=target)
#                                   R2HTML::HTML(HTMLmatrix(qm$prob, 1:nrow(qm$prob), 1:ncol(qm$prob), "from\\to"), file=target)
                                  },
                "combinedprobabilities" = {
                                            rep.body <- paste(rep.body, "<h4><a name='", reportData$title, "-", reportData$nReports, "-combinedprobabilities'></a>Combined Probabilities: </h4><br>", sep="")
#                                           R2HTML::HTML(paste("<h4><a name='", reportData$title, "-combinedprobabilities'></a>Combined Probabilities: </h4><br>", sep=""), file=target)
                                            rep.menu <- paste(rep.menu, "<li><a href='javascript:void(0);' name='", reportData$title, "-", reportData$nReports, "-combinedprobabilities'>Combined Probabilities</a></li>", sep="")
#                                           R2HTML::HTML(paste("<li><a href='report_main.html#", reportData$title, "-combinedprobabilities' target='main'>Combined Probabilities</a></li>", sep=""), file=menu)
#                                           print(unlist(section$data$combinedprobabilities))
                                            auxDataFrame <- data.frame(Pn(qm, unlist(section$data$combinedprobabilities)))
                                            colnames(auxDataFrame) <- c(paste("P(", paste(section$data$combinedprobabilities, collapse=","), ")", collapse=""))
                                            rep.body <- paste(rep.body, HTMLdatatable(auxDataFrame), sep="")
#                                           R2HTML::HTML(HTMLdatatable(auxDataFrame),  file=target)
                                          },
                "LEvolution" = {
                                 rep.body <- paste(rep.body, "<h4><a name='", reportData$title, "-", reportData$nReports, "-levolution'></a>Evolution of L: </h4><br>", sep="")
                                 rep.menu <- paste(rep.menu, "<li><a href='javascript:void(0);' name='", reportData$title, "-", reportData$nReports, "-levolution'>Evolution of L</a></li>", sep="")
                                 outfile <- tempfile(fileext='.png')
                                 if(class(qm)[1] == 'list')
                                      if (!is.null(qm[[1]]$transitions))
                                        maxrange <- qm[[1]]$staclients + qm[[1]]$transitions
                                      else
                                        maxrange <- qm[[1]]$staclients + qm[[1]]$nclients
                                 else
                                      maxrange <- qm$staclients + qm$nclients
                                 plot(qm, 1, maxrange, "L", depth=section$data$depth, nSimulation=section$data$nsim)
                                 ggplot2::ggsave(outfile, width=11, height=6.5, dpi=75)
                                 rep.body <- paste(rep.body,  base64::img(outfile),  sep="")
                },
                "LqEvolution" = {
                                  rep.body <- paste(rep.body, "<h4><a name='", reportData$title, "-", reportData$nReports, "-lqevolution'></a>Evolution of Lq: </h4><br>", sep="")
                                  rep.menu <- paste(rep.menu, "<li><a href='javascript:void(0);' name='", reportData$title, "-", reportData$nReports, "-lqevolution'>Evolution of Lq</a></li>", sep="")
                                  outfile <- tempfile(fileext='.png')
                                  if(class(qm)[1] == 'list')
                                    if (!is.null(qm[[1]]$transitions))
                                      maxrange <- qm[[1]]$staclients + qm[[1]]$transitions
                                    else
                                      maxrange <- qm[[1]]$staclients + qm[[1]]$nclients
                                  else
                                    maxrange <- qm$staclients + qm$nclients
                                  plot(qm, 1, maxrange, "Lq", depth=section$data$depth, nSimulation=section$data$nsim)
                                  ggplot2::ggsave(outfile, width=11, height=6.5, dpi=75)
                                  rep.body <- paste(rep.body,  base64::img(outfile), sep="")                  
                },
                "WEvolution" = {
                                rep.body <- paste(rep.body, "<h4><a name='", reportData$title, "-", reportData$nReports, "-wevolution'></a>Evolution of W: </h4><br>", sep="")
                                rep.menu <- paste(rep.menu, "<li><a href='javascript:void(0);' name='", reportData$title, "-", reportData$nReports, "-wevolution'>Evolution of W</a></li>", sep="")
                                outfile <- tempfile(fileext='.png')
                                if(class(qm)[1] == 'list')
                                  if (!is.null(qm[[1]]$transitions))
                                    maxrange <- qm[[1]]$staclients + qm[[1]]$transitions
                                  else
                                    maxrange <- qm[[1]]$staclients + qm[[1]]$nclients
                                else
                                  maxrange <- qm$staclients + qm$nclients
                                plot(qm, 1, maxrange, "W", depth=section$data$depth, nSimulation=section$data$nsim)
                                ggplot2::ggsave(outfile, width=11, height=6.5, dpi=75)
                                rep.body <- paste(rep.body,  base64::img(outfile), sep="")                  
                },
                "WqEvolution" = {
                                rep.body <- paste(rep.body, "<h4><a name='", reportData$title, "-", reportData$nReports, "-wqevolution'></a>Evolution of Wq: </h4><br>", sep="")
                                rep.menu <- paste(rep.menu, "<li><a href='javascript:void(0);' name='", reportData$title, "-", reportData$nReports, "-wqevolution'>Evolution of Wq</a></li>", sep="")
                                outfile <- tempfile(fileext='.png')
                                if(class(qm)[1] == 'list')
                                  if (!is.null(qm[[1]]$transitions))
                                    maxrange <- qm[[1]]$staclients + qm[[1]]$transitions
                                else
                                  maxrange <- qm[[1]]$staclients + qm[[1]]$nclients
                                else
                                  maxrange <- qm$staclients + qm$nclients
                                plot(qm, 1, maxrange, "Wq", depth=section$data$depth, nSimulation=section$data$nsim)
                                ggplot2::ggsave(outfile, width=11, height=6.5, dpi=75)
                                rep.body <- paste(rep.body,  base64::img(outfile),  sep="")                  
                },
                "ClientsEvolution" = {
                                rep.body <- paste(rep.body, "<h4><a name='", reportData$title, "-", reportData$nReports, "-clientsevolution'></a>Evolution of Clients: </h4><br>", sep="")
                                rep.menu <- paste(rep.menu, "<li><a href='javascript:void(0);' name='", reportData$title, "-", reportData$nReports, "-clientsevolution'>Evolution of Clients</a></li>", sep="")
                                outfile <- tempfile(fileext='.png')
                                if(class(qm)[1] == 'list')
                                  maxrange <- qm[[1]]$staclients + qm[[1]]$nclients
                                else
                                  maxrange <- qm$staclients + qm$nclients
                                #print(section$data$nsim)
                                plot(qm, 1, maxrange, "Clients", depth=section$data$depth, nSimulation=section$data$nsim)
                                ggplot2::ggsave(outfile, width=11, height=6.5, dpi=75)
                                rep.body <- paste(rep.body,  base64::img(outfile),  sep="")                  
                },
                "IntensityEvolution" = {
                                rep.body <- paste(rep.body, "<h4><a name='", reportData$title, "-", reportData$nReports, "-intensityevolution'></a>Evolution of Intensity: </h4><br>", sep="")
                                rep.menu <- paste(rep.menu, "<li><a href='javascript:void(0);' name='", reportData$title, "-", reportData$nReports, "-intensityevolution'>Evolution of Intensity</a></li>", sep="")
                                outfile <- tempfile(fileext='.png')
                                if(class(qm)[1] == 'list')
                                  maxrange <- qm[[1]]$staclients + qm[[1]]$nclients
                                else
                                  maxrange <- qm$staclients + qm$nclients
                                plot(qm, 1, maxrange, "Intensity", depth=section$data$depth, nSimulation=section$data$nsim)
                                ggplot2::ggsave(outfile, width=11, height=6.5, dpi=75)
                                rep.body <- paste(rep.body, base64::img(outfile),  sep="")                  
                }
      )
#     R2HTML::HTML("</ul>", file=menu)
    rep.menu <- paste(rep.menu, "</ul><br>", sep="")
    rep.body <- paste(rep.body, "</div><hr>", sep="")
#     R2HTML::HTML("</div>",file=target)
#     HTMLhr(file=target)
    return(list(rep.menu, rep.body))
}

#' Is the main function of the UI. Generate the panels and rules the behavior of the inputs and buttons.
#' @param model Queue model
#' @param session The session in the server
#' @param input The list of shiny inputs avaiable in the HTML
#' @param output The list of shiny outputs avaiable in the HTML
#' @param parameters Optional initial values for the inputs
loadUIModel <- function(model, session, input, output, parameters=NULL) {UseMethod("loadUIModel", model)}

#' @rdname loadUIModel
#' @method loadUIModel MarkovianModel
#' @details
#' \code{loadUIModel.MarkovianModel} implements the method for a Markovian model
loadUIModel.MarkovianModel <- function(model, session, input, output, parameters=NULL) {
  numTab <- input$results$total
  updateTabInput(session, "results", list("add"),  list(newTab(model$name, generatePanel(session, input, model, parameters))), removeButton=TRUE)
  updateSaveReportInput(session, paste("saveReport", numTab, sep=""), action="setSections", sections=list("summary"="Summary", "probabilities" = "Probabilities", "waitingtimes"="Waiting times", "waitingplots" ="Waiting Distribution plots", "probabilitiesplots"="Probabilities plots"))
  eval(parse(text=paste("updateTabInput(session, 'ModelOutputTabs", numTab , "', action=list('add'), value=list(newTab('Summary', '<div id=\"error", numTab, "\" class=\"shiny-html-output\"></div><h4>Basic information:</h4><hr><div id=\"summaryDatatable", numTab, "\" class=\"shiny-mydatatable-output\"></div><h4>Probabilities:</h4><hr><div id=\"pnDatatable", numTab, "\" class=\"shiny-mydatatable-output\"></div><br><br><h4>Waiting times:</h4><hr><div id=\"wtDatatable", numTab, "\" class=\"shiny-mydatatable-output\"></div>')))\n", sep="")))
  eval(parse(text=paste("updateTabInput(session, 'ModelOutputTabs", numTab , "', action=list('add'), value=list(newTab('Waiting distributions plot', '<div id=\"waitplotDiv", numTab, "\" class=\"shiny-image-output\"></div>')))\n", sep=""))) 
  eval(parse(text=paste("updateTabInput(session, 'ModelOutputTabs", numTab , "', action=list('add'), value=list(newTab('Probabilites plot', '<div id=\"probplotDiv", numTab, "\" class=\"shiny-image-output\"></div>')))\n", sep=""))) 
  
  eval(parse(text=paste("observe({\n",
                        "updateNumericInput(session, 'PnQnMin", numTab, "', value=input$PnQnSlider", numTab,"$values[1])\n",
                        "updateNumericInput(session, 'PnQnMax", numTab, "', value=input$PnQnSlider", numTab,"$values[2])}, priority=2)", sep="")))
  
  eval(parse(text=paste("observe({\n",
                        "updateNumericInput(session, 'WtWqtMin", numTab, "', value=input$WtWqtSlider", numTab,"$values[1], step=input$WtWqtSlider", numTab, "$step)\n",
                        "updateNumericInput(session, 'WtWqtMax", numTab, "', value=input$WtWqtSlider", numTab,"$values[2], step=input$WtWqtSlider", numTab, "$step)\n",
                        "}, priority=3)", sep="")))
  
  eval(parse(text=paste("observe({\n",
                        "updateJQueryUISliderInput(session, 'WtWqtSlider", numTab, "', step=input$WtWqtStep", numTab,")\n",
                        "}, priority=4)", sep="")))
  
  eval(parse(text=paste("observe({\n",
                          "if (!is.null(reportData <- input$saveReport", numTab, ") && reportData$button > 0)\n",
                              "isolate({updateSaveReportInput(session, 'saveReport", numTab, "', action='saveReport', html=generateReport(reportData, values$qm))})\n",
                        "})", sep="")))
  
   values <- reactiveValues()

   eval(parse(text=paste(
      "observe({\n",
        "tryCatch({\n",
          "values$qm <- model$fun(", generateArguments(model$fun, numTab),")\n",
           "isolate({mc <- ifelse(is.infinite(mc <- maxCustomers(values$qm)), 20, mc)\n",
           "updateJQueryUISliderInput(session, 'PnQnSlider", numTab, "', max=mc)})\n",
        "}, error=function(e) {\n",
          "values$qm <<- NULL\n",
          "values$error <<- e$message\n",
        "})\n",
      "})\n",
      "output$probplotDiv", numTab, "<- renderImage({outfile <- tempfile(fileext='.svg')\n",
                                                     "input$CalculateButton", input$results$total, "\n",
                                                     "if(is.null(values$qm)) stop(values$error)\n",
                                                     "isolate({\n",
                                                         "pnqnrange <- range(input$PnQnMin", numTab, ", input$PnQnMax", numTab, ")\n",
                                                     "})\n", 
                                                     "plot(values$qm, seq(pnqnrange[1], pnqnrange[2], 1), only='n')\n",
                                                     "ggplot2::ggsave(outfile, width=11, height=6.5, dpi=100)\n",
                                                     "title <- 'Pn: Steady-state probability of having n customers in the system.\n'\n",
                                                     "try({Qn(values$qm, 0); title <- paste(title, 'Qn: Steady-state probability of finding n customers in the system when a new customer arrives.', sep='')})\n",
                                                     "list(src=outfile, alt='Loading plot...', title=title)}, deleteFile=TRUE)\n",
      "output$waitplotDiv", numTab, "<- renderImage({outfile <- tempfile(fileext='.svg')\n",
                                                    "input$CalculateButton", input$results$total, "\n",
                                                    "if(is.null(values$qm)) stop(values$error)\n",
                                                    "isolate({\n",
                                                      "wtwqtrange <- range(input$WtWqtMin", numTab, ", input$WtWqtMax", numTab, ")\n",
                                                      "plot(values$qm, seq(wtwqtrange[1], wtwqtrange[2], by=input$WtWqtStep", numTab, "), only='t')\n",
                                                    "})\n", 
                                                    "ggplot2::ggsave(outfile, width=11, height=6.5, dpi=100)\n",
                                                    "list(src=outfile, alt='Loading plot...', title='W: Distribution function of the waiting time in the system.\nWq: Distribution function of the waiting time in the queue.')}, deleteFile=TRUE)\n",
      "output$error", numTab, "<- renderUI({\n",
                                        "if (is.null(values$qm)) stop(values$error)\n",
                                         "return()\n",
                                  "})\n",
      "output$summaryDatatable", numTab, "<- shiny::renderDataTable({if (is.null(values$qm)) stop(values$error)\n",
                                                              "isolate({\n",
                                                                  "toDatatable(values$qm)\n",
                                                              "})\n",
                                                             "},escape=FALSE, options=list(jQueryUI=TRUE, dom='rt', searching=FALSE, ordering=FALSE, noFooter=NULL, mainTitle=isolate({class(values$qm)[1]})))\n",
      "output$pnDatatable", numTab, "<- shiny::renderDataTable({input$CalculateButton", input$results$total, "\n",
                                                        "if (is.null(values$qm)) stop(values$error)\n",
                                                        "isolate({\n",
                                                            "datatablePnQn(values$qm, range(input$PnQnMin", numTab, ", input$PnQnMax", numTab, "))\n",
                                                        "})\n",
                                        "}, escape=FALSE, options = list(jQueryUI=TRUE, searching=FALSE, ordering=FALSE, pagingType='full_numbers', pageLength=10, orderClasses = TRUE, noFooter=NULL))\n",
      
      "output$wtDatatable", numTab, "<- shiny::renderDataTable({input$CalculateButton", input$results$total, "\n",
                                                        "if (is.null(values$qm)) stop(values$error)\n",
                                                        "isolate({\n",
                                                           "datatableWtWqt(values$qm, range(input$WtWqtMin", numTab, ", input$WtWqtMax", numTab, "), input$WtWqtStep", numTab, ")\n",
                                                        "})\n",
                                        "}, escape=FALSE, options = list(jQueryUI=TRUE, searching=FALSE, ordering=FALSE, searching=FALSE, pagingType='full_numbers', pageLength=10, orderClasses = TRUE, noFooter=NULL))\n",
      sep="")))
}

#' Reactive function for the Network Output
#' @param expr Expresion to execute
#' @param env Enviroment
#' @param quoted Logical to indicate if the expresion is quoted or not
#' @param func Deprecated argument
renderNetwork <- function(expr, env=parent.frame(), quoted=FALSE, func=NULL) {
  if (!is.null(func)) {
    shinyDeprecated(msg = "renderUI: argument 'func' is deprecated. Please use 'expr' instead.")
  }
  else {
    func <- exprToFunction(expr, env, quoted)
  }
  function() {
    result <- func()
    if (is.null(result) || length(result) == 0) 
      return(NULL)
    return(result)
  }
}

#' @rdname loadUIModel
#' @method loadUIModel OpenJackson
#' @details
#' \code{loadUIModel.OpenJackson} implements the method for a OpenJackson model
loadUIModel.OpenJackson <- function(model, session, input, output, parameters=NULL) {
  numTab <- input$results$total
  updateTabInput(session, "results", list("add"),  list(newTab(model$name, generatePanel(session, input, model, parameters))), removeButton=TRUE)
  updateSaveReportInput(session, paste("saveReport", numTab, sep=""), action="setSections", sections=list("summary"="Summary", "combinedprobabilities" = "Combined Probabilities", "networkgraph"="Network Graph"))
  eval(parse(text=paste("updateTabInput(session, 'ModelOutputTabs", numTab , "', action=list('add'), value=list(newTab('Summary', '<div id=\"error", numTab, "\" class=\"shiny-html-output\"></div><div id=\"summaryDatatable", numTab, "\" class=\"shiny-mydatatable-output\"></div><div id=\"probDatatable", numTab, "\" class=\"shiny-mydatatable-output\"></div>')))\n", sep="")))
  eval(parse(text=paste("updateTabInput(session, 'ModelOutputTabs", numTab , "', action=list('add'), value=list(newTab('Network', '<div id=\"networkDiv", numTab, "\" class=\"shiny-network-output\"></div>')))\n", sep="")))  
  espChar <- "[\\\\]"
  values <- reactiveValues()
  
  eval(parse(text=paste("observe({\n",
                        "if (!is.null(reportData <- input$saveReport", numTab, ") && reportData$button > 0)\n",
                        "isolate({updateSaveReportInput(session, 'saveReport", numTab, "', action='saveReport', html=generateReport(reportData, values$qm))})\n",
                        "})", sep="")))
  skipfromfileupload <- FALSE
  eval(parse(text=paste("observe({\n",
                          "if (!is.null(input$numNodes", numTab, ") & !skipfromfileupload){\n",
                            "isolate({\n",
                              "numServers <- length(input$s", numTab, ")\n",
                              "if (input$numNodes", numTab, " >= numServers){\n",
                                 "updateVectorInput(session, 'lambda", numTab, "', value=c(input$lambda", numTab, ", rep(input$lambda", numTab, "[length(input$lambda", numTab,")], input$numNodes", numTab, " -numServers)))\n",
                                 "updateVectorInput(session, 'mu", numTab, "', value=c(input$mu", numTab, ", rep(input$mu", numTab, "[length(input$mu", numTab,")], input$numNodes", numTab, " - numServers)))\n",
                                 "updateVectorInput(session, 's", numTab, "', value=c(input$s", numTab, ", rep(input$s", numTab, "[length(input$s", numTab,")], input$numNodes", numTab, " - numServers)))\n",
                               "} else {\n",
                                 "updateVectorInput(session, 'lambda", numTab, "', value=input$lambda", numTab, "[-((input$numNodes", numTab, "+1):numServers)])\n",
                                 "updateVectorInput(session, 'mu", numTab, "', value=input$mu", numTab, "[-((input$numNodes", numTab, "+1):numServers)])\n",
                                 "updateVectorInput(session, 's", numTab, "', value=input$s", numTab, "[-((input$numNodes", numTab, "+1):numServers)])\n",                      
                               "}\n",
                              "updateNodesMatrixInput(session, 'p", numTab, "', input$numNodes", numTab, ")\n",
                        "})\n",
                        "} else\n",
                            "skipfromfileupload <<- FALSE\n",
                        "})\n", sep="")))
  
  eval(parse(text=paste(
    "observe({\n",
      "input$compute", numTab, "\n",
      "isolate({\n",
          "tryCatch({\n",
            "values$qm <- model$fun(", generateArguments(model$fun, numTab),")\n",
          "}, error=function(e) {\n",
              "values$qm <<- NULL\n",
              "values$error <<- e$message\n",
          "}, finally=updateButtonInput(session, 'compute", numTab, "', FALSE))\n",
       "})\n",
    "})\n", 
    "output$error", numTab, "<- renderUI({\n",
                                "if (is.null(values$qm)) stop(values$error)\n",
                                "return()\n",
                                "})\n",
    "output$summaryDatatable", numTab, "<- shiny::renderDataTable({\n",
                                        "if (is.null(values$qm)) stop(values$error)\n",
                                        "isolate({\n",
                                          "toDatatable(values$qm)\n",
                                        "})\n",
                                        "}, options=list(jQueryUI=TRUE, dom='rt', searching=FALSE, ordering=FALSE, noFooter=NULL, mainTitle=isolate({class(values$qm)[1]}), rowHeaders=TRUE))\n",
    "output$probDatatable", numTab, "<- shiny::renderDataTable({\n",
                                      "if (is.null(values$qm)) return()\n",
                                      "ns <- ''\n",
                                      "for (i in input$pn1nk", numTab, "[-length(input$pn1nk",numTab, ")])\n",
                                          "ns <- paste(ns, i, ', ', sep='')\n",
                                      "ns <- paste(ns, input$pn1nk", numTab, "[length(input$pn1nk", numTab, ")], sep='')\n",
                                      "data.frame(paste('P(', ns, ')', sep=''), Pn(values$qm, input$pn1nk", numTab, "))\n",
                                  "}, options=list(jQueryUI=TRUE, dom='rt', searching=FALSE, ordering=FALSE, noFooter=NULL, rowHeaders=TRUE, colnames=c('Combined Probabilities', 'Value')))\n",
    "output$networkDiv", numTab, "<- renderNetwork({\n",
                                        "if (is.null(values$qm)) stop(values$error)\n",
                                        "isolate({\n",
                                              "res <- list(type='Jackson', lambda=values$qm$lambda, mu=values$qm$mu, s=values$qm$s, p=values$qm$p,
                                                           l=values$qm$out$l, lq=values$qm$out$lq, w=values$qm$out$w, wq=values$qm$out$wq)\n",
                                         "})\n",
                                         "res\n",
                                     "})\n",
    #Si cambia el modelo, actualizamos los nodos disponibles en la lista y la etiqueta de las probabilidades conjuntas
    "observe({\n",
        "if (!is.null(values$qm)){\n",
          "isolate({updateSelectInput(session=session, inputId='nodeSelector", numTab, "', choices=c('----', as.character(1:length(values$qm$s))))\n",
                   "updateVectorInput(session=session, inputId='pn1nk", numTab, "', label=paste('Pn<sub>1</sub>...n<sub>', length(values$qm$s), '</sub>:', sep=''), value=1:length(values$qm$s))\n",
                   "updateNumericInput(session=session, inputId='p0i", numTab, "', max=length(values$qm$s))})\n",
        "} else {\n",
          "updateSelectInput(session, inputId='nodeSelector", numTab, "', choices=c('----'))\n",
        "}\n",
    "})\n",
    #Si se selecciona un nodo de la lista, cargamos una nueva pestaña con ese modelo
    "observe({\n",
        "selected <- input$nodeSelector", numTab, "\n",
        "isolate({\n",
            "if (!is.null(values$qm) && selected!='----'){\n",
              "qm <- values$qm$nodes[[as.numeric(selected)]]\n",
              "loadUIModel(uiList[[2]],session, input, output, parameters=c(lambda=rate(qm$arrivalDistribution), mu=rate(qm$serviceDistribution), s=qm$servers))\n",
        "}})\n",
    "})\n",
    #Si se sube un archivo cargamos sus datos
    "observe({\n",
        "if (!is.null(input$fileInput", numTab, ")){\n",
            "route <- gsub('", espChar, "', '/', input$fileInput", numTab, "['datapath'])\n",
            "inputdata <- read.table(route, fill=TRUE)\n",
            "numnodes <- ncol(inputdata)\n",
            "colnames(inputdata) <- as.character(1:numnodes)\n",
            "isolate({\n",
              "skipfromfileupload <<- TRUE\n",
              "updateNumericInput(session, 'numNodes", numTab, "', value=numnodes)\n",
              "updateVectorInput(session, 'lambda", numTab, "', value=as.numeric(inputdata[1,]))\n",
              "updateVectorInput(session, 'mu", numTab, "', value=as.numeric(inputdata[2,]))\n",
              "updateVectorInput(session, 's", numTab, "', value=as.numeric(inputdata[3,]))\n",
              "updateMatrixInput(session, 'p", numTab, "', value=inputdata[4:(3+numnodes),], size=numnodes)\n",
            "})\n",
         "}\n",
    "})\n",
     sep="")))
}

#' @rdname loadUIModel
#' @method loadUIModel ClosedJackson
#' @details
#' \code{loadUIModel.ClosedJackson} implements the method for a ClosedJackson model
loadUIModel.ClosedJackson <- function(model, session, input, output, parameters=NULL) {
  numTab <- input$results$total
  updateTabInput(session, "results", list("add"),  list(newTab(model$name, generatePanel(session, input, model, parameters))), removeButton=TRUE)
  eval(parse(text=paste("updateTabInput(session, 'ModelOutputTabs", numTab , "', action=list('add'), value=list(newTab('Summary', '<div id=\"error", numTab, "\" class=\"shiny-html-output\"></div><div id=\"summaryDatatable", numTab, "\" class=\"shiny-mydatatable-output\"></div><div id=\"probDataTable", numTab, "\" class=\"shiny-mydatatable-output\"></div><div id=\"pnDiv", numTab, "\" class=\"shiny-mydatatable-output\"></div>')))\n", sep="")))
  eval(parse(text=paste("updateTabInput(session, 'ModelOutputTabs", numTab , "', action=list('add'), value=list(newTab('Network', '<div id=\"networkDiv", numTab, "\" class=\"shiny-network-output\"></div>')))\n", sep="")))
  updateSaveReportInput(session, paste("saveReport", numTab, sep=""), action="setSections", sections=list("summary"="Summary", "combinedprobabilities" = "Combined Probabilities", "probabilities" = "Probabilities", "networkgraph"="Network Graph"))
  eval(parse(text=paste("observe({\n",
                        "updateNumericInput(session, 'PnMin", numTab, "', value=input$PnSlider", numTab,"$values[1])\n",
                        "updateNumericInput(session, 'PnMax", numTab, "', value=input$PnSlider", numTab,"$values[2])}, priority=2)", sep="")))
  
  values <- reactiveValues()
  
  eval(parse(text=paste("observe({\n",
                        "if (!is.null(reportData <- input$saveReport", numTab, ") && reportData$button > 0)\n",
                        "isolate({updateSaveReportInput(session, 'saveReport", numTab, "', action='saveReport', html=generateReport(reportData, values$qm))})\n",
                        "})", sep="")))
  
  skipfrominputfile <- FALSE
  eval(parse(text=paste("observe({\n",
                          "if (!is.null(input$numNodes", numTab, ") & !skipfrominputfile){\n",
                             "isolate({\n",
                                 "numServers <- length(input$s", numTab, ")\n",
                                 "if (input$numNodes", numTab, " >= numServers){\n",
                                    "updateVectorInput(session, 'mu", numTab, "', value=c(input$mu", numTab, ", rep(input$mu", numTab, "[length(input$mu", numTab,")], input$numNodes", numTab, " - numServers)))\n",
                                    "updateVectorInput(session, 's", numTab, "', value=c(input$s", numTab, ", rep(input$s", numTab, "[length(input$s", numTab,")], input$numNodes", numTab, " - numServers)))\n",
                                  "} else {\n",
                                    "updateVectorInput(session, 'mu", numTab, "', value=input$mu", numTab, "[-((input$numNodes", numTab, "+1):numServers)])\n",
                                    "updateVectorInput(session, 's", numTab, "', value=input$s", numTab, "[-((input$numNodes", numTab, "+1):numServers)])\n",                      
                                  "}\n",
                                "updateNodesMatrixInput(session, 'p", numTab, "', input$numNodes", numTab, ")\n",
                            "})\n",
                          "} else\n",
                              "skipfrominputfile <<- FALSE\n",
                        "})\n", sep="")))
  
  espChar <- "[\\\\]"
  eval(parse(text=paste(
    "observe({\n",
      "input$compute", numTab, "\n",
      "isolate({\n",
          "tryCatch({\n",
             "values$qm <- model$fun(", generateArguments(model$fun, numTab),")\n",
           "}, error=function(e) {\n",
               "values$qm <<- NULL\n",
               "values$error <<- e$message\n",
           "}, finally=updateButtonInput(session, 'compute", numTab, "', FALSE))\n",
        "})\n", 
      "})\n",
      "output$error", numTab, "<- renderUI({\n",
                                    "if (is.null(values$qm)) stop(values$error)\n",
                                    "return()\n",
                                  "})\n",
      "output$summaryDatatable", numTab, "<- shiny::renderDataTable({\n",
                              "if (is.null(values$qm)) return(data.frame())\n",
                              "isolate({\n",
                                  "toDatatable(values$qm)\n",
                              "})\n",
                              "}, options=list(jQueryUI=TRUE, dom='rt', searching=FALSE, ordering=FALSE, noFooter=NULL, mainTitle=isolate({class(values$qm)[1]}), rowHeaders=TRUE))\n",
      "output$probDataTable", numTab, "<- shiny::renderDataTable({\n",
                "if (is.null(values$qm)) stop(values$error)\n",
                "ns <- ''\n",
                "for (i in input$pn1nk", numTab, "[-length(input$pn1nk",numTab, ")])\n",
                    "ns <- paste(ns, i, ', ', sep='')\n",
                "ns <- paste(ns, input$pn1nk", numTab, "[length(input$pn1nk", numTab, ")], sep='')\n",
                "data.frame(paste('P(', ns, ')', sep=''), sprintf('%9.5g', Pn(values$qm, input$pn1nk", numTab, ")))\n",
               "}, options=list(jQueryUI=TRUE, dom='rt', searching=FALSE, ordering=FALSE, noFooter=NULL, rowHeaders=TRUE, colnames=c('Combined Probabilites', 'Value')))\n",
      "output$pnDiv", numTab, "<- shiny::renderDataTable({\n",
                                     "if (is.null(values$qm)) return(data.frame())\n",
                                     "if (input$CalculateButton", numTab, ">= 0){\n",
                                        "isolate({\n",
                                          "res <- data.frame()\n",
                                          "rangePn <- input$PnMin", numTab, ":input$PnMax", numTab, "\n",
                                          "for(i in 1:length(values$qm$s)){\n",
                                               "res <- rbind(res, data.frame(list(n=rangePn, Pn=Pi(values$qm, rangePn, i), node=rep(i, length(rangePn)))))\n",
                                          "}\n",
                                          "return(res)\n",
                                          "})\n",
                                     "}\n",
                                  "}, options = list(jQueryUI=TRUE, searching=FALSE, ordering=FALSE, pagingType='full_numbers', pageLength=10, orderClasses = TRUE))\n",
      "output$networkDiv", numTab, "<- renderNetwork({\n",
            "if (is.null(values$qm)) stop(values$error)\n",
             "isolate({\n",
                 "res <- list(type='Jackson', lambda=values$qm$lambda, mu=values$qm$mu, s=values$qm$s, p=values$qm$p,
                               l=values$qm$out$l, lq=values$qm$out$lq, w=values$qm$out$w, wq=values$qm$out$wq)\n",
              "})\n",
             "res\n",
             "})\n",
       #Si cambia el modelo, actualizamos la etiqueta de las probabilidades conjuntas
       "observe({\n",
           "if (!is.null(values$qm)){\n",
              "div <- floor(rep(values$qm$n/length(values$qm$servers), length(values$qm$servers)))\n",
              "res <- values$qm$n-sum(div)\n",
              "if (res > 0) {\n",
                "div[1:res] <- div[1:res]+1}\n",
              "isolate({updateVectorInput(session=session, inputId='pn1nk", numTab, "', label=paste('Pn<sub>1</sub>...n<sub>', length(values$qm$s), '</sub>:', sep=''), value=div)})\n",
            "} else {\n",
             "updateSelectInput(session, inputId='nodeSelector", numTab, "', choices=c('----'))\n",
           "}\n",
       "})\n",
    #Si se sube un archivo cargamos sus datos
    "observe({\n",
        "if (!is.null(input$fileInput", numTab, ")){\n",
            "route <- gsub('", espChar, "', '/', input$fileInput", numTab, "['datapath'])\n",
        "inputdata <- read.table(route, fill=TRUE)\n",
        "numnodes <- ncol(inputdata)\n",
        "colnames(inputdata) <- as.character(1:numnodes)\n",
        "isolate({\n",
           "skipfrominputfile <<-TRUE\n",
           "updateNumericInput(session, 'numNodes", numTab, "', value=numnodes)\n",
           "print(inputdata[3:(2+numnodes),])\n",
           "updateVectorInput(session, 'mu", numTab, "', value=as.numeric(inputdata[1,]))\n",
           "updateVectorInput(session, 's", numTab, "', value=as.numeric(inputdata[2,]))\n",
           "updateMatrixInput(session, 'p", numTab, "', value=inputdata[3:(2+numnodes),], size=numnodes)\n",
           "updateNumericInput(session, 'n", numTab, "', value=as.numeric(inputdata[nrow(inputdata),1]))\n",
        "})\n",
      "}\n",
    "})\n",
    sep="")))
}

#' @rdname loadUIModel
#' @method loadUIModel SimulatedModel
#' @details
#' \code{loadUIModel.SimulatedModel} implements the method for a Simulated Model
loadUIModel.SimulatedModel <- function(model, session, input, output, parameters=NULL) {
  numTab <- input$results$total
  updateTabInput(session, "results", list("add"),  list(newTab(model$name, generatePanel(session, input, model, parameters))), removeButton=TRUE)
  eval(parse(text=paste("nsim <- input$nsim", numTab, sep="")))
  eval(parse(text=paste("updateTabInput(session, 'ModelOutputTabs", numTab , "', action=list('add'), value=list(newTab('Convergence', '", selectInput(inputId=paste("convergenceSelector", input$results$total, sep=""), label=tags$b("Select a variable:"), choices=c("L"="L", "Lq"="Lq", "W"="W", "Wq"="Wq", "Clients"="Clients", "Intensity" = "Intensity")),"<input id=\"simulationInput", numTab, "\" class=\"myNumberInputBinding\" type=\"number\" value=1 min=1 max=", nsim," ></input><div id=\"errorConvergence", numTab, "\" class=\"shiny-html-output\"></div><div id=\"convergenceDiv", numTab, "\" class=\"shiny-image-output\"></div>')))\n", sep="")))
  eval(parse(text=paste("updateTabInput(session, 'ModelOutputTabs", numTab , "', action=list('add'), value=list(newTab('Summary', '<div id=\"summaryDatatable", numTab, "\" class=\"shiny-mydatatable-output\"></div>')))\n", sep="")))
  
  updateSaveReportInput(session, paste('saveReport', numTab, sep=""), disable=TRUE)
  updateSelectDistrInput(session=session, inputId=paste("arrivalDistribution", numTab, sep=""), distributions=selectDistr(WithNoArrivals=FALSE))
  updateSelectDistrInput(session, paste("serviceDistribution", numTab, sep=""), selectDistr(WithNoArrivals=FALSE))
  
  #updateNumericInput(session, paste("nproc", numTab, sep=""), value=cores, min=1)
  #updateNumericInput(session, paste("nsim", numTab, sep=""), value=cores, min=1)
  
  values <- reactiveValues()
  values$qm <- NULL
  values$error <- 'Click on the button Compute to iniciate the simulation'
  buttonvalue <- 0
  
  eval(parse(text=paste("observe({\n",
    "if (!is.null(reportData <- input$saveReport", numTab, ") && reportData$button > 0 && buttonvalue > 0)\n",
        "isolate({updateSaveReportInput(session, 'saveReport", numTab, "', action='saveReport', html=generateReport(reportData, values$qm))})\n",
    "})\n", sep="")))
  
  #updateButtonInput(session, paste("compute", numTab, sep=""), disabled = TRUE)
  eval(parse(text=paste("observe({\n",
                            "if (!is.null(input$compute", numTab, ")) {\n",
                              "if (input$compute", numTab, " > 0) {\n",
                                "buttonvalue <<- buttonvalue + 1\n",
                                "isolate({\n",
                                    "if (!input$historic", numTab, ")\n",
                                        "updateSaveReportInput(session, 'saveReport", numTab, "', action='setSections', sections=list('summary'='Summary'))\n",
                                    "else\n",
                                        "updateSaveReportInput(session, 'saveReport", numTab, "', action='setSections', sections=list('summary'='Summary', 'LEvolution'='Evolution of L', 'LqEvolution' = 'Evolution of Lq', 'WEvolution' = 'Evolution of W', 'WqEvolution'='Evolution of Wq', 'ClientsEvolution' = 'Evolution of Clients', 'IntensityEvolution'='Evolution of Intesity'))\n",
                                    "tryCatch({\n",
                                        "values$qm <- model$fun(", generateArguments(model$fun, numTab),")\n",
                                    "}, error=function(e) {\n",
                                         "values$qm <<- NULL\n",
                                          "values$error <<- e$message\n",
                                    "}, finally={\n",
                                        "updateButtonInput(session, 'compute", numTab, "', FALSE)\n",
                                        "updateSaveReportInput(session, 'saveReport", numTab, "', disable=FALSE)\n",
                                    "})\n",
                                 "})\n",
                                "}\n",
                             "}\n",
                        "})\n", 
                        "observe({\n",
                          "values$qm\n",                          
                          "output$summaryDatatable", numTab, "<- shiny::renderDataTable({\n",
                                  "if (is.null(values$qm) && buttonvalue==0) stop(values$error)\n",
                                  "if (is.null(values$qm) && buttonvalue> 0) stop(values$error)\n",
                                  "isolate({\n", 
                                      "toDatatable(combineSimulations(values$qm))\n",
                                  "})\n",
                          "}, options=list(jQueryUI=TRUE, dom='rt', searching=FALSE, ordering=FALSE, noFooter=NULL, mainTitle=isolate({if(class(values$qm)[1] == 'list'){class(values$qm[[1]])[1]}else{class(values$qm)[1]}}), rowHeaders=is.list(values$qm$out$l)))\n",   
                        "})\n",
                        "observe({\n",
                            "if (!is.null(selected <- input$convergenceSelector", numTab, ")) {\n",
                                "isolate({\n",
                                    "if (selected == 'Clients'){\n",
                                        "updateMyNumberInput(session, 'simulationInput",numTab,"', attr='style', attrValue='visibility:visible;', min=1, max=input$nsim", numTab, ", value=input$simulationInput", numTab, ")\n",
                                    "}\n",
                                    "else\n",
                                        "updateMyNumberInput(session, 'simulationInput", numTab, "', attr='style', attrValue='visibility:hidden;', value=input$simulationInput", numTab, ")\n",
                                "})\n",
                            "}\n",
                        "})\n",
                        "output$errorSummary", numTab, "<- output$errorConvergence", numTab, " <- renderUI({\n",
                          "if (is.null(values$qm) && buttonvalue == 0)\n",
                            "return(tagList(tags$h5(values$error)))\n",
                           "else if (is.null(values$qm))\n",
                               "stop(values$error)\n",
                           "return()\n",
                        "})\n",
                        "output$convergenceDiv", numTab, "<- renderImage({\n",
                              "if (is.null(values$qm)) stop()\n",
                              "input$CalculateButton", numTab, "\n",
                              "selected <- input$convergenceSelector", numTab, "\n",
                              "input$simulationInput", numTab, "\n",
                              "outfile <- tempfile(fileext='.svg')\n",
                              "isolate({\n",
                                "if(class(values$qm)[1] == 'list')\n",
                                    "maxrange <- values$qm[[1]]$staclients + values$qm[[1]]$nclients\n",
                                "else\n",
                                    "maxrange <- values$qm$staclients + values$qm$nclients\n",
                                "if (selected == 'Clients')\n",
                                    "plot(values$qm, 1, maxrange, selected, depth=input$depth", numTab, ", nSimulation=input$simulationInput", numTab, ")\n",
                                "else\n",
                                    "plot(values$qm, 1, maxrange, selected, depth=input$depth", numTab, ")\n",
                                "ggplot2::ggsave(outfile, width=11, height=6.5, dpi=100)\n",
                              "})\n",
                              "list(src=outfile, alt='Loading plot...', title='')
                        }, deleteFile=TRUE)\n", sep="")))
}

#' @rdname loadUIModel
#' @method loadUIModel Open
#' @details
#' \code{loadUIModel.Open} implements the method for a Simulated Open Network Model
loadUIModel.Open <- function(model, session, input, output, parameters=NULL) {
  numTab <- input$results$total
  updateTabInput(session, "results", list("add"),  list(newTab(model$name, generatePanel(session, input, model, parameters))), removeButton=TRUE)
  eval(parse(text=paste("updateTabInput(session, 'ModelOutputTabs", numTab , "', action=list('add'), value=list(newTab('Convergence', '", selectInput(inputId=paste("convergenceSelector", input$results$total, sep=""), label=tags$b("Select a variable:"), choices=c("L"="L", "Lq"="Lq", "W"="W", "Wq"="Wq")), "<div id=\"errorConvergence", numTab, "\" class=\"shiny-html-output\"></div><div id=\"convergenceDiv", numTab, "\" class=\"shiny-image-output\"></div>')))\n", sep="")))
  eval(parse(text=paste("updateTabInput(session, 'ModelOutputTabs", numTab , "', action=list('add'), value=list(newTab('Summary', '<div id=\"summaryDatatable", numTab, "\" class=\"shiny-mydatatable-output\"></div>')))\n", sep="")))
  eval(parse(text=paste("updateTabInput(session, 'ModelOutputTabs", numTab , "', action=list('add'), value=list(newTab('Network', '<div id=\"errorNetwork", numTab, "\" class=\"shiny-html-output\"></div><div id=\"networkDiv", numTab, "\" class=\"shiny-network-output\"></div>')))\n", sep="")))
  updateVectorSelectDistrInput(session, paste("serviceDistribution", numTab, sep=""), selectDistr(WithNoArrivals=FALSE))
  updateVectorSelectDistrInput(session, paste("arrivalDistribution", numTab, sep=""), selectDistr())
  updateSaveReportInput(session, paste('saveReport', numTab, sep=""), disable=TRUE)
  
 # updateNumericInput(session, paste("nproc", numTab, sep=""), value=detectCores(), min=1)
  values <- reactiveValues()
  buttonvalue <- 0
  values$qm <- NULL
  values$error <- 'Click on the button Compute to iniciate the simulation'
 
  eval(parse(text=paste("observe({\n",
                          "if (!is.null(reportData <- input$saveReport", numTab, ") && reportData$button > 0 && buttonvalue > 0)\n",
                              "isolate({updateSaveReportInput(session, 'saveReport", numTab, "', action='saveReport', html=generateReport(reportData, values$qm))})\n",
                        "})\n", sep="")))
 
  eval(parse(text=paste("observe({\n",
                            "if (!is.null(input$numNodes", numTab, ")){\n",
                              "isolate({\n",
                                  "updateNodesVectorSelectDistrInput(session, 'serviceDistribution", numTab, "', input$numNodes", numTab, ")\n",
                                  "updateNodesVectorSelectDistrInput(session, 'arrivalDistribution", numTab, "', input$numNodes", numTab, ")\n",
                                  "numServers <- length(input$s", numTab, ")\n",
                                  "if(numServers <= input$numNodes", numTab, ")\n",
                                    "updateVectorInput(session, 's", numTab, "', value=c(input$s", numTab, ", rep(input$s", numTab, "[length(input$s", numTab,")], input$numNodes", numTab, "-numServers)))\n",
                                  "else\n",
                                    "updateVectorInput(session, 's", numTab, "', value=input$s", numTab, "[-((input$numNodes", numTab, "+1):numServers)])\n",
                                  "updateNodesMatrixInput(session, 'p", numTab, "', input$numNodes", numTab, ")\n",
                              "})\n",
                             "}\n",
                        "})\n", sep="")))
 
  eval(parse(text=paste("observe({\n",
                          "if (!is.null(input$compute", numTab, ")) {\n",
                            "if (input$compute", numTab, " > 0) {\n",
                              "buttonvalue <<- buttonvalue + 1\n",
                               "if (!input$historic", numTab, ")\n",
                                 "updateSaveReportInput(session, 'saveReport", numTab, "', action='setSections', sections=list('summary'='Summary', 'networkgraph'='Network graph'))\n",
                               "else\n",
                                 "updateSaveReportInput(session, 'saveReport", numTab, "', action='setSections', sections=list('summary'='Summary', 'networkgraph'='Network graph', 'LEvolution'='Evolution of L', 'LqEvolution' = 'Evolution of Lq', 'WEvolution' = 'Evolution of W', 'WqEvolution'='Evolution of Wq'))\n",
                               "isolate({\n",
                                 "tryCatch({\n",
                                    "values$qm <- model$fun(", generateArguments(model$fun, numTab),")\n",
                                  "}, error=function(e) {\n",
                                    "values$qm <<- NULL\n",
                                    "values$error <<- e$message\n",
                                  "}, finally=updateButtonInput(session, 'compute", numTab, "', FALSE))\n",
                                "})\n",
                            "}\n",
                          "}\n",
                        "})\n",
                        "output$errorConvergence", numTab, "<- output$errorSummary", numTab, "<- output$errorNetwork", numTab, " <- renderUI({\n",
                           "if (is.null(values$qm) && buttonvalue==0)\n",
                              "return(tagList(tags$h5(values$error)))\n",
                           "else if (is.null(values$qm))\n",
                              "stop(values$error)\n",
                           "return()\n",
                        "})\n",
                        "observe({\n",
                           "values$qm\n",
                           "output$summaryDatatable", numTab, "<- shiny::renderDataTable({\n",
                                 "if (is.null(values$qm) && buttonvalue >= 0) stop(values$error)\n",
                                 "isolate({\n", 
                                   "toDatatable(combineSimulations(values$qm))\n",
                                 "})\n",
                           "}, options=list(jQueryUI=TRUE, dom='rtp', searching=FALSE, ordering=FALSE, noFooter=NULL, mainTitle=isolate({if(class(values$qm)[1] == 'list'){class(values$qm[[1]])[1]}else{class(values$qm)[1]}}), rowHeaders=TRUE))\n",   
                        "})\n",
                        "output$networkDiv", numTab, "<- renderNetwork({\n",
                          "if (is.null(values$qm) && buttonvalue==0) stop()\n",
                          "if (is.null(values$qm) && buttonvalue> 0) stop(values$error)\n",
                          "isolate({\n",
                             "qm <- combineSimulations(values$qm)\n",
                             "res <- list(type='Open', arrivaldistr=sapply(qm$arrivalDistribution, distrToText),
                                                       servicedistr=sapply(qm$serviceDistribution, distrToText), 
                                                       s=qm$s, p=qm$prob,
                                                       l=if(is.list(qm$out$l)){qm$out$l$mean}else{qm$out$l}, lq=if(is.list(qm$out$lq)){qm$out$lq$mean}else{qm$out$lq}, 
                                                       w=if(is.list(qm$out$w)){qm$out$w$mean}else{qm$out$w}, wq=if(is.list(qm$out$wq)){qm$out$wq$mean}else{qm$out$wq})\n",
                           "})\n",
                           "res\n",
                         "})\n",
                        "output$convergenceDiv", numTab, "<- renderImage({\n",
                          "if (is.null(values$qm) && buttonvalue==0) stop()\n",
                          "if (is.null(values$qm) && buttonvalue> 0) stop(values$error)\n",
                          "input$CalculateButton", numTab, "\n",
                          "selected <- input$convergenceSelector", numTab, "\n",
                          "outfile <- tempfile(fileext='.svg')\n",
                          "isolate({\n",
                            "plot(values$qm, 1, (values$qm[[1]]$staclients + values$qm[[1]]$transitions), selected, depth=input$depth", numTab, ", nSimulation=input$simulationInput", numTab, ")\n",
                            "ggplot2::ggsave(outfile, width=11, height=6.5, dpi=100)\n",
                          "})\n",
                          "list(src=outfile, alt='Loading plot...', title='')
                        }, deleteFile=TRUE)\n",
  sep="")))
}

#' @rdname loadUIModel
#' @method loadUIModel Closed
#' @details
#' \code{loadUIModel.Closed} implements the method for a Simulated Closed Network Model
loadUIModel.Closed <- function(model, session, input, output, parameters=NULL) {
  numTab <- input$results$total
  updateTabInput(session, "results", list("add"),  list(newTab(model$name, generatePanel(session, input, model, parameters))), removeButton=TRUE)
  eval(parse(text=paste("updateTabInput(session, 'ModelOutputTabs", numTab , "', action=list('add'), value=list(newTab('Convergence', '", selectInput(inputId=paste("convergenceSelector", input$results$total, sep=""), label=tags$b("Select a variable:"), choices=c("L"="L", "Lq"="Lq", "W"="W", "Wq"="Wq")),"<div id=\"errorConvergence", numTab, "\" class=\"shiny-html-output\"></div><div id=\"convergenceDiv", numTab, "\" class=\"shiny-image-output\"></div>')))\n", sep="")))
  eval(parse(text=paste("updateTabInput(session, 'ModelOutputTabs", numTab , "', action=list('add'), value=list(newTab('Summary', '<div id=\"summaryDatatable", numTab, "\" class=\"shiny-mydatatable-output\"></div>')))\n", sep="")))
  eval(parse(text=paste("updateTabInput(session, 'ModelOutputTabs", numTab , "', action=list('add'), value=list(newTab('Network', '<div id=\"errorNetwork", numTab, "\" class=\"shiny-html-output\"></div><div id=\"networkDiv", numTab, "\" class=\"shiny-network-output\"></div>')))\n", sep="")))
  updateVectorSelectDistrInput(session, paste("serviceDistribution", numTab, sep=""), selectDistr(WithNoArrivals=FALSE))
  updateSaveReportInput(session, paste('saveReport', numTab, sep=""), disable=TRUE)
  
  values <- reactiveValues()
  buttonvalue <- 0
  values$qm <- NULL
  values$error <- 'Click on the button Compute to iniciate the simulation'
  
  eval(parse(text=paste("observe({\n",
                        "if (!is.null(reportData <- input$saveReport", numTab, ") && reportData$button > 0 && buttonvalue > 0)\n",
                        "isolate({updateSaveReportInput(session, 'saveReport", numTab, "', action='saveReport', html=generateReport(reportData, values$qm))})\n",
                        "})\n", sep="")))
  
  eval(parse(text=paste("observe({\n",
                           "if (!is.null(input$numNodes", numTab, ")){\n",
                              "isolate({\n",
                                   "updateNodesVectorSelectDistrInput(session, 'serviceDistribution", numTab, "', input$numNodes", numTab, ")\n",
                                   "numServers <- length(input$s", numTab, ")\n",
                                   "if(numServers <= input$numNodes", numTab, ")\n",
                                      "updateVectorInput(session, 's", numTab, "', value=c(input$s", numTab, ", rep(input$s", numTab, "[length(input$s", numTab,")], input$numNodes", numTab, "-numServers)))\n",
                                   "else\n",
                                      "updateVectorInput(session, 's", numTab, "', value=input$s", numTab, "[-((input$numNodes", numTab, "+1):numServers)])\n",
                                   "updateNodesMatrixInput(session, 'p", numTab, "', input$numNodes", numTab, ")\n",
                                "})\n",
                            "}\n",
                        "})\n", sep="")))
  #updateNumericInput(session, paste("nproc", numTab, sep=""), value=detectCores(), min=1)
  eval(parse(text=paste("observe({\n",
                        "if (!is.null(input$compute", numTab, ")) {\n",
                          "if (input$compute", numTab, " > 0) {\n",
                             "buttonvalue <<- buttonvalue + 1\n",
                              "isolate({\n",
                                "if (!input$historic", numTab, ")\n",
                                  "updateSaveReportInput(session, 'saveReport", numTab, "', action='setSections', sections=list('summary'='Summary', 'networkgraph'='Network graph'))\n",
                                "else\n",
                                  "updateSaveReportInput(session, 'saveReport", numTab, "', action='setSections', sections=list('summary'='Summary', 'networkgraph'='Network graph', 'LEvolution'='Evolution of L', 'LqEvolution' = 'Evolution of Lq', 'WEvolution' = 'Evolution of W', 'WqEvolution'='Evolution of Wq'))\n",
                                "tryCatch({\n",
                                  "str(input$serviceDistribution", numTab, ")\n",
                                  "values$qm <- model$fun(", generateArguments(model$fun, numTab),")\n",
                                "}, error=function(e) {\n",
                                  "values$qm <<- NULL\n",
                                  "values$error <<- e$message\n",
                                "}, finally=updateButtonInput(session, 'compute", numTab, "', FALSE))\n",
                              "})\n",
                            "}\n",
                          "}\n",
                        "})\n",
                        "observe({\n",
                          "values$qm\n",
                          "output$summaryDatatable", numTab, "<- shiny::renderDataTable({\n",
                             "if (is.null(values$qm) && buttonvalue >= 0) stop(values$error)\n",
                             "isolate({\n", 
                               "toDatatable(combineSimulations(values$qm))\n",
                             "})\n",
                           "}, options=list(jQueryUI=TRUE, dom='rtp', searching=FALSE, ordering=FALSE, pagingType='full_numbers', pageLength=25, noFooter=NULL, mainTitle=isolate({if(class(values$qm)[1] == 'list'){class(values$qm[[1]])[1]}else{class(values$qm)[1]}}), rowHeaders=TRUE))\n",   
                        "})\n",
                        "output$networkDiv", numTab, "<- renderNetwork({\n",
                            "if (is.null(values$qm) && buttonvalue==0) stop()\n",
                            "if (is.null(values$qm) && buttonvalue> 0) stop(values$error)\n",
                            "isolate({\n",
                            "qm <- combineSimulations(values$qm)\n",
                            "res <- list(type='Closed', servicedistr=sapply(qm$serviceDistribution, distrToText), 
                                                      s=qm$s, p=qm$prob,
                                                      l=if(!is.null(qm$out$l$mean)){qm$out$l$mean}else{qm$out$l}, lq=if(!is.null(qm$out$lq$mean)){qm$out$lq$mean}else{qm$out$lq}, 
                                                      w=if(!is.null(qm$out$w$mean)){qm$out$w$mean}else{qm$out$w}, wq=if(!is.null(qm$out$wq$mean)){qm$out$wq$mean}else{qm$out$wq})\n",
                             "})\n",
                            "res\n",
                        "})\n",
                        "output$errorConvergence", numTab, "<- output$errorSummary", numTab, "<- output$errorNetwork", numTab, " <- renderUI({\n",
                          "if (is.null(values$qm) && buttonvalue==0)\n",
                            "return(tagList(tags$h5(values$error, sep='')))\n",
                          "else if (is.null(values$qm))\n",
                            "stop(values$error)\n",
                          "return()\n",
                        "})\n",
                        "output$convergenceDiv", numTab, "<- renderImage({\n",
                          "if (is.null(values$qm) && buttonvalue==0) stop()\n",
                          "if (is.null(values$qm) && buttonvalue> 0) stop(values$error)\n",
                          "input$CalculateButton", numTab, "\n",
                          "selected <- input$convergenceSelector", numTab, "\n",
                          "outfile <- tempfile(fileext='.svg')\n",
                          "isolate({\n",
                            "plot(values$qm, 1, (values$qm[[1]]$staclients + values$qm[[1]]$transitions), selected, depth=input$depth", numTab, ")\n",
                            "ggplot2::ggsave(outfile, width=11, height=6.5, dpi=100)\n",
                          "})\n",
                          "list(src=outfile, alt='Loading plot...', title='')
                        }, deleteFile=TRUE)\n", 
 sep="")))
}

loadUIModel.DataAnalysis <- function(model, session, input, output) {
  numTab <- input$results$total
  updateTabInput(session, "results", list("add"),  list(newTab("Data Analysis", generatePanel(session, input, model, parameters))), removeButton=TRUE)
  eval(parse(text=paste("updateTabInput(session, 'ModelOutputTabs", numTab , "', action=list('add'), value=list(newTab('Summary', '", "<div id=\"estimationsDatatable", numTab, "\" class=\"shiny-mydatatable-output\"></div><div id=\"summaryDatatable", numTab, "\" class=\"shiny-mydatatable-output\"></div>')))\n", sep="")))
  eval(parse(text=paste("updateTabInput(session, 'ModelOutputTabs", numTab , "', action=list('add'), value=list(newTab('Histogram', '", "<div id=\"histogramDiv", numTab, "\" class=\"shiny-image-output\"></div>')))\n", sep="")))
  eval(parse(text=paste("updateTabInput(session, 'ModelOutputTabs", numTab , "', action=list('add'), value=list(newTab('Cumulative Densities Plot', '", "<div id=\"cumulativeDiv", numTab, "\" class=\"shiny-image-output\"></div>')))\n", sep="")))
  eval(parse(text=paste("updateTabInput(session, 'ModelOutputTabs", numTab , "', action=list('add'), value=list(newTab('Q-Q Plot', '", "<div id=\"qqDiv", numTab, "\" class=\"shiny-image-output\"></div>')))\n", sep="")))
  updateVectorSelectDistrInput(session, paste('checkboxDistr', numTab, sep=""), as.list(eval(formals(fitData)$ldistr))) 
  values <- reactiveValues(fit =NULL, error="Upload a file with data to estimate an distribution.", buttons=list())
  espChar <- "[\\\\]"
  deleteDefaultValues <- function(text) {
    aux <- strsplit(text, split=c("[()]"))
    res <- paste(aux[[1]][1], "(", sep="")
    args <- aux[[1]][2]
    args <- strsplit(args, split=c(","))[[1]]
    for (i in args) {
      res <- paste(res, strsplit(i, split="=")[[1]][1], sep="")
      if (i == args[length(args)])
        res <- paste(res, ")", sep="")
      else
        res <- paste(res, ", ", sep="")
    }
    res
  }
  eval(parse(text=paste("observe({\n",
                          "if (!is.null(input$fileInput", numTab, ")){\n",
                            "tryCatch({\n",
                              "route <- gsub('", espChar, "', '/', input$fileInput", numTab, "['datapath'])\n",
                              "inputdata <- read.csv(route, dec='.', sep='', header=FALSE)\n",
                              "values$fit <- fitData(as.double(inputdata[,1]), input$checkboxDistr", numTab, ")\n",
                              "isolate({\n",
                                 "for (i in 1:length(values$fit))\n",
                                    "values$buttons[[length(values$buttons)+1]] <- paste('input$storeButton_", numTab, "_', i, sep='')\n",
                               "})\n",
                             "}, 
                              error=function(e) {\n",
                                    "values$fit <<- NULL\n",
                                    "values$error <<- e$message\n",
                              "})\n",
                            "}\n",
                        "})\n",
                        "output$summaryDatatable", numTab, "<- shiny::renderDataTable({\n",
                            "if (is.null(values$fit)) stop(values$error)\n",
                            "isolate({\n", 
                               "goodnessFit(values$fit)\n",
                            "})\n",
                          "}, options=list(jQueryUI=TRUE, dom='rt', searching=FALSE, ordering=FALSE, noFooter=NULL, mainTitle='Goodness of fit', rowHeaders=TRUE, colnames=c('Distributions/Statistics', 'Chi Square Statistic', 'Chi Square p-value', 'Kolmogorov-Smirnov Statistic', 'Kolmogorov-Smirnov p-value')))\n",   
                        "output$estimationsDatatable", numTab, "<- shiny::renderDataTable({\n",
                            "if (is.null(values$fit)) stop(values$error)\n",
                           #"print(input$ModelOutputTabs", numTab, "$selected)\n",
                            "isolate({\n", 
                              "valuescopy <- values$fit\n",
                              "if (!is.null(valuescopy$unif))\n",
                                  "names(valuescopy$unif$estimate) <- c('Min', 'Max')\n",
                              "dataFit <- toDatatable(valuescopy, ", numTab,")\n",
                            "})\n",
                        "}, options=list(jQueryUI=TRUE, dom='rt', searching=FALSE, ordering=FALSE, noFooter=NULL, colnames=c('Fitted distributions', ''), renderHTML=TRUE))\n", 
                        "output$cumulativeDiv", numTab, "<- renderImage({\n",
                            "if (is.null(values$fit)) stop(values$error)\n",
                            "outfile <- tempfile(fileext='.svg')\n",
                            "cdfcompggplot2(values$fit)\n",
                            "ggplot2::ggsave(outfile, width=11, height=6.5, dpi=100)\n",
                            "list(src=outfile, alt='Loading plot...', title='')
                        }, deleteFile=TRUE)\n", 
                        "output$histogramDiv", numTab, "<- renderImage({\n",
                          "if (is.null(values$fit)) stop(values$error)\n",
                          "outfile <- tempfile(fileext='.svg')\n",
                          "denscompggplot2(values$fit)\n",
                          "ggplot2::ggsave(outfile, width=11, height=6.5, dpi=100)\n",
                          "list(src=outfile, alt='Loading plot...', title='')
                        }, deleteFile=TRUE)\n", 
                        "output$qqDiv", numTab, "<- renderImage({\n",
                          "if (is.null(values$fit)) stop(values$error)\n",
                          "outfile <- tempfile(fileext='.svg')\n",
                          "qqcompggplot2(values$fit)\n",
                          "ggplot2::ggsave(outfile, width=11, height=6.5, dpi=100)\n",
                          "list(src=outfile, alt='Loading plot...', title='')
                        }, deleteFile=TRUE)\n", 
                        #Creamos los Observe para los botones de guardar las estimaciones
                        "observe({\n",
                            "for(i in values$buttons) {\n",
                                "aux <- paste('observe({\n', 
                                                            'value <- ', i, '$value\n',
                                                            'isolate({\n',
                                                                'distr <- ', i, '$distr\n',
                                                                'if (!is.null(distr) & !is.null(value)){\n',
                                                                  'if (value >= 1){\n',
                                                                      'argsDistr <- regmatches(distr, gregexpr(\"", '(?<=\\\\\\\\().*?(?=\\\\\\\\))' , "\", distr, perl=T))[[1]]\n',
                                                                      'if (!is.null(', i ,'$name)){\n',
                                                                        'registerDistribution(',
                                                                        'eval(parse(text=paste(\"function(\", argsDistr, \"){\n',
                                                                        'eval(parse(text=simpleCap(deleteDefaultValues(distr))))\n',
                                                                        '}\"))), ', i, '$name)\n',
                                                                      '}\n',
                                                                   '}\n',
                                                                '}\n',
                                                             '})\n',
                                                        '})\n', sep='')\n",
                                  "eval(parse(text=aux))\n",
                            "}\n",
                            "isolate({values$buttons <- list()})\n",
                        "})\n",
                        sep="")))
}

loadUIModel.Report <- function(model, session, input, output) {
  numTab <- input$results$total
  tryCatch({
      updateTabInput(session, "results", list("add"),  list(newTab("Reports", generatePanel(session, input, model, parameters))), removeButton=TRUE)
  }, finally = {
    
    eval(parse(text=paste("output$reportBodyOutput", numTab, " <- renderNetwork({list('numTab'=numTab, input$refresh", numTab, ")})\n",
                          "output$reportMenuOutput", numTab, " <- renderNetwork({list('numTab'=numTab, input$refresh", numTab, ")})\n",
                          "output$printAllOutput", numTab, " <- renderNetwork({if (!is.null(input$printAll", numTab, "))\n",
                                                                                  "isolate({\n",
                                                                                    "if (input$printAll", numTab, " > 0)\n",
                                                                                        "return(list('numTab'=numTab))\n",
                                                                                   "})\n",
                                                                               "})\n",
                          sep="")))
  })
}

#' Transforms a list into a numeric vector
#' @param x A list
#' @return A numeric vector
tonumeric <- function(x) {
    a <- c()
    for(i in 1:ncol(x))
      a <- c(a, x[[i]])
    return(a)
}

initMenu <- list(
  list(id = 0, title="Markovian models", submenu=generateMenu(uiList[sapply(sapply(uiList, class), function(v) {any(v=="MarkovianModel")}, simplify="array")])),
  list(id = 0, title="Simulated Models", submenu=generateMenu(uiList[sapply(sapply(uiList, class), function(v) {any(v=="SimulatedModel")}, simplify="array")])),
  list(id = length(uiList)+1, title="Data analysis", submenu=list()),
  list(id = length(uiList)+2, title="Reports", submenu=list())
)

dataAnalysisModel <- reportModel <- list()
class(dataAnalysisModel) <- "DataAnalysis"
class(reportModel) <- "Report"

firstTime <- TRUE
shinyServer(function(input, output, session) {
      options(shiny.usecairo=FALSE)
      if(firstTime) {
        #print("Inicializando Menu")
        isolate({
          updateMenuInput(session, "menu", action=list("set"), menu=initMenu)
          #'updateTabInput(session, "results", action=list("add"), value=list(newTab("Start", "Select a model in the menu at left to start.")), removeButton=TRUE)
        })
        firstTime <<- FALSE
      }
      observe({
        if (!is.null(input$menu$clicked)){
          isolate({
                  id <- as.numeric(input$menu$selected)
                  if (id == 0)
                      NULL
                  else if (id == length(uiList)+1)
                    loadUIModel(dataAnalysisModel, session, input, output)
                  else if (id == length(uiList)+2)
                    loadUIModel(reportModel, session, input, output)
                  else
                    loadUIModel(uiList[[id]], session, input, output)
          })
        }
      })
})