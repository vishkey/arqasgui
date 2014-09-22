library(distr)
library(reshape)
library(ggplot2)
library(doParallel)
library(foreach)
library(fitdistrplus)
library(gridExtra)
library(arqas)
library(arqasgui)

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
                           data.frame("_empty"="Error",
                                      "L"=sprintf("%5.4g", l$error), "Lq"=sprintf("%5.4g", lq$error), 
                                      "W"=sprintf("%5.4g", w$error), "Wq"=sprintf("%5.4g", wq$error), 
                                      "Intensity"=sprintf("%5.4g", rho$error), "Efficiency"=sprintf("%5.4g", eff$error))))
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
                data.frame("Mean"= 1:length(l$mean),
                           "L"=sprintf("%5.4g", l$mean), "Lq"=sprintf("%5.4g", lq$mean),
                           "W"=sprintf("%5.4g", w$mean), "Wq"=sprintf("%5.4g", wq$mean)))
    res <- rbind(res, with(qm$out,
                           data.frame("Mean"=c("Error", 1:length(l$mean)),
                                      "L"=c("", sprintf("%5.4g", l$error)), "Lq"=c("", sprintf("%5.4g", lq$error)),
                                      "W"=c("", sprintf("%5.4g", w$error)), "Wq"=c("", sprintf("%5.4g", wq$error)))))
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
  for(i in 1:(length(distrList)-cond)) {
     choicelist[[i]] <- list(name=distrList[[i]]$name, params=mapply(aux, as.list(names(formals(distrList[[i]]$fun))), formals(distrList[[i]]$fun), SIMPLIFY=FALSE))
  }
  return(choicelist)
}


distrToText <- function(d) {
  if (class(d)[1] == "no_distr") return("-")
  paramNames <- slotNames(param(d))
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

   res <- paste("{id:", distrList[[class(distrModel)[1]]]$id, ", params:[", sep="")
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
  
  argsnames <- names(args)
  inputs <- "<form class='pure-form-stacked arqas-form'><fieldset>"
  for(i in 1:length(model$args)) {
    label <- ifelse(is.null(l <- model$args[[i]]$label), argsnames[i], l)
    switch(model$args[[i]]$type,
        "numeric" = inputs <- paste(inputs, "<label for='", argsnames[i], input$results$total , "'>", simpleCap(label),":</label>\n<input id='", argsnames[i], input$results$total , "' type='number' min=0 value=", eval(args[[i]]) ," />\n", sep=""),
        "matrix" = inputs <- paste(inputs, "<label for='", argsnames[i], input$results$total, "'>", simpleCap(label),":</label><span id='", argsnames[i], input$results$total, "' ini-value='", matrixtostring(eval(args[[i]])) ,"' class='shiny-matrix-input'/><br>", sep=""),
        "vector" = inputs <- paste(inputs, "<label for='", argsnames[i], input$results$total, "'>", simpleCap(label), ":</label><input id='", argsnames[i], input$results$total, "' value='", vectortostring(eval(args[[i]])),"' class='shiny-vector-input'  /><br>", sep=""),
        "boolean" = inputs <- paste(inputs, "<label for='", argsnames[i], input$results$total, "'></label><input id='", argsnames[i], input$results$total, "' type='checkbox' ", ifelse(eval(args[[i]]), "checked", ""), ">  ", simpleCap(argsnames[i]), "</input><br>", sep=""),
        "distr" = inputs <- paste(inputs, "<label for='", argsnames[i], input$results$total, "'>", simpleCap(label), ":</label><div id='", argsnames[i], input$results$total, "' class='shiny-distr-input' defaultvalue=\"", distrDefaultValues(eval(args[[i]])), "\"></div><br>", sep=""),
        "vdistr" = inputs <- paste(inputs, "<label for='", argsnames[i], input$results$total, "'>", simpleCap(label), ":</label><div id='", argsnames[i], input$results$total, "' class='shiny-vdistr-input' defaultvalue=\"", vdistrDefaultValues(eval(args[[i]])), "\"></div><br>", sep="")
    )       
  }
  inputs <- paste(inputs, "<br></fieldset>", sep="")
  if(any(class(model) == "Network")) {
    #inputs <- paste(inputs, tagList(tags$label("for"=paste("fileInput", input$results$total, sep=""), "File"), tags$input(id=paste("fileInput", input$results$total, sep=""), type="file", multiple=FALSE, title=" "), tags$br(), tags$br()), sep="")
    inputs <- paste(inputs, tagList(tags$input(id=paste("fileInput", input$results$total, sep=""), type="file", multiple=FALSE, title=" ", style="display:none;"),
                                     tags$input(id=paste("fileInput", input$results$total, "button", sep=""), type="button", class="btn btn-info", value="Upload data file", onclick=paste("document.getElementById('fileInput",input$results$total,"').click();", sep="")), tags$br(), tags$br()))
  }
  mclass <- class(model)
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
#' @param value Value of the matrix
#' @param size Number of colums or rows. Expect square matrix.
updateMatrixInput <- function(session, inputId, value=NULL, size=NULL) {
  message <- list(value=value, size=size)
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

updateCheckboxDistrInput <- function(session, inputId, distributions=NULL) {
  message <- list(distributions=distributions)
  session$sendInputMessage(inputId, message)
}

updateButtonInput <- function(session, inputId, disabled=NULL) {
  message <- list(disabled=disabled)
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
                   "<button id='CalculateButton", input$results$total, "' type='button' class='btn btn-primary shiny-jquerybutton-input'>Compute</button>",
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
               "<button id='CalculateButton", input$results$total, "' type='button' class='btn btn-primary shiny-jquerybutton-input'>Compute</button><br>",
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
                 tags$input(id=paste("pn1nk", input$results$total, sep=""), value=vectortostring(1:length(defaultModel$servers)) ,class="shiny-vector-input")
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
                 tags$button(id=paste("CalculateButton", input$results$total, sep=""), class="btn btn-primary shiny-jquerybutton-input", "Compute")))
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

#' Generates a Distr object from the distrList.
#' @param distrInput The selected distribution
generateSimpleDistr <- function(distrInput) {
  lastid <- length(distrList)
  name <- distrList[[distrInput$distribution]]$name
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
  print(res)
  return(paste("distrList[[", distrInput$distribution, "]]$fun", res, sep=""))
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
  wqs <- ifelse(is.na(wqs) || is.infinite(wqs), 0, wqs)
  return(data.frame("t"=ranges, "W"=wts, "Wq"= wqs))
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
                                                     "summaryPnQn(values$qm, seq(pnqnrange[1], pnqnrange[2], 1))\n",
                                                     "ggsave(outfile, width=11, height=6.5, dpi=100)\n",
                                                     "title <- 'Pn: Steady-state probability of having n customers in the system.\n'\n",
                                                     "try({Qn(values$qm, 0); title <- paste(title, 'Qn: Steady-state probability of finding n customers in the system when a new customer arrives.', sep='')})\n",
                                                     "list(src=outfile, alt='Loading plot...', title=title)}, deleteFile=TRUE)\n",
      "output$waitplotDiv", numTab, "<- renderImage({outfile <- tempfile(fileext='.svg')\n",
                                                    "input$CalculateButton", input$results$total, "\n",
                                                    "if(is.null(values$qm)) stop(values$error)\n",
                                                    "isolate({\n",
                                                      "wtwqtrange <- range(input$WtWqtMin", numTab, ", input$WtWqtMax", numTab, ")\n",
                                                      "summaryWtWqt(values$qm, seq(wtwqtrange[1], wtwqtrange[2], by=input$WtWqtStep", numTab, "))\n",
                                                    "})\n", 
                                                    "ggsave(outfile, width=11, height=6.5, dpi=100)\n",
                                                    "list(src=outfile, alt='Loading plot...', title='W: Distribution function of the waiting time in the system.\nWq: Distribution function of the waiting time in the queue.')}, deleteFile=TRUE)\n",
      "output$error", numTab, "<- renderUI({\n",
                                        "if (is.null(values$qm)) stop(values$error)\n",
                                         "return()\n",
                                  "})\n",
      "output$summaryDatatable", numTab, "<- renderDataTable({if (is.null(values$qm)) stop(values$error)\n",
                                                              "isolate({\n",
                                                                  "toDatatable(values$qm)\n",
                                                              "})\n",
                                                             "}, options=list(bJQueryUI=TRUE, sDom='rt', bFilter=0, bSort=0, noFooter=NULL, mainTitle=isolate({class(values$qm)[1]})))\n",
      "output$pnDatatable", numTab, "<- renderDataTable({input$CalculateButton", input$results$total, "\n",
                                                        "if (is.null(values$qm)) stop(values$error)\n",
                                                        "isolate({\n",
                                                            "datatablePnQn(values$qm, range(input$PnQnMin", numTab, ", input$PnQnMax", numTab, "))\n",
                                                        "})\n",
                                        "}, options = list(bJQueryUI=TRUE, sPaginationType='full_numbers', iDisplayLength=10, bSortClasses = TRUE, noFooter=NULL))\n",
      
      "output$wtDatatable", numTab, "<- renderDataTable({input$CalculateButton", input$results$total, "\n",
                                                        "if (is.null(values$qm)) stop(values$error)\n",
                                                        "isolate({\n",
                                                           "datatableWtWqt(values$qm, range(input$WtWqtMin", numTab, ", input$WtWqtMax", numTab, "), input$WtWqtStep", numTab, ")\n",
                                                        "})\n",
                                        "}, options = list(bJQueryUI=TRUE, sPaginationType='full_numbers', iDisplayLength=10, bSortClasses = TRUE, noFooter=NULL))",
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
  eval(parse(text=paste("updateTabInput(session, 'ModelOutputTabs", numTab , "', action=list('add'), value=list(newTab('Summary', '<div id=\"error", numTab, "\" class=\"shiny-html-output\"></div><div id=\"summaryDatatable", numTab, "\" class=\"shiny-mydatatable-output\"></div><div id=\"probDatatable", numTab, "\" class=\"shiny-mydatatable-output\"></div>')))\n", sep="")))
  eval(parse(text=paste("updateTabInput(session, 'ModelOutputTabs", numTab , "', action=list('add'), value=list(newTab('Network', '<div id=\"networkDiv", numTab, "\" class=\"shiny-network-output\"></div>')))\n", sep="")))  
  espChar <- "[\\\\]"
  values <- reactiveValues()
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
    "output$summaryDatatable", numTab, "<- renderDataTable({\n",
                                        "if (is.null(values$qm)) stop(values$error)\n",
                                        "isolate({\n",
                                          "toDatatable(values$qm)\n",
                                        "})\n",
                                        "}, options=list(bJQueryUI=TRUE, sDom='rt', bFilter=0, bSort=0, noFooter=NULL, mainTitle=isolate({class(values$qm)[1]}), rowHeaders=TRUE))\n",
    "output$probDatatable", numTab, "<- renderDataTable({\n",
                                      "if (is.null(values$qm)) return()\n",
                                      "ns <- ''\n",
                                      "for (i in input$pn1nk", numTab, "[-length(input$pn1nk",numTab, ")])\n",
                                          "ns <- paste(ns, i, ', ', sep='')\n",
                                      "ns <- paste(ns, input$pn1nk", numTab, "[length(input$pn1nk", numTab, ")], sep='')\n",
                                      "data.frame(paste('P(', ns, ')', sep=''), Pn(values$qm, input$pn1nk", numTab, "))\n",
                                  "}, options=list(bJQueryUI=TRUE, sDom='rt', bFilter=0, bSort=0, noFooter=NULL, rowHeaders=TRUE, colnames=c('Combined Probabilities', 'Value')))\n",
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
            "isolate({if(class(values$qm)[1] == 'OpenJackson') {\n",
              "updateVectorInput(session, 'lambda", numTab, "', value=as.numeric(inputdata[1,]))\n",
              "updateVectorInput(session, 'mu", numTab, "', value=as.numeric(inputdata[2,]))\n",
              "updateVectorInput(session, 's", numTab, "', value=as.numeric(inputdata[3,]))\n",
              "updateMatrixInput(session, 'p", numTab, "', value=inputdata[4:(3+numnodes),], size=numnodes)\n",
             "}\n",
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
  
  eval(parse(text=paste("observe({\n",
                        "updateNumericInput(session, 'PnMin", numTab, "', value=input$PnSlider", numTab,"$values[1])\n",
                        "updateNumericInput(session, 'PnMax", numTab, "', value=input$PnSlider", numTab,"$values[2])}, priority=2)", sep="")))
  
  values <- reactiveValues()
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
      "output$summaryDatatable", numTab, "<- renderDataTable({\n",
                              "if (is.null(values$qm)) return(data.frame())\n",
                              "isolate({\n",
                                  "toDatatable(values$qm)\n",
                              "})\n",
                              "}, options=list(bJQueryUI=TRUE, sDom='rt', bFilter=0, bSort=0, noFooter=NULL, mainTitle=isolate({class(values$qm)[1]}), rowHeaders=TRUE))\n",
      "output$probDataTable", numTab, "<- renderDataTable({\n",
                "if (is.null(values$qm)) stop(values$error)\n",
                "ns <- ''\n",
                "for (i in input$pn1nk", numTab, "[-length(input$pn1nk",numTab, ")])\n",
                    "ns <- paste(ns, i, ', ', sep='')\n",
                "ns <- paste(ns, input$pn1nk", numTab, "[length(input$pn1nk", numTab, ")], sep='')\n",
                "data.frame(paste('P(', ns, ')', sep=''), sprintf('%9.5g', Pn(values$qm, input$pn1nk", numTab, ")))\n",
               "}, options=list(bJQueryUI=TRUE, sDom='rt', bFilter=0, bSort=0, noFooter=NULL, rowHeaders=TRUE, colnames=c('Combined Probabilites', 'Value')))\n",
      "output$pnDiv", numTab, "<- renderDataTable({\n",
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
                                  "}, options = list(bJQueryUI=TRUE, sPaginationType='full_numbers', iDisplayLength=10, bSortClasses = TRUE))\n",
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
  eval(parse(text=paste("updateTabInput(session, 'ModelOutputTabs", numTab , "', action=list('add'), value=list(newTab('Convergence', '", selectInput(inputId=paste("convergenceSelector", input$results$total, sep=""), label=tags$b("Select a variable:"), choices=c("L"="L", "Lq"="Lq", "W"="W", "Wq"="Wq", "Clients"="Clients", "Intensity" = "Intensity")), "<div id=\"errorConvergence", numTab, "\" class=\"shiny-html-output\"></div><div id=\"convergenceDiv", numTab, "\" class=\"shiny-image-output\"></div>')))\n", sep="")))
  eval(parse(text=paste("updateTabInput(session, 'ModelOutputTabs", numTab , "', action=list('add'), value=list(newTab('Summary', '<div id=\"errorSummary", numTab, "\" class=\"shiny-html-output\"></div><div id=\"summaryDatatable", numTab, "\" class=\"shiny-mydatatable-output\"></div>')))\n", sep="")))
  
  updateSelectDistrInput(session=session, inputId=paste("arrivalDistribution", numTab, sep=""), distributions=selectDistr(WithNoArrivals=FALSE))
  updateSelectDistrInput(session, paste("serviceDistribution", numTab, sep=""), selectDistr(WithNoArrivals=FALSE))
  
  #updateNumericInput(session, paste("nproc", numTab, sep=""), value=cores, min=1)
  #updateNumericInput(session, paste("nsim", numTab, sep=""), value=cores, min=1)
  
  values <- reactiveValues()
  values$qm <- NULL
  values$error <- 'Click on the button Compute to iniciate the simulation'
  buttonvalue <- 0
  #updateButtonInput(session, paste("compute", numTab, sep=""), disabled = TRUE)
  eval(parse(text=paste("observe({\n",
                            "if (!is.null(input$compute", numTab, ")) {\n",
                              "if (input$compute", numTab, " > 0) {\n",
                                "buttonvalue <<- buttonvalue + 1\n",
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
                        "observe({\n",
                          "values$qm\n",
                          "output$summaryDatatable", numTab, "<- renderDataTable({\n",
                                  "if (is.null(values$qm) && buttonvalue==0) stop()\n",
                                  "if (is.null(values$qm) && buttonvalue> 0) stop(values$error)\n",
                                  "isolate({\n", 
                                      "toDatatable(combineSimulations(values$qm))\n",
                                  "})\n",
                          "}, options=list(bJQueryUI=TRUE, sDom='rt', bFilter=0, bSort=0, noFooter=NULL, mainTitle=isolate({if(class(values$qm)[1] == 'list'){class(values$qm[[1]])[1]}else{class(values$qm)[1]}}), rowHeaders=is.list(values$qm$out$l)))\n",   
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
                              "outfile <- tempfile(fileext='.svg')\n",
                              "isolate({\n",
                                "if(class(values$qm)[1] == 'list')\n",
                                    "maxrange <- values$qm[[1]]$Staclients + values$qm[[1]]$Nclients\n",
                                "else\n",
                                    "maxrange <- values$qm$Staclients + values$qm$Nclients\n",
                                "summarySimple(values$qm, 1, maxrange, selected, depth=input$depth", numTab, ")\n",
                                "ggsave(outfile, width=11, height=6.5, dpi=100)\n",
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
  eval(parse(text=paste("updateTabInput(session, 'ModelOutputTabs", numTab , "', action=list('add'), value=list(newTab('Convergence', '", selectInput(inputId=paste("convergenceSelector", input$results$total, sep=""), label=tags$b("Select a variable:"), choices=c("L"="L", "Lq"="Lq", "W"="W", "Wq"="Wq", "Clients"="Clients", "Intensity" = "Intensity")),"<div id=\"errorConvergence", numTab, "\" class=\"shiny-html-output\"></div><div id=\"convergenceDiv", numTab, "\" class=\"shiny-image-output\"></div>')))\n", sep="")))
  eval(parse(text=paste("updateTabInput(session, 'ModelOutputTabs", numTab , "', action=list('add'), value=list(newTab('Summary', '<div id=\"errorSummary", numTab, "\" class=\"shiny-html-output\"></div><div id=\"summaryDatatable", numTab, "\" class=\"shiny-mydatatable-output\"></div>')))\n", sep="")))
  eval(parse(text=paste("updateTabInput(session, 'ModelOutputTabs", numTab , "', action=list('add'), value=list(newTab('Network', '<div id=\"errorNetwork", numTab, "\" class=\"shiny-html-output\"></div><div id=\"networkDiv", numTab, "\" class=\"shiny-network-output\"></div>')))\n", sep="")))
  updateVectorSelectDistrInput(session, paste("serviceDistribution", numTab, sep=""), selectDistr(WithNoArrivals=FALSE))
  updateVectorSelectDistrInput(session, paste("arrivalDistribution", numTab, sep=""), selectDistr())
 # updateNumericInput(session, paste("nproc", numTab, sep=""), value=detectCores(), min=1)
  values <- reactiveValues()
  buttonvalue <- 0
  values$qm <- NULL
  values$error <- 'Click on the button Compute to iniciate the simulation'
  eval(parse(text=paste("observe({\n",
                          "if (!is.null(input$compute", numTab, ")) {\n",
                            "if (input$compute", numTab, " > 0) {\n",
                              "buttonvalue <<- buttonvalue + 1\n",
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
                           "output$summaryDatatable", numTab, "<- renderDataTable({\n",
                                 "if (is.null(values$qm) && buttonvalue==0) stop()\n",
                                 "if (is.null(values$qm) && buttonvalue> 0) stop(values$error)\n",
                                 "isolate({\n", 
                                   "toDatatable(combineSimulations(values$qm))\n",
                                 "})\n",
                           "}, options=list(bJQueryUI=TRUE, sDom='rt', bFilter=0, bSort=0, noFooter=NULL, mainTitle=isolate({if(class(values$qm)[1] == 'list'){class(values$qm[[1]])[1]}else{class(values$qm)[1]}}), rowHeaders=TRUE))\n",   
                        "})\n",
                        "output$networkDiv", numTab, "<- renderNetwork({\n",
                          "if (is.null(values$qm) && buttonvalue==0) stop()\n",
                          "if (is.null(values$qm) && buttonvalue> 0) stop(values$error)\n",
                          "isolate({\n",
                             "qm <- combineSimulations(values$qm)\n",
                             "res <- list(type='Open', arrivaldistr=sapply(qm$arrivalDistribution, distrToText),
                                                       servicedistr=sapply(qm$serviceDistribution, distrToText), 
                                                       s=qm$Servs, p=qm$Prob,
                                                       l=if(!is.null(qm$out$l$mean)){qm$out$l$mean}else{qm$out$l}, lq=if(!is.null(qm$out$lq$mean)){qm$out$lq$mean}else{qm$out$lq}, 
                                                       w=if(!is.null(qm$out$w$mean)){qm$out$w$mean}else{qm$out$w}, wq=if(!is.null(qm$out$wq$mean)){qm$out$wq$mean}else{qm$out$wq})\n",
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
                            "summarySimple(values$qm, 1, (values$qm[[1]]$Staclients + values$qm[[1]]$Transitions), selected, depth=input$depth", numTab, ")\n",
                            "ggsave(outfile, width=11, height=6.5, dpi=100)\n",
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
  eval(parse(text=paste("updateTabInput(session, 'ModelOutputTabs", numTab , "', action=list('add'), value=list(newTab('Convergence', '", selectInput(inputId=paste("convergenceSelector", input$results$total, sep=""), label=tags$b("Select a variable:"), choices=c("L"="L", "Lq"="Lq", "W"="W", "Wq"="Wq", "Clients"="Clients", "Intensity" = "Intensity")),"<div id=\"errorConvergence", numTab, "\" class=\"shiny-html-output\"></div><div id=\"convergenceDiv", numTab, "\" class=\"shiny-image-output\"></div>')))\n", sep="")))
  eval(parse(text=paste("updateTabInput(session, 'ModelOutputTabs", numTab , "', action=list('add'), value=list(newTab('Summary', '<div id=\"errorSummary", numTab, "\" class=\"shiny-html-output\"></div><div id=\"summaryDatatable", numTab, "\" class=\"shiny-mydatatable-output\"></div>')))\n", sep="")))
  eval(parse(text=paste("updateTabInput(session, 'ModelOutputTabs", numTab , "', action=list('add'), value=list(newTab('Network', '<div id=\"errorNetwork", numTab, "\" class=\"shiny-html-output\"></div><div id=\"networkDiv", numTab, "\" class=\"shiny-network-output\"></div>')))\n", sep="")))
  updateVectorSelectDistrInput(session, paste("serviceDistribution", numTab, sep=""), selectDistr(WithNoArrivals=FALSE))
  values <- reactiveValues()
  buttonvalue <- 0
  values$qm <- NULL
  values$error <- 'Click on the button Compute to iniciate the simulation'
  #updateNumericInput(session, paste("nproc", numTab, sep=""), value=detectCores(), min=1)
  eval(parse(text=paste("observe({\n",
                        "if (!is.null(input$compute", numTab, ")) {\n",
                          "if (input$compute", numTab, " > 0) {\n",
                             "buttonvalue <<- buttonvalue + 1\n",
                              "isolate({\n",
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
                          "output$summaryDatatable", numTab, "<- renderDataTable({\n",
                             "if (is.null(values$qm) && buttonvalue==0) stop()\n",
                             "if (is.null(values$qm) && buttonvalue> 0) stop(values$error)\n",
                             "isolate({\n", 
                               "toDatatable(combineSimulations(values$qm))\n",
                             "})\n",
                           "}, options=list(bJQueryUI=TRUE, sDom='rt', bFilter=0, bSort=0, noFooter=NULL, mainTitle=isolate({if(class(values$qm)[1] == 'list'){class(values$qm[[1]])[1]}else{class(values$qm)[1]}}), rowHeaders=TRUE))\n",   
                        "})\n",
                        "output$networkDiv", numTab, "<- renderNetwork({\n",
                            "if (is.null(values$qm) && buttonvalue==0) stop()\n",
                            "if (is.null(values$qm) && buttonvalue> 0) stop(values$error)\n",
                            "isolate({\n",
                            "qm <- combineSimulations(values$qm)\n",
                            "res <- list(type='Closed', servicedistr=sapply(qm$serviceDistribution, distrToText), 
                                                      s=qm$Servs, p=qm$Prob,
                                                      l=if(!is.null(qm$out$l$mean)){qm$out$l$mean}else{qm$out$l}, lq=if(!is.null(qm$out$lq$mean)){qm$out$lq$mean}else{qm$out$lq}, 
                                                      w=if(!is.null(qm$out$w$mean)){qm$out$w$mean}else{qm$out$w}, wq=if(!is.null(qm$out$wq$mean)){qm$out$wq$mean}else{qm$out$wq})\n",
                             "})\n",
                            "res\n",
                        "})\n",
                        "output$errorConvergence", numTab, "<- output$errorSummary", numTab, "<- output$errorNetwork", numTab, " <- renderUI({\n",
                          "if (is.null(values$qm) && buttonvalue==0)\n",
                            "return(tagList(tags$h5(values$error, ':a', sep='')))\n",
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
                            "summarySimple(values$qm, 1, (values$qm[[1]]$Staclients + values$qm[[1]]$Transitions), selected, depth=input$depth", numTab, ")\n",
                            "ggsave(outfile, width=11, height=6.5, dpi=100)\n",
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
                        "output$summaryDatatable", numTab, "<- renderDataTable({\n",
                            "if (is.null(values$fit)) stop(values$error)\n",
                            "isolate({\n", 
                               "goodnessFit(values$fit)\n",
                            "})\n",
                          "}, options=list(bJQueryUI=TRUE, sDom='rt', bFilter=0, bSort=0, noFooter=NULL, mainTitle='Goodness of fit', rowHeaders=TRUE, colnames=c('Distributions/Statistics', 'Chi Square Statistic', 'Chi Square p-value', 'Kolmogorov-Smirnov Statistic', 'Kolmogorov-Smirnov p-value')))\n",   
                        "output$estimationsDatatable", numTab, "<- renderDataTable({\n",
                            "if (is.null(values$fit)) stop(values$error)\n",
                           #"print(input$ModelOutputTabs", numTab, "$selected)\n",
                            "isolate({\n", 
                                 #Creamos los Observe para los botones de guardar las estimacions
                              "valuescopy <- values$fit\n",
                              "if (!is.null(valuescopy$unif))\n",
                                  "names(valuescopy$unif$estimate) <- c('Min', 'Max')\n",
                              "dataFit <- toDatatable(valuescopy, ", numTab,")\n",
                            "})\n",
                        "}, options=list(bJQueryUI=TRUE, sDom='rt', bFilter=0, bSort=0, noFooter=NULL, colnames=c('Fitted distributions', '')))\n", 
                        "output$cumulativeDiv", numTab, "<- renderImage({\n",
                            "if (is.null(values$fit)) stop(values$error)\n",
                            "outfile <- tempfile(fileext='.svg')\n",
                            "cdfcompggplot2(values$fit)\n",
                            "ggsave(outfile, width=11, height=6.5, dpi=100)\n",
                            "list(src=outfile, alt='Loading plot...', title='')
                        }, deleteFile=TRUE)\n", 
                        "output$histogramDiv", numTab, "<- renderImage({\n",
                          "if (is.null(values$fit)) stop(values$error)\n",
                          "outfile <- tempfile(fileext='.svg')\n",
                          "denscompggplot2(values$fit)\n",
                          "ggsave(outfile, width=11, height=6.5, dpi=100)\n",
                          "list(src=outfile, alt='Loading plot...', title='')
                        }, deleteFile=TRUE)\n", 
                        "output$qqDiv", numTab, "<- renderImage({\n",
                          "if (is.null(values$fit)) stop(values$error)\n",
                          "outfile <- tempfile(fileext='.svg')\n",
                          "qqcompggplot2(values$fit)\n",
                          "ggsave(outfile, width=11, height=6.5, dpi=100)\n",
                          "list(src=outfile, alt='Loading plot...', title='')
                        }, deleteFile=TRUE)\n", 
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

#' Transforms a list into a numeric vector
#' @param x A list
#' @return A numeric vector
tonumeric <- function(x) {
    a <- c()
    for(i in 1:ncol(x))
      a <- c(a, x[[i]])
    return(a)
}
initialize <- TRUE
shinyServer(function(input, output, session) {
  if (initialize) {
    options(shiny.usecairo=FALSE)
    initMenu <- list(
                  list(id = 0, title="Markovian models", submenu=generateMenu(uiList[sapply(sapply(uiList, class), function(v) {any(v=="MarkovianModel")}, simplify="array")])),
                  list(id = 0, title="Simulated Models", submenu=generateMenu(uiList[sapply(sapply(uiList, class), function(v) {any(v=="SimulatedModel")}, simplify="array")])),
                  list(id = length(uiList)+1, title="Data analysis", submenu=list())
                )
    isolate({
      updateMenuInput(session, "menu", action=list("set"), menu=initMenu)
      updateTabInput(session, "results", action=list("add"), value=list(newTab("Start", "Select a model in the menu at left to start.")), removeButton=TRUE)
    })
    dataAnalysisModel <- list()
    class(dataAnalysisModel) <- "DataAnalysis"
    observe({
      if (!is.null(input$menu$clicked)){
        isolate({
                id <- as.numeric(input$menu$selected)
                 switch(as.character(input$menu$selected),
                        "0" = NULL, 
                        loadUIModel(uiList[[id]], session, input, output),
                        "21" = loadUIModel(dataAnalysisModel, session, input, output)
                 )
        })
      }
    })
    initialize <<- FALSE
  }
})