#' Returns a HTML visualization of the data for a queue model
#' @param qm Queue model
toHTML <- function(qm) {UseMethod("toHTML", qm)}

#' @rdname toHTML
#' @method toHTML SimulatedModel
#' @details
#' \code{toHTML.SimulatedModel} implements the method for a Simulated model
toHTML.SimulatedModel <- function(qm) {
  if (!is.list(qm$out$l))
    tag("center",
        tagList(
          tags$table(border=2, class="pure-table",
                     tags$thead(
                       tags$tr(tags$th(colspan=6, (paste("Model: ", class(qm)[1], sep=""))))),
                     tags$tbody(
                       tags$tr(tags$th(width=150, "L"), tags$th(width=150, "Lq"), tags$th(width=150, "W"), tags$th(width=150, "Wq"), tags$th(width=150, "Intensity"), tags$th(width=150, "Efficiency")),
                       tags$tr(align="center", tags$td(sprintf("%5.9g", qm$out$l)), tags$td(sprintf("%5.9g", qm$out$lq)),
                               tags$td(sprintf("%5.9g", qm$out$w)), tags$td(sprintf("%5.9g", qm$out$wq)),
                               tags$td(sprintf("%5.9g", qm$out$rho)), tags$td(sprintf("%5.9g", qm$out$eff)))
                     )
          )
        )
    )
  else
    tag("center",
        tagList(
          tags$table(border=2, class="pure-table",
                     tags$thead(
                       tags$tr(tags$th(colspan=6, (paste("Model: ", class(qm)[1], sep=""))))),
                     tags$tbody(
                       tags$tr(tags$th(width=150, "L"), tags$th(width=150, "Lq"), tags$th(width=150, "W"), tags$th(width=150, "Wq"), tags$th(width=150, "Intensity"), tags$th(width=150, "Efficiency")),
                       tags$tr(align="center", tags$td(sprintf("%5.9g", qm$out$l$mean)), tags$td(sprintf("%5.9g", qm$out$lq$mean)),
                               tags$td(sprintf("%5.9g", qm$out$w$mean)), tags$td(sprintf("%5.9g", qm$out$wq$mean)),
                               tags$td(sprintf("%5.9g", qm$out$rho$mean)), tags$td(sprintf("%5.9g", qm$out$eff$mean)))
                     )
          )
        )
    )
}

#' @rdname toHTML
#' @method toHTML OpenJackson
#' @details
#' \code{toHTML.OpenJackson} implements the method for a OpenJackson model
toHTML.OpenJackson <- function(qm) {
  table <- paste("tag('center', tagList(
      tags$table(border=2, class='pure-table',
          tags$thead(
              tags$tr(tags$th(colspan=6, 'Model: ", class(qm)[1], "'))
          ),
          tags$tbody(
            tags$tr(tags$th(width=150, 'Node'), tags$th(width=150, 'L'), tags$th(width=150, 'Lq'), tags$th(width=150, 'W'), tags$th(width=150, 'Wq')),\n", sep="")
  
  for(i in 1:(length(qm$out$l)-1))
    table <- paste(table, "tags$tr(tags$td(", i, "), tags$td(", sprintf("%5.9g", qm$out$l[i]) ,"), tags$td(", sprintf("%5.9g", qm$out$lq[i]), "), tags$td(", sprintf("%5.9g",qm$out$w[i]), "), tags$td(", sprintf("%5.9g", qm$out$wq[i]), ")), ", sep="")
  table <- paste(table, "tags$tr(tags$td(", length(qm$out$l), "), tags$td(", sprintf("%5.9g",last(qm$out$l)) ,"), tags$td(", sprintf("%5.9g",last(qm$out$lq)), "), tags$td(", sprintf("%5.9g",last(qm$out$w)), "), tags$td(",sprintf("%5.9g", last(qm$out$wq)), ")), ", sep="")                 
  table <- paste(table, "tags$tr(tags$th('Total'), tags$td(", sprintf("%5.9g", qm$out$lt) ,"), tags$td(", sprintf("%5.9g", qm$out$lqt) , "), tags$td(", sprintf("%5.9g", qm$out$wt), "), tags$td(", sprintf("%5.9g", qm$out$wqt), "))))))", sep="")
  return(eval(parse(text=table)))
} 

#' @rdname toHTML
#' @method toHTML ClosedJackson
#' @details
#' \code{toHTML.ClosedJackson} implements the method for a ClosedJackson model
toHTML.ClosedJackson <- function(qm) {
  table <- paste("tag('center', tagList(
      tags$table(border=2, class='pure-table',
          tags$thead(
              tags$tr(tags$th(colspan=6, 'Model: ", class(qm)[1], "'))
          ),
          tags$tbody(
            tags$tr(tags$th(width=150, 'Node'), tags$th(width=150, 'L'), tags$th(width=150, 'Lq'), tags$th(width=150, 'W'), tags$th(width=150, 'Wq')),\n", sep="")
  
  for(i in 1:(length(qm$out$l)-1))
    table <- paste(table, "tags$tr(tags$td(", i, "), tags$td(", sprintf("%5.9g", qm$out$l[i]) ,"), tags$td(", sprintf("%5.9g", qm$out$lq[i]), "), tags$td(", sprintf("%5.9g",qm$out$w[i]), "), tags$td(", sprintf("%5.9g", qm$out$wq[i]), ")), ", sep="")
  table <- paste(table, "tags$tr(tags$td(", length(qm$out$l), "), tags$td(", sprintf("%5.9g",last(qm$out$l)) ,"), tags$td(", sprintf("%5.9g",last(qm$out$lq)), "), tags$td(", sprintf("%5.9g",last(qm$out$w)), "), tags$td(",sprintf("%5.9g", last(qm$out$wq)), ")))))) ", sep="")
  return(eval(parse(text=table)))
}

#' @rdname toHTML
#' @method toHTML MarkovianModel
#' @details
#' \code{toHTML.MarkovianModel} implements the method for a Markovian model
toHTML.MarkovianModel <- function(qm) {
  tag("center",
      tagList(
        tags$table(border=2, class="pure-table",
                   tags$thead(
                     tags$tr(tags$th(colspan=6, (paste("Model: ", class(qm)[1], sep=""))))),
                   tags$tbody(
                     tags$tr(tags$th(width=150, "L"), tags$th(width=150, "Lq"), tags$th(width=150, "W"), tags$th(width=150, "Wq"), tags$th(width=150, "Intensity"), tags$th(width=150, "Efficiency")),
                     tags$tr(align="center", tags$td(sprintf("%5.9g", qm$out$l)), tags$td(sprintf("%5.9g", qm$out$lq)),
                                             tags$td(sprintf("%5.9g", qm$out$w)), tags$td(sprintf("%5.9g", qm$out$wq)),
                                             tags$td(sprintf("%5.9g", qm$out$barrho)), tags$td(sprintf("%5.9g", qm$out$eff)))
                   )
        )
      )
  )
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

#' Capitalize the fist character of a string
#' @param x String
simpleCap <- function(x) {
  paste(toupper(substring(x, 1,1)), substring(x, 2),
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
selectDistr <- function() {
  aux  <- function(n, v) {list(name=n, value=v)}
  choicelist <- list()
  for(i in 1:length(distrList)) {
     choicelist[[i]] <- list(name=distrList[[i]]$name, params=mapply(aux, as.list(names(formals(distrList[[i]]$fun))), formals(distrList[[i]]$fun), SIMPLIFY=FALSE))
  }
  return(choicelist)
}



distrDefaultValues <- function(distrModel) {
   aux  <- function(n, v) {list(name=n, value=v)}
   parameters <- param(distrModel)
   paramNames <- slotNames(parameters)

   res <- paste("defaultvalue=\"{id:", distrList[[class(distrModel)[1]]]$id, ", params:[", sep="")
   for(i in 1:(length(paramNames)-1)) {
    res <- paste(res, "{name:'", paramNames[i], "' , value:", slot(parameters, paramNames[i]), "}", sep="")
    if (i != length(paramNames)-1)
      res <- paste(res, ", ", sep="")
   }
   res <- paste(res, "]}\"", sep="")
   print(res)
   return(res)
}

#' Generate the necesary HTML for each type of argument into a model
#' @param input The list of shiny inputs avaiable in the HTML
#' @param model The queue model to generate the inputs
#' @param parameters Optional inital values for the inputs.
generateInputs <- function(input, model, parameters) {
  if (!is.null(parameters))
    args <- parameters
  else
    args <- formals(model$fun)
  
  argsnames <- names(args)
  inputs <- "<form >"
  for(i in 1:length(model$args)) {
    label <- ifelse(is.null(l <- model$args[[i]]$label), argsnames[i], l)
    switch(model$args[[i]]$type,
        "numeric" = inputs <- paste(inputs, "<label for='", argsnames[i], input$results$total , "'>", simpleCap(label),":</label><input id='", argsnames[i], input$results$total , "' type='number' min=0 value=", args[[i]] ," /><br>", sep=""),
        "matrix" = inputs <- paste(inputs, "<label for='", argsnames[i], input$results$total, "'>", simpleCap(label),":</label><span id='", argsnames[i], input$results$total, "' ini-value='", matrixtostring(eval(args[[i]])) ,"' class='shiny-matrix-input'/><br>", sep=""),
        "vector" = inputs <- paste(inputs, "<label for='", argsnames[i], input$results$total, "'>", simpleCap(label), ":</label><input id='", argsnames[i], input$results$total, "' value='", vectortostring(eval(args[[i]])),"' class='shiny-vector-input'  /><br>", sep=""),
        "boolean" = inputs <- paste(inputs, "<label for='", argsnames[i], input$results$total, "'>", simpleCap(label), ":</label><input id='", argsnames[i], input$results$total, "' type='checkbox' ", ifelse(eval(args[[i]]), "checked", ""), "/><br>", sep=""),
        "distr" = inputs <- paste(inputs, "<label for='", argsnames[i], input$results$total, "'>", simpleCap(label), ":</label><div id='", argsnames[i], input$results$total, "' class='shiny-distr-input' ", distrDefaultValues(eval(args[[i]])), "></div><br>", sep="")
    )       
  }
  inputs <- paste(inputs, "<br></form>", sep="")
  if(any(class(model) == "Network")) {
    inputs <- paste(inputs, tagList(tags$label("for"=paste("fileInput", input$results$total, sep=""), "File"), tags$input(id=paste("fileInput", input$results$total, sep=""), type="file", multiple=FALSE, title=" ")), sep="")
  }
  if(any(class(model) == "SimulatedModel")) {
    inputs <- paste(inputs, "<button id='simulate", input$results$total, "' type='button' class='btn shiny-jquerybutton-input' once>Simulate</button>", sep="")
  }
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
  res <- ""
  defaultModel <- model$fun()
  tryCatch({Qn(defaultModel, 0)
            res <- paste(res, "<label for='PnQnSlider", input$results$total, "'><b> Pn(n) and Q(n)</b><br><br> (n from ", sep="")
           },
           error=function(e) {
             res <<- paste(res, "<label for='PnQnSlider", input$results$total, "'><b> Pn(n)</b><br><br> (n from  ", sep="")             
  
          })
 
  maxSliderPn <- ifelse(is.infinite(mc <- maxCustomers(defaultModel)), 20, mc)
  return(paste(res, "<input type='number' style='width:4.5em' value='0' min='0' id='PnQnMin", input$results$total, "'></input> to <input type='number' style='width:4.5em' value='", maxSliderPn ,"' min='0' id='PnQnMax", input$results$total,"'></input>):</label><br>",
               "<div id='PnQnSlider", input$results$total, "' class='shiny-slider-input' range='true' min=0 max=", maxSliderPn ," step=1 values='[0, ", maxSliderPn ,"]'></div><br>
                <label for='WtWqtSlider", input$results$total, "'><b>W(t) and Wq(t)</b><br><br> (t from <input type='number' min='0' step='0.01' style='width:4em' id='WtWqtMin", input$results$total, "' value='0'/> to <input type='number' min='0' step='0.01' style='width:4em' id='WtWqtMax", input$results$total,"' value='0.5'/> with step <input type='number' min='0' step='0.01' style='width:4em' id='WtWqtStep", input$results$total,"' value='0.05'/>):</label><br>
                <div id='WtWqtSlider", input$results$total, "' class='shiny-slider-input' range='true' min=0 max=2 step=0.05 values='[0, 0.5]'></div><br>
                <button id='CalculateButton", input$results$total, "' type='button' class='btn shiny-jquerybutton-input'>Compute</button><br>", sep=""))
}

#' @rdname generateToolbox
#' @method generateToolbox Network
#' @details
#' \code{generateToolbox.Network} implements the method for Networks models
generateToolbox.Network <- function(input, model) {
  defaultModel <- model$fun()
  maxSliderPn <- ifelse(is.infinite(mc <- maxCustomers(defaultModel)), 20, mc)
  res <- paste("<label for='pn1nk", input$results$total, "'><b>Pn1..n", length(defaultModel$servers), "</b></label><br>\n",
               "<input id='pn1nk", input$results$total, "' value=", vectortostring(1:length(defaultModel$servers)), " class='shiny-vector-input'/><br><br>\n",
               "<label for='PnSlider", input$results$total, "'><b> Pn(n)</b><br><br> (n from <input type='number' style='width:4.5em' value='0' min='0' id='PnMin", input$results$total, "'></input> to <input type='number' style='width:4.5em' value='5' min='0' id='PnMax", input$results$total,"'></input>):</label><br>",
               "<div id='PnSlider", input$results$total, "' class='shiny-slider-input' range='true' min=0 max=", maxSliderPn ," step=1 values='[0, 5]'></div><br>",
               "<button id='CalculateButton", input$results$total, "' type='button' class='btn shiny-jquerybutton-input'>Compute</button><br>",
               sep="")
  return(res)
}

#' @rdname generateToolbox
#' @method generateToolbox OpenJackson
#' @details
#' \code{generateToolbox.OpenJackson} implements the method for OpenJackson model
generateToolbox.OpenJackson <- function(input, model) {
  defaultModel <- model$fun()
  return(tagList(selectInput(inputId=paste("nodeSelector", input$results$total, sep=""), label=tags$b("More info of node:"), choices=c("-----", as.character(1:length(defaultModel$servers))), multiple=FALSE), tags$br(), 
                 tags$label("for"=paste("pn1nk", input$results$total, sep=""), paste("Pn1..n", length(defaultModel$servers), ":",  sep="")), 
                 tags$input(id=paste("pn1nk", input$results$total, sep=""), value=vectortostring(1:length(defaultModel$servers)) ,class="shiny-vector-input")
  ))
}

#' @rdname generateToolbox
#' @method generateToolbox SimulatedModel
#' @details
#' \code{generateToolbox.SimulatedModel} implements the method for a Simulated model
generateToolbox.SimulatedModel <- function(input, model) {
  return(tagList(selectInput(inputId=paste("convergenceSelector", input$results$total, sep=""), label=tags$b("Select a variable:"), choices=c("L"="L", "Lq"="Lq", "W"="W", "Wq"="Wq"))))
}

#' Generates the divs of the tree main panels. The inputs panel, the output panel and the tools panel.
#' @param session Session in the server
#' @param input The list of shiny inputs avaiable in the HTML
#' @param model The model to generate the different panels.
#' @param parameters Optional inital values for the inputs.
generatePanel <- function(session, input, model, parameters) {
  list(
    paste("<div class='InputToolsBox'><div class='InputDataBox ui-widget-content ui-corner-all'><h4>Input data:</h4><hr><br>", generateInputs(input, model, parameters) ,"</div>",
          "<div class='ToolsBox ui-widget-content ui-corner-all'><h4>Tools:</h4><hr><br>", generateToolbox(input, model),"</div></div>\n",
          "<div class='ModelOutputBox ui-widget-content ui-corner-all'><h4>Output:</h4><hr><br><div id='ModelOutputTabs", input$results$total ,"' class='shiny-tabs-input ModelOutputTabs'><ul></ul></div></div>",
          sep="")
  )
}

#' Generates a Distr object from the distrList.
#' @param distrInput The selected distribution
generateDistr <- function(distrInput) {
  res <- "("
  for(i in 1:length(distrInput$params)) {
    res <- paste(res, distrInput$params[[i]]$name, "=", distrInput$params[[i]]$value, sep="")
    
    if (i == length(distrInput$params))
      res <- paste(res, ")", sep="")
    else
      res <- paste(res, ", ", sep="")
  }
  return(eval(parse(text=paste("distrList[[", distrInput$distribution, "]]$fun", res, sep=""))))
}

#' Generates the diferent arugments for a model function
#' @param fun The model function
#' @param id The number of the tab where the model is executed
generateArguments <- function(fun, id) {
  args <- names(formals(fun))
  res <- ""
  for(i in 1:length(args)) {
      if (args[i] == "p")
        res <- paste(res, "t(matrix(unlist(input$", args[i], id, "$matrix), nrow=length(input$", args[i], id, "$matrix)))", sep="")
      else if (args[i] == "arrivalDistribution" || args[i] == "serviceDistribution") {
          res <- paste(res, "generateDistr(input$", args[i], id, ")", sep="")
        } else
        res <- paste(res, "input$", args[i], id, sep="")
      
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
  ranges <- seq(rangeWtWqt[[1]], rangeWtWqt[[2]], step)
  wts <-  FW(model, ranges)
  wqs <- FWq(model, ranges)
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
  eval(parse(text=paste("updateTabInput(session, 'ModelOutputTabs", numTab , "', action=list('add'), value=list(newTab('Summary', '<div id=\"summarySpan", numTab, "\" class=\"shiny-html-output\"></div><div id=\"pnDatatable", numTab, "\" class=\"shiny-datatable-output\"></div><br><div id=\"wtDatatable", numTab, "\" class=\"shiny-datatable-output\"></div>')))\n", sep="")))
  eval(parse(text=paste("updateTabInput(session, 'ModelOutputTabs", numTab , "', action=list('add'), value=list(newTab('Waiting Plot', '<div id=\"waitplotDiv", numTab, "\" class=\"shiny-image-output\"></div>')))\n", sep=""))) 
  eval(parse(text=paste("updateTabInput(session, 'ModelOutputTabs", numTab , "', action=list('add'), value=list(newTab('Probabilty Plot', '<div id=\"probplotDiv", numTab, "\" class=\"shiny-image-output\"></div>')))\n", sep=""))) 
  
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
                                                     "ggsave(outfile, width=9, height=4.5, dpi=100)\n",
                                                     "title <- 'Pn: Steady-state probability of having n customers in the system.\n'\n",
                                                     "try({Qn(values$qm, 0); title <- paste(title, 'Qn: Steady-state probability of finding n customers in the system when a new customer arrives.', sep='')})\n",
                                                     "list(src=outfile, alt='Loading plot...', title=title)}, deleteFile=TRUE)\n",
      "output$waitplotDiv", numTab, "<- renderImage({outfile <- tempfile(fileext='.svg')\n",
                                                    "input$CalculateButton", input$results$total, "\n",
                                                    "if(is.null(values$qm)) stop(values$error)\n",
                                                    "isolate({\n",
                                                      "wtwqtrange <- range(input$WtWqtMin", numTab, ", input$WtWqtMax", numTab, ")\n",
                                                      "summaryWtWqt(values$qm, seq(wtwqtrange[1], wtwqtrange[2], input$WtWqtStep", numTab, "))\n",
                                                    "})\n", 
                                                    "ggsave(outfile, width=9, height=4.5, dpi=100)\n",
                                                    "list(src=outfile, alt='Loading plot...', title='W: Distribution function of the waiting time in the system.\nWq: Distribution function of the waiting time in the queue.')}, deleteFile=TRUE)\n",
      "output$summarySpan", numTab, "<- renderUI({input$CalculateButton", input$results$total, "\n",
                                                 "if (is.null(values$qm)) stop(values$error)\n",
                                                 "isolate({\n",
                                                    "tagList(toHTML(values$qm), tags$br(), tags$br())", 
                                                    #"tag('center', tagList(tags$div(style='overflow:hidden', tablePnQnVertical(values$qm, range(input$PnQnMin", numTab, ", input$PnQnMax", numTab, ")),",
                                                    #"tableWtWqtVertical(values$qm, range(input$WtWqtMin", numTab, ", input$WtWqtMax", numTab, "), input$WtWqtStep", numTab, ")))))\n", 
                                                 "})\n", 
                                                 "})\n",
      "output$pnDatatable", numTab, "<- renderDataTable({input$CalculateButton", input$results$total, "\n",
                                                        "if (is.null(values$qm)) stop(values$error)\n",
                                                        "isolate({\n",
                                                            "datatablePnQn(values$qm, range(input$PnQnMin", numTab, ", input$PnQnMax", numTab, "))\n",
                                                        "})\n",
                                        "}, options = list(bJQueryUI=TRUE, sPaginationType='full_numbers', iDisplayLength=10, bSortClasses = TRUE))\n",
      
      "output$wtDatatable", numTab, "<- renderDataTable({input$CalculateButton", input$results$total, "\n",
                                                        "if (is.null(values$qm)) stop(values$error)\n",
                                                        "isolate({\n",
                                                           "datatableWtWqt(values$qm, range(input$WtWqtMin", numTab, ", input$WtWqtMax", numTab, "), input$WtWqtStep", numTab, ")\n",
                                                        "})\n",
                                        "}, options = list(bJQueryUI=TRUE, sPaginationType='full_numbers', iDisplayLength=10, bSortClasses = TRUE))",
  #    "outputOptions(output, 'summarySpan", numTab, "', suspendWhenHidden = FALSE)\n",
   #   "outputOptions(output, 'waitplotDiv", numTab, "', suspendWhenHidden = FALSE)\n",
    #  "outputOptions(output, 'probplotDiv", numTab, "', suspendWhenHidden = FALSE)\n",
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
  eval(parse(text=paste("updateTabInput(session, 'ModelOutputTabs", numTab , "', action=list('add'), value=list(newTab('Summary', '<div id=\"summarySpan", numTab, "\" class=\"shiny-html-output\"></div><div id=\"probDiv", numTab, "\" class=\"shiny-html-output\"></div>')))\n", sep="")))
  eval(parse(text=paste("updateTabInput(session, 'ModelOutputTabs", numTab , "', action=list('add'), value=list(newTab('Graph', '<div id=\"networkDiv", numTab, "\" class=\"shiny-network-output\"></div>')))\n", sep="")))  
  espChar <- "[\\\\]"
  values <- reactiveValues()
  eval(parse(text=paste(
    "observe({\n",
      "tryCatch({\n",
        "values$qm <- model$fun(", generateArguments(model$fun, numTab),")\n",
      "}, error=function(e) {\n",
          "values$qm <<- NULL\n",
          "values$error <<- e$message\n",
      "})\n",
    "})\n", 
    "output$summarySpan", numTab, "<- renderUI({\n",
                                        "if (is.null(values$qm)) stop(values$error)\n",
                                        "isolate({\n",
                                          "tagList(toHTML(values$qm), tags$br())\n",
                                        "})})\n",
    "output$probDiv", numTab, "<- renderUI({\n",
                                      "if (is.null(values$qm)) return()\n",
                                      "tag('center', tagList(tags$table(border=2, width=400, class='pure-table', tags$tr(tags$td(tags$b('Pn1..nk:')), tags$td(sprintf('%5.9g', Pn(values$qm, input$pn1nk", numTab, ")))))))\n",
                                  "})\n",
    "output$networkDiv", numTab, "<- renderNetwork({\n",
                                        "if (is.null(values$qm)) stop(values$error)\n",
                                        "isolate({\n",
                                              "res <- list(lambda=values$qm$lambda, mu=values$qm$mu, s=values$qm$s, p=values$qm$p,
                                                           l=values$qm$out$l, lq=values$qm$out$lq, w=values$qm$out$w, wq=values$qm$out$wq)\n",
                                         "})\n",
                                         "res\n",
                                     "})\n",
    #Si cambia el modelo, actualizamos los nodos disponibles en la lista y la etiqueta de las probabilidades conjuntas
    "observe({\n",
        "if (!is.null(values$qm)){\n",
          "isolate({updateSelectInput(session=session, inputId='nodeSelector", numTab, "', choices=c('----', as.character(1:length(values$qm$s))))\n",
                   "updateVectorInput(session=session, inputId='pn1nk", numTab, "', label=paste('Pn1..n', length(values$qm$s), ':', sep=''), value=1:length(values$qm$s))\n",
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
              "str(qm)\n",
              "loadUIModel(uiList[[2]],session, input, output, parameters=c(lambda=rate(qm$arrivalDistribution), mu=rate(qm$serviceDistribution), s=qm$servers))\n",
        "}})\n",
    "})\n",
    #Si se sube un archivo cargamos sus datos
    "observe({\n",
        "if (!is.null(input$fileInput", numTab, ")){\n",
            "route <- gsub('", espChar, "', '/', input$fileInput", numTab, "['datapath'])\n",
            "inputdata <- read.table(route, fill=TRUE)\n",
            "print(tonumeric(inputdata[1,]))\n",
            "numnodes <- ncol(inputdata)\n",
            "colnames(inputdata) <- as.character(1:numnodes)\n",
            "isolate({if(class(values$qm)[1] == 'OpenJackson') {\n",
              "print(inputdata[1,])\n",
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
  eval(parse(text=paste("updateTabInput(session, 'ModelOutputTabs", numTab , "', action=list('add'), value=list(newTab('Summary', '<div id=\"summarySpan", numTab, "\" class=\"shiny-html-output\"></div><div id=\"probDiv", numTab, "\" class=\"shiny-html-output\"></div><div id=\"pnDiv", numTab, "\" class=\"shiny-datatable-output\"></div>')))\n", sep="")))
  eval(parse(text=paste("updateTabInput(session, 'ModelOutputTabs", numTab , "', action=list('add'), value=list(newTab('Graph', '<div id=\"networkDiv", numTab, "\" class=\"shiny-network-output\"></div>')))\n", sep="")))
  
  eval(parse(text=paste("observe({\n",
                        "updateNumericInput(session, 'PnMin", numTab, "', value=input$PnSlider", numTab,"$values[1])\n",
                        "updateNumericInput(session, 'PnMax", numTab, "', value=input$PnSlider", numTab,"$values[2])}, priority=2)", sep="")))
  
  values <- reactiveValues()
  espChar <- "[\\\\]"
  eval(parse(text=paste(
    "observe({\n",
      "tryCatch({\n",
         "values$qm <- model$fun(", generateArguments(model$fun, numTab),")\n",
       "}, error=function(e) {\n",
           "values$qm <<- NULL\n",
           "values$error <<- e$message\n",
       "})\n",
     "})\n", 
      "output$summarySpan", numTab, "<- renderUI({\n",
                "if (is.null(values$qm)) stop(values$error)\n",
                "isolate({\n",
                    "tagList(toHTML(values$qm), tags$br())\n",
                "})})\n",
      "output$probDiv", numTab, "<- renderUI({\n",
              "if (is.null(values$qm)) return()\n",
                 "tag('center', tagList(tags$table(border=2, width=400, class='pure-table', tags$tr(tags$td(tags$b('Pn1..nk:')), tags$td(sprintf('%5.9g', Pn(values$qm, input$pn1nk", numTab, "))))), tags$br()))\n",
              "})\n",
      "output$pnDiv", numTab, "<- renderDataTable({\n",
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
                 "res <- list(lambda=values$qm$lambda, mu=values$qm$mu, s=values$qm$s, p=values$qm$p,
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
              "isolate({updateVectorInput(session=session, inputId='pn1nk", numTab, "', label=paste('Pn1..n', length(values$qm$s), sep=''), value=div)})\n",
            "} else {\n",
             "updateSelectInput(session, inputId='nodeSelector", numTab, "', choices=c('----'))\n",
           "}\n",
       "})\n",
    #Si se sube un archivo cargamos sus datos
    "observe({\n",
        "if (!is.null(input$fileInput", numTab, ")){\n",
            "route <- gsub('", espChar, "', '/', input$fileInput", numTab, "['datapath'])\n",
        "inputdata <- read.table(route, fill=TRUE)\n",
        "print(tonumeric(inputdata[1,]))\n",
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
  eval(parse(text=paste("updateTabInput(session, 'ModelOutputTabs", numTab , "', action=list('add'), value=list(newTab('Convergence', '<div id=\"convergenceDiv", numTab, "\" class=\"shiny-image-output\"></div>')))\n", sep="")))
  eval(parse(text=paste("updateTabInput(session, 'ModelOutputTabs", numTab , "', action=list('add'), value=list(newTab('Summary', '<div id=\"summarySpan", numTab, "\" class=\"shiny-html-output\"></div>')))\n", sep="")))
  
  updateSelectDistrInput(session=session, inputId=paste("arrivalDistribution", numTab, sep=""), distributions=selectDistr())
  updateSelectDistrInput(session, paste("serviceDistribution", numTab, sep=""), selectDistr())
  values <- reactiveValues()
 # updateButtonInput(session, paste("simulate", numTab, sep=""), TRUE)
  eval(parse(text=paste("observe({\n",
                            "input$simulate", numTab, "\n",
                            "isolate({\n",
                                "tryCatch({\n",
                                    "values$qm <- model$fun(", generateArguments(model$fun, numTab),")\n",
                                "}, error=function(e) {\n",
                                     "values$qm <<- NULL\n",
                                      "values$error <<- e$message\n",
                                "}, finally=updateButtonInput(session, 'simulate", numTab, "', 'enabled'))\n",
                              "})\n",
                        "})\n", 
                        "output$summarySpan", numTab, "<- renderUI({\n",
                                "if (is.null(values$qm)) stop(values$error)\n",
                                "isolate({\n", 
                                    "toHTML(values$qm)\n",
                                  "})\n",
                        "})\n",          
                        "output$convergenceDiv", numTab, "<- renderImage({\n",
                              "if (is.null(values$qm)) stop(values$error)\n",
                              "selected <- input$convergenceSelector", numTab, "\n",
                              "outfile <- tempfile(fileext='.svg')\n",
                              "isolate({\n",
                                "summarySimple(values$qm, 1, (values$qm$Staclients + values$qm$Nclients), selected)\n",
                                "ggsave(outfile, width=9, height=4.5, dpi=100)\n",
                              "})\n",
                              "list(src=outfile, alt='Loading plot...', title='')
                        }, deleteFile=TRUE)\n", sep="")))
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

shinyServer(function(input, output, session) {
  options(shiny.usecairo=FALSE)
  initMenu <- list(
                list(id = 0, title="Markovian models", submenu=generateMenu(uiList[sapply(sapply(uiList, class), function(v) {any(v=="MarkovianModel")}, simplify="array")])),
                list(id = 0, title="Simulated Models", submenu=generateMenu(uiList[sapply(sapply(uiList, class), function(v) {any(v=="SimulatedModel")}, simplify="array")]))
                #list(id = 0, title="Data analysis", submenu=list())
              )
  
  isolate({
    updateMenuInput(session, "menu", action=list("set"), menu=initMenu)
    updateTabInput(session, "results", action=list("add"), value=list(newTab("Start", "Select a model in the menu at left to start.")), removeButton=TRUE)
  })
  
  observe({
    if (!is.null(input$menu$clicked)){
      isolate({
              id <- as.numeric(input$menu$selected)
               switch(as.character(input$menu$selected),
                      "0" = NULL,
                      loadUIModel(uiList[[id]], session, input, output)
               )
      })
    }
  }) 
  
})