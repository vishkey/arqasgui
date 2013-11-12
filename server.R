library(shiny)
library(arqas)
library(ggplot2)

toHTML <- function(qm) {UseMethod("toHTML", qm)}

toHTML.SimulatedModel <- function(qm) {
  tag$h1("No definido")
}
toHTML.OpenJackson <- function(qm) {
  table <- paste("tag('center', tagList(
      tags$table(border=2, class='pure-table',
          tags$thead(
              tags$tr(tags$th(colspan=6, 'Model: ", class(qm)[1], "'))
          ),
          tags$tbody(
            tags$tr(tags$th(width=150, 'L'), tags$th(width=150, 'Lq'), tags$th(width=150, 'W'), tags$th(width=150, 'Wq')),\n", sep="")
  
  for(i in 1:(length(qm$out$l)-1))
    table <- paste(table, "tags$tr(tags$td(", sprintf("%5.9g", qm$out$l[i]) ,"), tags$td(", sprintf("%5.9g", qm$out$lq[i]), "), tags$td(", sprintf("%5.9g",qm$out$w[i]), "), tags$td(", sprintf("%5.9g", qm$out$wq[i]), ")), ", sep="")
  table <- paste(table, "tags$tr(tags$td(", sprintf("%5.9g",last(qm$out$l)) ,"), tags$td(", sprintf("%5.9g",last(qm$out$lq)), "), tags$td(", sprintf("%5.9g",last(qm$out$w)), "), tags$td(",sprintf("%5.9g", last(qm$out$wq)), ")))))) ", sep="")                 
  return(eval(parse(text=table)))
} 

toHTML.ClosedJackson <- function(qm) {toHTML.OpenJackson(qm)}

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

getOptions <- function (l1, l2) {
  res <- c()
  for(i in 1:length(l1)) {
    res <- eval(parse(text=paste("c(res, '", l1[i], "' = ", l2[i], ")")))
  }
  return(res)
}

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

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

vectortostring <- function(v) {
  res <- "";
  for(i in 1:(length(v)-1)) {
    res <- paste(res, v[i], ";", sep="")
  }
  res <- paste(res, last(v), sep="")
  return(res)
}

generateInputs <- function(input, model) {
  args <- formals(model$fun)
  argsnames <- names(args)
  inputs <- "<form class='pure-form-stacked'>"
  for(i in 1:length(argsnames)) {
    switch(model$types[i],
        "numeric" = inputs <- paste(inputs, "<label for='", argsnames[i], input$results$total , "'>", simpleCap(argsnames[i]),":</label><input id='", argsnames[i], input$results$total , "' type='number' min=0 value=", args[[i]] ," /><br>", sep=""),
        "matrix" = inputs <- paste(inputs, "<label for='", argsnames[i], input$results$total, "'>", simpleCap(argsnames[i]),":</label><span id='", argsnames[i], input$results$total, "' ini-value='", matrixtostring(eval(args[[i]])) ,"' class='shiny-matrix-input'/><br>", sep=""),
        "vector" = inputs <- paste(inputs, "<label for='", argsnames[i], input$results$total, "'>", simpleCap(argsnames[i]), ":</label><input id='", argsnames[i], input$results$total, "' value='", vectortostring(eval(args[[i]])),"' class='shiny-vector-input'  /><br>", sep="")
    )       
  }
  inputs <- paste(inputs, "<br></form>", sep="")
  if(class(model)[1] == "network") {
    inputs <- paste(inputs, tagList(fileInput(paste("fileInput", input$results$total, sep=""), "File", FALSE, "")), sep="")
  }
  return(inputs)
}

generateMenu <- function(l) {
  res <- vector("list", length(l))
  for(i in 1:length(l)) {
    res[[i]] <- list(title=l[[i]]$name, submenu=list(), id=l[[i]]$id)
  }
  return(res)
}

newTab <- function(title, content) {
  list(title=title, content=content)
}

updateTabInput <- function(session, inputId, action=NULL, value=NULL, removeButton=FALSE) {
  message <- list(action=action, value=value, removeButton=removeButton)
  session$sendInputMessage(inputId, message)
}

updateMenuInput <- function(session, inputId, action=NULL, menu=NULL) {
  message <- list(action=action, menu=menu)
  session$sendInputMessage(inputId, message)
}

updateJQueryUISliderInput <- function(session, inputId, values=NULL, step=NULL, min=NULL, max=NULL) {
  message <- list(values=values, step=step, min=min, max=max)
  session$sendInputMessage(inputId, message)
}

generateToolbox <- function(input, model) {UseMethod("generateToolbox", model)}

generateToolbox.markovian <- function(input, model) {
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
                <button id='CalculateButton", input$results$total, "' type='button' class='btn action-button'>Compute</button><br>", sep=""))
}

generateToolbox.network <- function(input, model) {
  
}
generatePanel <- function(session, input, model) {
  list(
    paste("<div class='InputToolsBox'><div class='InputDataBox ui-widget-content ui-corner-all'><h4>Input data:</h4><hr><br>", generateInputs(input, model) ,"</div>",
          "<div class='ToolsBox ui-widget-content ui-corner-all'><h4>Tools:</h4><hr><br>", generateToolbox(input, model),"</div></div>\n",
          "<div class='ModelOutputBox ui-widget-content ui-corner-all'><h4>Output:</h4><hr><br><div id='ModelOutputTabs", input$results$total ,"' class='shiny-tabs-input ModelOutputTabs'><ul></ul></div></div>",
          sep="")
  )
}

generateArguments <- function(fun, id) {
  args <- names(formals(fun))
  res <- ""
  for(i in 1:(length(args)-1)) {
      if (args[i] == "p")
        res <- paste(res, "t(matrix(unlist(input$", args[i], id, "$matrix), nrow=length(input$", args[i], id, "$matrix))), ", sep="")
      else
        res <- paste(res, "input$", args[i], id, ", ", sep="")
  }
  if (last(args) == "p")
    res <- paste(res, "t(matrix(unlist(input$", last(args), id, "$matrix), nrow=length(input$", last(args), id, "$matrix)))", sep="")
  else
    res <- paste(res, "input$", last(args), id, sep="")
  
  return(res)
}

tablePnQnVertical <- function(model, rangePnQn) {
  ranges <- seq(rangePnQn[[1]], rangePnQn[[2]], 1)
  pns <- sprintf("%5.6g", Pn(model, ranges))
  tryCatch ({
    qns <- sprintf("%5.6g", Qn(model, ranges))
    table <- paste("tags$table(border=2, style='float:left; margin-left: 20px', class='pure-table', tags$thead(tags$tr(tags$th(width=80, 'n'), tags$th(width=150, 'P(n)'), tags$th(width=150, 'Q(n)'))), tags$tbody(", sep="")
    if (length(ranges) > 1) {
        for(i in 1:(length(ranges)-1)) {
          table <- paste(table, "tags$tr(tags$td(", ranges[i], "), tags$td(", pns[i], "), tags$td(", qns[i] ,")), ", sep="")
        }
        table <- paste(table, "tags$tr(tags$td(", ranges[length(ranges)], "), tags$td(", pns[length(ranges)], "), tags$td(", qns[length(ranges)], "))))")
    } else {
        table <- paste(table, "tags$tr(tags$td(", rangePnQn[1], "), tags$td(", pns[1], "), tags$td(", qns[1],"))))", sep="")
    }
    return(eval(parse(text=table))) 
  }, error= function(e) {
    table <- paste("tags$table(border=2, style='float:left; margin-left: 60px', class='pure-table', tags$thead(tags$tr(tags$th(width=90, 'n'), tags$th(width=150, 'P(n)'))), tags$tbody(", sep="")
    if (length(ranges) > 1) {
      for(i in 1:(length(ranges)-1)) {
        table <- paste(table, "tags$tr(tags$td(", ranges[i], "), tags$td(", pns[i], ")), ", sep="")
      }
      table <- paste(table, "tags$tr(tags$td(", ranges[length(ranges)], "), tags$td(", pns[length(ranges)], "))))")
    } else {
      table <- paste(table, "tags$tr(tags$td(", rangePnQn[1], "), tags$td(", pns[1], "))))", sep="")
    }  
    return(eval(parse(text=table))) 
  })
}

tableWtWqtVertical <- function(model, rangeWtWqt, step) {
    ranges <- seq(rangeWtWqt[[1]], rangeWtWqt[[2]], step)
    wts <- sprintf("%5.6g", FW(model, ranges))
    wqs <- sprintf("%5.6g", FWq(model, ranges))
    
    table <- paste("tags$table(border=2, style='float:right; margin-right:20px', class='pure-table', tags$thead(tags$tr(tags$th(width=110, 't'), tags$th(width=150, 'W(t)'), tags$th(width=150, 'Wq(t)'))), tags$tbody(", sep="")
    if (length(ranges) > 1) {
      for(i in 1:(length(ranges)-1)) {
        table <- paste(table, "tags$tr(tags$td(", ranges[i], "), tags$td(", wts[i], "), tags$td(", wqs[i] ,")), ", sep="")
      }
      table <- paste(table, "tags$tr(tags$td(", ranges[length(ranges)], "), tags$td(", wts[length(ranges)], "), tags$td(", wqs[length(ranges)], "))))")
    } else {
      table <- paste(table, "tags$tr(tags$td(", rangeWtWqt[1], "), tags$td(", wts[1], "), tags$td(", wqs[1],"))))", sep="")
    }
    return(eval(parse(text=table))) 
}

loadUIModel <- function(model, session, input, output) {UseMethod("loadUIModel", model)}

loadUIModel.markovian <- function(model, session, input, output) {
  numTab <- input$results$total
  updateTabInput(session, "results", list("add"),  list(newTab(model$name, generatePanel(session, input, model))), removeButton=TRUE)
  eval(parse(text=paste("updateTabInput(session, 'ModelOutputTabs", numTab , "', action=list('add'), value=list(newTab('Summary', '<div id=\"summarySpan", numTab, "\" class=\"shiny-html-output\"></div>')))\n", sep="")))
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
                                                    "list(src=outfile, alt='Loading plot...', title='Pn: Func1\nQn: func2')}, deleteFile=TRUE)\n",
      "output$waitplotDiv", numTab, "<- renderImage({outfile <- tempfile(fileext='.svg')\n",
                                                    "input$CalculateButton", input$results$total, "\n",
                                                    "if(is.null(values$qm)) stop(values$error)\n",
                                                    "isolate({\n",
                                                      "wtwqtrange <- range(input$WtWqtMin", numTab, ", input$WtWqtMax", numTab, ")\n",
                                                      "summaryWtWqt(values$qm, seq(wtwqtrange[1], wtwqtrange[2], input$WtWqtStep", numTab, "))\n",
                                                    "})\n", 
                                                    "ggsave(outfile, width=9, height=4.5, dpi=100)\n",
                                                    "list(src=outfile, alt='Loading plot...', title='W: Func1\nWq: func2')}, deleteFile=TRUE)\n",
      "output$summarySpan", numTab, "<- renderUI({input$CalculateButton", input$results$total, "\n",
                                                 "if (is.null(values$qm)) stop(values$error)\n",
                                                 "isolate({\n",
                                                    "tagList(toHTML(values$qm), tags$br(), tags$br(), 
                                                     tag('center', tagList(tags$div(style='overflow:hidden', tablePnQnVertical(values$qm, range(input$PnQnMin", numTab, ", input$PnQnMax", numTab, ")),
                                                     tableWtWqtVertical(values$qm, range(input$WtWqtMin", numTab, ", input$WtWqtMax", numTab, "), input$WtWqtStep", numTab, ")))))\n", 
                                                 "})\n", 
                                                 "})\n",
      "outputOptions(output, 'summarySpan", numTab, "', suspendWhenHidden = FALSE)\n",
      "outputOptions(output, 'waitplotDiv", numTab, "', suspendWhenHidden = FALSE)\n",
      "outputOptions(output, 'probplotDiv", numTab, "', suspendWhenHidden = FALSE)\n",
      sep="")))
}

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

loadUIModel.network <- function(model, session, input, output) {
  numTab <- input$results$total
  updateTabInput(session, "results", list("add"),  list(newTab(model$name, generatePanel(session, input, model))), removeButton=TRUE)
  eval(parse(text=paste("updateTabInput(session, 'ModelOutputTabs", numTab , "', action=list('add'), value=list(newTab('Summary', '<div id=\"summarySpan", numTab, "\" class=\"shiny-html-output\"></div>')))\n", sep="")))
  eval(parse(text=paste("updateTabInput(session, 'ModelOutputTabs", numTab , "', action=list('add'), value=list(newTab('Graph', '<div id=\"networkDiv", numTab, "\" class=\"shiny-network-output\"></div>')))\n", sep="")))
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
                                          "tagList(toHTML(values$qm), tags$br(), tags$br())\n",
                                        "})})\n",
    "output$networkDiv", numTab, "<- renderNetwork({\n",
                                        "if (is.null(values$qm)) stop(values$error)\n",
                                        "isolate({\n",
                                              "res <- list(lambda=values$qm$lambda, mu=values$qm$mu, s=values$qm$s, p=values$qm$p,
                                                           l=values$qm$out$l, lq=values$qm$out$lq, w=values$qm$out$w, wq=values$qm$out$wq)\n",
                                         "})\n",
                                         "res\n",
                                     "})\n",
    "observe({\n",
        "if (!is.null(input$fileInput", numTab, ")){\n",
            "route <- gsub('", espChar, "', '/', input$fileInput", numTab, "['datapath'])\n",
            "print(read.table(route))\n",
         "}\n",
    "})\n",
    "outputOptions(output, 'summarySpan", numTab, "', suspendWhenHidden = FALSE)\n",
    "outputOptions(output, 'networkDiv", numTab, "', suspendWhenHidden = FALSE)\n",
     sep="")))
}

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output, session) {
  options(shiny.usecairo=FALSE)
  initMenu <- list(
                list(id = 0, title="Markovian models", submenu=generateMenu(uiList[sapply(sapply(uiList, class), function(v) {any(v=="markovian")}, simplify="array")])),
                list(id = 0, title="Simulated Models", submenu=generateMenu(uiList[sapply(sapply(uiList, class), function(v) {any(v=="simulation")}, simplify="array")]))
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