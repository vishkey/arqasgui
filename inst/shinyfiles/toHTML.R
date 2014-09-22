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
                       tags$tr(align="center", tags$td(sprintf("%9.5g", qm$out$l)), tags$td(sprintf("%9.5g", qm$out$lq)),
                               tags$td(sprintf("%9.5g", qm$out$w)), tags$td(sprintf("%9.5g", qm$out$wq)),
                               tags$td(sprintf("%9.5g", qm$out$rho)), tags$td(sprintf("%9.5g", qm$out$eff)))
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
                       tags$tr(align="center", tags$td(sprintf("%9.5g", qm$out$l$mean)), tags$td(sprintf("%9.5g", qm$out$lq$mean)),
                               tags$td(sprintf("%9.5g", qm$out$w$mean)), tags$td(sprintf("%9.5g", qm$out$wq$mean)),
                               tags$td(sprintf("%9.5g", qm$out$rho$mean)), tags$td(sprintf("%9.5g", qm$out$eff$mean)))
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
    table <- paste(table, "tags$tr(tags$td(", i, "), tags$td(", sprintf("%9.5g", qm$out$l[i]) ,"), tags$td(", sprintf("%9.5g", qm$out$lq[i]), "), tags$td(", sprintf("%9.5g",qm$out$w[i]), "), tags$td(", sprintf("%9.5g", qm$out$wq[i]), ")), ", sep="")
  table <- paste(table, "tags$tr(tags$td(", length(qm$out$l), "), tags$td(", sprintf("%9.5g",last(qm$out$l)) ,"), tags$td(", sprintf("%9.5g",last(qm$out$lq)), "), tags$td(", sprintf("%9.5g",last(qm$out$w)), "), tags$td(",sprintf("%9.5g", last(qm$out$wq)), ")), ", sep="")                 
  table <- paste(table, "tags$tr(tags$th('Total'), tags$td(", sprintf("%9.5g", qm$out$lt) ,"), tags$td(", sprintf("%9.5g", qm$out$lqt) , "), tags$td(", sprintf("%9.5g", qm$out$wt), "), tags$td(", sprintf("%9.5g", qm$out$wqt), "))))))", sep="")
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
    table <- paste(table, "tags$tr(tags$td(", i, "), tags$td(", sprintf("%9.5g", qm$out$l[i]) ,"), tags$td(", sprintf("%9.5g", qm$out$lq[i]), "), tags$td(", sprintf("%9.5g",qm$out$w[i]), "), tags$td(", sprintf("%9.5g", qm$out$wq[i]), ")), ", sep="")
  table <- paste(table, "tags$tr(tags$td(", length(qm$out$l), "), tags$td(", sprintf("%9.5g",last(qm$out$l)) ,"), tags$td(", sprintf("%9.5g",last(qm$out$lq)), "), tags$td(", sprintf("%9.5g",last(qm$out$w)), "), tags$td(",sprintf("%9.5g", last(qm$out$wq)), ")))))) ", sep="")
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
                     tags$tr(align="center", tags$td(sprintf("%9.5g", qm$out$l)), tags$td(sprintf("%9.5g", qm$out$lq)),
                                             tags$td(sprintf("%9.5g", qm$out$w)), tags$td(sprintf("%9.5g", qm$out$wq)),
                                             tags$td(sprintf("%9.5g", qm$out$barrho)), tags$td(sprintf("%9.5g", qm$out$eff)))
                   )
        )
      )
  )
}