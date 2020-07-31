# insertSource("vignettes/allen-electro-bnlearn-layout.R", 'bnlearn', functions = c('graphviz.backend', 'graphviz.compare.backend', 'graphviz.compare'))

graphviz.compare <- function (x, ..., groups, edgeAttrs=NULL, layout = "dot", shape = "circle", main = NULL,
          sub = NULL, diff = "from-first", diff.args = list())
{
  available.diff.methods = c("none", "from-first")
  check.and.load.package("Rgraphviz")
  check.bn(x)
  nodes = names(x$nodes)
  netlist = c(list(x), list(...))
  check.customlist(netlist, nodes = nodes)
  if (!is.null(main))
    if (!is.string.vector(main) || (length(main) != length(netlist)))
      stop("'main' must a vector of character strings, one for each network.")
  if (!is.null(sub))
    if (!is.string.vector(sub) || (length(sub) != length(netlist)))
      stop("'sub' must a vector of character strings, one for each network.")
  check.label(diff, choices = available.diff.methods, argname = "diff")
  if (diff == "none") {
    check.unused.args(diff.args, character(0))
  }
  else if (diff == "from-first") {
    args = c("tp.col", "tp.lty", "tp.lwd", "fp.col", "fp.lty",
             "fp.lwd", "fn.col", "fn.lty", "fn.lwd", "show.first")
    check.unused.args(diff.args, args)
    if ("tp.lty" %in% names(diff.args))
      check.lty(diff.args$tp.lty)
    else diff.args$tp.lty = "solid"
    if ("fp.lty" %in% names(diff.args))
      check.lty(diff.args$fp.lty)
    else diff.args$fp.lty = "solid"
    if ("fn.lty" %in% names(diff.args))
      check.lty(diff.args$fn.lty)
    else diff.args$fn.lty = "dashed"
    if ("tp.col" %in% names(diff.args))
      check.colour(diff.args$tp.col)
    else diff.args$tp.col = "black"
    if ("fp.col" %in% names(diff.args))
      check.colour(diff.args$fp.col)
    else diff.args$fp.col = "red"
    if ("fn.col" %in% names(diff.args))
      check.colour(diff.args$fn.col)
    else diff.args$fn.col = "blue"
    for (lwd in c("tp.lwd", "fp.lwd", "fn.lwd")) if (lwd %in%
                                                     names(diff.args))
      if (!is.positive(diff.args[[lwd]]))
        stop("diff.args$", lwd, " must be a positive number.")
    if ("show.first" %in% names(diff.args))
      check.logical(diff.args$show.first)
    else diff.args$show.first = TRUE
  }
  graphviz.compare.backend(netlist = netlist, nodes = nodes,
                           groups = groups, layout = layout, shape = shape, main = main,
                           sub = sub, diff = diff, diff.args = diff.args,
                           edgeAttrs = edgeAttrs )
}
graphviz.compare.backend <- function (netlist, nodes, groups, layout, shape, main, sub, diff,
          diff.args, edgeAttrs=NULL)
{
  arclist = lapply(netlist, function(net) {
    if (is(net, "bn"))
      return(net$arcs)
    else return(net)
  })
  merged = empty.graph(nodes)
  arcs(merged, check.cycles = FALSE) = unique.arcs(do.call("rbind",
                                                           arclist), nodes = nodes)

  gr = graphviz.backend(nodes, merged$arcs, groups = groups,
                        layout = layout, shape = shape, render = FALSE, edgeAttrs = edgeAttrs)
  ref.arcs = apply(arclist[[1]], 1, paste, collapse = "~")
  grlabels = names(graph::edgeRenderInfo(gr)[["splines"]])
  graph::edgeRenderInfo(gr)[["lty"]] = "solid"
  graphlist = vector(length(arclist), mode = "list")
  for (i in seq_along(arclist)) {
    gr.temp = gr
    edges.temp = graph::edgeRenderInfo(gr.temp)
    cur.arcs = arcs2grlabels(arclist[[i]])
    directed = which.directed(arclist[[i]], nodes)
    und.arcs = intersect(cur.arcs[!directed], grlabels)
    edges.temp[["arrowhead"]][und.arcs] = "none"
    edges.temp[["arrowtail"]][und.arcs] = "none"
    dir.arcs = grlabels[grlabels %in% arcs2grlabels(arclist[[i]][directed,
                                                                 , drop = FALSE], both = TRUE)]
    for (arc in dir.arcs) {
      if (arc %in% cur.arcs) {
        edges.temp[["direction"]][arc] = "forward"
        edges.temp[["arrowhead"]][arc] = "open"
        edges.temp[["arrowtail"]][arc] = "none"
      }
      else {
        edges.temp[["direction"]][arc] = "back"
        edges.temp[["arrowhead"]][arc] = "none"
        edges.temp[["arrowtail"]][arc] = "open"
        splines = edges.temp[["splines"]][[arc]]
        splines = rev(splines)
        edges.temp[["splines"]][[arc]] = splines
      }
    }
    if (diff == "none") {
      edges.temp[["col"]][] = "transparent"
      edges.temp[["col"]][und.arcs] = "black"
      edges.temp[["col"]][dir.arcs] = "black"
    }
    else if (diff == "from-first") {
      if (i == 1) {
        edges.temp[["col"]][] = "transparent"
        edges.temp[["col"]][und.arcs] = "black"
        edges.temp[["col"]][dir.arcs] = "black"
      }
      else {
        edges.temp[["col"]][] = "transparent"
        sorted = compare.backend(arclist[[1]], arclist[[i]],
                                 nodes, arcs = TRUE)
        dupes = sorted$fp[which.listed(sorted$fp, sorted$fn,
                                       either = TRUE), , drop = FALSE]
        sorted$fn = sorted$fn[!which.listed(sorted$fn,
                                            dupes, either = TRUE), , drop = FALSE]
        tp.labels = grlabels[grlabels %in% arcs2grlabels(sorted$tp,
                                                         both = TRUE)]
        if (length(tp.labels) > 0) {
          edges.temp[["col"]][tp.labels] = diff.args$tp.col
          edges.temp[["lty"]][tp.labels] = diff.args$tp.lty
          if ("tp.lwd" %in% names(diff.args))
            edges.temp[["lwd"]][tp.labels] = diff.args$tp.lwd
        }
        fp.labels = grlabels[grlabels %in% arcs2grlabels(sorted$fp,
                                                         both = TRUE)]
        if (length(fp.labels) > 0) {
          edges.temp[["col"]][fp.labels] = diff.args$fp.col
          edges.temp[["lty"]][fp.labels] = diff.args$fp.lty
          if ("fp.lwd" %in% names(diff.args))
            edges.temp[["lwd"]][fp.labels] = diff.args$fp.lwd
        }
        fn.labels.fwd = grlabels[grlabels %in% arcs2grlabels(sorted$fn)]
        fn.labels.bwd = grlabels[grlabels %in% arcs2grlabels(sorted$fn[,
                                                                       2:1, drop = FALSE])]
        if (length(c(fn.labels.fwd, fn.labels.bwd)) >
            0) {
          edges.temp[["col"]][c(fn.labels.fwd, fn.labels.bwd)] = diff.args$fn.col
          edges.temp[["lty"]][c(fn.labels.fwd, fn.labels.bwd)] = diff.args$fn.lty
          if ("fn.lwd" %in% names(diff.args))
            edges.temp[["lwd"]][c(fn.labels.fwd, fn.labels.bwd)] = diff.args$fn.lwd
        }
        both = names(which(edges.temp[["direction"]] ==
                             "both"))
        for (arc in both) {
          if (arc %in% setdiff(fn.labels.fwd, fn.labels.bwd)) {
            edges.temp[["direction"]][arc] = "forward"
            edges.temp[["arrowhead"]][arc] = "open"
            edges.temp[["arrowtail"]][arc] = "none"
          }
          else if (arc %in% setdiff(fn.labels.bwd, fn.labels.fwd)) {
            edges.temp[["direction"]][arc] = "back"
            edges.temp[["arrowhead"]][arc] = "none"
            edges.temp[["arrowtail"]][arc] = "open"
            splines = edges.temp[["splines"]][[arc]]
            splines = rev(splines)
            edges.temp[["splines"]][[arc]] = splines
          }
        }
      }
    }
    graph::edgeRenderInfo(gr.temp) = edges.temp
    graph::graphRenderInfo(gr.temp)$main = main[i]
    graph::graphRenderInfo(gr.temp)$sub = sub[i]
    graphlist[[i]] = gr.temp
    if ((i == 1) && (diff == "from-first") && !diff.args$show.first)
      next
    Rgraphviz::renderGraph(gr.temp)
  }
  invisible(graphlist)
}
graphviz.backend <- function (nodes, arcs, highlight = NULL, groups, arc.weights = NULL,
            layout = "dot", shape = "circle", main = NULL, sub = NULL,
            render = TRUE,
            edgeAttrs = NULL)
  {
    node.shapes = c("ellipse", "circle", "rectangle")
    highlight.params = c("nodes", "arcs", "col", "fill", "lwd",
                         "lty", "textCol")
    highlighting = FALSE
    check.label(layout, choices = graphviz.layouts, argname = "graph layout")
    if (!is.string(shape))
      stop("node shape must be a character string.")
    if (shape %!in% node.shapes)
      stop("valid node schapes are:", paste0(" '", node.shapes,
                                             "'"), ".")
    if (!is.null(arc.weights)) {
      if (!is.numeric(arc.weights))
        stop("arc weights must be numeric values.")
      if (length(arc.weights) != nrow(arcs))
        stop("mismatch between the number of weights and the number of arcs.")
    }
    if (!is.null(highlight) || length(highlight) > 0) {
      highlighting = TRUE
      if (!is.list(highlight) || any(names(highlight) %!in%
                                     highlight.params))
        stop("highlight must be a list with a subset of the following",
             " elements:", paste0(" '", highlight.params,
                                  "'"), ".")
      if ("nodes" %in% names(highlight))
        check.nodes(highlight$nodes, graph = nodes)
      if ("arcs" %in% names(highlight)) {
        highlight$arcs = check.arcs(highlight$arcs, nodes = nodes)
        if (nrow(highlight$arcs) == 0)
          highlight$arcs = NULL
      }
      if ("col" %in% names(highlight))
        check.colour(highlight$col)
      else highlight$col = "red"
      if ("fill" %in% names(highlight)) {
        if ("nodes" %!in% names(highlight))
          warning("no node to apply the 'fill' color to, ignoring.")
        check.colour(highlight$fill)
      }
      else highlight$fill = "transparent"
      if ("lwd" %in% names(highlight)) {
        if ("arcs" %!in% names(highlight))
          warning("no arc to apply the 'lwd' setting to, ignoring.")
        if (!is.positive(highlight$lwd))
          stop("the line width must be a positive number.")
      }
      if ("lty" %in% names(highlight)) {
        if ("arcs" %!in% names(highlight))
          warning("no arc to apply the 'lty' setting to, ignoring.")
        check.lty(highlight$lty)
      }
      if ("textCol" %in% names(highlight)) {
        if ("nodes" %!in% names(highlight))
          warning("no node to apply the 'textColor' color to, ignoring.")
        check.colour(highlight$textCol)
      }
      else highlight$textCol = "black"
    }
    graph.obj = new("graphNEL", nodes = nodes, edgeL = arcs2elist(arcs,
                                                                  nodes), edgemode = "directed")
    graph::graphRenderInfo(graph.obj)[["main"]] = main
    graph::graphRenderInfo(graph.obj)[["sub"]] = sub
    if (shape %in% c("ellipse", "rectangle")) {
      attrs = list(node = list(fixedsize = FALSE))
      node.attrs = list(shape = rep(shape, length(nodes)))
      names(node.attrs$shape) = nodes
    }
    else if (shape == "circle") {
      attrs = node.attrs = list()
    }
    if (!missing(groups) && !is.null(groups)) {
      check.node.groups(groups, graph = nodes)
      subGList = lapply(groups, function(g) {
        list(graph = graph::subGraph(g, graph.obj), cluster = TRUE)
      })
    }
    else {
      subGList = NULL
    }
    # browser()
    attrs$edge$fontsize <- 20
    attrs$edge$color <- "red"
    graph.plot = Rgraphviz::layoutGraph(graph.obj, subGList = subGList,
                                        attrs = attrs, nodeAttrs = node.attrs,
                                        edgeAttrs=edgeAttrs,
                                        layoutType = layout)
    u = names(which(graph::edgeRenderInfo(graph.plot)[["direction"]] ==
                      "both"))
    graph::edgeRenderInfo(graph.plot)[["arrowhead"]][u] = "none"
    graph::edgeRenderInfo(graph.plot)[["arrowtail"]][u] = "none"
    if (!is.null(arc.weights) && (length(arc.weights) > 0)) {
      to.weight = apply(arcs, 1, paste, collapse = "~")
      for (i in 1:length(to.weight)) {
        if (arc.weights[i] > 0)
          graph::edgeRenderInfo(graph.plot)[["lwd"]][to.weight[i]] = arc.weights[i]
        else graph::edgeRenderInfo(graph.plot)[["lty"]][to.weight[i]] = "dashed"
      }
    }
    if (highlighting) {
      if ("nodes" %in% names(highlight)) {
        graph::nodeRenderInfo(graph.plot)[["col"]][highlight$nodes] = highlight$col
        graph::nodeRenderInfo(graph.plot)[["fill"]][highlight$nodes] = highlight$fill
        graph::nodeRenderInfo(graph.plot)[["textCol"]][highlight$nodes] = highlight$textCol
      }
      if ("arcs" %in% names(highlight)) {
        to.highlight = apply(highlight$arcs, 1, paste, collapse = "~")
        graph::edgeRenderInfo(graph.plot)[["col"]][to.highlight] = highlight$col
        if ("lwd" %in% names(highlight))
          graph::edgeRenderInfo(graph.plot)[["lwd"]][to.highlight] = highlight$lwd
        if ("lty" %in% names(highlight))
          graph::edgeRenderInfo(graph.plot)[["lty"]][to.highlight] = highlight$lty
      }
    }
    if (render) {
      if (nrow(arcs) > 0) {
        Rgraphviz::renderGraph(graph.plot)
      }
      else {
        Rgraphviz::renderGraph(graph.plot, drawEdges = function(x) {
        })
      }
    }
    invisible(graph.plot)
  }
