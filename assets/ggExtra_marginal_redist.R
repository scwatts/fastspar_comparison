# ggExtra/R/ggMarginalHelpers.R
# YEAR: 2015
# COPYRIGHT HOLDER: Dean Attali

toParamList <- function(exPrm, xPrm, yPrm) {
  
  if (is.null(exPrm[['colour']]) && 
      is.null(exPrm[['color']]) && 
      is.null(exPrm[['col']])) {
    exPrm[['colour']] <- "black"
  }
  
  if (is.null(exPrm[['fill']])) {
    exPrm[['fill']] <- "grey"
  }
  
  list(
    exPrm = exPrm,
    xPrm = xPrm,
    yPrm = yPrm
  )
}

reconcileScatPlot <- function(p, data, x, y) {
  
  if (missing(p)) {
    if (missing(data) || missing(x) || missing(y)) {
      stop("`data`, `x`, and `y` must be provided if `p` is not provided",
           call. = FALSE)
    }
    p <- ggplot2::ggplot(data, ggplot2::aes_string(x, y)) + 
      ggplot2::geom_point()
  }
  p
}

wasFlipped <- function(scatPbuilt) {
  classCoord <- class(scatPbuilt$plot$coordinates)
  any(grepl("flip", classCoord, ignore.case = TRUE))
}

getVarDF <- function(scatPbuilt, marg) {
  
  if (wasFlipped(scatPbuilt = scatPbuilt)) {
    marg <- switch(marg,
                   "x" = "y",
                   "y" = "x"
    )
  }
  var <- scatPbuilt[["data"]][[1]][[marg]]
  
  data.frame(var = var)
}

needsFlip <- function(marg, type) {
  
  # If the marginal plot is: (for the x margin (top) and is a boxplot) or 
  #                          (for the y margin (right) and is not a boxplot), 
  # ... then have to flip
  topAndBoxP <- marg == "x" && type == "boxplot"
  rightAndNonBoxP <- marg == "y" && type != "boxplot"
  topAndBoxP || rightAndNonBoxP
}

margPlotNoGeom <- function(marg, type, data) {
  
  # Build plot (sans geom). Note: Boxplot is the only plot type that needs y aes
  if (type == "boxplot") {
    plot <- ggplot2::ggplot(data = data, 
                            ggplot2::aes_string(x = 'var', y = 'var'))
  } else {
    plot <- ggplot2::ggplot(data = data, ggplot2::aes_string(x = 'var'))
  }
  
  if (needsFlip(marg = marg, type = type)) {
    plot <- plot +  ggplot2::coord_flip()
  }
  
  plot
}

alterParams <- function(marg, type, prmL, scatPbuilt) {
  
  # merge the parameters in an order that ensures that marginal plot params 
  # overwrite general params
  prmL$exPrm <- append(prmL[[paste0(marg, "Prm")]], prmL$exPrm)
  prmL$exPrm <- prmL$exPrm[!duplicated(names(prmL$exPrm))]
  
  # pull out limit function and use if histogram
  panScale <- getPanelScale(marg = marg, builtP = scatPbuilt)
  lim_fun <- panScale$get_limits
  if (type == "histogram" && !is.null(lim_fun)) {
    prmL$exPrm[["boundary"]] <- lim_fun()[1]
  } 
  
  # we're using geom_line for the density plot so that there will be no bottom line...
  # ...so we have to tell geom_line to use non-default stat (stat = density)
  if (type == "density") {
    prmL$exPrm[['stat']] <- "density"
  }
  
  prmL$exPrm
}

getPanelScale <- function(marg, builtP) {
  above_221 <- utils::packageVersion("ggplot2") > "2.2.1"
  if (above_221) {
    if (marg == "x") {
      builtP$layout$panel_scales_x[[1]]
    } else {
      builtP$layout$panel_scales_y[[1]]
    }
  } else {
    if (marg == "x") {
      builtP$layout$panel_scales$x[[1]]
    } else {
      builtP$layout$panel_scales$y[[1]]
    }
  }
}

getGeomFun <- function(type) {
  switch (type,
          "density" = ggplot2::geom_line,
          "histogram" = ggplot2::geom_histogram,
          "boxplot" = ggplot2::geom_boxplot
  )
}

# Wrapper function to create a "raw" marginal plot
genRawMargPlot <- function(marg, type, scatPbuilt, prmL) {
  data <- getVarDF(scatPbuilt = scatPbuilt, marg = marg)
  noGeomPlot <- margPlotNoGeom(marg = marg, type = type, data = data)
  finalParms <- alterParams(marg = marg, type = type, prmL = prmL, 
                            scatPbuilt = scatPbuilt)
  geomFun <- getGeomFun(type = type)
  layer <- do.call(geomFun, finalParms)
  noGeomPlot + layer
}

# Wrapper function to create a "final" marginal plot
genFinalMargPlot <- function(marg, type, scatPbuilt, prmL) {
  rawMarg <- genRawMargPlot(marg = marg, type = type, scatPbuilt = scatPbuilt, 
                            prmL = prmL)
  margThemed <- addMainTheme(rawMarg = rawMarg, marg = marg, 
                             scatPTheme = scatPbuilt$plot$theme)
  margThemed + getScale(marg = marg, type = type, builtP = scatPbuilt)
}

# Given a plot, copy some theme properties from the main plot so that they will
# resemble each other more and look better beside each other, and also add
# some common theme properties such as 0 margins and transparent text colour
addMainTheme <- function(rawMarg, marg, scatPTheme) {
  try(
    {rawMarg <- rawMarg + ggplot2::theme_void()},
    silent = TRUE
  )
  
  # copy theme from main plot
  themeProps <- c("text",
                  "axis.text","axis.text.x", "axis.text.y",
                  "axis.ticks", "axis.ticks.length",
                  "axis.title", "axis.title.x", "axis.title.y",
                  "plot.title"
  )
  for(property in themeProps) {
    rawMarg$theme[[property]] <- scatPTheme[[property]]
  }
  
  # make text and line colours transparent
  transparentProps <- c("text",
                        "axis.text", "axis.text.x", "axis.text.y",
                        "axis.ticks",
                        "axis.title", "axis.title.x", "axis.title.y",
                        "line")
  
  for(property in transparentProps) {
    
    if (!is.null(rawMarg$theme[[property]])) {
      rawMarg$theme[[property]]$colour <- "transparent"
    } else if (property %in% c("axis.ticks", "line")) {
      themePair <- list()
      themePair[[property]] <- ggplot2::element_line(colour = "transparent")
      rawMarg <- rawMarg + do.call(ggplot2::theme, themePair)
    } else {
      themePair <- list()
      themePair[[property]] <- ggplot2::element_text(colour = "transparent")
      rawMarg <- rawMarg + do.call(ggplot2::theme, themePair)
    }
    
  }
  
  # some more theme properties
  rawMarg <- rawMarg +
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      axis.ticks.length = grid::unit(0, "null")
    )
  
  # since the tick marks are removed on the marginal plot, we need to add
  # space for them so that the marginal plot will align with the main plot
  if (is.null(scatPTheme$axis.ticks.length)) {
    marginUnit <- "null"
    marginLength <- 0
  } else {
    marginUnit <- attr(scatPTheme$axis.ticks.length, "unit")
    marginLength <- as.numeric(scatPTheme$axis.ticks.length, "unit")
  }
  
  if (marg == "x") {
    rawMarg <- rawMarg + 
      ggplot2::theme(
        axis.title.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        plot.margin = grid::unit(c(0, 0, 0, marginLength), marginUnit)
      )
  } else {
    rawMarg <- rawMarg + 
      ggplot2::theme(
        axis.title.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        plot.margin = grid::unit(c(0, 0, marginLength, 0), marginUnit)
      )
  }
  
  rawMarg
}

# Copy the scale transformation from the original plot (reverse/log/limits/etc)
# We have to do a bit of a trick on the marginal plot that's flipped by
# taking the original x/y scale and manually changing it to the other axis
getScale <- function(marg, type, builtP) {
  
  scale <- getPanelScale(marg = marg, builtP = builtP)
  
  if (needsFlip(marg = marg, type = type)) {
    scale$aesthetics <- gsub("^x", "y", scale$aesthetics)
  }
  
  scale
}

# Get the axis range of the x or y axis of the given ggplot build object
# This is needed so that if the range of the plot is manually changed, the
# marginal plots will use the same range
getLimits <- function(marg, builtP) {
  
  scale <- getPanelScale(marg = marg, builtP = builtP)
  
  range <- scale$limits
  if (is.null(range)) {
    range <- scale$range$range
  }
  range
}

# Helper functions for appending the tableGrob that represents the scatter-plot
# (i.e., the main plot, p) with the marginal plots - one for the x margin and
# one for the y margin (x margin = top plot, y margin = right plot)
getPanelPos <- function(gtableGrob) {
  layDF <- gtableGrob$layout
  panelPos <- layDF[layDF$name == "panel", c("t", "l", "b", "r")]
  panelPos
}

getMargGrob <- function(margPlot) {
  margG <- ggplot2::ggplotGrob(margPlot)
  cleanMargG <- gtable::gtable_filter(margG, pattern = "panel")
  cleanMargG
}

addTopMargPlot <- function(ggMargGrob, top, size) {
  panelPos <- getPanelPos(gtableGrob = ggMargGrob)
  topMargG <- getMargGrob(margPlot = top)
  gt <- gtable::gtable_add_rows(x = ggMargGrob, 
                                heights = grid::unit(1/size, "null"), pos = 0)
  gt <- gtable::gtable_add_grob(x = gt, grobs = topMargG, t = 1, b = 1, 
                                l = panelPos[["l"]], r = panelPos[["r"]], 
                                z = Inf, clip = "on", name = "topMargPlot")
  gt
}

addRightMargPlot <- function(ggMargGrob, right, size) {
  panelPos <- getPanelPos(gtableGrob = ggMargGrob)
  rightMargG <- getMargGrob(margPlot = right)
  gt <- gtable::gtable_add_cols(x = ggMargGrob, 
                                widths = grid::unit(1/size, "null"),
                                pos = -1)
  gt <- gtable::gtable_add_grob(x = gt, grobs = rightMargG, t = panelPos[["t"]], 
                                b = panelPos[["b"]], r = ncol(gt), l = ncol(gt),
                                z = Inf, clip = "on", name = "rightMargPlot")
  gt
}

# Pull out the title and subtitle grobs for a plot, after we have checked to 
# make sure there is a title. Note: plot.title and plot.subtitle will actually
# always exist (I believe) in recent versions of ggplot2, even if the user 
# doesn't specify a title/subtitle. In these cases, the title/subtitle grobs
# will be "zeroGrobs." However, a 'label' won't exist 
# (i.e, !is.null(pb$plot$labels$title) will be true) when there is no title,
# so it's not like we will be needlessly adding zeroGrobs to our plot (though
# it wouldn't be a problem, even if we did add the zeroGrobs - it would just take 
# a little longer.
getTitleGrobs <- function(p) {
  grobs <- ggplot2::ggplotGrob(p)$grobs
  gindTitle <- sapply(grobs, function(x) {
    grepl(pattern = "plot\\.title", x$name)
  })
  gindSub <- sapply(grobs, function(x) {
    grepl(pattern = "plot\\.subtitle", x$name)
  })
  list(
    titleG = grobs[gindTitle][[1]],
    subTitleG = grobs[gindSub][[1]]
  )
}

# Helper function for addTitleGrobs
rbindGrobs <- function(topGrob, gtable, l, r) {
  topH <- grid::grobHeight(topGrob)
  gt_t <- gtable::gtable_add_rows(x = gtable, heights = topH, pos = 0)
  gtable::gtable_add_grob(x = gt_t, grobs = topGrob, t = 1, b = 1,
                          l = l, r = r, z = Inf)
}

# Add the title/subtitle grobs to the main ggextra plot, along with a little 
# padding
addTitleGrobs <- function(ggxtraNoTtl, titleGrobs) {
  layout <- ggxtraNoTtl$layout
  l <- layout[layout$name == "panel", "l"]
  spacerGrob <- grid::rectGrob(height = grid::unit(.2, "cm"), 
                               gp = grid::gpar(col = "white", fill = NULL))
  plotWSpace <- rbindGrobs(topGrob = spacerGrob, gtable = ggxtraNoTtl,
                           l = l, r = l)
  plotWSubTitle <- rbindGrobs(topGrob = titleGrobs$subTitleG, 
                              gtable = plotWSpace, l = l, r = l)
  rbindGrobs(topGrob = titleGrobs$titleG,
             gtable = plotWSubTitle, l = l, r = l)
}


# ggExtra/R/ggMarginal.R
# YEAR: 2015
# COPYRIGHT HOLDER: Dean Attali

ggMarginal <- function(p, data, x, y, type = c("density", "histogram", "boxplot"),
                       margins = c("both", "x", "y"), size = 5,
                       ..., xparams = list(), yparams = list()) {
  
  # Figure out all the default parameters.
  type <- match.arg(type)
  margins <- match.arg(margins)
  
  # Fill in param defaults and consolidate params into single list (prmL).
  prmL <- toParamList(exPrm = list(...), xPrm = xparams, yPrm = yparams)
  
  # After ggplot2 v1.0.1, layers became strict about parameters
  if (type == "density") {
    prmL[['exPrm']][['fill']] <- NULL
  }
  
  # Create one version of the scat plot (scatP), based on values of p, data, x, 
  # and y...also remove all margin around plot so that it's easier to position 
  # the density plots beside the main plot
  scatP <- reconcileScatPlot(p = p, data = data, x = x, y = y) + 
    ggplot2::theme(plot.margin = grid::unit(c(0, 0, .25, .25), "cm"))
  
  # Decompose scatP to grab all sorts of information from it
  scatPbuilt <- ggplot2::ggplot_build(scatP)
  
  # Pull out the plot title if one exists and save it as a grob for later use.
  hasTitle <- (!is.null(scatPbuilt$plot$labels$title))
  if (hasTitle) {
    titleGrobs <- getTitleGrobs(p = p)
    scatP$labels$title <- NULL
    scatP$labels$subtitle <- NULL
  }
  
  # Create the margin plots by calling genFinalMargPlot
  # In order to ensure the marginal plots line up nicely with the main plot,
  # several things are done:
  # - Use the same label text size as the original 
  # - Remove the margins from all plots
  # - Make all text in marginal plots transparent
  # - Remove all lines and colours from marginal plots
  # - Use the same axis range as the main plot
  
  # If margins = x or 'both' (x and y), then you have to create top plot
  # Top plot = horizontal margin plot, which corresponds to x marg
  if (margins != "y") { 
    top <- genFinalMargPlot(marg = "x", type = type, scatPbuilt = scatPbuilt, 
                            prmL = prmL)
  }
  
  # If margins = y or 'both' (x and y), then you have to create right plot. 
  # (right plot = vertical margin plot, which corresponds to y marg)
  if (margins != "x") { 
    right <- genFinalMargPlot(marg = "y", type = type, scatPbuilt = scatPbuilt, 
                              prmL = prmL)
  }
  
  # Now add the marginal plots to the scatter plot
  pGrob <- ggplot2::ggplotGrob(scatP)
  
  withCallingHandlers({
    suppressMessages({
      if (margins == "both") {
        ggxtraTmp <- addTopMargPlot(ggMargGrob = pGrob, top = top, size = size)
        ggxtraNoTtl <- addRightMargPlot(ggMargGrob = ggxtraTmp, right = right, 
                                        size = size)
      } else if (margins == "x") {
        ggxtraTmp <- gtable::gtable_add_padding(x = pGrob, 
                                                grid::unit(c(0, 0.5, 0, 0), 
                                                           "lines"))
        ggxtraNoTtl <- addTopMargPlot(ggMargGrob = ggxtraTmp, top = top, 
                                      size = size)
      } else if (margins == "y") {
        ggxtraTmp <- gtable::gtable_add_padding(x = pGrob, 
                                                grid::unit(c(0.5, 0, 0, 0), 
                                                           "lines"))
        ggxtraNoTtl <- addRightMargPlot(ggMargGrob = ggxtraTmp, right = right,
                                        size = size)
      }
    })
  }, warning = function(w) {
    if (grepl("did you forget aes", w, ignore.case = TRUE)) 
      invokeRestart("muffleWarning")
  })
  
  # Add the title to the resulting ggExtra plot if it exists
  if (hasTitle) {
    ggExtraPlot <- addTitleGrobs(ggxtraNoTtl = ggxtraNoTtl, 
                                 titleGrobs = titleGrobs)
  } else {
    ggExtraPlot <- ggxtraNoTtl
  }
  # Add a class for S3 method dispatch for printing the ggExtra plot
  class(ggExtraPlot) <- c("ggExtraPlot", class(ggExtraPlot))
  ggExtraPlot
}

#' Print a ggExtraPlot object
#' 
#' \code{ggExtraPlot} objects are created from \code{ggMarginal}. This is the S3
#' generic print method to print the result of the scatterplot with its marginal
#' plots.
#' 
#' @param x ggExtraPlot object.
#' @param newpage Should a new page (i.e., an empty page) be drawn before the 
#' ggExtraPlot is drawn?
#' @param ... ignored
#' @seealso \code{\link{ggMarginal}}
#' @export
#' @keywords internal
print.ggExtraPlot <- function(x, newpage = grDevices::dev.interactive(), ...) {
  if (newpage) grid::grid.newpage()
  grid::grid.draw(x)
}
