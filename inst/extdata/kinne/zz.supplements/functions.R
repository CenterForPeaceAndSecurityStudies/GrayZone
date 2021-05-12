## A handful of additional helpful functions

## A funcction for summary statistics
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
    library(plyr)

    ## New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

    ## This does the summary. For each group's data frame, return a vector with
    ## N, mean, and sd
    datac <- ddply(data, groupvars, .drop=.drop,
                   .fun = function(xx, col) {
                       c(N    = length2(xx[[col]], na.rm=na.rm),
                         mean = mean   (xx[[col]], na.rm=na.rm),
                         sd   = sd     (xx[[col]], na.rm=na.rm)
                         )
                   },
                   measurevar
                   )

    ## Rename the "mean" column    
    datac <- rename(datac, c("mean" = measurevar))

    ## Calculate standard error of the mean
    datac$se <- datac$sd / sqrt(datac$N)

    ## Confidence interval multiplier for standard error
    ## Calculate t-statistic for confidence interval: 
    ## e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult

    return(datac)
}


#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#


## Heatmap style legend (mainly for maps)
legend.map <- function(col,ends,tsz,xoff,yoff,tit,toff,lgth){
    opar <- par
    ## Get current plotting dimensions
    bx <- par("usr")
    ## Set legend to cover middle portion of plot
    xrange <- c(.35 * bx[1], .35 * bx[2])
    print(xrange)
    yrange <- c(
        mean(bx[3] + bx[4]) + 0.025*(bx[3]),
        mean(bx[3] + bx[4]) + 0.025*(bx[4])
    )
    print(yrange)
    text(
        c(xrange[1]-xoff, xrange[2]+xoff),
        c(yrange[2]+yoff, yrange[2]+yoff),
        labels=ends,
        cex=tsz
    )
    text(mean(xrange),yrange[2]+toff,labels=tit,cex=(tsz*1.25))
    ## rasterImage needs 4 coordinates, in order: xleft, ybottom, xright, ytop
    ## Think of these as bottom left corner and top right corner
    ## Draw vertical, then reorient sideways using angle argument
    rasterImage(rev(as.raster(col)),
                ## Use these first two to "peg" the bottom left corner,
                ## around which we'll rotate the image
		xrange[1],
		yrange[1],
		xrange[1] + (yrange[2] - yrange[1]),
		yrange[1] + (xrange[2] - xrange[1]) + lgth, # 'lgth' is fudge factor
		angle=-90, interpolate=TRUE
		)
    par <- opar
    rm(opar, bx, xrange, yrange)
}


#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#


## A script to add a common legend to grid graphics
grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
    plots <- list(...)
    position <- match.arg(position)
    g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
    legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    lwidth <- sum(legend$width)
    gl <- lapply(plots, function(x) x + theme(legend.position="none"))
    gl <- c(gl, ncol = ncol, nrow = nrow)

    combined <- switch(position,
                       "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                              legend,
                                              ncol = 1,
                                              heights = unit.c(unit(1, "npc") - lheight, lheight)),
                       "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                             legend,
                                             ncol = 2,
                                             widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
    grid.newpage()
    grid.draw(combined)
}


#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#


## Get legend from a plot object
g_legend <- function(a.gplot) {
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    legend
}


#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#


## Helper function for viewport layouts
subplot <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
