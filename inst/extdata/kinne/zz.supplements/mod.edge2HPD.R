## Attribution: https://gist.github.com/Vessy

mod.edge2HPD <- function(edge_df = NULL, unique.rows = TRUE, axis.cols = NULL, type = "2D", desc = NULL, edge.weight = NULL, edge.color = NULL, node.color = NULL, node.size = NULL, node.radius = NULL, node.axis = NULL) 
{
    ##edge.weight - a list corresponding to edge weights (same order as in edge_df)
    ##edge.color - a lis corresponding to edge colors (same order as in edge_df)
    ##node.color - a data frame consisting of two columns: column 1 - node labels, column 2 - node color
    ##node.size - a data frame consisting of two columns: column 1 - node labels, column 2 - node size
    ##node.radius - a data frame consisting of two columns: column 1 - node labels, column 2 - node radius
    ##node.axis - a data frame consisting of two columns: column 1 - node labels, column 2 - node axis
    
    if (is.null(edge_df)){
        stop("No edge data provided")
    }
    if (!is.data.frame(edge_df)){
        stop("edge_df is not a data frame")
    }
    if (unique.rows)
    {
        nr.old <- nrow(edge_df)
        edge_df <- unique(edge_df)
        
        if (nr.old > nrow(edge_df))
            cat("\n\t", nr.old - nrow(edge_df), "non-unique data-frame rows removed!\n\n")
    }
    
    ## Get node labels
    lab1 <- as.character(unlist(edge_df[, 1]))
    lab2 <- as.character(unlist(edge_df[, 2]))
    
    
    ## Get number of unique nodes
    nn <- length(unique(c(lab1, lab2)))
    
    ## Define node ID
    id <- 1:nn
    ## Define node label
    label <- unique(c(lab1, lab2))
    ## Create a data frame for node attributes
    node.attributes <- data.frame(id, label)
    
    ##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
    ## Node size definition
    if (!is.null(node.size))
    {
        if (is.numeric(node.size[, 2]) | is.integer(node.size[, 2]))
        {
            nSize <- c()
            
            for (i in 1:length(label))
            {
                indx <- which(as.character(node.size[,1]) == label[i])
                
                if (length(indx[1]) != 0)
                    nSize = c(nSize, node.size[indx[1],2])
                else
                {
                    msg <- paste("No size data provided for the node ",
                                 nodes$id[n],
                                 ". Value 1 will be assigned to this node!",
                                 sep = "")
                    warning(msg)
                    nSize = c(nSize, 1)
                }
            }
            
            node.attributes <- cbind(node.attributes, size = nSize)
            rm(i, nSize, indx)
        }#is.numeric
        else{
            stop("Node size is not numeric or integer.")  
        }
    }#is.null
    
    if (is.null(node.size))
    {
        warning("No data provided for the node size. All nodes will be assigned size 1!")
        node.attributes <- cbind(node.attributes, size = rep(1, nn))
    }
    
    ##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
    ## Node color definition
    
    if (!is.null(node.color))
    {
        nCol <- c()
        
        for (i in 1:length(label))
        {
            indx <- which(as.character(node.color[,1]) == label[i])
            
            if (length(indx[1]) != 0)
                nCol = c(nCol, as.character(node.color[indx[1],2]))
            else
            {
                msg <- paste("No color data provided for the node ",
                             nodes$id[n],
                             ". Black color will be assigned to this node!",
                             sep = "")
                warning(msg)
                nCol = c(nCol, "black")
            }
        }
        
        node.attributes <- cbind(node.attributes, color = nCol)
        rm(i, nCol, indx)
    }#is.null
    
    if (is.null(node.color))
    {
        warning("No data provided for the node color. All nodes will be colored black!")
        node.attributes <- cbind(node.attributes, color = as.character(rep("black", nn)))
    }
    
    ##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
    ## Node radius definition

    if (!is.null(node.radius))
    {
        if (is.numeric(node.radius[, 2]) | is.integer(node.radius[, 2]))
        {
            nSize <- c()
            
            for (i in 1:length(label))
            {
                indx <- which(as.character(node.radius[,1]) == label[i])
                
                if (length(indx[1]) != 0)
                    nSize = c(nSize, node.radius[indx[1],2])
                else
                {
                    msg <- paste("No raidus data provided for the node ",
                                 nodes$id[n],
                                 ". Random values will be assigned!",
                                 sep = "")
                    warning(msg)
                    nSize = c(nSize,  sample(nn, 1))
                }
            }
            
            node.attributes <- cbind(node.attributes, radius = nSize)
            rm(i, nSize, indx)
        }#is.numeric
        else{
            stop("Node raidus is not integer.")  
        }
    }#is.null
    
    if (is.null(node.radius))
    {
        warning("No data provided for the node radius. All nodes will be assigned random radius values")
        node.attributes <- cbind(node.attributes, radius = sample(nn, nn))
    }
    
    ##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
    ## Node axis definition
    
    if (!is.null(node.axis))
    {
        if (is.integer(node.axis[, 2]))
        {
            nSize <- c()
            
            for (i in 1:length(label))
            {
                indx <- which(as.character(node.axis[,1]) == label[i])
                
                if (length(indx[1]) != 0)
                    nSize = c(nSize, node.axis[indx[1],2])
                else
                {
                    msg <- paste("No axis data provided for the node ",
                                 nodes$id[n],
                                 ". This node will be assigned to axis 1!",
                                 sep = "")
                    warning(msg)
                    nSize = c(nSize,  1)
                }
            }
            
            node.attributes <- cbind(node.attributes, axis = nSize)
            rm(i, nSize, indx)
        }#is.integer
        else{
            stop("Node axis is not integer.")  
        }
    }#is.null
    
    if (is.null(node.axis))
    {
        warning("No data provided for the node axis. All nodes will be assigned to axis 1")
        node.attributes <- cbind(node.attributes, axis = rep(1, nn))
    }

    ##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
    
    ## Create HPD object
    HPD <- list()
    
    ## Define node attributes
    HPD$nodes$id <- as.integer(node.attributes$id)
    HPD$nodes$lab <- as.character(node.attributes$label)
    HPD$nodes$axis <- as.integer(node.attributes$axis)
    HPD$nodes$radius <- as.numeric(node.attributes$radius)
    HPD$nodes$size <- as.numeric(node.attributes$size)
    HPD$nodes$color <- as.character(node.attributes$color)
    
    ##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
    
    ## Get number of edges
    ne <- nrow(edge_df)
    
    ##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
    ## Edge weight definition
    
    if (!(is.null(edge.weight))) 
    {
        if (length(edge.weight) != nrow(edge_df))
            stop("Edge weights are not provided for all edges!") 
        
        if (is.numeric(edge.weight) | is.integer(edge.weight))
            edge_df <- cbind(edge_df, weight = edge.weight)
        else
            stop("Edge weight column is not numeric or integer.")  
    } 

    if (is.null(edge.weight))
    {
        warning("No edge weight provided Setting default edge weight to 1")
        edge_df <- cbind(edge_df, weight = rep(1, ne))
    }
    
    ##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
    ## Edge color definition
    
    if (!(is.null(edge.color))) 
    {
        if (length(edge.color) != nrow(edge_df))
            stop("Edge colors are not provided for all edges!") 
        else 
            edge_df <- cbind(edge_df, color = as.character(edge.color))
    } 
    
    if (is.null(edge.color))
    {
        warning("No edge color provided. Setting default edge color to gray")
        edge_df <- cbind(edge_df, color = rep("gray", ne))
    }
    
    ##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
    ## Set up edge list
    ## Merge by default sorts things and changes the order of edges,
    ## so edge list has to stay paired
    edge.hlp <- merge(edge_df, node.attributes[, 1:2], by.x = 1, by.y = "label")
    edge <- merge(edge.hlp, node.attributes[1:2], by.x = 2, by.y = "label")
    
    HPD$edges$id1 <- as.integer(edge$id.x)
    HPD$edges$id2 <- as.integer(edge$id.y)
    
    HPD$edges$weight <- as.numeric(edge$weight)
    HPD$edges$color <- as.character(edge$color)
    
    HPD$nodes <- as.data.frame(HPD$nodes)
    HPD$edges <- as.data.frame(HPD$edges)
    
    ## Add description
    if (is.null(desc)) {
        desc <- "No description provided"
    }
    HPD$desc <- desc
    
    ## Define axis columns
    if (is.null(axis.cols)){
        axis.cols <- brewer.pal(length(unique(HPD$nodes$axis)), "Set1")
    }

    
    HPD$axis.cols <- axis.cols
    HPD$nodes$axis <- as.integer(HPD$nodes$axis)
    HPD$nodes$size <- as.numeric(HPD$nodes$size)
    HPD$nodes$color <- as.character(HPD$nodes$color)
    HPD$nodes$lab <- as.character(HPD$nodes$lab)
    HPD$nodes$radius <- as.numeric(HPD$nodes$radius)
    HPD$nodes$id <- as.integer(HPD$nodes$id)
    HPD$edges$id1 <- as.integer(HPD$edges$id1)
    HPD$edges$id2 <- as.integer(HPD$edges$id2)
    HPD$edges$weight <- as.numeric(HPD$edges$weight)
    HPD$edges$color <- as.character(HPD$edges$color)
    HPD$type <- type
    
    class(HPD) <- "HivePlotData"
    
    ## Check HPD object
    chkHPD(HPD)
    return (HPD)
}
