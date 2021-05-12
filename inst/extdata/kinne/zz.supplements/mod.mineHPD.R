## Attribution: https://gist.github.com/Vessy

mod.mineHPD <- function(HPD, option = "", radData = NULL) 
{
    edges <- HPD$edges
    nodes <- HPD$nodes
    nn <- length(nodes$id)   
    
    ##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
    
    if (option == "axis <- source.man.sink") {
        
        ## A change that allows this function to be used for undirected graphs
        ## Now all nodes will be assigned to an axis
        
        done <- FALSE # a check to make sure all nodes get an axis
        
        for (n in 1:nn) {    
            id1 <- which(n ==edges$id1)
            id2 <- which(n ==edges$id2)
            
            if ((length(id1) == 0) & (length(id2) > 0 )) {
                nodes$axis[n] <- 2
                done <- TRUE
                next
            } # these are sinks, as they only receive an edge
            
            ## note that set operations below drop duplicate values
            
            ## Change 1 starts here
            if (length(id1) > 0)
            {
                if (length(id2) == 0)
                {
                    nodes$axis[n] <- 1
                    done <- TRUE
                    next
                }        
                else
                {
                    ## Change 1 ends here
                    common <- union(id1, id2)          
                    source <- setdiff(id1, common)
                    if (length(source) == 1) {
                        nodes$axis[n] <- 1
                        done <- TRUE
                        next		
                    } # these are sources
                    
                    if (length(common) >= 1) {
                        nodes$axis[n] <- 3
                        done <- TRUE
                        next		
                    } # these are managers
                }
            } 
            
            if (!done) {
                msg <- paste("node ", nodes$id[n], " was not assigned to an axis", sep = "")
                warning(msg)
            }  # alert the user there was a problem
            
        } # end of loop inspecting nodes
        
        nodes$axis <- as.integer(nodes$axis)
        
    }  ##### end of option == "axis <- source.man.sink
    
    ##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
    
    if (option == "rad <- random") {
        
        ## This option assigns a random radius value to a node
        
        for (n in 1:nn)           
            nodes$radius[n] <- sample(1:nn, 1)
        
    }  ##### end of option == "rad <- random"
    
    ##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
    
    if (option == "rad <- userDefined") {
        
        ## This option assigns a radius value to a node
        ## based upon user specified values.
        
        if (is.null(radData)){
            stop("No edge data provided")
        }
        
        if (length(intersect(as.character(radData[,1]), as.character(nodes$lab))) == 0){
            stop("Provided data does not contain correct node labels")
        }          
        
        for (n in 1:nn)           
        {
            indexHlp <- which(as.character(radData[,1]) == nodes$lab[n])
            
            if (length(indexHlp) != 0)        
                nodes$radius[n] <- radData[indexHlp[1], 2]
            else
            {
                msg <- paste("No data provided for the node ", nodes$id[n], ". Value 1 will be assigned to this node!", sep = "")
                warning(msg)
                nodes$radius[n] <- 1
            }
        }
    }  ##### end of option == "rad <- userDefined"
    
    ##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
    
    if (option == "axis <- deg_one_two_more") 
    {
        
        ## This option assigns a node to an axis
        ## based upon whether its degree is 1, 2, or greater than two
        ##     
        ## degree 1 = axis 1, degree 2 = axis 2, degree >2 = axis3
        
        done <- FALSE # a check to make sure all nodes get an axis
        
        for (n in 1:nn) 
        {    
            id1 <- which(n ==edges$id1)
            id2 <- which(n ==edges$id2)         
            
            if ((length(id1) + length(id2)) == 1)
            {
                nodes$axis[n] <- 1
                done <- TRUE
                next
            } 
            
            if ((length(id1) + length(id2)) == 2)
            {
                nodes$axis[n] <- 2
                done <- TRUE
                next
            } 
            
            if ((length(id1) + length(id2)) > 2)
            {
                nodes$axis[n] <- 3
                done <- TRUE
                next
            }                 
            
            if (!done) {
                msg <- paste("node ", nodes$id[n], " was not assigned to an axis", sep = "")
                warning(msg)
            }  # alert the user there was a problem
            
        } # end of loop inspecting nodes
        
        nodes$axis <- as.integer(nodes$axis)
        
    }  ##### end of option == "axis <- deg_1_2_more
    
    ##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
    
    if (option == "axis <- deg_five_ten_more") 
    {
        
        ## This option assigns a node to an axis
        ## based upon whether its degree is <=5, 6-10, or greater than 10
        ##     
        ## degree <=5 = axis 1, degree between 6 and 10 = axis 2, degree >10 = axis32
        
        done <- FALSE # a check to make sure all nodes get an axis
        
        for (n in 1:nn) 
        {    
            id1 <- which(n ==edges$id1)
            id2 <- which(n ==edges$id2)         
            
            if ((length(id1) + length(id2)) <= 5)
            {
                nodes$axis[n] <- 1
                done <- TRUE
                next
            } 
            
            if (((length(id1) + length(id2)) > 5) & ((length(id1) + length(id2)) <= 10))
            {
                nodes$axis[n] <- 2
                done <- TRUE
                next
            } 
            
            if ((length(id1) + length(id2)) > 10)
            {
                nodes$axis[n] <- 3
                done <- TRUE
                next
            }                 
            
            if (!done) {
                msg <- paste("node ", nodes$id[n], " was not assigned to an axis", sep = "")
                warning(msg)
            }  # alert the user there was a problem
            
        } # end of loop inspecting nodes
        
        nodes$axis <- as.integer(nodes$axis)
        
    }  ##### end of option == "axis <- deg_five_ten_more"
    
    ##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
    
    if (option == "remove axis edge") {
        
        ## This option removes edges which start and end on the same axis
        ## It re-uses code from sumHPD
        
        ## Create a list of edges to be drawn
        
        n1.lab <- n1.rad <- n2.lab <- n2.rad <- n1.ax <- n2.ax <- c()
        
        for (n in 1:(length(HPD$edges$id1))) {
            i1 <- which(HPD$edges$id1[n] == HPD$nodes$id)
            i2 <- which(HPD$edges$id2[n] == HPD$nodes$id)
            n1.lab <- c(n1.lab, HPD$nodes$lab[i1])
            n2.lab <- c(n2.lab, HPD$nodes$lab[i2])
            n1.rad <- c(n1.rad, HPD$nodes$radius[i1])
            n2.rad <- c(n2.rad, HPD$nodes$radius[i2])
            n1.ax <- c(n1.ax, HPD$nodes$axis[i1])
            n2.ax <- c(n2.ax, HPD$nodes$axis[i2])
        }
        
        fd <- data.frame(
            n1.id = HPD$edges$id1,
            n1.ax,
            n1.lab,
            n1.rad,
            n2.id = HPD$edges$id2,
            n2.ax,
            n2.lab,
            n2.rad,
            e.wt = HPD$edges$weight,
            e.col = HPD$edges$color)  	
        
        prob <- which(fd$n1.ax == fd$n2.ax)
        if (length(prob) == 0) cat("\n\t No edges were found that start and end on the same axis\n")
        if (length(prob) > 0) {
            edges <- edges[-prob,]
            cat("\n\t", length(prob), "edges that start and end on the same axis were removed\n")
        }
        
    }  ##### end of option == "remove axis edge"

    ##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
    
    if (option == "axis <- split") {
        
        ## This option splits all axes into 2 new axes 
        ## It can be used to address the "edge on the same axis" issue
        ## This option may increase the number of nodes - a single node
        ## from the parent axis may appear on 2 "children" axes
        
        nodesNew <- nodes
        nodesOld <- nodes
        
        nAxes <- unique(nodes$axis)
        numAxes <- length(nAxes)

        ## Renumerate axes
        for (i in numAxes:1)
            nodesOld[which(nodesOld$axis == nAxes[i]), "axis"] <- as.integer(2*nAxes[i] - 1)
        
        
        ## Duplicate nodes 
        ## Renumerate axes
        for (i in numAxes:1)
            nodesNew[which(nodesNew$axis == nAxes[i]), "axis"] <- as.integer(2*nAxes[i])
        
        ## Re-numerate node ids
        nodesNew$id <- nodesNew$id + nn
        
        ## Duplicated set of nodes with correct axis and node ids
        nodes <- rbind(nodesOld, nodesNew)
        rm(nodesOld, nodesNew)
        
        ## Now create duplicated set of edges and re-numerate node ids for interactions
        edgesNew1 <- edges
        edgesNew1$id1 <- edgesNew1$id1 + nn
        edgesNew1$id2 <- edgesNew1$id2 + nn
        
        edgesNew2 <- edges
        edgesNew2$id1 <- edgesNew2$id1 + nn
        
        edgesNew3 <- edges
        edgesNew3$id2 <- edgesNew3$id2 + nn
        
        edges <- rbind(edges, edgesNew1, edgesNew2, edgesNew3)
        
        nodesAxis <- nodes[, c("id", "axis")]
        
        edgesHlp <- merge(edges, nodesAxis, by.x = "id1", by.y = "id")
        edges <- merge(edgesHlp, nodesAxis, by.x = "id2", by.y = "id")
        
        edgesOK <- edges[((edges$axis.x == 1) & (edges$axis.y == 2*numAxes)) | ((edges$axis.x == 2*numAxes) & (edges$axis.y == 1)), ]
        edgesHlp <- edgesOK

        if (numAxes > 1)
            for (i in 1:(numAxes - 1))
            {
                edgesOK <- edges[((edges$axis.x == 2*i) & (edges$axis.y == (2*i + 1))) | ((edges$axis.x == (2*i + 1)) & (edges$axis.y == 2*i)), ]
                edgesHlp <- rbind(edgesHlp, edgesOK)
            }

        for (i in 1:numAxes)
        {
            edgesOK <- edges[((edges$axis.x == (2*i - 1)) & (edges$axis.y == 2*i)) | ((edges$axis.x == 2*i) & (edges$axis.y == (2*i - 1))), ]
            edgesHlp <- rbind(edgesHlp, edgesOK)
        }

        edges <- edgesHlp[, 1:4]
        
        unique.ids <- unique(c(edges$id1, edges$id2))
        
        nodes <- nodes[nodes$id %in% unique.ids, ]  

        ## Check if the new number of axes is 2 times larger than old one
        ## if not, we need to adjust axis numbers
        nodesAxis.new <- sort(unique(nodes$axis))
        
        if(length(nodesAxis.new) != 2*numAxes)
            for (i in 1:length(nodesAxis.new))
                if (i != nodesAxis.new[i]){
                    nodes[which(nodes$axis == nodesAxis.new[i]), "axis"] <- i
                }     
        
    }  ##### end of option == "axis <- split"
    
    ##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
    
    ## Final assembly and checking...
    
    HPD$edges <- edges
    HPD$nodes <- nodes
    chkHPD(HPD)
    HPD
}
