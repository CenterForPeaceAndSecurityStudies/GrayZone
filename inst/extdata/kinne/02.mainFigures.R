## This file performs additional analysis and plots figures. Be sure to
## run the `01.estimates.do' file in Stata before running this file.
## Note that the code for each figure is self contained. As long as you've
## loaded the `dat' file, you can scroll down and run the code for whichever
## figure you want. 

library(ggplot2)
library(grid)
library(gridExtra)
library(splines)
library(PRROC)
library(RColorBrewer)
library(classInt)
library(cshapes)
library(plyr)
library(countrycode)
library(reshape2)
library(network)
library(sna)
library(igraph)
library(HiveR)
library(grDevices)
library(SDMTools)
library(scales)

## Set directory and create an output directory
setwd("YourDirectory/IO-replication")
dir.create("figures")

## Call some helper functions
source("zz.supplements/functions.R")

## Grab the full dataset. The dataset includes both directions of dyads. However,
## any data used in the analysis are symmetrized. Some data, such as for arms
## trade, are not symmetrized. Some of the below operations require dropping
## the "reverse" dyads.
dat <- read.csv("zz.data", sep=",", header=T)






##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
## Figure 1 - Line plots of DCAs versus alliances
##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

## Use nondirected data for this figure, to avoid double-counting agreements
dat.full <- dat; rm(dat)
dat <- dat.full[ dat.full$ccode1 < dat.full$ccode2, ]

## First grab some yearly alliance and DCA data, focusing
## only on newly signed deals
ally <- read.csv("zz.supplements/allies")
new.allies <- ally[ , c("year","TotAllies")]
rm(ally)
new.dcas <- tapply(dat$DCA, dat$year, FUN=sum)
new.dcas[is.na(new.dcas)] <- 0

## Put it all in a data frame
df1 <- data.frame(
    year = rep(c(1980:2010),2),
    dcas = c(
        new.dcas[as.character(c(1980:2010))],
        new.allies[new.allies$year>1979 & new.allies$year<2011,]$TotAllies
    ),
    id = c(rep("dcas",31),rep("allies",31))
)

## Aggregate the dyad-level annual data for second panel. For DCAs, approximate
## the # of DCAs in force by labeling any deal signed within past 15 years. This
## underestimates the true # in force, since many deals are indefinite.
for ( zz in 1980:2010 ) {
    if ( zz == 1980 ) {
        dyads.dcas <- array(rep(0,length(1980:2010)),dim=length(1980:2010))
        names(dyads.dcas) <- c(1980:2010)
    }
    ## Count DCAs signed this year or prior 15 years
    use <- dat[ dat$year < (zz+1) & dat$year > (zz-15) , ]
    pairs <- tapply(use$DCA, use$dyadid, FUN=sum, na.rm=TRUE)
    ## Dichotomize, since we're counting dyads, not individual deals
    pairs[pairs>1] <- 1
    ## Then update the array
    dyads.dcas[as.character(zz)] <- sum(pairs,na.rm=TRUE)
    rm(use,pairs)
}; rm(zz)

## Find # of dyads with non-NATO alliances
allies.nonato <- tapply(dat$alliesNoNATO, dat$year, FUN=sum)

## And # of dyads with alliances including NATO
allies.nato <- tapply(dat$alliesWNATO, dat$year, FUN=sum)

## Put it in a data frame
df2 <- data.frame(
    year = rep(c(1980:2010),3),
    deals = c(
        dyads.dcas[as.character(c(1980:2010))],
        allies.nonato[as.character(c(1980:2010))],
        allies.nato[as.character(c(1980:2010))]
    ),
    id = c(rep("dcas",31),rep("nonato",31),rep("natos",31))
)

## Build some ggplots
cols <- brewer.pal(5,"Paired")
gg.new <- ggplot(df1, aes(x=year, y=dcas, color=id)) +
    geom_line() +
    geom_point(aes(shape=id)) +
    ylab("New agreements signed annually") +
    xlab("Year") +
    scale_color_manual(name=NULL,
                       breaks=c("dcas","allies"),
                       labels=c("DCAs","Alliances"),
                       values=cols[c(2,4)]
                       ) +
    scale_shape_manual(name=NULL,
                       breaks=c("dcas","allies"),
                       labels=c("DCAs","Alliances"),
                       values=c(1,2)
                       ) +
    guides(color=guide_legend(title=NULL)) +
    theme(
        axis.title.y = element_text(size=10, margin=margin(r=5)),
        axis.title.x = element_text(size=10),
        axis.text = element_text(size=7),
        legend.justification=c(0,1),
        legend.position=c(0.01,0.99),
        legend.margin=margin(0,3,3,3),
        legend.key.size=unit(0.8,"line"),
        legend.text=element_text(size=8)
    )

gg.current <- ggplot(df2, aes(x=year, y=deals, color=id)) +
    geom_line() +
    geom_point(aes(shape=id)) +
    ylab("Dyads with agreements in place") +
    xlab("Year") +
    scale_color_manual(name=NULL,
                       breaks=c("dcas","nonato","natos"),
                       labels=c("DCAs","Non-NATO alliances","All alliances"),
                       values=cols[c(4,2,1)]
                       ) +
    scale_shape_manual(name=NULL,
                       breaks=c("dcas","nonato","natos"),
                       labels=c("DCAs","Non-NATO alliances","All alliances"),
                       values=c(2,1,0)
                       ) +
    guides(color=guide_legend(title=NULL)) +
    theme(
        axis.title.y = element_text(size=10, margin=margin(r=5)),
        axis.title.x = element_text(size=10),
        axis.text = element_text(size=7),
        legend.justification=c(0,1),
        legend.position=c(0.01,0.5),
        legend.margin=margin(0,3,3,3),
        legend.key.size=unit(0.8,"line"),
        legend.text=element_text(size=8)
    )

plots <- list(gg.new,gg.current)

pdf("figures/Fig1.pdf", width=8, height=3, paper="special")
par(mar=c(0,0,0,0))
do.call(grid.arrange, c(plots, list(ncol=2, widths=c(1,1))))
dev.off()

rm(dat)
dat <- dat.full

## Clear the workspace
rm(list=setdiff(ls(), "dat"))





##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
## Figure 2 - Effect of DCAs on outcomes
##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

source("zz.supplements/functions.R")

## Keep only post Cold War dyads that sign at least one DCA
## for a "within" style comparison
dat.use <- dat[dat$year > 1989 & dat$noDeals == 0 , ]

## Specify before/after DCA signed
dat.use$group <- NA
dat.use[dat.use$newDeal == 1, ]$group <- "Before DCA Signed"
dat.use[dat.use$postDeal == 1, ]$group <- "After DCA Signed"

## Sum all varieties of peacekeeping missions
dat.use$allpk <- dat.use$unpk + dat.use$igopk + dat.use$statepk

## Peacekeeping, joint exercises, and MIDs data are symmetric. Use the
## nondirected version to avoid double counting. Arms imports and
## ICEWS cooperation are both asymmetric. Use the directed version
## for those.
titles <- c("PK missions","Joint exercises","MID coop.",
            "MID conflict","Arms imports","Coop. events")

use <- dat.use[dat.use$ccode1 < dat.use$ccode2,]
for ( i in c("allpk","jme","midsameside","mid") ) {
    if ( i == "allpk" ) ct <- 1
    tmp <- use
    gg.use <- summarySE(tmp, measurevar=i, groupvars="group", na.rm=TRUE)
    names(gg.use)[3] <- "DV"
    gg.use <- gg.use[rev(rownames(gg.use)),]
    gg.use$group <- factor(gg.use$group, levels=c("Before DCA Signed","After DCA Signed"))
    out <- ggplot(gg.use, aes(x=1, y=DV, group=group, fill=group)) +
        geom_bar(stat="identity", position="dodge", width=0.65) +
        geom_errorbar(aes(ymin=DV-ci, ymax=DV+ci), width=.1, position=position_dodge(0.65)) +
        ggtitle(titles[ct]) +
        scale_fill_manual(
            values=c("#7bccc4","#0868ac"),
            labels=c("Before DCA signed     ", "After DCA signed")
        ) +
        theme(
            legend.title=element_blank(),
            legend.position="bottom",
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_text(size=7),
            plot.title = element_text(size=10, hjust=0.5),
            plot.margin=unit(c(0.2,0.2,0,0), "cm")
        )
    if ( ct == 1 ) {
        legend <- g_legend(out)
        out <- out +
            ylab("Mean annual level") +
            scale_y_continuous(breaks=0:3, labels=c("0.00","1.00","2.00","3.00")) +
            theme(
                axis.title.y=element_text(size=10, angle=90, margin=margin(r=5)),
                plot.margin=unit(c(0.2,0.2,0,0.2), "cm")
            )
    }
    out <- out + theme(legend.position="none")
    assign(paste("gg", ct, sep=""), out)
    rm(out, gg.use, tmp)
    ct <- ct + 1
}
rm(use)

## Switch back to nondirected version for arms imports and ICEWS cooperation. Keep
## the 'ct' counter from above loop for assigning objects.
use <- dat.use
for ( i in c("armsImports","coop") ) {
    if ( i == "coop" ) {
        tmp <- use[complete.cases(use),]
    } else {
        tmp <- use
    }
    gg.use <- summarySE(tmp, measurevar=i, groupvars="group", na.rm=TRUE)
    names(gg.use)[3] <- "DV"
    gg.use <- gg.use[rev(rownames(gg.use)),]
    gg.use$group <- factor(gg.use$group, levels=c("Before DCA Signed","After DCA Signed"))
    out <- ggplot(gg.use, aes(x=1, y=DV, group=group, fill=group)) +
        geom_bar(stat="identity", position="dodge", width=0.65) +
        geom_errorbar(aes(ymin=DV-ci, ymax=DV+ci), width=.1, position=position_dodge(0.65)) +
        ylab("") +
        ggtitle(titles[ct]) +
        scale_fill_manual(
            values=c("#7bccc4","#0868ac"),
            labels=c("Before DCA signed     ", "After DCA signed")
        ) +
        theme(
            legend.title=element_blank(),
            legend.position="none",
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_text(size=7),
            plot.title = element_text(size=10, hjust=0.5),
            plot.margin=unit(c(0.2,0.2,0,0), "cm")
        )
    assign(paste("gg", ct, sep=""), out)
    rm(out, gg.use, tmp)
    ct <- ct + 1
}
rm(use)

## Specify viewport layout
Layout <- grid.layout(
    nrow = 2, ncol = 6,
    widths = unit(c(1.2,0.95,0.975,1,0.925,0.975), "null"),
    heights = unit(c(1, 0.15), "null")
)

## Plot using viewport to get a single legend for everything
pdf("figures/Fig2.pdf", width=8, height=3, paper="special",onefile=FALSE)
par(mar=c(0,0,0,0))
grid.newpage()
pushViewport(viewport(layout = Layout))
print(gg1, vp = subplot(1, 1))
print(gg2, vp = subplot(1, 2))
print(gg3, vp = subplot(1, 3))
print(gg4, vp = subplot(1, 4))
print(gg5, vp = subplot(1, 5))
print(gg6, vp = subplot(1, 6))
pushViewport(subplot(2,1:6))
grid.draw(legend)
dev.off()

## Clear up the workspace
rm(list=setdiff(ls(), "dat"))




##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
## Figure 3 - Maps of overall DCA activity
##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

source("zz.supplements/functions.R")

yr.use <- c(1990, 2000, 2010)
yrs <- c("1980s","1990s","2000s")

## Open graphics device
pdf("figures/Fig3.pdf", width=6.5, height=8, paper="special")
par(mar=c(1,0,1,0))
layout(matrix(c(1,2,3,4),4,1,byrow=TRUE), heights=c(1,1,1,0.2))
## Then plot each map via a loop
colFunc <- colorRampPalette(brewer.pal(7,"GnBu"), bias = 2, space = "rgb", interpolate = "linear")
colCodes <- colFunc(29) # Set based on max number of UNIQUE degrees
ct <- 1
for (j in yr.use) {
    ## Since we're interested in each country's DCA participation, we need full dataset
    use <- dat[dat$year <= j & dat$year > (j-10),]
    ## Sum DCAs for each country during this time period
    sums <- as.matrix(tapply(use$DCA, use$ccode1, FUN=sum))
    map <- cshp(date=as.Date(paste(j, "-12-31", sep=""), useGW=F))
    m <- match(map$COWCODE, rownames(sums))
    dat.use <- sums[m,]
    ## Any missings can be safely assumed to be zero
    dat.use[is.na(dat.use)] <- 0
    print(max(dat.use, na.rm=T))
    names(dat.use) <- map$FEATUREID
    mapM <- spCbind(map,dat.use)
    map.cols <- sapply(dat.use, function(x) colCodes[which(sort(unique(dat.use)) == x)])
    plot(mapM, bty="n", col=map.cols, lwd=0.1)
    text(-120,-15,labels=yrs[ct],cex=1.5)
    print(par("usr"))
    rm(use, sums, map, m, dat.use, mapM, map.cols)
    ct <- ct + 1
}
## Use 100 colors for legend, to smooth gradient
leg.cols <- colFunc(100)
plot(0,type='n',axes=FALSE,ann=FALSE, xlim=c(-1,1), ylim=c(-1,1))
par(mar=c(0,0,0,0))
print(par("usr"))
legend.map(col=leg.cols,ends=c(0,50),tsz=1,xoff=0,yoff=0.75,lgth=20.5,tit="DCAs signed",toff=1.25)

dev.off()

## Clear the workspace
rm(list=setdiff(ls(), "dat"))





##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
## Figure 4 - Ilustrative network
##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

## First grab the network DV used for the analysis, then sum over 10 years
for (i in 2001:2010) {
    if ( i == 2001 ) {
        use <- dat[dat$year==i,c("abbrev1","abbrev2","DCA")]
        mat <- acast(use, abbrev1~abbrev2, value.var="DCA")
        print(dim(mat))
        print(sum(mat))
        rm(use)
    } else {
        use2 <- dat[dat$year==i,c("abbrev1","abbrev2","DCA")]
        mat2 <- acast(use2, abbrev1~abbrev2, value.var="DCA")
        ## Drop E. Timor, since no data
        mat2 <- mat2[!rownames(mat2) %in% "ETM",!colnames(mat2) %in% "ETM"]
        print(dim(mat2))
        print(sum(mat2))
        print(all.equal(rownames(mat),rownames(mat2)))
        print(all.equal(colnames(mat),colnames(mat2)))
        print(sum(mat))
        print(sum(mat2))
        print(isSymmetric(mat))
        print(isSymmetric(mat2))
        mat <- mat + mat2
        rm(mat2,use2)
    }
}

## Dichotomize
mat[mat > 1] <- 1
## Restrict to Asia
asia <- c("AFG","TKM","TAJ","KYR","UZB","KZK","CHN","MON","TAW","KOR","PRK",
          "ROK","JPN","JPN","IND","BHU","PAK","BNG","MYA","SRI","MAD","NEP","THI",
          "CAM","LAO","DRV","RVN","MAL","SIN","BRU","PHI","INS","ETM","AUL","PNG",
          "NEW","VAN","SOL","KIR","TUV","FIJ","TON","NAU","MSI","PAL","FSM","WSM")
use.mat <- mat[rownames(mat) %in% asia, colnames(mat) %in% asia]
## Drop isolates
use.mat <- use.mat[rowSums(use.mat)!=0,colSums(use.mat)!=0]

## Select a palette and specify a color for each degree
F2 <- colorRampPalette(
    brewer.pal(9,"GnBu")[c(2,7)],
    bias = length(unique(rowSums(use.mat))),
    space = "rgb", interpolate = "linear"
)
colCodes <- F2(length(unique(rowSums(use.mat))))
col.leg <- F2(100) # Smoother gradient for legend
node.color <- sapply(
    rowSums(use.mat),
    function(x) colCodes[which(sort(unique(rowSums(use.mat))) == x)]
)

## Convert to a network
net <- as.network(use.mat, directed=F, loops=F, matrix.type="a")
deg <- sqrt(rowSums(as.matrix(net)))
nms <- net %v% "vertex.names"

## Open a graphics device and plot the network, along with a raster legend
pdf("figures/Fig4.pdf", width=6, height=3)
par(mar=c(0, 0, 0, 0))

plot.network(
    net, vertex.col=node.color, main="", label.cex=0.45, label.pos=5,
    label.col="black", vertex.cex=sqrt(deg+1)+0.5, displayisolates=TRUE,
    edge.col="gray75", vertex.border=node.color, mode="fruchtermanreingold",
    displaylabels=TRUE,label=nms, edge.lwd=0.1,
    jitter=TRUE, frame.plot=F, pad=0, xlim=NULL, ylim=NULL
)

bx <- par("usr")
rng1 <- bx[2] - bx[1]
rng2 <- bx[4] - bx[3]
coords <- 	c(
    bx[2] - (0.15 * rng1),
    bx[4] - (0.3 * rng2),
    bx[2] - (0.125 * rng1),
    bx[4] - (0.125 * rng2)
)
rasterImage(
    rev(as.raster(col.leg)),
    coords[1], coords[2], coords[3], coords[4],
    interpolate=TRUE
)
ymk <- c(
    coords[2] + ((coords[4] - coords[2]) * 0.25),
    coords[2] + ((coords[4] - coords[2]) * 0.5),
    coords[2] + ((coords[4] - coords[2]) * 0.75)
)
text(rep("-",3), x = coords[1], y = ymk, col="white", cex=0.5)
text(rep("-",3), x = coords[3], y = ymk, col="white", cex=0.5)
text(as.character(c(3,6,9)), x = coords[1], y = ymk, col="black", cex=0.6, pos=2)
text(
    "# of\nDCAs", x = coords[1] - (0.05 * rng1),
    y = coords[4] + (0.03 * rng2), pos=4, col="black", cex=0.65
)
dev.off()

## Clear the workspace
rm(list=setdiff(ls(), "dat"))




##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
## Figure 6 - Hive plots to illustrate topography
##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

source("zz.supplements/functions.R")

## Let's look at the post-2000 period
dat.use <- dat[dat$year > 2000,]
dat.use <- dat.use[dat.use$ccode1!=860 & dat.use$ccode2!=860,]

## Make a network, summed over post-2000 period
for (jj in unique(dat.use$year)) {
    use <- dat.use[dat.use$year==jj,c("ccode1","ccode2","DCA")]
    names(use) <- c("ccode1","ccode2","dca")
    if ( jj == unique(dat.use$year)[1] ) {
        net <- acast(use, ccode1~ccode2, value.var="dca")
    } else {
        dum <- acast(use, ccode1~ccode2, value.var="dca")
        print(all(rownames(dum) == rownames(net)))
        print(all(colnames(dum) == colnames(net)))
        net <- net + dum
        rm(dum)
    }
    rm(use)
}; rm(jj)
net[net>1] <- 1 # Dichotomize
isSymmetric(net)

## Make an edgelist version
net.edg <- melt(net); names(net.edg) <- c("ccode1","ccode2","dca")
net.edg <- net.edg[net.edg$ccode1 != net.edg$ccode2,]
net.edg <- net.edg[net.edg$dca!=0,]

## Calculate degree for all nodes
deg <- sna::degree(net, gmode="graph")

## Code regions, based on COW codes
reg <- vector(length=nrow(net))
for (z in 1:length(reg)) { # Five regions
    if ( as.numeric(colnames(net)[z]) < 200 ) reg[z] <- 1
    if ( as.numeric(colnames(net)[z]) > 199 & as.numeric(colnames(net)[z]) < 400 ) reg[z] <- 2
    if ( as.numeric(colnames(net)[z]) > 399 & as.numeric(colnames(net)[z]) < 600 ) reg[z] <- 3
    if ( as.numeric(colnames(net)[z]) > 599 & as.numeric(colnames(net)[z]) < 700 ) reg[z] <- 4
    if ( as.numeric(colnames(net)[z]) > 699 ) reg[z] <- 5
}

## Put node-level data in a single data frame
node.list <- data.frame(
    name = rownames(net),
    degree = deg,
    region=reg
)

## Specify network statistics at edge level, first for 2paths
two.paths <- (net %*% net) * net
## Function for mapping 2paths to edgelist
F1 <- function(x) {
    data.frame(
        two.paths = two.paths[
            which(rownames(two.paths) == as.character(x$ccode1)),
            which(colnames(two.paths) == as.character(x$ccode2))
        ]
    )
}
data.edges <- ddply(
    net.edg, .variables=c("ccode1", "ccode2", "dca"),
    function(x) data.frame(F1(x))
)
rm(F1)

## Then for mutual degree
mean.degree <- outer(deg, deg, "+") / 2
rownames(mean.degree) <- rownames(net); colnames(mean.degree) <- colnames(net)
F1 <- function(x) {
    data.frame(
        mean.degree = mean.degree[
            which(rownames(mean.degree) == as.character(x$ccode1)),
            which(colnames(mean.degree) == as.character(x$ccode2))
        ]
    )
}
data.edges$mean.degree  <- ddply(
    net.edg, .variables=c("ccode1", "ccode2", "dca"),
    function(x) data.frame(F1(x))
)$mean.degree
rm(F1)

## Define node color based on nodal degree
F2 <- colorRampPalette(
    adjustcolor(brewer.pal(9,"Oranges"), alpha.f=0.5),
    bias = 2, space = "rgb", interpolate = "linear"
)
colCodes <- F2(length(unique(node.list$degree)))
## Add vector of colors to the list of nodal attributes
node.list$color <- sapply(
    node.list$degree,
    function(x) colCodes[which(sort(unique(node.list$degree)) == x)]
)
leg2 <- F2(100) # Use more colors to smooth gradient for legend
rm(F2,colCodes)

## Assign visual attributes to edges using the same approach as for nodes
F2 <- colorRampPalette(brewer.pal(9,"GnBu")[2:9])
colCodes <- F2(length(unique(data.edges$two.paths)))
data.edges$color1 <- sapply(
    data.edges$two.paths,
    function(x) colCodes[which(sort(unique(data.edges$two.paths)) == x)]
)
data.edges[data.edges$two.paths==0,]$color1 <- NA
rm(colCodes)

## Now use same palette for mutual degree
colCodes <- F2(length(unique(data.edges$mean.degree)))
data.edges$color2 <- sapply(
    data.edges$mean.degree,
    function(x) colCodes[which(sort(unique(data.edges$mean.degree)) == x)]
)
leg1 <- F2(100) # Use more colors to smooth gradient for legend
rm(F2, colCodes)

## Assign nodes to axes, based on region
node.list$axis <- as.integer(node.list$region)

## Make axis labels
labs <- c("Americas","Europe","SS Africa","MENA","Asia")

## Create a hive plot, using some helper functions
source("zz.supplements/mod.edge2HPD.R")
source("zz.supplements/mod.mineHPD.R")

## Basic hive plot
hive1 <- mod.edge2HPD(edge_df = data.edges[, 1:2],
                      edge.weight = data.edges$dca/2,
                      edge.color = rep("gray",nrow(data.edges)),
                      node.color = node.list[,c("name", "color")],
                      node.size = node.list[,c("name", "degree")],
                      node.radius = node.list[,c("name", "degree")],
                      node.axis = node.list[,c("name", "axis")],
                      axis.cols=rep("gray",5)
                      )
hive1$nodes$size <- 0.65
hive1 <- mineHPD(hive1, option = "remove zero edge")

## Edges weighted by mutual degree
hive2 <- mod.edge2HPD(edge_df = data.edges[, 1:2],
                      edge.weight = rescale(log(data.edges$mean.degree+1), to=c(0.5,1)),
                      edge.color = data.edges$color2,
                      node.color = node.list[,c("name", "color")],
                      node.size = node.list[,c("name","degree")],
                      node.radius = node.list[,c("name", "degree")],
                      node.axis = node.list[,c("name", "axis")],
                      axis.cols=rep("gray",5)
                      )
hive2$nodes$size <- 0.65
hive2 <- mineHPD(hive2, option = "remove zero edge")

## Edges weighted by two-paths closed
hive3 <- mod.edge2HPD(edge_df = data.edges[, 1:2],
                      edge.weight = rescale(log(data.edges$two.paths+1),to=c(0.5,1)),
                      edge.color = data.edges$color1,
                      node.color = node.list[,c("name", "color")],
                      node.size = node.list[,c("name", "degree")],
                      node.radius = node.list[,c("name", "degree")],
                      node.axis = node.list[,c("name", "axis")],
                      axis.cols=rep("gray",5)
                      )
hive3$nodes$size <- 0.65
hive3 <- mineHPD(hive3, option = "remove zero edge")


## Now throw everything into a single plot, along with raster legends. This approch
## embeds viewports within viewports, so things may get wonky, depending on
## your graphics device.
pdf("figures/Fig6.pdf", width=12, height=6, paper="special")

grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 3)))
pushViewport(subplot(1, 1)) # left plot

plotHive(
    hive1, method = "abs", dr.nodes=TRUE, bkgnd = "white",
    axLab.gpar=gpar(col="black", fontsize=8),  axLab.pos = c(2,5,3,3,5),
    axLabs=labs, np=FALSE
)
grid.raster(
    rev(as.raster(leg2)),x=0.1,y=0.7,width=0.05,height=0.2,
    vp=subplot(1,1), just=c("right","bottom")
)
grid.text(
    as.character(c(10,20,30)), x = 0.11, y = seq(.75,.85,.05),
    just="left", gp=gpar(col="black", fontsize=8)
)
grid.text(
    rep("-",3), x = 0.09, y = seq(.75,.85,.05),
    just="left", gp=gpar(col="white", fontsize=8)
)
grid.text(
    rep("-",3), x = 0.045, y = seq(.75,.85,.05),
    just="left", gp=gpar(col="white", fontsize=8)
)
grid.text(
    "Nodal\ndegree", x = 0.05, y = .94, just="left",
    gp=gpar(col="black", fontsize=8)
)
grid.text("All network ties", x = 0.5, y = 0.025, just="center",
          gp=gpar(col="black", fontsize=12))

popViewport(2)
pushViewport(subplot(1, 2)) # middle plot
grid.text("test2")

plotHive(
    hive2, method = "abs", dr.nodes=TRUE, bkgnd = "white",
    axLab.gpar=gpar(col="black", fontsize=8),  axLab.pos = c(2,5,3,3,5),
    axLabs=labs, np=FALSE
)
grid.raster(
    rev(as.raster(leg1)),x=0.9,y=0.7,width=0.05,height=0.2,
    vp=subplot(1,1), just=c("right","bottom")
)
grid.text(
    as.character(c(5,15,25)), x = 0.84, y = seq(.75,.85,.05),
    just="right", gp=gpar(col="black", fontsize=8)
)
grid.text(
    rep("-",3), x = 0.86, y = seq(.75,.85,.05), just="right",
    gp=gpar(col="white", fontsize=8)
)
grid.text(
    rep("-",3), x = 0.905, y = seq(.75,.85,.05), just="right",
    gp=gpar(col="white", fontsize=8)
)
grid.text(
    "Mutual\ndegree", x = 0.815, y = .94, just="left",
    gp=gpar(col="black", fontsize=8)
)
grid.text(
    "Ties weighted by mutual degree", x = 0.5, y = 0.025,
    just="center", gp=gpar(col="black", fontsize=12)
)

popViewport(2)
pushViewport(subplot(1, 3)) # right plot

plotHive(
    hive3, method = "abs", dr.nodes=TRUE, bkgnd = "white",
    axLab.gpar=gpar(col="black", fontsize=8),  axLab.pos = c(2,5,3,3,5),
    axLabs=labs, np=FALSE
)
grid.raster(
    rev(as.raster(leg1)),x=0.9,y=0.7,width=0.05,height=0.2,
    vp=subplot(1,1), just=c("right","bottom")
)
grid.text(
    as.character(c(4,7,10)), x = 0.84, y = seq(.75,.85,.05),
    just="right", gp=gpar(col="black", fontsize=8)
)
grid.text(
    rep("-",3), x = 0.86, y = seq(.75,.85,.05), just="right",
    gp=gpar(col="white", fontsize=8)
)
grid.text(
    rep("-",3), x = 0.905, y = seq(.75,.85,.05), just="right",
    gp=gpar(col="white", fontsize=8)
)
grid.text(
    "Two-paths\nclosed", x = 0.815, y = .94, just="left",
    gp=gpar(col="black", fontsize=8)
)
grid.text(
    "Ties weighted by two-paths", x = 0.5, y = 0.025, just="center",
    gp=gpar(col="black", fontsize=12)
)

dev.off()

## Clear the workspace
rm(list=setdiff(ls(), "dat"))





##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
## Figure 7 - Distributions across groups
##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

## Grab two sets of data, one for full population, and one only for DCA partners
use1 <- dat[dat$year > 1989 & dat$ccode1 != dat$ccode2 , ]
use2 <- dat[dat$year > 1989 & dat$ccode1 != dat$ccode2 & dat$postDeal==1,]
df <- data.frame(
    deg = c(use1$DCAdegreeRAW,use2$DCAdegreeRAW),
    mod = c(rep("All dyads",nrow(use1)),rep("Dyads with at least one DCA",nrow(use2)))
)
df <- count(df, vars=c("deg","mod"))
df <- ddply(df, .(mod), transform, p = freq/sum(freq))

## Make sure facets plot in order
df$mod <- factor(df$mod,levels=c("All dyads","Dyads with at least one DCA"))
gg.deg.dist <- ggplot(df, aes(deg,p)) +
    geom_bar(
        stat="identity", width=.25, fill="#2c7bb6", colour="#2c7bb6"
    ) +
    xlab("Mean mutual degree") +
    ylab("Density") +
    theme(
        axis.title.y = element_text(vjust=1, size=9),
        axis.title.x = element_text(vjust=-0.25, size=9),
        axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7)
    ) +
    facet_grid(. ~ mod)

pdf("figures/Fig7a.pdf", width=7, height=2.5, paper="special")
gg.deg.dist
dev.off()

rm(df,gg.deg.dist)

## Do same thing for 2paths
df <- data.frame(
    twopaths = c(use1$DCA2pathsRAW,use2$DCA2pathsRAW),
    mod = c(rep("All dyads",nrow(use1)),rep("Dyads with at least one DCA",nrow(use2)))
)
df <- count(df, vars=c("twopaths","mod"))
df <- ddply(df, .(mod), transform, p = freq/sum(freq))

## Make sure facets plot in order
df$mod <- factor(df$mod,levels=c("All dyads","Dyads with at least one DCA"))
gg.2path.dist <- ggplot(df, aes(twopaths, p)) +
    geom_bar(
        stat="identity", width=.75, fill="#2c7bb6", colour="#2c7bb6"
    ) +
    xlab("Number of shared two-paths") +
    ylab("Density") +
    theme(
        axis.title.y = element_text(vjust=1, size=9),
        axis.title.x = element_text(vjust=-0.25, size=9),
        axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7)
    ) +
    facet_grid(. ~ mod)

pdf("figures/Fig7b.pdf", width=7, height=2.5, paper="special")
gg.2path.dist
dev.off()

## Clear the workspace
rm(list=setdiff(ls(), "dat"))




##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
## Figures 8 & 9 - Forest plots of estimates
##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

## First, grab the estimates for the FE model
ests <- read.csv("output/estimates.csv")
ests.fe <- ests[18:30,8:11]
ests.fe[] <- lapply(ests.fe, function(x) as.numeric(as.character(x)))
rownames(ests.fe) <- ests[18:30,1]

## Vector of names
fe.names <- c(
    "Two-paths",
    "Mutual degree",
    "Defense pact (non-NATO)",
    "NATO membership",
    "NATO-PfP membership",
    "Arms match",
    "UNGA ideal point diff.",
    "Bilateral trade",
    "Mutual enemy",
    "Mutual terrorist threat",
    "Mutual democracy",
    "Mean power",
    "Mean GDP/capita"	
)

## Rescale and standardize for nice plotting, then put everything into a data frame
for (i in 1:nrow(ests.fe)) {
    ad <- 0.5 / ests.fe[i,2]
    ests.fe[i,1] <- ests.fe[i,1] * ad
    ests.fe[i,2] <- ests.fe[i,2] * ad
    rm(ad)

    ad <- 0.5 / ests.fe[i,4]
    ests.fe[i,3] <- ests.fe[i,3] * ad
    ests.fe[i,4] <- ests.fe[i,4] * ad
    rm(ad)
}; rm(i)

dfe <- data.frame(
    nms=rep(fe.names,2),
    x=c(ests.fe$est4,ests.fe$est5),
    xlo=c(
    (ests.fe$est4 - (1.96 * ests.fe$X.4)),
    (ests.fe$est5 - (1.96 * ests.fe$X.5))
    ),
    xhi=c(
    (ests.fe$est4 + (1.96 * ests.fe$X.4)),
    (ests.fe$est5 + (1.96 * ests.fe$X.5))
    ),
    mod=c(
        rep("No prior DCAs",length(fe.names)),
        rep("At least one prior DCA",length(fe.names))
    )
)

dfe$p.cols <- ""
for (ii in 1:nrow(dfe)) {
    if (dfe[ii,"xlo"] < 0 && dfe[ii,"xhi"] > 0) {
        dfe$p.cols[ii] <- "insig" # Color for insignificant estimates
    } else {
        dfe$p.cols[ii] <- "sig" # Color for significant estimates
    }
}

dfe <- dfe[
    c(
	c(1,2,12,13,6,9,10,11,7,8,4,5,3),
	c(1,2,12,13,6,9,10,11,7,8,4,5,3)+13
    ),
    ]

marg <- c(0,0.5,0.1,0)

## Get various factor levels in order
dfe$mod <- factor(dfe$mod,levels=c("No prior DCAs","At least one prior DCA"))
dfe$nms <- factor(dfe$nms,levels=rev(dfe$nms[1:13]))
gg.ests.fe <- ggplot(dfe, aes(y=nms, x=x, group=p.cols, fill=p.cols, color=p.cols)) +
    geom_vline(xintercept=0, lty=2, size=0.25) +
    geom_point(size=1.5,shape=16) +
    facet_grid(. ~ mod) +
    geom_segment(aes(x=xlo,xend=xhi,yend=nms)) +
    ylab("") +
    xlab("Rescaled estimates + 95% CIs") +
    scale_color_manual(
        values=c("#d7191c","#2c7bb6")
    ) +
    theme(
	axis.title.x = element_text(size = 8),
        axis.text.y=element_text(color="black",size=8),
        axis.text.x=element_text(size=7),
        plot.margin=unit(marg,"cm"),
        legend.position="none"
    )

## Add a rectangle around estimates of interest
gg.ests.fe <- gg.ests.fe + geom_rect(
                               xmin=-Inf,xmax=Inf,ymax=as.numeric(dfe$nms[1])+0.4,
                               ymin=as.numeric(dfe$nms[2])-0.4, fill=NA, linetype=1,
                               color="gray50", size=0.25
                           )

pdf("figures/Fig9.pdf", width=6, height=3.5, paper="special")
gg.ests.fe
dev.off()

##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

## Now the pooled model for 1980-2010, which we'll plot along with marginal effects
ests.1980 <- ests[3:17,2:3]
ests.1980[] <- lapply(ests.1980, function(x) as.numeric(as.character(x)))
rownames(ests.1980) <- ests[3:17,1]

## Rescale for forest plots
for (i in 1:nrow(ests.1980)) {
    ad <- 0.5 / ests.1980[i,2]
    ests.1980[i,1] <- ests.1980[i,1] * ad
    ests.1980[i,2] <- ests.1980[i,2] * ad
    rm(ad)
}

## Label rows and put everything in a data frame for ggplot
pool.names <- c(
    "Two-paths",
    "Mutual degree",
    "Defense pact (non-NATO)",
    "NATO membership",
    "NATO-PfP membership",
    "Former colony",
    "Distance",
    "Arms match",
    "UNGA ideal point diff.",
    "Bilateral trade",
    "Mutual enemy",
    "Mutual terrorist threat",
    "Mutual democracy",
    "Mean power",
    "Mean GDP/capita"	
)

dpool <- data.frame(
    nms=pool.names,
    x=ests.1980$est1,
    xlo=ests.1980$est1 - (1.96 * ests.1980$X.1),
    xhi=ests.1980$est1 + (1.96 * ests.1980$X.1)
)

## Reorganize so they show up in same order as hypotheses
dpool <- dpool[c(1,2,14,15,8,11,12,13,9,10,4,5,3,7,6),]

dpool$p.cols <- ""
for (ii in 1:nrow(dpool)) {
    if (dpool[ii,"xlo"] < 0 && dpool[ii,"xhi"] > 0) {
        dpool$p.cols[ii] <- "sig" # Color for significant estimates
    } else {
        dpool$p.cols[ii] <- "insig" # Color for insignificant estimates
    }
}

## Get various factor levels in order
dpool$nms <- factor(dpool$nms,levels=rev(dpool$nms))

## Plot as a ggplot object
gg.ests.1980 <- ggplot(
    dpool, aes(y=nms, x=x, group=p.cols, fill=p.cols, color=p.cols)) +
    geom_vline(xintercept=0, lty=2, size=0.25) +
    geom_point(size=1.5,shape=16) +
    geom_segment(aes(x=xlo,xend=xhi,yend=nms)) +
    ylab("") +
    xlab("Rescaled estimates + 95% CIs") +
    ggtitle("All dyads, full 1980-2010 period") +
    scale_color_manual(
        values=c("#2c7bb6","#d7191c")
    ) +
    theme(
        axis.title.x = element_text(size=8, margin=margin(t=3)),
        axis.title.y = element_blank(),
        axis.text.y=element_text(color="black",size=9),
        axis.text.x=element_text(size=6),
        plot.title = element_text(size = 10, hjust=0.5),
        plot.margin=unit(c(0.1,0.5,0.2,0),"cm"),
        legend.position="none"
    )

## Add a rectangle around estimates of interest
gg.ests.1980 <- gg.ests.1980 + geom_rect(
                                   xmin=-Inf,xmax=Inf,ymax=as.numeric(dpool$nms[1])+0.4,
                                   ymin=as.numeric(dpool$nms[2])-0.4, fill=NA, linetype=1,
                                   color="gray50", size=0.25
                               )

## Now grab the predictive margins for the two sets of models
probs <- read.table("output/PredictProbsColdWar.dat", header=TRUE)
names(probs) <- c("new.2paths","new.2pathsSE","new.degree","new.degreeSE",
                  "old.2paths","old.2pathsSE","old.degree","old.degreeSE")

x.in <- (0:(nrow(probs)-1))/(nrow(probs)-1)

## Make a data frame of margins, smoothed with splines
dat1 <- data.frame(
    y = c(
        predict(interpSpline(x.in,probs$new.2paths))$y,
        predict(interpSpline(x.in,probs$old.2paths))$y
    ),
    x = c(
        predict(interpSpline(x.in,x.in))$x,
        predict(interpSpline(x.in,x.in))$x
    ),
    u.ci = c(
        predict(interpSpline(x.in,probs$new.2paths+(1.96*probs$new.2pathsSE)))$y,
        predict(interpSpline(x.in,probs$old.2paths+(1.96*probs$old.2pathsSE)))$y
    ),
    l.ci = c(
        predict(interpSpline(x.in,probs$new.2paths-(1.96*probs$new.2pathsSE)))$y,
        predict(interpSpline(x.in,probs$old.2paths-(1.96*probs$old.2pathsSE)))$y
    )
)
dat1$mod <- c( rep("1980-1989",nrow(dat1)/2), rep("1990-2010",nrow(dat1)/2) )

## Make a data frame of margins, smoothed with splines
dat2 <- data.frame(
    y = c(
        predict(interpSpline(x.in,probs$new.degree))$y,
        predict(interpSpline(x.in,probs$old.degree))$y
    ),
    x = c(
        predict(interpSpline(x.in,x.in))$x,
        predict(interpSpline(x.in,x.in))$x
    ),
    u.ci = c(
        predict(interpSpline(x.in,probs$new.degree+(1.96*probs$new.degreeSE)))$y,
        predict(interpSpline(x.in,probs$old.degree+(1.96*probs$old.degreeSE)))$y
    ),
    l.ci = c(
        predict(interpSpline(x.in,probs$new.degree-(1.96*probs$new.degreeSE)))$y,
        predict(interpSpline(x.in,probs$old.degree-(1.96*probs$old.degreeSE)))$y
    )
)
dat2$mod <- c( rep("1980-1989",nrow(dat1)/2), rep("1990-2010",nrow(dat1)/2) )

both <- rbind(dat1,dat2)
both$eff <- c(rep("Effect of two-paths",102),rep("Effect of mutual degree",102))

## Plot two paths and mutual degree in separate ggplot objects
gg.marg1 <- ggplot(dat1, aes(x=x, y=y, group=mod, color=mod)) +
    geom_ribbon(aes(ymin=l.ci,ymax=u.ci), alpha=0.2, color=NA) +
    geom_line(aes(y=y,x=x,linetype=mod),size=0.5, alpha=0.75) +
    ylab("Probability of DCA") +
    xlab("Number of third-party ties") +
    ggtitle("Marginal effect of two-paths") + 
    scale_color_manual(
        labels=c("1980-1989", "1990-2010"),
        values=c("1990-2010"="#2c7bb6","1980-1989"="#d7191c")
    ) +
    scale_x_continuous(
        labels=c("Min.","","Med.","","Max.")
    ) +
    scale_linetype_manual(values=c("dotted","solid")) +
    theme(
	axis.title.y = element_text(size=7, margin=margin(r=3)),
	axis.title.x = element_text(size=8, margin=margin(t=3)),
        legend.position=c(0.2,0.75),
        legend.margin=margin(0,3,3,3),
        legend.title=element_blank(),
        legend.text=element_text(size=7),
        legend.key.size = unit(0.4, "cm"),
        axis.text.y=element_text(size=6),
        axis.text.x=element_text(size=6),
        plot.title = element_text(size = 10, hjust=0.5),
        plot.margin=unit(c(0.1,0.5,0.2,0),"cm")
    )

gg.marg2 <- ggplot(dat2, aes(x=x, y=y, group=mod, color=mod)) +
    geom_ribbon(aes(ymin=l.ci,ymax=u.ci), alpha=0.2, color=NA) +
    geom_line(aes(y=y,x=x,linetype=mod),size=0.5, alpha=0.75) +
    ylab("Probability of DCA") +
    xlab("Mean degree centrality") +
    ggtitle("Marginal effect of mutual degree") + 
    scale_color_manual(
        labels=c("1980-1989", "1990-2010"),
        values=c("1990-2010"="#2c7bb6","1980-1989"="#d7191c")
    ) +
    scale_x_continuous(
        labels=c("Min.","","Med.","","Max.")
    ) +
    scale_linetype_manual(values=c("dotted","solid")) +
    theme(
        axis.title.y = element_text(size=7, margin=margin(r=3)),
	axis.title.x = element_text(size=8, margin=margin(t=3)),
        legend.position=c(0.2,0.75),
        legend.margin=margin(0,3,3,3),
        legend.title=element_blank(),
        legend.text=element_text(size=7),
        legend.key.size = unit(0.4, "cm"),
        axis.text.y=element_text(size=6),
        axis.text.x=element_text(size=6),
        plot.title = element_text(size = 10, hjust=0.5),
        plot.margin=unit(c(0.2,0.5,0.1,0),"cm")
    )

pdf("figures/Fig8.pdf", width=7, height=4, paper="special")
grid.arrange(gg.ests.1980, arrangeGrob(gg.marg1,gg.marg2,ncol=1), ncol=2, widths=c(1,0.7))
dev.off()

## Clear the workspace
rm(list=setdiff(ls(), "dat"))





##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
## Figure 10 - Marginal effects of the FE model
##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

probs <- read.table("output/PredictProbs.dat", header=TRUE)

names(probs) <- c("new.2paths","new.2pathsSE","new.degree","new.degreeSE",
                  "old.2paths","old.2pathsSE","old.degree","old.degreeSE")

x.in <- (0:(nrow(probs)-1))/(nrow(probs)-1)

## Make a data frame of margins, smoothed with splines
dat1 <- data.frame(
    y = c(
        predict(interpSpline(x.in,probs$new.2paths))$y,
        predict(interpSpline(x.in,probs$old.2paths))$y
    ),
    x = c(
        predict(interpSpline(x.in,x.in))$x,
        predict(interpSpline(x.in,x.in))$x
    ),
    u.ci = c(
        predict(interpSpline(x.in,probs$new.2paths+(1.96*probs$new.2pathsSE)))$y,
        predict(interpSpline(x.in,probs$old.2paths+(1.96*probs$old.2pathsSE)))$y
    ),
    l.ci = c(
        predict(interpSpline(x.in,probs$new.2paths-(1.96*probs$new.2pathsSE)))$y,
        predict(interpSpline(x.in,probs$old.2paths-(1.96*probs$old.2pathsSE)))$y
    )
)
dat1$mod <- c( rep("First DCA",nrow(dat1)/2), rep("Subsequent DCA",nrow(dat1)/2) )

## Make another data frame of margins, smoothed with splines
dat2 <- data.frame(
    y = c(
        predict(interpSpline(x.in,probs$new.degree))$y,
        predict(interpSpline(x.in,probs$old.degree))$y
    ),
    x = c(
        predict(interpSpline(x.in,x.in))$x,
        predict(interpSpline(x.in,x.in))$x
    ),
    u.ci = c(
        predict(interpSpline(x.in,probs$new.degree+(1.96*probs$new.degreeSE)))$y,
        predict(interpSpline(x.in,probs$old.degree+(1.96*probs$old.degreeSE)))$y
    ),
    l.ci = c(
        predict(interpSpline(x.in,probs$new.degree-(1.96*probs$new.degreeSE)))$y,
        predict(interpSpline(x.in,probs$old.degree-(1.96*probs$old.degreeSE)))$y
    )
)
dat2$mod <- c( rep("First DCA",nrow(dat1)/2), rep("Subsequent DCA",nrow(dat1)/2) )

both <- rbind(dat1,dat2)
both$eff <- c(rep("Effect of two-paths",102),rep("Effect of mutual degree",102))

gg.margins <- ggplot(both, aes(x=x, y=y, group=mod, color=mod, linetype=mod)) +
    geom_ribbon(aes(ymin=l.ci,ymax=u.ci), alpha=0.2, color=NA) +
    geom_line(aes(y=y,x=x),size=0.5, alpha=0.75) +
    ylab("Probability of DCA") +
    scale_color_manual(
        "",
        labels=c("First agreement        ", "Subsequent agreements"),
        values=c("#2c7bb6","#d7191c")
    ) +
    scale_linetype_manual(
        "",
        labels=c("First agreement        ", "Subsequent agreements"),
        values=c(1,2)
    ) +
    scale_x_continuous(
        "",
        labels=c("Min.","","Med.","","Max.")
    ) +
    facet_grid(. ~ eff) +
    theme(
        axis.title.y = element_text(vjust=1,size=8),
        axis.title.x = element_blank(),
        legend.position="bottom",
        legend.margin=margin(0,0,0,0),
        legend.text=element_text(size=8),
        axis.text.y=element_text(size=7),
        axis.text.x=element_text(size=7),
        plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm")
    )

pdf("figures/Fig10.pdf", width=6, height=3, paper="special")
gg.margins
dev.off()

## Clear the workspace
rm(list=setdiff(ls(), "dat"))





##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
## Figure 11 - Interaction models with plots
##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

source("zz.supplements/functions.R")

probs <- read.csv("output/MarginsPooledLogit.csv", header=TRUE)

nms <- c("IIGOS", "IINST", "AMBASS", "ATOP")
lab <- c(rbind(
    paste(nms, "2path", sep=""),
    paste(nms, "2pathSE", sep=""),
    paste(nms, "degree", sep=""),
    paste(nms, "degreeSE", sep="")	
))
names(probs) <- lab

## Grab the estimates for IGOs first
use <- probs[,seq(1,ncol(probs),2)]
use <- use[,1:4]

dat1 <- data.frame(
    x = vector(),
    y = vector(),
    grp = character()
)

## Smooth with splines, put into a data frame
for ( k in nms[1:2] ) {
    for ( w in c("2path","degree") ) {
        drp <- use[[paste(k, w, sep="")]]
        vec <- vector()
        for ( i in 1:3 ) {
            vec <- c(vec, predict(interpSpline(seq(0,1,0.1), drp[ seq(i,length(drp),3) ]))$y)
        }
        df <- data.frame(
            x = vec,
            y = rep(predict(interpSpline(seq(0,1,0.1),seq(0,1,0.1)))$x),
            grp = c(
                rep("90th",(length(vec)/3)),
                rep("50th",(length(vec)/3)),
                rep("10th",(length(vec)/3))
            ),
            var = paste(k, w, sep="")
        )
        dat1 <- rbind(dat1, df)
        rm(drp,vec,df)
    }
}

labs <- c("IGOs x Two-paths", "IGOs x Mutual degree",
          "Inst. IGOs x Two-paths", "Inst. IGOs x Mutual degree")
dat1$grp <- factor(dat1$grp,levels=rev(levels(dat1$grp)))

## Assign each set of results to a unique ggplot object
for ( z in unique(dat1$var) ) {
    if ( z == unique(dat1$var)[1] ) idx <- 1
    dat.use <- dat1[dat1$var==z,]
    gg.out <- ggplot(dat.use, aes(x=y, y=x, group=grp, color=grp, linetype=grp)) +
	geom_line(aes(y=x,x=y),size=0.75, alpha=0.75) +
	ggtitle(labs[idx]) +
	scale_x_continuous(
            labels=c("Min.","","Med.","","Max.")
	) +
	theme(
            axis.title.y = element_text(vjust=1,size=8),
            axis.title.x = element_text(size=8, margin=margin(t=4)),
            axis.text.y=element_text(size=7),
            axis.text.x=element_text(size=7),
            legend.position=c(0.3,0.75),
            legend.key.size=unit(1, "line"),
            legend.margin=margin(0,3,3,3),
            legend.text = element_text(size=8),
            legend.title=element_blank(),
            plot.title = element_text(size = 8, hjust=0.5, margin=margin(b=2))
        )
    
    if ( idx == 1 | idx == 2 ) {
        gg.out <- gg.out +
            scale_color_manual(
                NULL,
                values=c(
                    "10th" = "#4eb3d3",
                    "50th" = "#4eb3d3",
                    "90th" = "#084081"
                ), 
                labels=c(
                    "IGOs at 10th cent.     ",
                    "IGOs at 50th cent.     ",
                    "IGOs at 90th cent."
                )
            ) +
            scale_linetype_manual(
                NULL,
                values=c(
                    "10th" = "dotted",
                    "50th" = "dashed",
                    "90th" = "solid"
                ), 
                labels=c(
                    "IGOs at 10th cent.     ",
                    "IGOs at 50th cent.     ",
                    "IGOs at 90th cent."
                )
            )
    }
    if ( idx == 3 | idx == 4 ) {
        gg.out <- gg.out +
            scale_color_manual(
                NULL,
                values=c(
                    "10th" = "#4eb3d3",
                    "50th" = "#4eb3d3",
                    "90th" = "#084081"
                ), 
                labels=c(
                    "Inst. IGOs at 10th cent.     ",
                    "Inst. IGOs at 50th cent.     ",
                    "Inst. IGOs at 90th cent."
                )
            ) +
            scale_linetype_manual(
                NULL,
                values=c(
                    "10th" = "dotted",
                    "50th" = "dashed",
                    "90th" = "solid"
                ), 
                labels=c(
                    "Inst. IGOs at 10th cent.     ",
                    "Inst. IGOs at 50th cent.     ",
                    "Inst. IGOs at 90th cent."
                )
            )
    }

    if ( idx == 1 | idx == 3 | idx == 5 ) {
        gg.out <- gg.out + xlab("Number of third-party ties") + ylab("pr(DCA)") + 	
            theme(plot.margin=unit(c(0.1,0.1,0.2,0.2),"cm"))
    } else {
        gg.out <- gg.out + xlab("Mean degree centrality") + ylab("") + 
            theme(plot.margin=unit(c(0.1,0.1,0.2,0.1),"cm"))
    }
    gg.out <- gg.out + theme(legend.title=element_blank())
    assign(paste("gg.out", idx, sep=""), gg.out)
    rm(gg.out)
    idx <- idx + 1
}; rm(z,idx)

## Now do the binary interactions, i.e., ambassadors and institutionalized alliances
use <- probs[,seq(1,ncol(probs),2)]
use <- use[1:22,5:8]

dat1 <- data.frame(
    x = vector(),
    y = vector(),
    grp = character()
)

for ( k in nms[3:4] ) {
    for ( w in c("2path","degree") ) {
        drp <- use[[paste(k, w, sep="")]]
        vec <- vector()
        for ( i in 1:2 ) {
            vec <- c(vec, predict(interpSpline(seq(0,1,0.1), drp[ seq(i,length(drp),2) ]))$y)
        }
        df <- data.frame(
            x = vec,
            y = rep(predict(interpSpline(seq(0,1,0.1),seq(0,1,0.1)))$x),
            grp = c( rep("Zero",(length(vec)/2)), rep("One",(length(vec)/2))),
            var = paste(k, w, sep="")
        )
        dat1 <- rbind(dat1, df)
        rm(drp,vec,df)
    }
}

labs <- c("Embassy x Two-paths", "Embassy x Mutual degree",
          "Inst. alliance x Two-paths", "Inst. alliance x Mutual degree")
dat1$grp <- factor(dat1$grp,levels=rev(levels(dat1$grp)))

for ( z in unique(dat1$var) ) {
    if ( z == unique(dat1$var)[1] ) idx <- 1
    dat.use <- dat1[dat1$var==z,]
    gg.out <- ggplot(dat.use, aes(x=y, y=x, group=grp, color=grp, linetype=grp)) +
	geom_line(aes(y=x,x=y),size=0.75, alpha=0.75) +
	ggtitle(labs[idx]) +
	scale_x_continuous(
            labels=c("Min.","","Med.","","Max.")
	) +
	theme(
            axis.title.y = element_text(vjust=1,size=8),
            axis.title.x = element_text(size=8, margin=margin(t=4)),
            axis.text.y=element_text(size=7),
            axis.text.x=element_text(size=7),
            legend.position=c(0.3,0.75),
            legend.key.size=unit(1, "line"),
            legend.margin=margin(0,3,3,3),
            legend.text = element_text(size=8),
            legend.title=element_blank(),
            plot.title = element_text(size = 8, hjust=0.5, margin=margin(b=2))
        )
    
    if ( idx == 1 | idx == 2 ) {
        gg.out <- gg.out +
            scale_color_manual(
                NULL,
                values=c(
                    "One" = "#7bccc4",
                    "Zero" = "#0868ac"
                ),
                labels=c(
                    "No embassy     ",
                    "Embassy"
                )
            ) +
            scale_linetype_manual(
                NULL,
                values=c(
                    "One" = "dashed",
                    "Zero" = "solid"
                ),
                labels=c(
                    "No embassy     ",
                    "Embassy"
                )
            )
    }

    if ( idx == 3 | idx == 4 ) {
        gg.out <- gg.out +
            scale_color_manual(
                NULL,
                values=c(
                    "One" = "#7bccc4",
                    "Zero" = "#0868ac"
                ),
                labels=c(
                    "No inst. alliance     ",
                    "Inst. alliance"
                )
            ) +
            scale_linetype_manual(
                NULL,
                values=c(
                    "One" = "dashed",
                    "Zero" = "solid"
                ),
                labels=c(
                    "No inst. alliance     ",
                    "Inst. alliance"
                )
            )
    }
    
    if ( idx == 1 | idx == 3 ) {
        gg.out <- gg.out + xlab("Number of third-party ties") + ylab("pr(DCA)") +
            theme(plot.margin=unit(c(0.1,0.1,0.2,0.2),"cm"))
    } else {
        gg.out <- gg.out + xlab("Mean degree centrality") + ylab("") +
            theme(plot.margin=unit(c(0.1,0.1,0.2,0.1),"cm"))
    }

    assign(paste("gg.out", idx+4, sep=""), gg.out)
    rm(gg.out)
    idx <- idx + 1
}; rm(z,idx)

## Now bring in the estimates themselves, to plot alongside the marginal effects
ests <- read.csv("output/implications.csv")
ests <- ests[3:35,]
ests[,2:ncol(ests)] <- lapply(ests[,2:ncol(ests)], function(x) as.numeric(as.character(x)))

rws <- c("l_IGOs","l_instIGOs","l_ambassador","l_ATOP")

for ( i in rws[1:2] ) { # Do continuous interactions first
    if ( i == rws[1] ) idx <- 1
    use1 <- data.frame(
        b = c(
            ests[ests$X == "l_DCA2paths", paste("est", idx, sep="")],
            ests[ests$X == "l_DCAdegree", paste("est", idx, sep="")],
            ests[ests$X == i, paste("est", idx, sep="")],
            ests[ests$X == paste("c.l_DCA2paths#c.", i, sep=""), paste("est", idx, sep="")],
            ests[ests$X == "l_DCA2paths", paste("est", idx + 1, sep="")],
            ests[ests$X == "l_DCAdegree", paste("est", idx + 1, sep="")],
            ests[ests$X == i, paste("est", idx + 1, sep="")],
            ests[ests$X == paste("c.l_DCAdegree#c.", i, sep=""), paste("est", idx + 1, sep="")]
        ),
        se = c(
            ests[ests$X == "l_DCA2paths", paste("X.", idx, sep="")],
            ests[ests$X == "l_DCAdegree", paste("X.", idx, sep="")],
            ests[ests$X == i, paste("X.", idx, sep="")],
            ests[ests$X == paste("c.l_DCA2paths#c.", i, sep=""), paste("X.", idx, sep="")],
            ests[ests$X == "l_DCA2paths", paste("X.", idx + 1, sep="")],
            ests[ests$X == "l_DCAdegree", paste("X.", idx + 1, sep="")],
            ests[ests$X == i, paste("X.", idx + 1, sep="")],
            ests[ests$X == paste("c.l_DCAdegree#c.", i, sep=""), paste("X.", idx + 1, sep="")]
        ),
        var = c(
            c("Two-paths", "Mutual degree", i, paste(i, " X 2paths", sep="")),
            c("Two-paths", "Mutual degree", i, paste(i, " X degree", sep=""))
        ),
        mod = c(
            rep(paste(i, idx, sep=""), 4),
            rep(paste(i, idx+1, sep=""), 4)
        )
    )
    
    if ( i == rws[1] ) {
        use.c <- use1
    } else {
        use.c <- rbind(use.c,use1)
    }
    rm(use1)
    idx <- idx + 2
}; rm(i,idx)

for ( i in rws[3:4] ) { # Then dichotomous interactions
    if ( i == rws[3] ) idx <- 5
    j <- paste("1.", i, sep="")
    use1 <- data.frame(
        b = c(
            ests[ests$X == "l_DCA2paths", paste("est", idx, sep="")],
            ests[ests$X == "l_DCAdegree", paste("est", idx, sep="")],
            ests[ests$X == j, paste("est", idx, sep="")],
            ests[ests$X == paste(j, "#c.l_DCA2paths", sep=""), paste("est", idx, sep="")],
            ests[ests$X == "l_DCA2paths", paste("est", idx + 1, sep="")],
            ests[ests$X == "l_DCAdegree", paste("est", idx + 1, sep="")],
            ests[ests$X == j, paste("est", idx + 1, sep="")],
            ests[ests$X == paste(j, "#c.l_DCAdegree", sep=""), paste("est", idx + 1, sep="")]
        ),
        se = c(
            ests[ests$X == "l_DCA2paths", paste("X.", idx, sep="")],
            ests[ests$X == "l_DCAdegree", paste("X.", idx, sep="")],
            ests[ests$X == j, paste("X.", idx, sep="")],
            ests[ests$X == paste(j, "#c.l_DCA2paths", sep=""), paste("X.", idx, sep="")],
            ests[ests$X == "l_DCA2paths", paste("X.", idx + 1, sep="")],
            ests[ests$X == "l_DCAdegree", paste("X.", idx + 1, sep="")],
            ests[ests$X == j, paste("X.", idx + 1, sep="")],
            ests[ests$X == paste(j, "#c.l_DCAdegree", sep=""), paste("X.", idx + 1, sep="")]
        ),
        var = c(
            c("Two-paths", "Mutual degree", i, paste(i, " X 2paths", sep="")),
            c("Two-paths", "Mutual degree", i, paste(i, " X degree", sep=""))
        ),
        mod = c(
            rep(paste(i, idx, sep=""), 4),
            rep(paste(i, idx+1, sep=""), 4)
        )
    )
    
    use.c <- rbind(use.c,use1)
    rm(use1,j)
    idx <- idx + 2
}; rm(i,idx)

## Standardize SEs and rescale estimates
use.c$b <- use.c$b * ( 0.5 / use.c$se )
use.c$se <- use.c$se * ( 0.5 / use.c$se )
use.c$lo <- use.c$b - ( 1.96 * use.c$se)
use.c$hi <- use.c$b + ( 1.96 * use.c$se)

use.c$x <- c(
    "Two-paths","Mutual degree","IGOs","Interaction",
    "Two-paths","Mutual degree","IGOs","Interaction",
    "Two-paths","Mutual degree","Inst. IGOs","Interaction",
    "Two-paths","Mutual degree","Inst. IGOs","Interaction",
    "Two-paths","Mutual degree","Embassy","Interaction",
    "Two-paths","Mutual degree","Embassy","Interaction",
    "Two-paths","Mutual degree","Inst. alliance","Interaction",
    "Two-paths","Mutual degree","Inst. alliance","Interaction"
)
use.c$x <- factor(
    use.c$x,
    levels=rev(c("Two-paths","Mutual degree","IGOs","Inst. IGOs",
                 "Embassy","Inst. alliance","Interaction")))

use.c$p.cols <- ""
for (ii in 1:nrow(use.c)) {
    if (use.c[ii,"lo"] < 0 && use.c[ii,"hi"] > 0) {
        use.c$p.cols[ii] <- "insig" # Color for insignificant estimates
    } else {
        use.c$p.cols[ii] <- "sig" # Color for significant estimates
    }
}

use.c$mod <- c(
    rep("IGOs x Two-paths",4),
    rep("IGOs x Degree",4),
    rep("Inst. IGOs x Two-paths",4),
    rep("Inst. IGOs x Degree",4),
    rep("Embassy x Two-paths",4),
    rep("Embassy x Degree",4),
    rep("Inst. alliance x Two-paths",4),
    rep("Inst. alliance x Degree",4)
)

marg <- c(0.1,0.1,0.1,0.1)

for ( z in unique(use.c$mod) ) {
    if ( z == unique(use.c$mod)[1] ) idx <- 1
    dat.use <- use.c[use.c$mod==z,]

    gg.ests <- ggplot(dat.use, aes(y=x, x=b, fill=p.cols, color=p.cols)) +
	geom_vline(xintercept=0, lty=2, size=0.35) +
	geom_point(size=1,shape=16) +
	geom_segment(aes(x=lo,xend=hi,yend=x)) +
	scale_color_manual(
            values=c("insig"="#d7191c","sig"="#2c7bb6")
        ) +
 	theme_bw() +
 	theme(
            panel.border = element_blank(),
            panel.grid.minor = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y=element_text(color="black",size=7),
            axis.text.x=element_text(color="black",size=5),
            plot.margin=unit(marg,"cm"),
            axis.line.x = element_line(colour = "black", size=0.05),
            axis.line.y = element_line(colour = "black", size=0.05),
            legend.position="none",
            plot.title = element_text(hjust = 0.5)
 	)
    assign(paste("gg.ests", idx, sep=""), gg.ests)
    idx <- idx + 1
    rm(gg.ests,dat.use)
}

## Combine everything into four plots, with estimates embedded in marginal effects plots
for ( i in c(1,3,5,7)) {
    a1 <- get(paste("gg.out",i, sep=""))
    b1 <- ggplotGrob(get(paste("gg.ests", i, sep="")))
    r1 <- ggplot_build(a1)$layout$panel_ranges[[1]]$y.range
    ymin1 <- r1[1] + (0.55 * (r1[2] - r1[1]))
    ymax1 <- r1[1] + (0.95 * (r1[2] - r1[1]))
    c1 <- a1 + annotation_custom(grob = b1, xmin=0.025, xmax=0.65, ymin=ymin1, ymax=ymax1)

    a2 <- get(paste("gg.out",i+1, sep=""))
    b2 <- ggplotGrob(get(paste("gg.ests", i+1, sep="")))
    r2 <- ggplot_build(a2)$layout$panel_ranges[[1]]$y.range
    ymin2 <- r2[1] + (0.55 * (r2[2] - r2[1]))
    ymax2 <- r2[1] + (0.95 * (r2[2] - r2[1]))
    c2 <- a2 + annotation_custom(grob = b2, xmin=0.025, xmax=0.65, ymin=ymin2, ymax=ymax2)

    pdf(paste("figures/Fig11-", i, ".pdf", sep=""), width=6, height=3, paper="special", onefile=FALSE)
    grid_arrange_shared_legend(c1,c2,ncol=2,nrow=1, widths=c(1,1), position="bottom")
    dev.off()

    rm(a1,b1,r1,ymin1,ymax1,c1,a2,b2,r2,ymin2,ymax2,c2)
}

## Clear the workspace
rm(list=setdiff(ls(), "dat"))






##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
## Figure 12 - ROC and PR plots for OOS prediction
##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

preds <- read.csv("output/PredictOOS-FE.csv")

## Concatenate all predictions together
dat.obj <- matrix(ncol=3,nrow=0)
for ( i in 1:10 ) {
    mat <- cbind(
        preds[paste("prNets",i,sep="")],
        preds[paste("prNoNets",i,sep="")],
        preds$DCA
    )
    names(mat) <- c("nets","nonets","dca")
    dat.obj <- rbind(dat.obj,mat)
    rm(mat)
}
dat.obj <- na.omit(dat.obj)

## Generate PR and ROC curves, along with AUC values
pr.nets <- pr.curve(scores.class0=dat.obj$nets, weights.class0=dat.obj$dca, curve=TRUE)
pr.auc.nets <- pr.nets$auc.integral
pr.curve.nets <- pr.nets$curve[,1:2]
pr.curve.nets <- rbind(c(1,0),pr.curve.nets)

roc.nets <- roc.curve(scores.class0=dat.obj$nets, weights.class0=dat.obj$dca, curve=TRUE)
roc.auc.nets <- roc.nets$auc
roc.curve.nets <- roc.nets$curve[,1:2]

pr.nonets <- pr.curve(scores.class0=dat.obj$nonets, weights.class0=dat.obj$dca, curve=TRUE)
pr.auc.nonets <- pr.nonets$auc.integral
pr.curve.nonets <- pr.nonets$curve[,1:2]
pr.curve.nonets <- rbind(c(1,0),pr.curve.nonets)

roc.nonets <- roc.curve(scores.class0=dat.obj$nonets, weights.class0=dat.obj$dca, curve=TRUE)
roc.auc.nonets <- roc.nonets$auc
roc.curve.nonets <- roc.nonets$curve[,1:2]

## Put the above results together in a ggplot data frame
df <- data.frame(
    x = c(roc.curve.nonets[,1],roc.curve.nets[,1],pr.curve.nonets[,1],pr.curve.nets[,1]),
    y = c(roc.curve.nonets[,2],roc.curve.nets[,2],pr.curve.nonets[,2],pr.curve.nets[,2]),
    net = c(
        rep("nonets",nrow(roc.curve.nonets)),
        rep("nets",nrow(roc.curve.nets)),
        rep("nonets",nrow(pr.curve.nonets)),
        rep("nets",nrow(pr.curve.nets))
    ),
    rocpr = c(
        rep("roc",nrow(roc.curve.nonets)),
        rep("roc",nrow(roc.curve.nets)),
        rep("pr",nrow(pr.curve.nonets)),
        rep("pr",nrow(pr.curve.nets))	
    )
)

df.use <- df[df$rocpr=="roc",]
roc.plot <- ggplot(df.use, aes(x=x,y=y,group=net,color=net,size=net,linetype=net)) +
    geom_abline(intercept = 0, slope = 1, linetype=2, size=0.25) +
    geom_line() +
    scale_color_manual(
    	values = c("#2c7bb6","#d7191c"),
    	labels = c(
            paste("With network effects (AUC = ", round(roc.auc.nets, digits=2), ")", sep=""),
            paste("No network effects (AUC = ", round(roc.auc.nonets, digits=2), ")", sep="")
    	)
    ) +
    scale_linetype_manual(
    	values=c("solid","dashed"),
    	labels = c(
            paste("With network effects (AUC = ", round(roc.auc.nets, digits=2), ")", sep=""),
            paste("No network effects (AUC = ", round(roc.auc.nonets, digits=2), ")", sep="")
    	)
    ) +
    ggtitle("ROC curves") +
    ylab("True positive rate (TPR)") +
    xlab("False positive rate (FPR)") +
    scale_size_manual(values=c(0.5,0.1), guide=FALSE) +
    scale_x_continuous(limits = c(0,1)) +
    scale_y_continuous(limits = c(0,1)) +
    theme(
        legend.position=c(0.95,0.05),
        legend.justification=c(1,0),
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.margin=margin(0,3,3,3),
        legend.key.size=unit(0.75, "line"),
        legend.text=element_text(size=9),
        plot.margin=unit(c(0,0.1,0.1,0.1),"cm"),
        axis.title.x = element_text(vjust=-0.5, size=10),
        axis.title.y = element_text(vjust=1, size=10),
        axis.text.y=element_text(size=7),
        axis.text.x=element_text(size=7),
        plot.title=element_text(hjust=0.5,size=11)
    )


rm(df.use)
df.use <- df[df$rocpr=="pr",]
pr.plot <- ggplot(df.use, aes(x=x,y=y,group=net,color=net,size=net,linetype=net)) +
    geom_abline(intercept = 1, slope = -1, linetype=2, size=0.25) +
    geom_line(aes(x=x,y=y),data=df.use) +
    scale_color_manual(
    	values = c("#2c7bb6","#d7191c"),
    	labels = c(
            paste("With network effects (AUC = ", round(pr.auc.nets, digits=2), ")", sep=""),
            paste("No network effects (AUC = ", round(pr.auc.nonets, digits=2), ")", sep="")
    	)
    ) +
    scale_linetype_manual(
    	values = c("solid","dashed"),
    	labels = c(
            paste("With network effects (AUC = ", round(pr.auc.nets, digits=2), ")", sep=""),
            paste("No network effects (AUC = ", round(pr.auc.nonets, digits=2), ")", sep="")
    	)
    ) +
    ggtitle("PR curves") +
    ylab("Precision") +
    xlab("Recall (TPR)") +
    scale_x_continuous(limits = c(0,1)) +
    scale_y_continuous(limits = c(0,1)) +
    scale_size_manual(values=c(0.5,0.1), guide=FALSE) +
    theme(
        legend.position=c(0.65,0.05),
        legend.justification=c(1,0),
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.margin=margin(0,3,3,3),
        legend.key.size=unit(0.75, "line"),
        legend.text=element_text(size=9),
        plot.margin=unit(c(0,0.1,0.1,0.1),"cm"),
        axis.title.x = element_text(vjust=-0.5, size=10),
        axis.title.y = element_text(vjust=1, size=10),
        axis.text.y=element_text(size=7),
        axis.text.x=element_text(size=7),
        plot.title=element_text(hjust=0.5, size=11)
    )

pdf("figures/Fig12.pdf", width=8, height=4, paper="special")
par(mar=c(0,0,0,0))
grid.arrange(roc.plot,pr.plot,ncol=2,top="", widths=c(1,1))
dev.off()

## Clear the workspace
rm(list=setdiff(ls(), "dat"))




##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
## Figure 13 - Geogaphic map of OOS predictions
##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

source("zz.supplements/functions.R")
preds <- read.csv("output/PredictOOS-FE.csv")

## Find the "mean" prediction across the various samples
dat.obj <- data.frame(
    ccode1 = preds$ccode1,
    ccode2 = preds$ccode2,
    year = preds$year,
    mnsNets = rowMeans(preds[,paste("prNets",seq(1,10),sep="")], na.rm=TRUE),
    mnsNoNets = rowMeans(preds[,paste("prNoNets",seq(1,10),sep="")], na.rm=TRUE),
    dca = preds$DCA
)

## Grab all states the naive model gets wrong, count DCAs for each
pairs <- dat.obj[ dat.obj$mnsNets >= 0.5 & dat.obj$mnsNoNets < 0.5 & dat.obj$dca==1, c("ccode1","ccode2") ]
pairs <- c(pairs$ccode1,pairs$ccode2)
sums <- as.matrix(table(pairs))

## Make a map out of it, with a heatmap-style legend
yr.use <- 2010
pdf("figures/Fig13.pdf", width=6.5, height=3, paper="special")
par(mar=c(0,0,0,0))
layout(matrix(c(1,2),2,1,byrow=TRUE), heights=c(1,0.15))

map <- cshp(date=as.Date(paste(yr.use, "-12-31", sep=""), useGW=F))
m <- match(map$COWCODE, rownames(sums))
dat.use <- sums[m,]
dat.use[is.na(dat.use)] <- 0 # Any missings can be safely assumed to be zero
print(max(dat.use, na.rm=T))
names(dat.use) <- map$FEATUREID
mapM <- spCbind(map,dat.use)
colFunc <- colorRampPalette(brewer.pal(7,"GnBu"), bias = 2, space = "rgb", interpolate = "linear")
colCodes <- colFunc(length(unique(dat.use)))
leg.cols <- colFunc(100)
map.cols <- sapply(dat.use, function(x) colCodes[which(sort(unique(dat.use)) == x)])
plot(mapM, bty="n", col=map.cols, lwd=0.1)
print(par("usr"))

plot(0,type='n',axes=FALSE,ann=FALSE, xlim=c(-1.5,1.25), ylim=c(-1.25,1.5))
par(mar=c(0,0,0,0))
print(par("usr"))
legend.map(col=leg.cols,ends=c(0,12),tsz=0.5,xoff=0,yoff=0.5,lgth=16.25,tit="Network model's # of TP improvements",toff=0.85)

dev.off()

## Clear the workspace
rm(list=setdiff(ls(), "dat"))





##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
## Figure 14 - Compare the OOS predictions
##~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

source("zz.supplements/functions.R")
preds <- read.csv("output/PredictOOS-FE.csv")

## How many dyads to compare?
n.dyads <- 10

## Concatenate predictions while retaining dyad ids
dat.obj.id <- matrix(ncol=11,nrow=0)
colnames(dat.obj.id) <- c("ccode1","ccode2","abbrev1","abbrev2","year","dyadid","DCA","prNets","prNets.se","prNoNets","prNoNets.se")
for ( i in 1:10 ) {
    use <- cbind(
        preds[c("ccode1","ccode2","abbrev1","abbrev2","year","dyadid","DCA")],
        preds[paste("prNets", i, sep="")],
        preds[paste("prNets", i, "_se", sep="")],
        preds[paste("prNoNets", i, sep="")],
        preds[paste("prNoNets", i, "_se", sep="")]
    )
    use <- use[complete.cases(use),]
    colnames(use) <- c("ccode1","ccode2","abbrev1","abbrev2",
                       "year","dyadid","DCA","prNets",
                       "prNets.se","prNoNets","prNoNets.se")
    dat.obj.id <- rbind(dat.obj.id, use)
    rm(use)
}

## Find the largest differences in predictions
dat.obj.id$diff <- dat.obj.id$prNets - dat.obj.id$prNoNets
dat.obj.id <- dat.obj.id[order(dat.obj.id$diff),]
dat.obj.id$test <- dat.obj.id$prNets - (1.96 * dat.obj.id$prNets.se)
dat.obj.id <- dat.obj.id[dat.obj.id$test>0,]
dat.obj.id$test <- dat.obj.id$prNoNets - (1.96 * dat.obj.id$prNoNets.se)
dat.obj.id <- dat.obj.id[dat.obj.id$test>0,]
dat.obj.id <- dat.obj.id[-dat.obj.id$test]

heads <- head(dat.obj.id,100)
heads <- heads[!duplicated(heads$dyadid),]

tails <- tail(dat.obj.id,100)
tails <- tails[order(tails$diff,decreasing=TRUE),]
tails <- tails[!duplicated(tails$dyadid),]

use.id <- rbind(
    heads[1:n.dyads,],
    tails[1:n.dyads,]
)

use.id$mod <- c(
    rep("Non-network model predicts a DCA",n.dyads),
    rep("Network model predicts a DCA",n.dyads)
)
use.id$lab <- paste(use.id$abbrev1, "-", use.id$abbrev2, "\n", use.id$year, sep="")
rownames(use.id) <- 1:(nrow(use.id))

use.id <- rbind(
    use.id[order(use.id$prNoNets),][(n.dyads+1):(n.dyads*2),],
    use.id[order(use.id$prNets),][(n.dyads+1):(n.dyads*2),]
)

use.plot <- use.id

use.gg <- data.frame(
    n = rep(use.plot$lab,2),
    x = rep(1:n.dyads,4),
    y = c(use.plot$prNets,use.plot$prNoNets),
    ci.u = c(use.plot$prNets,use.plot$prNoNets) + ( 1.96 * c(use.plot$prNets.se,use.plot$prNoNets.se)),
    ci.l = c(use.plot$prNets,use.plot$prNoNets) - ( 1.96 * c(use.plot$prNets.se,use.plot$prNoNets.se)),
    s = c(use.plot$prNets.se,use.plot$prNoNets.se),
    g = c(rep("nets",nrow(use.plot)),rep("nonets",nrow(use.plot))),
    m = rep(use.plot$mod,2),
    d = rep(use.plot$DCA,2)
)

## Specify colors for DCA signed vs. DCA not signed
use.gg$dots <- NA
use.gg[use.gg$d==1,]$dots <- "black"
use.gg[use.gg$d==0,]$dots <- "white"

out1 <- use.gg[c(11:20,31:40),]
gg.out1 <- ggplot(out1, aes(x=x, y=y, group=g, fill=g)) +
    geom_hline(aes(yintercept=0.5), color="red", linetype=2, alpha=0.5) +
    geom_bar(stat="identity", position="dodge", width=.65) +
    geom_errorbar(aes(ymax=ci.u, ymin=ci.l), position=position_dodge(0.65), width=0.25, alpha=0.4) +
    geom_point(data=out1, aes(x=x, y=0, shape="Stuff", color=out1$dots), fill="white", size=3, shape=16) +
    xlab("") +
    ylab("Predicted probability of DCA") +
    ggtitle("Network model predicts a DCA") +
    scale_x_continuous(
        breaks=1:10,
	labels=unique(as.character(out1$n))
    ) +
    scale_y_continuous(limits=c(0,1)) +
    scale_fill_manual("", values=c("#0868ac","#7bccc4"), labels=c("With network effects     ", "Without network effects")) +
    scale_color_manual("", values=c("black","white"), labels=c("DCA signed     ","No DCA signed")) +
    theme(
        legend.position="none",
        plot.margin=unit(c(0.1,0.1,0.2,0.1),"cm"),
        axis.title.y=element_text(size=9,vjust=0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_text(size=5,angle=0,vjust=0.5,color="black"),
        axis.text.y=element_text(size=7),
        plot.title = element_text(hjust = 0.5, size=10, margin=margin(b=3))
    )

out2 <- use.gg[c(1:10,21:30),]
gg.out2 <- ggplot(out2, aes(x=x, y=y, group=g, fill=g)) +
    geom_hline(aes(yintercept=0.5), color="red", linetype=2, alpha=0.5) +
    geom_bar(stat="identity", position="dodge", width=.65) +
    geom_errorbar(aes(ymax=ci.u, ymin=ci.l), position=position_dodge(0.65), width=0.25, alpha=0.4) +
    geom_point(data=out2, aes(x=x, y=0, shape="Stuff", color=out2$dots), fill="white", size=3, shape=16) +
    xlab("") +
    ggtitle("Non-network model predicts a DCA") +
    scale_x_continuous(
        breaks=1:10,
	labels=unique(as.character(out2$n))
    ) +
    scale_y_continuous(limits=c(0,1)) +
    scale_fill_manual("", values=c("#0868ac","#7bccc4"), labels=c("With network effects     ", "Without network effects     ")) +
    scale_color_manual("", values=c("black","white"), labels=c("DCA signed     ","No DCA signed")) +
    theme(
        legend.position="bottom",
        legend.margin=margin(0.25,0.25,0.25,0.25),
        legend.text=element_text(size=9),
        legend.key.size = unit(1, "line"),
        plot.margin=unit(c(0.1,0,0.2,0),"cm"),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_text(size=5,angle=0,vjust=0.5,color="black"),
        axis.text.y=element_blank(),
        plot.title = element_text(hjust = 0.5, size=10, margin=margin(b=3))
    )

## Grab legend from ggplot object, then strip legend
legend <- g_legend(gg.out2)
gg.out2 <- gg.out2 + theme(legend.position="none")

## Specify viewport layout
Layout <- grid.layout(nrow = 2, ncol = 2, widths = unit(c(1, 0.875), c("null", "null")), heights = unit(c(1, 0.15), c("null", "null")))

pdf("figures/Fig14.pdf", width=8, height=3, paper="special")
par(mar=c(0,0,0,0))
grid.newpage()
pushViewport(viewport(layout = Layout))
print(gg.out1, vp = subplot(1, 1))
print(gg.out2, vp = subplot(1, 2))
pushViewport(subplot(2,1:2))
grid.draw(legend)
dev.off()

## Clear the workspace
rm(list=setdiff(ls(), "dat"))

