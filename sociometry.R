# R scripts for sociometric analyisis of software development teams.
# 
# Author: rlegendi
###############################################################################

rm(list=ls()) # Cleaning up workspace to prevent pollution. I'm a pro in that :-)

### Libraries
library("igraph", "1.0.1")

### Debugging
options(error=stop)       # Stop on error
#options(error=recover)   # Debug on error
#options(warn=2)          # Warnings handled as errors too

### Global settings

# Project data directory
DATA_DIR <- "data"
OUTPUT_BASE_DIR <- "pix"
CSV_SEPARATOR <- ";"
SIMPLIFY <- TRUE
CHART_WIDTH <- 800
CHART_HEIGHT <- 600

# Used for community detection, denotes the number of communities to taxify.
# See https://arxiv.org/abs/physics/0512106 for the details.
RANDOMWALK_COMMUNITY_NUMBER <- 4

# Determines if messages should be logged in the output
MESSAGES <- TRUE

### Utility functions

# Compares two sequences.
#
# Returns: TRUE, if they are equal, FALSE otherwise
alleq <- function(xs, ys) {
	identical(all.equal(xs, ys), TRUE)
}

# Logs the specified message to the output if required (i.e., the `MESSAGES`.
# variable is set to `TRUE`).
#
# msg: log message to print
log <- function(msg) {
	if (MESSAGES) {
		print(msg)
	}
}

# Reads a CSV file.
#
# Pre-condition: CSV file must contain a header (exactly 1 line)
# Returns: the read data
readCsv <- function(csvName) {
	read.csv(file=file.path(DATA_DIR, csvName), head=TRUE, as.is=TRUE, sep=CSV_SEPARATOR, strip.white=TRUE)
}

### Graphing methods
# layoutHandling - default is idontcare, creates a new layout for each graph
plotGraph <- function(relations, title, showLabels, layoutHandling=c("idontcare", "store", "forget")) {
	dir.create(OUTPUT_BASE_DIR, showWarnings = FALSE)
	
	g <- graph.data.frame(d=relations, directed=T)
	
	if (SIMPLIFY) {
		title <- paste(title, "Simplified")
		g <- simplify(g, remove.multiple = T, remove.loops = F)
	}
	
	if (layoutHandling == "store") {
		act.layout <<- layout.fruchterman.reingold(g)
	}
	
	if (!showLabels) {
		title <- paste(title, "Anon")
		V(g)$label <- NA
	}
	
	membership <- cut_at(walktrap.community(g), no=RANDOMWALK_COMMUNITY_NUMBER)
	
	fileName <- paste(title, ifelse(showLabels, "Labels", "NoLabels"))
	
	png(paste(OUTPUT_BASE_DIR, "\\", fileName, ".png", sep=""), width=CHART_WIDTH, height=CHART_HEIGHT)
	plot(g, main=title, layout=act.layout, vertex.color= rainbow(10, .8, .8, alpha=.8)[membership], vertex.size=degree(g, mode="in")+8)
	dev.off()
	
	if (layoutHandling == "forget") {
		act.layout <<- NULL
	}
}

plotLeads <- function(csv, showLabels, layoutHandling=c("store", "forget")) {
	rel <- data.frame(from = csv[, "Your.name."], to = csv[,"Who.would.you.choose.to.lead.you."])
	
	plotGraph(rel, "Leads", showLabels, layoutHandling=layoutHandling)
}

plotWork <- function(csv, showLabels, layoutHandling=c("store", "forget")) {
	rel <- data.frame(
			from = c(rep(csv[,"Your.name."], 4)),
			to = c(
					csv[, "Who.would.you.choose.to.lead.you."],
					csv[, "Who.would.you.choose.to..be.with.you.on.a.four.member.team..Name.1"],
					csv[, "Name.2"],
					csv[, "Name.3"]
			))
	
	plotGraph(rel, "Work", showLabels, layoutHandling=layoutHandling)
}

plotFun <- function(csv, showLabels, layoutHandling=c("store", "forget")) {
	rel <- data.frame(
			from = c(rep(csv[,"Your.name."], 3)),
			to = c(
					csv[, "Who.would.you.hang.out.with.just.for.fun..Name.1"],
					csv[, "Name.22"],
					csv[, "Name.32"]
			))
	
	plotGraph(rel, "Fun", showLabels, layoutHandling=layoutHandling)
}

plotAll <- function(csv, showLabels, layoutHandling=c("store", "forget")) {
	rel <- data.frame(
			from = c(rep(csv[,"Your.name."], 7)),
			to = c(
					csv[, "Who.would.you.choose.to.lead.you."],
					csv[, "Who.would.you.choose.to..be.with.you.on.a.four.member.team..Name.1"],
					csv[, "Name.2"],
					csv[, "Name.3"],
					csv[, "Who.would.you.hang.out.with.just.for.fun..Name.1"],
					csv[, "Name.22"],
					csv[, "Name.32"]
			))
	
	plotGraph(rel, "All", showLabels, layoutHandling=layoutHandling)
}

### Processing

csv <- readCsv("Sociometry - 2017H1.csv")

plotLeads(csv, TRUE, "store")
plotLeads(csv, FALSE, "forget")

plotWork(csv, TRUE, "store")
plotWork(csv, FALSE, "forget")

plotFun(csv, TRUE, "store")
plotFun(csv, FALSE, "forget")

plotAll(csv, TRUE, "store")
plotAll(csv, FALSE, "forget")

alarm()

# write.graph(g, "test.net", format="pajek")
