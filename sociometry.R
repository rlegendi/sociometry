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
	read.csv(file=file.path(DATA_DIR, csvName), head=TRUE, as.is=TRUE, sep=CSV_SEPARATOR)
}

### Graphing methods
plotGraph <- function(relations, title, showLabels) {
	dir.create(OUTPUT_BASE_DIR, showWarnings = FALSE)
	
	g <- graph.data.frame(d=relations, directed=T)
	
	if (SIMPLIFY) {
		title <- paste(title, "Simplified")
		g <- simplify(g, remove.multiple = T, remove.loops = F)
	}
	
	if (!showLabels) {
		title <- paste(title, "Anon")
		V(g)$label <- NA
	}
	
	fileName <- paste(title, ifelse(showLabels, "Labels", "NoLabels"))
	
	png(paste(OUTPUT_BASE_DIR, "\\", fileName, ".png", sep=""), width=CHART_WIDTH, height=CHART_HEIGHT)
	plot(g, main=title)
	dev.off()
}

plotLeads <- function(csv, showLabels) {
	rel <- data.frame(from=csv[, "Your.name."], to=csv[,"Who.would.you.choose.to.lead.you."])
	
	plotGraph(rel, "Leads", showLabels)
}

plotWork <- function(csv, showLabels) {
	rel <- data.frame(
			from=c(rep(csv[,"Your.name."], 4)),
			to =c(
					csv[, "Who.would.you.choose.to.lead.you."],
					csv[, "Who.would.you.choose.to..be.with.you.on.a.four.member.team..Name.1"],
					csv[, "Name.2"],
					csv[, "Name.3"]
			))
	
	plotGraph(rel, "Work", showLabels)
}

plotFun <- function(csv, showLabels) {
	rel <- data.frame(
			from=c(rep(csv[,"Your.name."], 3)),
			to =c(
					csv[, "Who.would.you.hang.out.with.just.for.fun..Name.1"],
					csv[, "Name.22"],
					csv[, "Name.32"]
			))
	
	plotGraph(rel, "Fun", showLabels)
}

plotAll <- function(csv, showLabels) {
	rel <- data.frame(
			from=c(rep(csv[,"Your.name."], 7)),
			to=c(
					csv[, "Who.would.you.choose.to.lead.you."],
					csv[, "Who.would.you.choose.to..be.with.you.on.a.four.member.team..Name.1"],
					csv[, "Name.2"],
					csv[, "Name.3"],
					csv[, "Who.would.you.hang.out.with.just.for.fun..Name.1"],
					csv[, "Name.22"],
					csv[, "Name.32"]
			))
	
	plotGraph(rel, "All", showLabels)
}

### Processing

csv <- readCsv("Sociometry - 2016Q4.csv")

for (showLabels in c(TRUE, FALSE)) {
	plotLeads(csv, showLabels)
	plotWork(csv, showLabels)
	plotFun(csv, showLabels)
	plotAll(csv, showLabels)
}

alarm()

# write.graph(g, "test.net", format="pajek")
