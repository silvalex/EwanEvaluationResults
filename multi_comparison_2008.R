#! /bin/env Rscript

library(ggplot2)
library(dplyr)
library(gdata)
library(xtable)

#-------------------------------------

# Magic numbers

conf.level=0.95
significance.warning=999
significance.error=888
print.width=200
decimal.places=2
p.decimal.places=6
time.in.secs=TRUE

options("width"=print.width)

pretty.approach.labels <- c("jacky", "ewan", "alex")
set.numbers <- c("1", "2", "4")
overallResultTable <- read.table("combinedResults.stat")
colnames(overallResultTable) <- c("approach", "run", "dataset", "time", "BestAvail", "BestRelia", "BestTime", "BestCost")


# Calculate final fitness

calculateFinalFitness <- function(fitness.data,w1,w2,w3,w4,total.data) {
  a <- as.numeric(fitness.data['BestAvail'])
  r <- as.numeric(fitness.data['BestRelia'])
  t <- as.numeric(fitness.data['BestTime'])
  c <- as.numeric(fitness.data['BestCost'])
  dat.num <- as.numeric(fitness.data['dataset'])

  filtered.data <- total.data[total.data$"dataset"==dat.num,]
  minA <- min(filtered.data[,c("BestAvail")])
  maxA <- max(filtered.data[,c("BestAvail")])
  minR <- min(filtered.data[,c("BestRelia")])
  maxR <- max(filtered.data[,c("BestRelia")])
  minT <- min(filtered.data[,c("BestTime")])
  maxT <- max(filtered.data[,c("BestTime")])
  minC <- min(filtered.data[,c("BestCost")])
  maxC <- max(filtered.data[,c("BestCost")])

  normA <- ifelse(maxA - minA == 0, 1, (a - minA)/(maxA - minA))
  normR <- ifelse(maxR - minR == 0, 1, (r - minR)/(maxR - minR))
  normT <- ifelse(maxT - minT == 0, 1, (maxT - t)/(maxT - minT))
  normC <- ifelse(maxC - minC == 0, 1, (maxC - c)/(maxC - minC))
  fitness <- w1*normA + w2*normR + w3*normT + w4*normC
  return(fitness)
}

overallResultTable$"FinalFitness" <- apply(overallResultTable[,c("BestAvail","BestRelia","BestTime","BestCost","dataset")],1,calculateFinalFitness,w1=0.25,w2=0.25,w3=0.25,w4=0.25,total.data=overallResultTable)

#-------------------------------------

# Run significance tests
significance <- function(first.group, second.group) {
  p.value <- tryCatch(wilcox.test(first.group, second.group, paired = TRUE, conf.int=T, conf.level=conf.level)$p.value,
  error = function(e) return(significance.error))
  return(p.value)
}

significanceForDataset <- function(dataset, approach1label, approach2label, table, dimension) {
  filtered.data1 <- table[table$"dataset" == dataset & table$"approach" == approach1label,]
  filtered.data2 <- table[table$"dataset" == dataset & table$"approach" == approach2label,]

  p.value <- significance(as.numeric(unlist(filtered.data1[dimension])), as.numeric(unlist(filtered.data2[dimension])))
  p.value <- round(p.value, p.decimal.places)

  mean.data1 <- mean(as.numeric(unlist(filtered.data1[dimension])))
  mean.data2 <- mean(as.numeric(unlist(filtered.data2[dimension])))

  ifelse (p.value < (1 - conf.level),
    (ifelse (mean.data1 > mean.data2,
	 p.value <- sprintf("%.6f (%s)", p.value, approach1label),
	 p.value <- sprintf("%.6f (%s)", p.value, approach2label)
    )),
    p.value <- sprintf("%.6f (N/A)", p.value)
  )
  return(p.value)
}

significanceForAllDatasets <- function(app1label, app2label, dim) {
	sigVals <- cbind(lapply(set.numbers, significanceForDataset, approach1label=app1label, approach2label=app2label, table=overallResultTable, dimension=dim))
	return(sigVals)
}

# A magical function stolen from the Web to create all possible name combos
expand.grid.unique <- function(x, y, include.equals=FALSE){
    x <- unique(x)
    y <- unique(y)
    g <- function(i){
        z <- setdiff(y, x[seq_len(i-include.equals)])
        if(length(z)) cbind(x[i], z, deparse.level=0)
    }
    do.call(rbind, lapply(seq_along(x), g))
}

prettyApproachCombos <- expand.grid.unique(pretty.approach.labels, pretty.approach.labels)
colnames(prettyApproachCombos) <- c("a1", "a2")

createPColNames <- function(name1, name2) {
	return(sprintf("%s vs %s", name1, name2))
}

p.names <- apply(prettyApproachCombos, 1, function(row) createPColNames(row["a1"], row["a2"]))

time.p.values <- apply(prettyApproachCombos, 1, function(row) significanceForAllDatasets(row["a1"], row["a2"], dim="time"))
time.p.values <- do.call("cbind", time.p.values)
time.p.values <- cbind(sprintf("%s", set.numbers), time.p.values)
colnames(time.p.values) <- c("Dataset", unlist(p.names))

fitness.p.values <- apply(prettyApproachCombos, 1, function(row) significanceForAllDatasets(row["a1"], row["a2"], dim="FinalFitness"))
fitness.p.values <- do.call("cbind", fitness.p.values)
fitness.p.values <- cbind(sprintf("%s",set.numbers), fitness.p.values)
colnames(fitness.p.values) <- c("Dataset", unlist(p.names))

#-------------------------------------

# Create Win-Draw-Lose table

simpleSignificanceForDataset <- function(dataset, approach1label, approach2label, table, dimension) {
  filtered.data1 <- table[table$"dataset" == dataset & table$"approach" == approach1label,]
  filtered.data2 <- table[table$"dataset" == dataset & table$"approach" == approach2label,]

  p.value <- significance(as.numeric(unlist(filtered.data1[dimension])), as.numeric(unlist(filtered.data2[dimension])))
  p.value <- round(p.value, p.decimal.places)

  mean.data1 <- mean(as.numeric(unlist(filtered.data1[dimension])))
  mean.data2 <- mean(as.numeric(unlist(filtered.data2[dimension])))

  ifelse (p.value < (1 - conf.level),
    (ifelse (mean.data1 > mean.data2,
   p.value <- approach1label,
   p.value <- approach2label
    )),
    p.value <- "N/A"
  )
  return(p.value)
}

simpleSignificanceForAllDatasets <- function(app1label, app2label, dim) {
  sigVals <- cbind(lapply(set.numbers, simpleSignificanceForDataset, approach1label=app1label, approach2label=app2label, table=overallResultTable, dimension=dim))
  return(sigVals)
}

simplePrettyApproachCombos <- expand.grid(pretty.approach.labels, pretty.approach.labels)
colnames(simplePrettyApproachCombos) <- c("a1", "a2")
simplePrettyApproachCombos <- simplePrettyApproachCombos[simplePrettyApproachCombos$"a1" != simplePrettyApproachCombos$"a2",]


simple.p.names <- apply(simplePrettyApproachCombos, 1, function(row) createPColNames(row["a1"], row["a2"]))

simple.time.p.values <- apply(simplePrettyApproachCombos, 1, function(row) simpleSignificanceForAllDatasets(row["a1"], row["a2"], dim="time"))
simple.time.p.values <- do.call("cbind", simple.time.p.values)
colnames(simple.time.p.values) <- simple.p.names
row.names(simple.time.p.values) <- set.numbers

simple.fitness.p.values <- apply(simplePrettyApproachCombos, 1, function(row) simpleSignificanceForAllDatasets(row["a1"], row["a2"], dim="FinalFitness"))
simple.fitness.p.values <- do.call("cbind", simple.fitness.p.values)
colnames(simple.fitness.p.values) <- simple.p.names
row.names(simple.fitness.p.values) <- set.numbers

countFitnessWins <- function(values, approach, dataset) {
  relevantNames <- lapply(colnames(values), grep, pattern=sprintf("^%s vs", approach), value=T)
  relevantNames <- relevantNames[lapply(relevantNames, length) > 0]
  filteredValues <- values[dataset, unlist(relevantNames)]
  return(sum(filteredValues == approach))
}

countTimeWins <- function(values, approach, dataset) {
  relevantNames <- lapply(colnames(values), grep, pattern=sprintf("^%s vs", approach), value=T)
  relevantNames <- relevantNames[lapply(relevantNames, length) > 0]
  filteredValues <- values[dataset, unlist(relevantNames)]
  filteredValues <- filteredValues[filteredValues != "N/A"]
  filteredValues <- filteredValues[filteredValues != approach]
  return(length(filteredValues))
}

countDraws <- function(values, approach, dataset) {
  relevantNames <- lapply(colnames(values), grep, pattern=sprintf("^%s vs", approach), value=T)
  relevantNames <- relevantNames[lapply(relevantNames, length) > 0]
  filteredValues <- values[dataset, unlist(relevantNames)]
  return(sum(filteredValues == "N/A"))
}

countFitnessLosses <- function(values, approach, dataset) {
  relevantNames <- lapply(colnames(values), grep, pattern=sprintf("^%s vs", approach), value=T)
  relevantNames <- relevantNames[lapply(relevantNames, length) > 0]
  filteredValues <- values[dataset, unlist(relevantNames)]
  filteredValues <- filteredValues[filteredValues != "N/A"]
  filteredValues <- filteredValues[filteredValues != approach]
  return(length(filteredValues))
}

countTimeLosses <- function(values, approach, dataset) {
  relevantNames <- lapply(colnames(values), grep, pattern=sprintf("^%s vs", approach), value=T)
  relevantNames <- relevantNames[lapply(relevantNames, length) > 0]
  filteredValues <- values[dataset, unlist(relevantNames)]
  return(sum(filteredValues == approach))
}

tallyFitnessResults <- function(values, approach, dataset) {
  wins <- countFitnessWins(values, approach, dataset)
  draws <- countDraws(values, approach, dataset)
  losses <- countFitnessLosses(values, approach, dataset)
  return(sprintf("%d/%d/%d", wins, draws, losses))
}

tallyTimeResults <- function(values, approach, dataset) {
  wins <- countTimeWins(values, approach, dataset)
  draws <- countDraws(values, approach, dataset)
  losses <- countTimeLosses(values, approach, dataset)
  return(sprintf("%d/%d/%d", wins, draws, losses))
}

tallyFitnessForDataset <- function(dataset, data, titles) {
  datasetResults <- lapply(titles, tallyFitnessResults, values=data, dataset=dataset)
  return(datasetResults)
}

tallyTimeForDataset <- function(dataset, data, titles) {
  datasetResults <- lapply(titles, tallyTimeResults, values=data, dataset=dataset)
  return(datasetResults)
}

time.tally <- lapply(set.numbers, tallyTimeForDataset, data=simple.time.p.values, titles=pretty.approach.labels)
time.tally <- do.call("rbind", time.tally)
row.names(time.tally) <- set.numbers
time.tally <- cbind(sprintf("%s",set.numbers), time.tally)
colnames(time.tally) <- c("Dataset", unlist(pretty.approach.labels))

fitness.tally <- lapply(set.numbers, tallyFitnessForDataset, data=simple.fitness.p.values, titles=pretty.approach.labels)
fitness.tally <- do.call("rbind", fitness.tally)
row.names(fitness.tally) <- set.numbers
fitness.tally <- cbind(sprintf("%s",set.numbers), fitness.tally)
colnames(fitness.tally) <- c("Dataset", unlist(pretty.approach.labels))

#-------------------------------------

# Calculate mean and standard deviation

time.aggregate <- merge(aggregate(overallResultTable$"time", list(dataset=overallResultTable$"dataset",approach=overallResultTable$"approach"), mean), aggregate(overallResultTable$"time", list(dataset=overallResultTable$"dataset",approach=overallResultTable$"approach"), sd), by=c("dataset","approach"))
colnames(time.aggregate) <- c("dataset", "approach", "avg", "std")

# Adjust time to seconds, if requested
adjust.time <- function(table) {
	table$"avg" <- table$"avg" / 1000
	table$"std" <- table$"std" / 1000
	return(table)
}

ifelse(time.in.secs, time.aggregate <- adjust.time(time.aggregate), )


fitness.aggregate <- merge(aggregate(overallResultTable$"FinalFitness", list(dataset=overallResultTable$"dataset",approach=overallResultTable$"approach"), mean), aggregate(overallResultTable$"FinalFitness", list(dataset=overallResultTable$"dataset",approach=overallResultTable$"approach"), sd), by=c("dataset","approach"))
colnames(fitness.aggregate) <- c("dataset", "approach", "avg", "std")



combine.values <- function(average, stand.dev) {
	combined <- sprintf("$%s \\pm %s$", round(as.numeric(average), decimal.places), round(as.numeric(stand.dev), decimal.places))
	return(combined)
}

get.column <- function(label, table) {
	filtered.rows <- table[table$"approach" == label,]
	new.col <- data.frame(apply(filtered.rows, 1, function(row) combine.values(row['avg'], row['std'])))
	return(new.col)
}

print.time <- lapply(pretty.approach.labels, FUN=get.column, table=time.aggregate)
print.time <- do.call("cbind", print.time)
print.time <- cbind(set.numbers, print.time)
colnames(print.time) <- c ("Dataset", unlist(pretty.approach.labels))
rownames(print.time) <- NULL


print.fitness <- lapply(pretty.approach.labels, FUN=get.column, table=fitness.aggregate)
print.fitness <- do.call("cbind", print.fitness)
print.fitness <- cbind(set.numbers, print.fitness)
colnames(print.fitness) <- c("Dataset", unlist(pretty.approach.labels))
rownames(print.fitness) <- NULL

p.table.digits <- c(p.decimal.places)

logFile="tables_2008.tex"
timeLabel <- ""
ifelse(time.in.secs, timeLabel <-"Time (s)", timeLabel <- "Time (ms)")
cat("\\documentclass{article}", file=logFile, append=FALSE, sep = "\n")
cat("\\usepackage[margin=0.5in]{geometry}", file=logFile, append=TRUE, sep ="\n")
cat("\\usepackage{amsmath}", file=logFile, append=TRUE, sep = "\n")
cat("\\begin{document}", file=logFile, append=TRUE, sep = "\n")
print(xtable(print.time,caption=timeLabel), include.rownames=F, append=T, type="latex", file=logFile, sanitize.text.function=identity,table.placement=NULL)
print(xtable(time.p.values,caption="P values for Time",digits=p.table.digits), include.rownames=F, append=T, type="latex", file=logFile, sanitize.text.function=identity,table.placement=NULL)
print(xtable(time.tally,caption="Tally for P-values of Time",digits=p.table.digits), include.rownames=F, append=T, type="latex", file=logFile, sanitize.text.function=identity,table.placement=NULL)
print(xtable(print.fitness,caption="Fitness"),include.rownames=F, append=T, type="latex", file=logFile, sanitize.text.function=identity,table.placement=NULL)
print(xtable(fitness.p.values,caption="P values for Fitness",digits=p.table.digits), include.rownames=F, append=T, type="latex", file=logFile, sanitize.text.function=identity,table.placement=NULL)
print(xtable(fitness.tally,caption="Tally for P-values of Fitness",digits=p.table.digits), include.rownames=F, append=T, type="latex", file=logFile, sanitize.text.function=identity,table.placement=NULL)
cat("\\end{document}", file=logFile, append=TRUE, sep = "\n")


system("need R")
system(sprintf("pdflatex %s", logFile))

#-------------------------------------

# Plot time

pdf("multi_timePlot_2008.pdf", width=10)

time.aggregate$approach <- factor(time.aggregate$approach, levels=pretty.approach.labels)
ggplot(data=time.aggregate, aes(x=factor(dataset), y=avg, fill=approach)) + geom_bar( width=0.5, stat="identity",position="dodge") + labs(x = "Dataset", y = timeLabel) + theme(text = element_text(size=20), axis.text.x=element_text(colour="black"), axis.text.y=element_text(colour="black")) + scale_fill_discrete(name="Approaches")

# Time boxplot

pdf("multi_timeBoxplot_2008.pdf", width=10)

overallResultTable$approach <- factor(overallResultTable$approach, levels=pretty.approach.labels)
ggplot(data=overallResultTable, aes(x=factor(dataset), y=time, fill=approach)) + geom_boxplot(position=position_dodge(1)) + labs(x = "Dataset", y = timeLabel) + theme(text = element_text(size=20), axis.text.x=element_text(colour="black"), axis.text.y=element_text(colour="black")) + scale_fill_discrete(name="Approaches")

# Plot fitness

pdf("multi_fitnessPlot_2008.pdf", width=10)

fitness.aggregate$approach <- factor(fitness.aggregate$approach, levels=pretty.approach.labels)
ggplot(data=fitness.aggregate, aes(x=factor(dataset), y=avg, fill=approach)) + geom_bar(width=0.5, stat="identity",position="dodge") + labs(x = "Dataset", y = "Fitness") + theme(text = element_text(size=20), axis.text.x=element_text(colour="black"), axis.text.y=element_text(colour="black")) + scale_fill_discrete(name="Approaches")

# Fitness boxplot

pdf("multi_fitnessBoxplot_2008.pdf", width=10)

overallResultTable$approach <- factor(overallResultTable$approach, levels=pretty.approach.labels)
ggplot(data=overallResultTable, aes(x=factor(dataset), y=FinalFitness, fill=approach)) + geom_boxplot(position=position_dodge(1)) + labs(x = "Dataset", y = "Fitness") + theme(text = element_text(size=20), axis.text.x=element_text(colour="black"), axis.text.y=element_text(colour="black")) + scale_fill_discrete(name="Approaches")

writeLines("Done!")
