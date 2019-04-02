#' Output the Stream and its Description
#'
#' Write in a bunch of files the result of the stream generation. Output the data as a .csv file
#' in a <prefix>_data.txt file. Output the labels in a <prefix>_labels.txt file. Output an
#' extensive description of the data, outliers and subspaces in a <prefix>_description.txt file
#'
#' @param stream An object of class stream.
#' @param prefix Prefix to give the output files
#'
#' @return Nothing
#'
#' @examples
#' stream <- generate.dynamic.stream()
#' output.stream(stream, "example")
#'
#' @author Edouard Fouché, \email{edouard.fouche@kit.edu}
#'
#' @export
#' @importFrom utils write.csv
output.stream <- function(stream, prefix) {
	stopifnot(class(stream) == "stream")
	options(streamgenerator.max.print=1000000)
	write.csv(stream$data, paste(prefix, "data.txt", sep="_"), row.names=FALSE)
	write(stream$labels, paste(prefix, "labels.txt", sep="_"))

	conn <-file(paste(prefix, "description.txt", sep="_"))

	sink(conn, append=TRUE)

	cat(paste("Synthetic Data Stream",
			  "=====================\n", sep="\n"))

	cat(paste("\nParameters",
			  "----------\n\n", sep="\n"))

	cat(paste(paste("Number of steps:\t", length(stream$n)),
			     paste("Proportion of outliers:\t", stream$prop),
			     paste("Proportion type:\t", stream$proptype),
			     paste("Number of Dimensions:\t", stream$stream.config$dim),
			     paste("Minimum number of dim:\t", stream$stream.config$mindim),
			     paste("Maximum number of dim:\t", stream$stream.config$maxdim),
			     paste("Possible margin values:\t", paste(stream$stream.config$values, collapse = ' ')),
			     paste("Cycle length:\t\t", stream$stream.config$cycle),
			     paste("Volatility:\t\t", stream$stream.config$volatility),
			     paste("Dependency:\t\t", stream$stream.config$dependency),
			     paste("Overlap Allowed:\t\t", stream$stream.config$allowOverlap),
			     paste("Discrete:\t\t", stream$stream.config$discretize), sep="\n"))

	cat(paste("\n\nData",
			  "----\n\n", sep="\n"))

	hiddensubspaceoutliers <- stream$labels[stream$labels != "0"]
	indexes <- 1:length(stream$labels)
	indexes <- indexes[stream$labels != "0"]

	cat(paste("Number of points:\t", length(stream$labels)))
	cat(paste("\nNumber of outliers:\t", length(hiddensubspaceoutliers)),"\n\n")

	contingency <- as.data.frame(table(stream$labels[stream$labels != "0"]))

	if(length(contingency) > 1) {
	  attributes(contingency)$names <- c("subspaces", "count")

	  cat("- Contingency Table\n\n")

	  print(contingency)
	}


	cat("\n- Patterns\n\n")

	cat("Index\t\tSubspaces\n")

	for(x  in 1:length(indexes)) {
		cat(paste(indexes[x], hiddensubspaceoutliers[x], "\n", sep="\t\t"))
	}

	cat(paste("\nSubspace description",
		      "--------------------\n\n", sep="\n"))

	cat("- Notes\n\n")
	cat("Rows:\t\tsubspaces (<start>:<end>)\n")
	cat("Columns:\t<step_number>.<step_size>\n")
	cat("Values:\t\t 0 ='no dependence', 0.9='high dependence'\n\n")

	cat("- Description\n\n")

	if(length(stream$n) == 1) {
		subspaces <- stream$stream.config$subspaces
		margins <- stream$stream.config$margins
		order <- order(sapply(subspaces,'[[',1))
		subspaces <- subspaces[order]
		subspaces <- sapply(subspaces, function(x) paste(x, collapse=","))
		margins <- margins[order]
		for(x in 1:length(stream$stream.config$subspaces)) {
			cat(subspaces[x], " : ", margins[x], "\n")
		}
	} else {
		subspaces <- unique(do.call(list, unlist(stream$stream.config$subspaceslist, recursive=FALSE)))
		subspaces <- subspaces[order(sapply(subspaces,'[[',1))]

		summary <- data.frame(row.names=subspaces)

		for(x in 1:length(stream$stream.config$subspaceslist)) {
			summary[paste(stream$stream.config$subspaceslist[[x]]),paste(x,stream$n[x], sep=".")] <- paste(stream$stream.config$marginslist[[x]])
		}
		summary[is.na(summary)] <- 0
		print(summary)

	}
	sink()
	close(conn)
	#print("Output completed !") # comment it because it gets somehow printed in the files (?)
}
