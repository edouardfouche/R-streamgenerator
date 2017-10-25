library(streamgenerator)

context("Statistical row generation")


test_that("Default parameters generate.row",{
	# Check that it holds for 100 runs
	for(i in 1:100) {
		row <- generate.row()
	  	expect_equal(class(row), "list")
	  	expect_equal(class(row$label), "character")
	  	expect_equal(length(row$label), 1)
	  	expect_equal(class(row$data), "numeric")
	  	expect_equal(length(row$data), 10)
	  	expect_true((row$label != "0") | (((row$data[3] <= 0.1) | (row$data[4] <= 0.1)) & ((row$data[7] <= 0.1) | (row$data[8] <= 0.1))))
	}
})

test_that("Custom parameters generate.row",{
	dim <- 100
	subspaces <- list(c(1,2), c(2,3,4), c(5,6,7,8)) 
	margins <- list(0.7,0.5,0.1)
	prop <- 0.05

	# Check that it holds for 100 runs
	for(i in 1:100) {
	  	row <- generate.row(dim, subspaces, margins, prop)

	  	expect_equal(class(row), "list")
	  	expect_equal(class(row$label), "character")
	  	expect_equal(length(row$label), 1)
	  	expect_equal(class(row$data), "numeric")

	  	expect_equal(length(row$data), dim)
	  	
	  	expect_true((row$label != "0") | (((row$data[1] <= 0.7) | (row$data[2] <= 0.7)) ))
	  	expect_true((row$label != "0") | (((row$data[2] <= 0.5) | (row$data[3] <= 0.5) | (row$data[4] <= 0.5)) ))
	  	expect_true((row$label != "0") | (((row$data[5] <= 0.1) | (row$data[6] <= 0.1) | (row$data[7] <= 0.1) | (row$data[8] <= 0.1)) ))
    }
})

test_that("generate.row always outputs an outlier if the point is from the hidden space and prop=1 and not an outlier otherwise",{
	dim <- 100
	subspaces <- list(c(1,2)) 
	margins <- list(0.1)
	prop <- 1

	# Check that it holds for 100 runs
	for(i in 1:100) {
	  	row <- generate.row(dim, subspaces, margins, prop)
	  	
	  	expect_equal(class(row), "list")
	  	expect_equal(class(row$label), "character")
	  	expect_equal(length(row$label), 1)
	  	expect_equal(class(row$data), "numeric")

	  	expect_equal(length(row$data), dim)
	  	
	  	if((row$data[subspaces[[1]][1]] > margins[[1]]) & (row$data[subspaces[[1]][2]] > margins[[1]])) {
	  		expect_true(row$label != "0")
	  	} else {
	  		expect_true(row$label == "0")
	  	}
    }
})

test_that("generate.row never outputs an outlier if prop=0",{
	dim <- 100
	subspaces <- list(c(1,2)) 
	margins <- list(0.1)
	prop <- 0

	# Check that it holds for 1000 runs
	for(i in 1:1000) {
	  	row <- generate.row(dim, subspaces, margins, prop)	  	
	  	expect_true((row$label == "0") & ((row$data[subspaces[[1]][1]] <= margins[[1]]) | (row$data[subspaces[[1]][2]] <= margins[[1]])))
    }
})