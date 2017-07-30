# R scripts for sociometric analyisis of software development teams

## Usage

Clone the `master` branch, it is always pointing to the latest stable version of the tool. All development is done in the `dev` branch and merged back to the mainline only if it is mature enough and tested - well, except a few days of CfP deadlines :-)

1. Download the source code or clone the repository:
	
	```bash
	$ git clone http://github.com/rlegendi/sociometry
	```
	
2. Go to the `sociometry` directory and start R or Rgui from the command line:

	```bash
	$ cd sociometry
	$ R
	R version 2.15.1 (2012-06-22) -- "Roasted Marshmallows"
	
	Copyright (C) 2012 The R Foundation for Statistical Computing
	ISBN 3-900051-07-0
	Platform: x86_64-pc-mingw32/x64 (64-bit)

	R is free software and comes with ABSOLUTELY NO WARRANTY.
	You are welcome to redistribute it under certain conditions.
	Type 'license()' or 'licence()' for distribution details.

	  Natural language support but running in an English locale

	R is a collaborative project with many contributors.
	Type 'contributors()' for more information and
	'citation()' on how to cite R or R packages in publications.

	Type 'demo()' for some demos, 'help()' for on-line help, or
	'help.start()' for an HTML browser interface to help.
	Type 'q()' to quit R.

	>
	```

	**Note** If you have already started your R environment you should set the current working directory to the `src` folder of the downloaded project. Assuming you put the project into your home directory this is done by the following command:
	
	```R
	> setwd("c:/007/work/eclipse_ws_r/sociometry")
	```

3. Install dependencies if not yet available in your environment:

	```bash
	> install.packages("igraph")
	```

4. *(Optional)* Run the tests to verify that everything is working as expected. This steps requires the installation of the `testthat` package.
	
	```R
	> test_file("sociometry-test.R")
	```
	
	In case you encounter any issues we are ready to help. Note that the authors of the `testthat` package are funny guys so sometimes you might encounter a few smileys or jokes in the test output :-)
	
5. Load the scripts:
	
	```R
	> source("sociometry.R")
	```
	
6. Success! You are ready to run the scripts!

## Input files

See `data/sample.csv` for details about the input format (order of the columns is irrelevant).
