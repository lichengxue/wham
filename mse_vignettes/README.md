# Background
The “multi-wham-mse” package is used to perform management strategy evaluation (MSE) in the situation where population structure is complex (e.g. multiple stocks in multiple regions) with different movement dynamics (e.g. natal homing). This package is designed specifically for the Woods Hole Assessment Model (WHAM), a state-space age-structured stock assessment model. So far, WHAM can incorporate multiple sources of process errors (treated as random effects) such as deviations in (1) recruitment/numbers-at-age, (2) selectivity, (3) natural mortality, (4) catchability, and (5) movement. WHAM is also capable of including environmental effects on population processes. 
## Download "multi-wham-mse" package 
You can download the “mse” branch of the “wham” package from Github by following the steps below:
#### 1. Open "Command Prompt" in your local computer.
#### 2. Type "cd directory" here you can type a specific path (replace "directory") to save the package (you can also skip this step and the package will be saved in the main directory). 
#### 3. Type "git clone -b mse https://github.com/lichengxue/wham.git".
#### 4. Close the Command and check if the package has been saved in the directory.

## Install "multi-wham-mse" package
For the users who are installing "wham" for the first time:
```r
install.packages(file.path(library/you/download/package,"wham"), dependencies = TRUE, repos = NULL, type = "source")
# devtools::install_local(file.path(main.dir,"wham"), dependencies = TRUE) # Alternative
# Remember load the "mse" package using:
library(wham)
````
For the users who has "single-wham" installed before, it's necessary to install the "mse" package in a different directory to avoid one overwriting the other:
```r
library_paths <- .libPaths()[1]
new_folder <- "wham_old"
if (file.exists(file.path(library_paths,new_folder))){
} else {
  dir.create(file.path(library_paths,new_folder))
}

file.copy(from = file.path(library_paths,"wham"), to = file.path(library_paths,new_folder), 
          overwrite = TRUE,  recursive = TRUE, copy.mode = TRUE)

# library(wham, lib.loc = file.path(library_paths,new_folder)) # Load the single wham package

install.packages(file.path(library/you/download/package,"wham"), dependencies = TRUE, repos = NULL, type = "source")
# devtools::install_local(file.path(main.dir,"wham"), dependencies = TRUE) # Alternative
# Remember load the "mse" package using:
library(wham)
````