## :computer: Work-in-progress :computer:

### Bimodal PTF for estimation of SWRC.

![](screenshot.png)

![](shiny.mov)

### For running an interactive Shiny app run in your R console:

```R

# First: 
# Install the following packages:
install.packages("shiny")
install.packages("devtools")
install.packages("tidymodels")
install.packages("ranger")

# Second: 
# Libraries needed:
library(shiny)
library(devtools)
library(tidymodels)
library(ranger)
# Download and run the Shinyapp
shiny::runGitHub("shinyapp_bimodal", "Saryace")

# Or, you can run the tar or zip directly
shiny::runUrl("https://github.com/Saryace/shinyapp_bimodal/archive/master.tar.gz")
shiny::runUrl("https://github.com/Saryace/shinyapp_bimodal/archive/master.zip")
```

* Colaboration? Comments? suggestions are welcome at seaceved@uc.cl.



