# This script contains the commands used to install the latest version of greta
# and some other useful packages in this RStudio.cloud project. It has already
# been run, this is just for reference!
install.packages(c("devtools", "DiagrammeR", "bayesplot"))
devtools::install_github("greta-dev/greta")
greta::install_tensorflow(extra_packages = "tensorflow-probability")
