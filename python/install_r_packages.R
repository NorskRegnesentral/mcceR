
# Installs the required R-packages
install.packages(c("remotes","data.table","Rfast","party","rpart","partykit"),repos="https://cloud.r-project.org")
remotes::install_github("NorskRegnesentral/mcceR")
#remotes::install_github("NorskRegnesentral/mcceR",ref="pass_fitted_tree") # Temporary install the PR version
