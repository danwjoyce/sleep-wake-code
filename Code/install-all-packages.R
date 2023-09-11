# requirements / packages

# run this script to set up R with the necessary packages to use the code
# some advice : often easier to run each install.packages() call seperately to help debug any failed installs due to dependencies

############
# You need devtools, the installation of which differs between OS
# For windows : 
#  see https://community.rstudio.com/t/cant-install-devtools-on-windows-10/11730

# For Ubuntu Linux, need to ..
# sudo apt install libharfbuzz-dev libfribidi-dev

###########
# specific packages
install.packages("devtools", dependencies = TRUE)
install.packages("ragg")

# need zoo for time-series 
install.packages("zoo")

# should be installed, but just in case
install.packages("reshape2")

install.packages("ggplot2")
