rm(list = ls())

# setup paths so they look are in sequence as
#   home.dir / location.dir / project.dir / <code or data>
# .. specific to operating system/user


if (Sys.info()["user"] == "dan") {
  home.dir <- "/home/dan"
} else {
  
  if( Sys.info()["nodename"] == "PSYPC560" ) {
    home.dir <- "C:/Users/hmehmet"
  }
  else {
    ## configure for Hussein use
    home.dir <- "C:/Users/huss8"
  }
}

location.dir <- "Dropbox"
project.dir <- "GreatMinds-Accel-Project"
data.dir <- "Data"
code.dir <- "Code"
output.dir <- "Post-processed"

if (Sys.info()["sysname"] == 'Linux'){
  # change this to you own "personal" dropbox location
  code.path <- file.path( home.dir, location.dir, project.dir, code.dir )
  data.path <- file.path( home.dir, location.dir, project.dir, data.dir )
  output.path <- file.path( home.dir, location.dir, project.dir, output.dir )
} else { 
  # for other OS users... 
  code.path <- file.path( home.dir, location.dir, project.dir, code.dir )
  data.path <- file.path( home.dir, location.dir, project.dir, data.dir )
  output.path <- file.path( home.dir, location.dir, project.dir, output.dir )
}
