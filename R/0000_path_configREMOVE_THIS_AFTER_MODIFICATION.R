## Paths configuration

## Rename this file to 0000_path_config.R after modification

# Path othe .Rdata files for surveys
RVersion =R.Version()
if (RVersion$os != "linux-gnu") {
  # Non linux OS ie Windows
  outputDir <- "C:/Users/Utilisateur/Documents/Data/R/"} else { 
  # linux OS
  outputDir <- "/home/mylogin/mydata/"} 


