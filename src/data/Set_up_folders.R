
# Make folders #


#### Function to create directory ####
mkdirs <- function(fp) {
  if(!dir.exists(fp)) {
    dir.create(fp)
  }
} 

#### Create directories ####
mkdirs("data")
mkdirs("data/external")
mkdirs("data/interim")
mkdirs("data/processed")
mkdirs("data/raw")
mkdirs("reports")
mkdirs("reports/figures")

mkdirs("src/data")
mkdirs("src/features")
mkdirs("src/models")
mkdirs("src/visualization")
mkdirs("src/backfun")
