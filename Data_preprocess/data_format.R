library(data.table)
library(stringr)
library(mice)

# Load all csv files
temp = list.files(path = "./2005-2017", pattern="*.csv")

# Extract years from all files
unique_years <- unique(stringr::str_extract(string = temp, pattern = "2\\d\\d\\d*"))

yearly_list <- list()

# Load one file to get column names
# Load file
monthly_dt <- fread(paste0("./2005-2017/", temp[1]), skip = 31)
# Get column namse
col_names <- colnames(monthly_dt)

lapply(unique_years, function(year){
  # Get all the files within certain year
  files <- temp[stringr::str_detect(temp,year)]
  
  # Load each month data
  monthly_list <- lapply(files, function(x){
    fread(paste0("./2005-2017/", x), skip = 31, select = col_names[6:length(col_names) - 1], na.strings = c("","NA"))
  })
  
  # Group all year data
  DT <- rbindlist(monthly_list)
  
  # Calculate the mean for each column
  col_names_DT <- colnames(DT)
  
  # Calcuate the mean of each column
  yearly_list[[year]] <<- DT[, lapply(.SD, function(x) mean(x, na.rm = T)), .SDcols = col_names_DT]
  
})

# Get together all data
DT_yearly <- rbindlist(yearly_list)
DT_yearly[DT_yearly == "NaN"] <- NA

DT_yearly[, Year := as.numeric(names(yearly_list))]

# Predict the missing value
numeric_columns <- names(DT_yearly)[sapply(DT_yearly, is.numeric)]
DT_yearly[,(numeric_columns) := round(.SD,2), .SDcols=numeric_columns]

col_names_DT_yearly <- colnames(DT_yearly)
setnames(DT_yearly, col_names_DT_yearly, paste0("metric", 1:21))
miceMod <- mice(DT_yearly, method="rf") # perform mice imputation, based on random forests.
miceOutput <- complete(miceMod)  # generate the completed data.
names(miceOutput) <- numeric_columns


write.csv(miceOutput, "weather.csv", row.names = F)


