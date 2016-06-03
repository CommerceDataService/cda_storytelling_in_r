
##Run code to assemble data from Census

##API Key
  variable <- data.frame( var_name = c("pct_poverty","pct_unemp","ed_less_than_9","ed_9_to_12"),
                          series_id = c("DP03_0119PE","DP03_0005PE","DP02_0059PE","DP02_0060PE"))

##Hit Census API to get specific datasets
  get_series <- function(api_key,var){
    require(jsonlite)
    url = paste("http://api.census.gov/data/2014/acs5/profile?get=",var,"&for=county:*&key=",api_key,sep="")
    data = fromJSON(txt=url)
    head <- data[1,]
    body <- as.data.frame(data[2:nrow(data),])
    colnames(body)<-head
    return(body)
  }
  
  for(k in 1:4){
    if(k == 1){
      full_data = get_series(api_key,variable$series_id[1])
    } else{
      full_data<-merge(full_data,get_series(api_key,variable$series_id[k]), id=c("county","state"))
    }
  }
  
  data <- full_data
  for(k in 3:ncol(full_data)){
    data[,k] <- as.numeric(as.character(data[,k]))
  }
  
  data$pct_hs_grad <- 100 - data$DP02_0059PE - data$DP02_0060PE
  data$GEOID <- paste(data$state,data$county,sep="")
  data <- data[,c(8,3,4,7)]
  colnames(data) <- c("GEOID","pct_poverty","pct_unemp","pct_hs_grad")

##Geographic Labels 
#Get county labels
  county <- read.csv("http://www2.census.gov/geo/docs/reference/codes/files/national_county.txt",header=F, colClasses="character")
  colnames(county) <- c("state_abb","state","county","geography","misc")
  county$GEOID <- paste(county[,2],county[,3],sep="")
  county <- county[,c(6,2,4)]
  county$state <- as.numeric(county$state)

#County to Region
  download.file("https://www.census.gov/popest/about/geo/state_geocodes_v2015.xls" ,destfile="codes.xls",method="libcurl")
  require(gdata)
  tables = read.xls ("codes.xls", sheet = 1, skip=3, header = TRUE)
  colnames(tables) <- c("region","division","state", "name")
  reg0 <- tables[tables$division==0,c(1,4)]
  tables <- tables[tables$division!=0,c(1,3)]
  tables <- merge(tables,reg0, id="region" )
  tables <- merge(county,tables, id="state")

#Merge data
  data <- merge(tables,data, id="GEOID")

#Remove temp data
  rm(county,full_data,reg0,tables,variable)
