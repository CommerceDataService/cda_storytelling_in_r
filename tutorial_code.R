##DATA STORYTELLING CODE
#Part 1: 2D Visuals

###########
##ggplot2##
###########
# - Simplified API versus vanilla R graphics API
# - Staple of data analysis process
# - Can be used for data storytelling as well
# - Produces well formed static visualizations
# - Useful in quickly wireframing an interactive visualization
# 
# To get started, we'll need to first install and load the package. 

#install ggplot2
#install.packages('ggplot2')

#load library
library(ggplot2)

# Within ggplot2, there are a number of datasets that are provided with the package to illustrate how to use the functionality. The *economics* dataset contains data from the US Bureau of Economic Analysis, US Census Bureau, and the US Bureau of Labor Statistics. *economics* is in wide form (e.g. each row represents a different day, each column contains a different variable). *economics_long*, a variant of the *economics* dataset, is provided in long or stacked form (e.g. each row represents a distinct combination of date and metric and variables are represented by two columns: a variable label and value). We'll rely on the *economics_long* dataset for the ggplot2 tutorial. For more explanation about wide and long form, take a look at [this UCLA tutorial](http://www.ats.ucla.edu/stat/r/faq/reshape.htm).

#Wide form dataset
head(economics)

#Long form dataset
head(economics_long)

# The syntax for ggplot2 is as follows:
#   
# - ggplot() initiates the function and accepts the dataset and variables
# - aes() controls aesthetics
# - geom_point() indicates the type of graph. This option can be swapped out for lines, points, bars among other forms


#create a ggplot area and add specific geoms
  ggplot(data, aes(x,y,color,group)) + geom_point()

#To put this to use,  we can run the following examples:
#Line graph with economics_long
#x = date variable
#y = value to be graphed
#group = variable label since data is in long form
  ggplot(economics_long, aes(x=date, y=value01, color=variable, group=variable)) + geom_line()

#example as area graphs
  ggplot(economics_long, aes(x=date, y=value01,color=variable, group=variable)) + geom_area()

#reshape your own tabular data
#install reshape2
#install.packages('reshape2')

#load library
library(reshape2)

#command to change to long form
new_var <- melt(dataframe, time_var)

#check example dataset
  head(economics)

#pick a variable you want to plot
  head(economics[c(1,3)])

#reshape data assigning to new dataset
  economics_melt <- melt(economics[c(1,3)],'date')

#check new set
  head(economics_melt)

##datatables
# The *datatables* package builds interactive, searchable tables from dataframes:
# - Present tabular data in a searchable paginated table
# - Useful for presenting the cleaned raw data to the client
# - Convey the shape of data
# - Useful in connecting to subject matter knowledge
# - Useful in discussing specific cases
# To get started, let's install and load the *DT* library.

#install DataTables
#install.packages('DT')

#load package
  library(DT)


# The datatable allows for stylized tables. But at a minimum, it just needs data.
# 
# #create interactive table
# datatable(data, options = list(), 
# rownames, colnames, container, caption = NULL, 
# filter = c("none", "bottom", "top"), 
# ...)


#Using the *economics* example, we can render a simple table as well as one with a max number of entries of 50.

#simple example
datatable(economics)

#customize page length
datatable(economics, options = list(pageLength = 50))


##Dygraphs
# 
# - R interface to popular dygraphs javascript library
# - Useful in presenting time-series data interactive form
# - Automatically graphs xts time-series objects
# - Helpful in visualizing long time-series
# 
# To get started, we need to install the *dygraphs* and *xts* packages. Note that *dygraphs* only accepts *xts* time series objects.

#install dygraphs and xts
#install.packages('dygraphs')
#install.packages('xts')

#load packages
library(dygraphs)
library(xts)


#Syntax is fairly straight forward.

#convert dataframe to xts
# xts(x = NULL,
# order.by = index(x),
# frequency = NULL,
# unique = TRUE,
# tzone = Sys.getenv("TZ"),
# ...)

# #create a dygraph
# dygraph(data, main = NULL, xlab = NULL, ylab = NULL,
# periodicity = NULL,
# group = NULL, width = NULL, height = NULL)


#In practice, we'll need to take the *economics* dataset and placing it into *xts* form. That means specifying that the date value in the *economics* dataset is in fact a date ([additional resource](http://www.statmethods.net/input/dates.html)), then setting the date value in a dataframe and converting it into an xts format, as shown below.

#convert string date to date format
  date_me <- as.Date(economics$date, format='%Y-%m-%d')

#combine with variable of interest: PCE
  value_me <- data.frame(date_me, economics$pce)

#convert data frame to xts format
  plot_me <- xts(value_me, order.by=value_me[,1])


#Once the data is in xts form, then we can drop the xts object into dygraphs.
#call dygraph
  dygraph(plot_me)

#If we want to add some flare, we can add a range selector:
  dygraph(plot_me) %>% dyRangeSelector()


#Part 2: 3D Visuals
##Get data 
#In order to get the data for this tutorial, you'll need to get an API key to use the Census API service [here](http://api.census.gov/data/key_signup.html). Once you have it, assign the key to the variable *api_key*.

  api_key <- "put api key here"

#Now that you have the API key, run the following code that is available on the Storytelling repository on Github. This will automatically pull in code that and assemble the dataset for the visualizations in this section.

  source("https://raw.githubusercontent.com/CommerceDataService/cda_storytelling_in_r/gh-pages/get_data.R")

##What's in the data?
#Sometimes two dimensional visuals are not enough. There is a lot more to the data that can be used to contextualize latent patterns. Often times, many analysts tend to think in two-dimensions -- like scatter plots. But there's more to it.  In the dataset that you've just imported, it has the following characteristics:

  summary(data)

# Let's say we were provided a nice clean set of data that contains the following:
# - county level data with county ID and region ID
# - variables: % unemployed, % in poverty, % with at least a HS degree
# What can you do with that data? Well, turns out that that these quantities are related. 

  library(threejs)
  
  unemp <- data$pct_unemp
  poverty <- data$pct_poverty
  pct_hs_grad <- data$pct_hs_grad
  
  data$colors <- "#011efe0"
  data$colors[data$region==2] <- "#0bff01"
  data$colors[data$region==3] <- "#fe00f6"
  data$colors[data$region==4] <- "#fdfe02"
  
  data <- data[order(data$region),]

#add labels to points
  scatterplot3js(data$pct_unemp, data$pct_poverty,data$pct_hs_grad,   
              axisLabels=c("unemployment","hs degree or above","poverty rate"),
              col=data$colors,
              labels=paste(data$region_name,": ",data$geography), 
              size=0.5,
              renderer="canvas")


##Threejs
# How did we get to this? The *threejs* library can be used to:
# - Builds upon three.js visualization engine for web browsers
# - Accepts vectors, matrices and data frames to create different types of interactive visualizations:

#Load in Threejs library      
  library(threejs)

#We can see that there are direct relationships between unemployment, poverty and education attainment. But there isn't much detail and the graphs aren't pretty.
  scatterplot3js(data$pct_unemp, data$pct_poverty,data$pct_hs_grad)


#Let's stylize the plots. First let's name the axes with *axisLabels*, which accepts a vector of axis names. The order matters and is as follows: x-axis, z-axis, y-axis
#Note that axis Labels should follow this order= c(x, z, y)
  scatterplot3js(data$pct_unemp, data$pct_poverty,data$pct_hs_grad,   
  axisLabels=c("unemployment","hs degree or above","poverty rate"))    


#Now let's change the rendering engine to give more depth to the plot. We do so by changing *renderer = "canvas"*. This just tells R threejs to use a different package to render the points

#Depth using render
  scatterplot3js(data$pct_unemp, data$pct_poverty,data$pct_hs_grad, 
                 axisLabels=c("unemployment","hs degree or above","poverty rate"),
                 renderer="canvas")   

# Now, let's set the color of the points, resize the points, and flip the y axis so it's ascending from the origin. To do so, we:
#   - set *col = "slategrey"*
#   - set *flip.y = FALSE*
#   - set *size = 0.5*
  
#Point size, color, don't flip y axis
  scatterplot3js(data$pct_unemp, data$pct_poverty,data$pct_hs_grad, 
                 axisLabels=c("unemployment","hs degree or above","poverty rate"),
                 renderer="canvas",  flip.y=FALSE, col="slategrey",
                 size=0.5)   

#Ultimately, we want to find more patterns. By using color, we can group regions by color. We can see some regions are worse off than others. But which? Turns out there are 4 regions:
  
  unique(data$region_name)     
  unique(data$region)  

#First, let's set each region to a different color by first creating a new variable for colors *data$colors*, then assign a hexcode to each region.

#Set up colors by 
  data$colors <- ""
  data$colors[data$region==1] <- "#011efe0"
  data$colors[data$region==2] <- "#0bff01"
  data$colors[data$region==3] <- "#fe00f6"
  data$colors[data$region==4] <- "#fdfe02"

#Now, let's set *col= data$colors* so that R knows which color corresponds to each of the 3000 points.
  data <- data[order(data$region),]
  
#Grouped patterns
  scatterplot3js(data$pct_unemp, data$pct_poverty,data$pct_hs_grad, 
               axisLabels=c("unemployment","hs degree or above","poverty rate"),
               col=data$colors,  flip.y=FALSE, 
               renderer="canvas", 
               size=0.5)   

# It's a bit annoying to look at the chart without knowing which point corresponds to which county. 
# Let's add labels for each point that show up upon mousing over. 

#add labels to points
scatterplot3js(data$pct_unemp, data$pct_poverty,data$pct_hs_grad,   
               axisLabels=c("unemployment","hs degree or above","poverty rate"),
               col=data$colors,
               labels=paste(data$region_name,": ",data$geography), 
               size=0.5,
               renderer="canvas")

#In short, we can tell the following key insights from this graph.

##Maps with Leaflet
#Sometimes graphs don't get the point across. Maps, while over used, can provide some better indication of patterns. 
#Based on our 3-d graphs, we could see clustering of regions's economic performance. We can see the mess of points more clearly on a map. 

library(leaflet)
library(stringr)

shape_direct <- function(url, shp) {
  library(rgdal)
  temp = tempfile()
  download.file(url, temp) ##download the URL taret to the temp file
  unzip(temp,exdir=getwd()) ##unzip that file
  return(readOGR(paste(shp,".shp",sep=""),shp))
}

shp <- shape_direct(url="http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_county_20m.zip",
                    shp= "cb_2014_us_county_20m")

shp@data$GEOID <- as.character(shp@data$GEOID)
shp <- merge(shp,data,id="GEOID")


pal <- colorQuantile("YlGn", NULL, n = 30)

state_popup <- paste0("<strong>County: </strong>", 
                      shp@data$geography, 
                      "<br><strong>Poverty Rate (%): </strong>", 
                      shp@data$pct_poverty)

leaflet(data = shp) %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(lng = -98.3, lat = 39.5, zoom = 4)  %>%
  addPolygons(fillColor = ~pal(pct_poverty), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 0.1, 
              popup = state_popup)     


###Getting started
# We can use the leaflet library to bring a geographic spin to the data: 
#   
#   - Builds upon leaflet.js to create interactive web maps
# - Functionality to take a dataset to map in minutes
# 
# To initiate a map, we only need to open the leaflet library, then run the following:
  
  library(leaflet)
  leaflet()  


# You'll see that the map is blank with a zoom control panel on the upper left. That's because the map doesn't have data in it. There are dozens on free layers we can use:
# - Stamen.Toner
# - CartoDB.Positron
# - HikeBike.HikeBike
# - CartoDB.DarkMatter


leaflet() %>%
    addProviderTiles("Stamen.Toner") 

leaflet() %>%
    addProviderTiles("CartoDB.Positron") 

#Now let's center and zoom in on the contiguous US at coordinates lon = -98.3 and lat = 39.5
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(lng = -98.3, lat = 39.5, zoom = 4) 


#We now need to download the shapefiles, which are a popular geospatial vector data format for geographic information system (GIS) software. Shapefiles allow for rendering of various types of data, including points (e.g. coordinates), polygons (e.g. county boundaries), and lines (e.g. streets, creeks). We're going to use the US County Shapefile from the US Census: [http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_county_20m.zip](http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_county_20m.zip). To download it and load it into R, we'll need to first install the *rgdal* library. We've also written a function *shape_direct* that can be run to import the shapefile and assign it to an object *shp*.
shape_direct <- function(url, shp) {
                                  library(rgdal)
                                  temp = tempfile()
                                  download.file(url, temp) ##download the URL taret to the temp file
                                  unzip(temp,exdir=getwd()) ##unzip that file
                                  return(readOGR(paste(shp,".shp",sep=""),shp))
                                  }

shp <- shape_direct(url="http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_county_20m.zip",
                    shp= "cb_2014_us_county_20m")

#With the new shapefile now imported, we can now set *data = shp*.
leaflet(data=shp) %>%
          addProviderTiles("CartoDB.Positron") %>%
          setView(lng = -98.3, lat = 39.5, zoom = 4) %>%
          addPolygons(fillColor = "blue", 
          fillOpacity = 0.8, 
          color = "white", 
          weight = 0.5)     


#In order to draw insight from a map, we'll need to color code county polygons. This is known as a choropleth map -- each county is color coded with respect to a certain value of a given variable like %poverty. The shapefile on its own doesn't have the socioeconomic data and we'll need to join the data to the shapefile. Let's just quickly check the data formats of the primary key *GEOID*.
str(shp@data$GEOID)
str(data$GEOID)


Since the *shp* primary key is in a factor format and the *data* primary key is in string or character format, we'll need to conform the formats, preferrably to strings. Then, we can merge the two datasets.

shp@data$GEOID <- as.character(shp@data$GEOID)
shp <- merge(shp,data,id="GEOID")


#With the merged datasets, we'll now need to specify a color scheme. Using *colorQuantile*, we can create a create a function that will slice any continuous variable into bins and assign colors to each bin. The syntax is as follows:
pal <- colorQuantile(<Color Code>, <variable>, n = <number of bins>)


#For our example, we'll use a Yellow-Green palette, leave <variable> as NULL so that we can re-use the palette function, and set the number of bins to 30.
palette <- colorQuantile("YlGn", NULL, n = 30)


Next we will add popup text for when a user clicks on a county in a map.

county_popup <- paste0("<strong>County: </strong>", 
                       shp@data$geography, 
                       "<br><strong>Poverty Rate (%): </strong>", 
                       shp@data$pct_poverty)


#Now we'll pull it all together.

leaflet(data = shp) %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng = -98.3, lat = 39.5, zoom = 4) %>%
    addPolygons(fillColor = ~palette(pct_poverty), 
    fillOpacity = 0.8, 
    color = "#BDBDC3", 
    weight = 0.1, 
    popup = county_popup)     

#We can run the same graph for *pct_unemp* by swapping out *pct_poverty*

county_popup <- paste0("<strong>County: </strong>", 
                      shp@data$geography, 
                      "<br><strong>Unemp (%): </strong>", 
                      shp@data$pct_unemp)

leaflet(data = shp) %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng = -98.3, lat = 39.5, zoom = 4) %>%
    addPolygons(fillColor = ~palette(pct_unemp), 
    fillOpacity = 0.8, 
    color = "#BDBDC3", 
    weight = 0.1, 
    popup = county_popup)     

   
