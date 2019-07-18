## Set working directory
setwd('C:/Users/kmc00/OneDrive - CEFAS/R_PROJECTS/BiotopePredictionShinyApp')

## Call required libraries
library(flexclust)
library(ggplot2)
library(leaflet)
library(DT)
library(rgdal)
library(mapview)
library(raster)
library(plyr)
library(shiny)


#### 1. BRING IN REQUIRED DATA ####

## Bring in baseline data for faunal clustering (for use in maps)
faunal.cluster=read.csv("DATA/BaselineFaunalCluster2.csv",header=T,na.strings=c("NA", "-","?","<null>"),stringsAsFactors=F,check.names=FALSE)

## Bring in baseline kcca object
resultsA <- readRDS("DATA/resultsA")

## Bring in baseline cluster results object
results <- readRDS("DATA/results")

## Baseline sample - muted colours 
BaseCol <- colorFactor(c("#9999F8","#99FFFF","#9BDDE6","#F8DEF8","#D6ADEB","#99EB99","#D6FFD6","#E19999","#FF9999","#FFD199","#FFFF99","#E1E19A"), faunal.cluster$FaunalCluster)

## Baseline sample - full colours 
#BaseCol <- colorFactor(c("blue2","cyan1","#05aae1","plum2","darkorchid3","green3","palegreen1","#b40202","red1","darkorange","yellow","#b4b404"), faunal.cluster$FaunalCluster)

## Bring in raster data for phycluster
phyclus = raster('DATA/PhysicalClusterClip.tif')

## Create layer for aggregate dredging areas
# licence = readOGR("DATA","Aggregates_Licence_20151112")

## Create layer for MCZ polygons
# mcz = readOGR("DATA","DESIGNATED")

## Bring in distances and percentiles for baseline dataset (generated using BiotopePredictionScript.R)
basedist=read.csv("DATA/DistancetoCentersTrain4.csv",header=T,na.strings=c("NA", "-","?","<null>"),stringsAsFactors=F,check.names=FALSE)

## Check baseline dataset and baseline distance from clustering have same length
dim(basedist)#[1] 27432    42
dim(faunal.cluster)# 27432     7

## Change numeric columns to 1dp
is.num <- sapply(basedist, is.numeric)
basedist[is.num] <- lapply(basedist[is.num], round, 1)

## Now add this distance info to the object faunal.cluster
faunal.cluster2=cbind(faunal.cluster,basedist[,4:17])
names(faunal.cluster2)

## Load baseline distances by cluster group (data required to calculate test data percentiles)
distsfor1=read.csv("DATA/distsfor1.csv",header=T,na.strings=c("NA", "-","?","<null>"),stringsAsFactors=F,check.names=FALSE,col.names=NA)
distsfor2=read.csv("DATA/distsfor2.csv",header=T,na.strings=c("NA", "-","?","<null>"),stringsAsFactors=F,check.names=FALSE,col.names=NA)
distsfor3=read.csv("DATA/distsfor3.csv",header=T,na.strings=c("NA", "-","?","<null>"),stringsAsFactors=F,check.names=FALSE,col.names=NA)
distsfor4=read.csv("DATA/distsfor4.csv",header=T,na.strings=c("NA", "-","?","<null>"),stringsAsFactors=F,check.names=FALSE,col.names=NA)
distsfor5=read.csv("DATA/distsfor5.csv",header=T,na.strings=c("NA", "-","?","<null>"),stringsAsFactors=F,check.names=FALSE,col.names=NA)
distsfor6=read.csv("DATA/distsfor6.csv",header=T,na.strings=c("NA", "-","?","<null>"),stringsAsFactors=F,check.names=FALSE,col.names=NA)
distsfor7=read.csv("DATA/distsfor7.csv",header=T,na.strings=c("NA", "-","?","<null>"),stringsAsFactors=F,check.names=FALSE,col.names=NA)
distsfor8=read.csv("DATA/distsfor8.csv",header=T,na.strings=c("NA", "-","?","<null>"),stringsAsFactors=F,check.names=FALSE,col.names=NA)
distsfor9=read.csv("DATA/distsfor9.csv",header=T,na.strings=c("NA", "-","?","<null>"),stringsAsFactors=F,check.names=FALSE,col.names=NA)
distsfor10=read.csv("DATA/distsfor10.csv",header=T,na.strings=c("NA", "-","?","<null>"),stringsAsFactors=F,check.names=FALSE,col.names=NA)
distsfor11=read.csv("DATA/distsfor11.csv",header=T,na.strings=c("NA", "-","?","<null>"),stringsAsFactors=F,check.names=FALSE,col.names=NA)
distsfor12=read.csv("DATA/distsfor12.csv",header=T,na.strings=c("NA", "-","?","<null>"),stringsAsFactors=F,check.names=FALSE,col.names=NA)



#### 2. APP USER INTERFACE ####

ui <- fluidPage(
  # Application title
  titlePanel(title=div(img(src="CefasLogo.png",height = 50, width = 100), "Faunal Cluster ID Tool")),
  
  fluidRow(
    column(2,h4("1. Download template"),
           downloadButton('downloadTemp', '.csv Template'),
           br(),
           br(),
           br(),
           fileInput("file1", h4("2. Upload data"),
                     accept = c(
                       "text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")),
           br(),
           h4("3. ID Faunal groups"),
           actionButton("match","Match")),
    
    column(5,leafletOutput("plot2",width = "100%", height="850px")), 
           
    
    column(5,style='border-left: 1px solid grey',
           tabsetPanel(
             tabPanel("Results",div(DT::dataTableOutput("results"),style = 'font-size:85%'),br(),# Button
                      downloadButton("downloadData", "Download table")),
             tabPanel("Distances",div(DT::dataTableOutput("distances"),style = 'font-size:85%'),br()),
             tabPanel("Percentiles",div(DT::dataTableOutput("percentiles"),style = 'font-size:85%'),br()),
             tabPanel("Cluster Characteristics",tags$img(src="Table1.png")),
             tabPanel("About",
                      br(),
                      p("This tool matches new macrofaunal data to the existing faunal cluster groups identified in Cooper and Barry (2017, http://rdcu.be/wi6C). For a full decription of the app methodology see Cooper (2019, <insert link to paper>).",
                        br(),
                        br(),"The physical cluster group shown in the Results tab is also based on work of Cooper and Barry (2017). Sampling stations are partitioned into 10 groups based on a k-means clustering of physical variables (Salinity, Temperature, Chlorophyll a, Suspended Particulate Matter, Water depth, Wave Orbital Velocity, Average current, Stress and Suspended Particulate Matter). Within the app, test sample coordinates are used to extract the physical cluster group from a 100% coverage geotiff file. This file was created using Random Forest modelling of point sample data. Knowledge of the physical cluster group identity of sampling stations is required for the Regional Seabed Monitoring Programme (RSMP).")))
           
    )))

textAlign = 'center'



#### 3. APP SERVER ####

server <- function(input, output) {
  
  ## Download a blank template
  output$downloadTemp <- downloadHandler(
    filename = function() {
      paste("ShinyTemplate.csv")
    },
    content = function(file) {
      myfile <- paste0('R/www/',"ShinyTemplate.csv", collapse = NULL)
      file.copy(myfile, file)
    })
  
  ## Create a series of reactive objects to use in leaflet maps
  ## Bring data into App
  data <- reactive({ 
    req(input$file1)
    inFile <- input$file1 
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath, header=TRUE)
    return(df)
  })
  
  pos=reactive({
    pos=data()[,1:3]
  })
  
  ## Define area to plot based on input data
  box=reactive({
    
    ## Define map extent
    MaxLat=max(pos()$Latitude_WGS84)+0.1
    MinLat=min(pos()$Latitude_WGS84)-0.1
    MaxLon=max(pos()$Longitude_WGS84)+0.1
    MinLon=min(pos()$Longitude_WGS84)-0.1
    box <-  c(MinLon,MinLat,MaxLon, MaxLat)
  })
  
  
  
  ## Create the object with no values
  res <- reactiveValues(k_st = NULL)
  
  observeEvent(input$match , { 
    
    ## Take faunal data
    ShinyTemplate3=data()[,4:706]
    
    ## Transform faunal data
    ShinyTemplate4=ShinyTemplate3^(0.25)
    
    ## Create a df 'pos.test' for Sample, Latitude_WGS84 and Longitude_WGS84 
    pos.test=data()[,1:3]
    
    ## Now use predict function to predict cluster groups for test data.
    pred_test <- predict(resultsA, newdata=ShinyTemplate4)
    
    ## Add cluster group from kmeans results file to df 'pos' which includes 'Sample','Latitude_WGS84' and 'Longitude_WGS84'
    faunal.cluster.test=cbind(pos.test,pred_test)
    
    ## Change name of col 'results$cluster' to 'ClusterNum'
    names(faunal.cluster.test)[4]<-paste("ClusterNum")
    
    ## Add a new empty col 'FaunalCluster' to df 'faunal.cluster
    faunal.cluster.test["FaunalCluster"]=NA
    
    ## Populate FaunalCluster col with new names
    faunal.cluster.test$FaunalCluster[faunal.cluster.test$ClusterNum == 11] <- "A1"
    faunal.cluster.test$FaunalCluster[faunal.cluster.test$ClusterNum == 1]<- "A2a"
    faunal.cluster.test$FaunalCluster[faunal.cluster.test$ClusterNum == 8] <- "A2b"
    faunal.cluster.test$FaunalCluster[faunal.cluster.test$ClusterNum == 3]<- "B1a"
    faunal.cluster.test$FaunalCluster[faunal.cluster.test$ClusterNum == 7] <- "B1b"
    faunal.cluster.test$FaunalCluster[faunal.cluster.test$ClusterNum == 4] <- "C1a"
    faunal.cluster.test$FaunalCluster[faunal.cluster.test$ClusterNum == 5] <- "C1b"
    faunal.cluster.test$FaunalCluster[faunal.cluster.test$ClusterNum == 12] <- "D1"
    faunal.cluster.test$FaunalCluster[faunal.cluster.test$ClusterNum == 2] <- "D2a"
    faunal.cluster.test$FaunalCluster[faunal.cluster.test$ClusterNum == 10] <- "D2b"
    faunal.cluster.test$FaunalCluster[faunal.cluster.test$ClusterNum == 6] <- "D2c"
    faunal.cluster.test$FaunalCluster[faunal.cluster.test$ClusterNum == 9]<- "D2d"
    
    ## Note col FaunalCluster is currently a chr - need to convert to a factor
    faunal.cluster.test$FaunalCluster=as.factor(faunal.cluster.test$FaunalCluster)
    
    ## Identified faunal cluster groups present in the test samples
    req.cols=data.frame("FaunalCode"=levels(faunal.cluster.test$FaunalCluster))
    
    ## DF for faunal cluster colours
    ColTable=data.frame("FaunalCode"=c("A1","A2a","A2b","B1a","B1b","C1a","C1b","D1","D2a","D2b","D2c","D2d"),"ColCode"=c("blue2","cyan1","#05aae1","plum2","darkorchid3","green3","palegreen1","#b40202","red1","darkorange","yellow","#b4b404"))
    
    ## Get required colours for test sample faunal cluster groups
    req.cols2=merge(req.cols,ColTable)
    
    ## Vector for required colours
    cols=as.character(req.cols2$ColCode)
    
    ## Define map extent
    MaxLat=max(faunal.cluster.test$Latitude_WGS84)+0.1
    MinLat=min(faunal.cluster.test$Latitude_WGS84)-0.1
    MaxLon=max(faunal.cluster.test$Longitude_WGS84)+0.1
    MinLon=min(faunal.cluster.test$Longitude_WGS84)-0.1
    
    ## Bounding box to display
    box <-  c(MinLon,MinLat,MaxLon, MaxLat ) 
    
    TestCol<-colorFactor(cols,faunal.cluster.test$FaunalCluster)
    
    ## List of items needed for maps
    res$k_st <- list( faunal.cluster.test, faunal.cluster,   TestCol, BaseCol, box)
    
    observe(leafletProxy("plot2",data=faunal.cluster.test)%>%clearMarkers() %>%
                addCircleMarkers(data=faunal.cluster.test,~as.numeric(Longitude_WGS84), ~as.numeric(Latitude_WGS84), popup = paste0("<b>Sample: </b>", faunal.cluster.test$Sample),radius = 3,stroke = TRUE, color = "black",weight = 1,fill = TRUE, fillColor = ~TestCol(FaunalCluster),fillOpacity = 1,group = "Test")%>%fitBounds(box()[1], box()[2], box()[3], box()[4])%>%addCircleMarkers(data=faunal.cluster2,~as.numeric(Longitude_WGS84), ~as.numeric(Latitude_WGS84), popup =paste0(
                "<b>Sample: </b>", faunal.cluster2$Sample, "<br>",
                "<b>SurveyName: </b>", faunal.cluster2$SurveyName,"<br>",
                "<b>Gear: </b>", faunal.cluster2$Gear,"<br>",
                "<b>Year: </b>", faunal.cluster2$Year,"<br>",                
                "<b>Distance to cluster centre A1: </b>",faunal.cluster2$A1,"<br>",
                "<b>Distance to cluster centre A2a: </b>", faunal.cluster2$A2a,"<br>",
                "<b>Distance to cluster centre A2b: </b>", faunal.cluster2$A2b,"<br>",
                "<b>Distance to cluster centre B1a: </b>", faunal.cluster2$B1a,"<br>",
                "<b>Distance to cluster centre B1b: </b>", faunal.cluster2$B1b,"<br>",
                "<b>Distance to cluster centre C1a: </b>", faunal.cluster2$C1a,"<br>",
                "<b>Distance to cluster centre C1b: </b>", faunal.cluster2$C1b,"<br>",
                "<b>Distance to cluster centre D1: </b>", faunal.cluster2$D1,"<br>",
                "<b>Distance to cluster centre D2a: </b>", faunal.cluster2$D2a,"<br>",
                "<b>Distance to cluster centre D2b: </b>", faunal.cluster2$D2b,"<br>",
                "<b>Distance to cluster centre D2c: </b>", faunal.cluster2$D2c,"<br>",
                "<b>Distance to cluster centre D2d: </b>", faunal.cluster2$D2d,"<br>",
                "<b>Faunal Cluster: </b>", faunal.cluster2$FaunalCluster,"<br>",
                "<b>Percentile: </b>", faunal.cluster2$Percentile),
                radius = 3,stroke = F, color = "black",weight = 1,fill = TRUE, fillColor =~BaseCol(FaunalCluster),fillOpacity = 1,group = "Baseline")%>%
              addLegend(
                position = "bottomright",
                colors = c("#0000EE","#00FFFF","#05aae1","#EEAEEE","#9A32CD","#00CD00","#9AFF9A","#B40202","#FF0000","#FF8C00","#FFFF00","#b4b404"),# NB have to use hex cols
                labels = c("A1","A2a","A2b","B1a","B1b","C1a","C1b","D1","D2a","D2b","D2c","D2d"),        
                opacity = 1,
                title = "Faunal Cluster"
              )%>%
              addLayersControl(
                overlayGroups = c("Test","Baseline"),options = layersControlOptions(collapsed = FALSE))%>% hideGroup("Baseline")
    )
    
  })
  
  
  ## Map
  output$plot2 <- renderLeaflet({
    
    ## Basic map
    leaflet() %>%
      addProviderTiles(providers$Esri.OceanBasemap,options = providerTileOptions(noWrap = TRUE))%>%
      #addPolygons(data=licence, weight = 1,fillColor="white",fillOpacity = 0)%>%
      #addPolygons(data=mcz, weight = 1,color="orange",fillColor="orange",fillOpacity = 0)%>%
      setView(-3,54.6,zoom=5.5)
    
  })
  
  ## Update map with imported positions
  observe(leafletProxy("plot2",data=pos())%>%
            addCircleMarkers(data=pos(),~as.numeric(Longitude_WGS84), ~as.numeric(Latitude_WGS84), popup = ~as.character(Sample),radius = 3,stroke = F, color = "black",weight = 1,fill = TRUE, fillColor ="black",fillOpacity = 1,group = "Baseline")%>%fitBounds(box()[1], box()[2], box()[3], box()[4])
  )
  
  
  
  output$results <- DT::renderDataTable({
    
    if ( !is.null(res$k_st) )  {
      
      ## Split off faunal data
      ShinyTemplate3=data()[,4:706]
      
      ## Transform faunal data
      ShinyTemplate4=ShinyTemplate3^(0.25)
      
      ## Create a df 'pos.test' for Sample, Latitude_WGS84 and Longitude_WGS84 
      pos.test=data()[,1:3]
      
      ## Now use predict function to predict cluster groups for test data.
      pred_test <- predict(resultsA, newdata=ShinyTemplate4)
      
      ## Get  phy cluster group from raster
      Phy <- extract(phyclus,  pos.test[,3:2])

      
      ## Add cluster group from kmeans results file to df 'pos' which includes 'Sample',
      # 'Latitude_WGS84' and 'Longitude_WGS84'
      faunal.cluster.test=cbind(pos.test,pred_test,Phy)#,physdata
      
      ## Change name of col 'results$cluster' to 'ClusterNum'
      names(faunal.cluster.test)[4]<-paste("ClusterNum")
      names(faunal.cluster.test)[2]<-paste("Lat")
      names(faunal.cluster.test)[3]<-paste("Long")
      
      ## Add a new empty col 'FaunalCluster' to df 'faunal.cluster
      faunal.cluster.test["Fauna"]=NA
      
      ## Populate FaunalCluster col with new names (see dendrogram from Step 21)
      faunal.cluster.test$Fauna[faunal.cluster.test$ClusterNum == 11] <- "A1"
      faunal.cluster.test$Fauna[faunal.cluster.test$ClusterNum == 1]<- "A2a"
      faunal.cluster.test$Fauna[faunal.cluster.test$ClusterNum == 8] <- "A2b"
      faunal.cluster.test$Fauna[faunal.cluster.test$ClusterNum == 3]<- "B1a"
      faunal.cluster.test$Fauna[faunal.cluster.test$ClusterNum == 7] <- "B1b"
      faunal.cluster.test$Fauna[faunal.cluster.test$ClusterNum == 4] <- "C1a"
      faunal.cluster.test$Fauna[faunal.cluster.test$ClusterNum == 5] <- "C1b"
      faunal.cluster.test$Fauna[faunal.cluster.test$ClusterNum == 12] <- "D1"
      faunal.cluster.test$Fauna[faunal.cluster.test$ClusterNum == 2] <- "D2a"
      faunal.cluster.test$Fauna[faunal.cluster.test$ClusterNum == 10] <- "D2b"
      faunal.cluster.test$Fauna[faunal.cluster.test$ClusterNum == 6] <- "D2c"
      faunal.cluster.test$Fauna[faunal.cluster.test$ClusterNum == 9]<- "D2d"
      
      ## Note col FaunalCluster is currently a chr - need to covery to a factor
      faunal.cluster.test$Fauna=as.factor(faunal.cluster.test$Fauna)
      
      ## Concatenate Faunal and Physical cluster
      faunal.cluster.test$PhyFauna=paste(faunal.cluster.test$Phy,faunal.cluster.test$Fauna,sep="_")
      names(faunal.cluster.test)[6]<-paste("Cluster")
      faunal.cluster.test[,c(1,2,3,6,5,7)]#,6
      
    } else {
      
      pos.test=data()[,1:3]
      
      ## Change labels for Lat and Long
      names(pos.test)[2]<-paste("Lat")
      names(pos.test)[3]<-paste("Long")
      
      pos.test
    }
  })
  

  output$distances <- DT::renderDataTable({
    
    if ( !is.null(res$k_st) )  {
      
      ## Split off faunal data
      ShinyTemplate3=data()[,4:706]
     
      ## Transform faunal data
      ShinyTemplate4=ShinyTemplate3^(0.25)
      
      ## Create a df 'pos.test' for Sample, Latitude_WGS84 and Longitude_WGS84 
      pos.test=data()[,1:3]
     
      ## Now use predict function to predict cluster groups for test data.
      pred_test <- predict(resultsA, newdata=ShinyTemplate4)
      
      ## Get  phy cluster group from raster
      Phy <- extract(phyclus,  pos.test[,3:2])
      
      ## Add cluster group from kmeans results file to df 'pos' which includes 'Sample', Latitude_WGS84' and 'Longitude_WGS84'
      faunal.cluster.test=cbind(pos.test,pred_test,Phy)#,physdata
      
      ## Change name of col 'results$cluster' to 'ClusterNum'
      names(faunal.cluster.test)[4]<-paste("ClusterNum")
      names(faunal.cluster.test)[2]<-paste("Lat")
      names(faunal.cluster.test)[3]<-paste("Long")
      
      ## Add a new empty col 'FaunalCluster' to df 'faunal.cluster
      faunal.cluster.test["Fauna"]=NA
      
      ## Populate FaunalCluster col with new names (see dendrogram from Step 21)
      faunal.cluster.test$Fauna[faunal.cluster.test$ClusterNum == 11] <- "A1"
      faunal.cluster.test$Fauna[faunal.cluster.test$ClusterNum == 1]<- "A2a"
      faunal.cluster.test$Fauna[faunal.cluster.test$ClusterNum == 8] <- "A2b"
      faunal.cluster.test$Fauna[faunal.cluster.test$ClusterNum == 3]<- "B1a"
      faunal.cluster.test$Fauna[faunal.cluster.test$ClusterNum == 7] <- "B1b"
      faunal.cluster.test$Fauna[faunal.cluster.test$ClusterNum == 4] <- "C1a"
      faunal.cluster.test$Fauna[faunal.cluster.test$ClusterNum == 5] <- "C1b"
      faunal.cluster.test$Fauna[faunal.cluster.test$ClusterNum == 12] <- "D1"
      faunal.cluster.test$Fauna[faunal.cluster.test$ClusterNum == 2] <- "D2a"
      faunal.cluster.test$Fauna[faunal.cluster.test$ClusterNum == 10] <- "D2b"
      faunal.cluster.test$Fauna[faunal.cluster.test$ClusterNum == 6] <- "D2c"
      faunal.cluster.test$Fauna[faunal.cluster.test$ClusterNum == 9]<- "D2d"
      
      ## Note col FaunalCluster is currently a chr - need to covery to a factor
      faunal.cluster.test$Fauna=as.factor(faunal.cluster.test$Fauna)
      
      ## Concatenate Faunal and Physical cluster
      faunal.cluster.test$PhyFauna=paste(faunal.cluster.test$Phy,faunal.cluster.test$Fauna,sep="_")
      
      faunal.cluster.test[,c(1,2,3,5,6,7)]#,6
      #View(faunal.cluster.test)
      
      DistancestoCentersTest <- as.matrix(dist(rbind(results$centers, ShinyTemplate4)))[-(1:12),1:12]
      
      ## Add Sample column
      names(pos.test)
      DistancetoCentersTest=cbind(as.character(pos.test$Sample),DistancestoCentersTest)
      
      ## Update column names
      colnames(DistancetoCentersTest)=c("Sample","A2a","D2a","B1a","C1a","C1b","D2c","B1b","A2b","D2d","D2b","A1","D1")
      
      ## Change column order
      DistancetoCentersTest=DistancetoCentersTest[,c(1,12,2,9,4,8,5,6,13,3,11,7,10)]
      
      ## Change object from matrix to dataframe
      class(DistancetoCentersTest)
      DistancetoCentersTest=as.data.frame(DistancetoCentersTest)
      
      ## Add column for faunal cluster group
      DistancetoCentersTest2=cbind(DistancetoCentersTest[,1],faunal.cluster.test$Fauna,DistancetoCentersTest[,2:13])
      colnames(DistancetoCentersTest2)[2]="Cluster"
      colnames(DistancetoCentersTest2)[1]="Sample"
      
      ## Create a copy of'DistancetoCentersTrain3'
      DistancetoCentersTest3=DistancetoCentersTest2
      
      # Change cols into correct format
      DistancetoCentersTest3$Sample <- as.character(as.character(DistancetoCentersTest3$Sample))
      DistancetoCentersTest3$Cluster <- as.character(as.character(DistancetoCentersTest3$Cluster))
      DistancetoCentersTest3$A1 <- as.numeric(as.character(DistancetoCentersTest3$A1))
      DistancetoCentersTest3$A2a <- as.numeric(as.character(DistancetoCentersTest3$A2a))
      DistancetoCentersTest3$A2b <- as.numeric(as.character(DistancetoCentersTest3$A2b))
      DistancetoCentersTest3$B1a <- as.numeric(as.character(DistancetoCentersTest3$B1a))
      DistancetoCentersTest3$B1b <- as.numeric(as.character(DistancetoCentersTest3$B1b))
      DistancetoCentersTest3$C1a <- as.numeric(as.character(DistancetoCentersTest3$C1a))
      DistancetoCentersTest3$C1b <- as.numeric(as.character(DistancetoCentersTest3$C1b))
      DistancetoCentersTest3$D1 <- as.numeric(as.character(DistancetoCentersTest3$D1))
      DistancetoCentersTest3$D2a <- as.numeric(as.character(DistancetoCentersTest3$D2a))
      DistancetoCentersTest3$D2b <- as.numeric(as.character(DistancetoCentersTest3$D2b))
      DistancetoCentersTest3$D2c <- as.numeric(as.character(DistancetoCentersTest3$D2c))
      DistancetoCentersTest3$D2d <- as.numeric(as.character(DistancetoCentersTest3$D2d))

      ## Change numeric columns to 1dp
      is.num <- sapply(DistancetoCentersTest3, is.numeric)
      DistancetoCentersTest3[is.num] <- lapply(DistancetoCentersTest3[is.num], round, 1)
      
      DistancetoCentersTest3
      

    } else {
      
      
    }
  })
  

  ## Percentiles
  output$percentiles <- DT::renderDataTable({
    
    if ( !is.null(res$k_st) )  {
      
      ## Split off faunal data
      ShinyTemplate3=data()[,4:706]
      
      ## Transform faunal data
      ShinyTemplate4=ShinyTemplate3^(0.25)
      
      ## Create a df 'pos.test' for Sample, Latitude_WGS84 and Longitude_WGS84 
      pos.test=data()[,1:3]
      
      ## Now use predict function to predict cluster groups for test data.
      pred_test <- predict(resultsA, newdata=ShinyTemplate4)
      
      
      testpercentile = rep(0,length(pred_test)) # numeric vector of 0's length 636 (test set) #636
      
      for (j in 1:length(pred_test)) {
        
        testcluster = pred_test[j]  # assiged test sample cluster groups 
        # loop through test cluster group and get distances to cluster centre of assigned group
        if (testcluster==1) {
          distfortest = sum((results$centers[1,] - ShinyTemplate4[j,])^2)
          combined = c(distfortest, distsfor1) }
        
        if (testcluster==2) {
          distfortest = sum((results$centers[2,] - ShinyTemplate4[j,])^2)
          combined = c(distfortest, distsfor2) }
        
        if (testcluster==3) {
          distfortest = sum((results$centers[3,] - ShinyTemplate4[j,])^2)
          combined = c(distfortest, distsfor3) }
        
        if (testcluster==4) {
          distfortest = sum((results$centers[4,] - ShinyTemplate4[j,])^2)
          combined = c(distfortest, distsfor4) }
        
        if (testcluster==5) {
          distfortest = sum((results$centers[5,] - ShinyTemplate4[j,])^2)
          combined = c(distfortest, distsfor5) }
        
        if (testcluster==6) {
          distfortest = sum((results$centers[6,] - ShinyTemplate4[j,])^2)
          combined = c(distfortest, distsfor6) }
        
        if (testcluster==7) {
          distfortest = sum((results$centers[7,] - ShinyTemplate4[j,])^2)
          combined = c(distfortest, distsfor7) }
        
        if (testcluster==8) {
          distfortest = sum((results$centers[8,] - ShinyTemplate4[j,])^2)
          combined = c(distfortest, distsfor8) }
        
        if (testcluster==9) {
          distfortest = sum((results$centers[9,] - ShinyTemplate4[j,])^2)
          combined = c(distfortest, distsfor9) }
        
        if (testcluster==10) {
          distfortest = sum((results$centers[10,] - ShinyTemplate4[j,])^2)
          combined = c(distfortest, distsfor10) }
        
        if (testcluster==11) {
          distfortest = sum((results$centers[11,] - ShinyTemplate4[j,])^2)
          combined = c(distfortest, distsfor11) }
        
        if (testcluster==12) {
          distfortest = sum((results$centers[12,] - ShinyTemplate4[j,])^2)
          combined = c(distfortest, distsfor12) }
        
        ## rank of sample divided by total number of samples in cluster *100
        combined=unlist(combined)
        ranktest = rank(combined)[1]
        
        testpercentile[j] = round(100*(ranktest - 0.5) / length(combined), 1)
        
      }
      
      #### percetile = near 100% means that your test percentile is near to the most extreme of the originals in that cluster

      testresults = as.data.frame(cbind(pos.test[1],pred_test, testpercentile))
      str(testresults)
      
      ## Swap cluster numbers for codes
      testresults$pred_test[testresults$pred_test == 11] <- "A1"
      testresults$pred_test[testresults$pred_test == 1]<- "A2a"
      testresults$pred_test[testresults$pred_test == 8] <- "A2b"
      testresults$pred_test[testresults$pred_test == 3]<- "B1a"
      testresults$pred_test[testresults$pred_test == 7] <- "B1b"
      testresults$pred_test[testresults$pred_test == 4] <- "C1a"
      testresults$pred_test[testresults$pred_test == 5] <- "C1b"
      testresults$pred_test[testresults$pred_test == 12] <- "D1"
      testresults$pred_test[testresults$pred_test == 2] <- "D2a"
      testresults$pred_test[testresults$pred_test == 10] <- "D2b"
      testresults$pred_test[testresults$pred_test == 6] <- "D2c"
      testresults$pred_test[testresults$pred_test == 9]<- "D2d"
      
      
      ## Note col FaunalCluster is currently a chr - need to covert to a factor
      testresults$pred_test=as.factor(testresults$pred_test)
      str(testresults) # Test data cluster group and associated percentile
      
      ## Change names of cols in object 'testresults'
      names(testresults)[1]<-paste("Sample")
      names(testresults)[2]<-paste("Cluster")
      names(testresults)[3]<-paste("Percentile")
      testresults
      
    } else {
      
      
    }
  })
  

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("download",".csv",sep="")#data2-",Sys.Date(),
    },
    content = function(file) {
    
      ## Split off faunal data
      ShinyTemplate3=data()[,4:706]
      
      ## Transform faunal data
      ShinyTemplate4=ShinyTemplate3^(0.25)
      
      ## Create a df 'pos.test' for Sample, Latitude_WGS84 and Longitude_WGS84. NB/You may need to update the colrefs for Lat and Long 
      pos.test=data()[,1:3]
      
      ## Now use predict function to predict cluster groups for test data.
      pred_test <- predict(resultsA, newdata=ShinyTemplate4)
      
      ## Get  phy cluster groupo from raster
      Phy <- extract(phyclus,  pos.test[,3:2])
      
      ## Add cluster group from kmeans results file to df 'pos' which includes 'Sample','Latitude_WGS84' and 'Longitude_WGS84'
      faunal.cluster.test=cbind(pos.test,pred_test,Phy)
      names(faunal.cluster.test)
      
      ## Change name of col 'results$cluster' to 'ClusterNum'
      names(faunal.cluster.test)[4]<-paste("ClusterNum")
      
      ## Add a new empty col 'FaunalCluster' to df 'faunal.cluster
      faunal.cluster.test["Fauna"]=NA
      
      ## Populate FaunalCluster col with new names (see dendrogram from Step 21)
      faunal.cluster.test$Fauna[faunal.cluster.test$ClusterNum == 11] <- "A1"
      faunal.cluster.test$Fauna[faunal.cluster.test$ClusterNum == 1]<- "A2a"
      faunal.cluster.test$Fauna[faunal.cluster.test$ClusterNum == 8] <- "A2b"
      faunal.cluster.test$Fauna[faunal.cluster.test$ClusterNum == 3]<- "B1a"
      faunal.cluster.test$Fauna[faunal.cluster.test$ClusterNum == 7] <- "B1b"
      faunal.cluster.test$Fauna[faunal.cluster.test$ClusterNum == 4] <- "C1a"
      faunal.cluster.test$Fauna[faunal.cluster.test$ClusterNum == 5] <- "C1b"
      faunal.cluster.test$Fauna[faunal.cluster.test$ClusterNum == 12] <- "D1"
      faunal.cluster.test$Fauna[faunal.cluster.test$ClusterNum == 2] <- "D2a"
      faunal.cluster.test$Fauna[faunal.cluster.test$ClusterNum == 10] <- "D2b"
      faunal.cluster.test$Fauna[faunal.cluster.test$ClusterNum == 6] <- "D2c"
      faunal.cluster.test$Fauna[faunal.cluster.test$ClusterNum == 9]<- "D2d"
      
      ## Note col FaunalCluster is currently a chr - need to covery to a factor
      faunal.cluster.test$Fauna=as.factor(faunal.cluster.test$Fauna)
      
      ## Concatenate Faunal and Physical cluster
      faunal.cluster.test$PhyFauna=paste(faunal.cluster.test$Phy,faunal.cluster.test$Fauna,sep="_")
      names(faunal.cluster.test)[6]<-paste("Cluster")
      write.csv(faunal.cluster.test[,c(1,2,3,6,5,7)],file,row.names = F)
    })
  
  
  
  
}


shinyApp(ui, server)