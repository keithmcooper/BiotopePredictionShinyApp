
#setwd("Y:/C7331_Marine Aggregate Reg/Working_Area/C7331_E FAUNAL GROUP PREDICTION")
#setwd("Y:/C7912_Marine_Aggregate_Reg/Working_Area/C7331/C7331_E FAUNAL GROUP PREDICTION")
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

#### BRING IN REQUIRED DATA ####

## Bring in baseline data for faunal clustering (for use in maps)
faunal.cluster=read.csv("DATA/BaselineFaunalCluster2.csv",header=T,na.strings=c("NA", "-","?","<null>"),stringsAsFactors=F,check.names=FALSE)
#View(faunal.cluster)

## Bring in baseline kcca object
resultsA <- readRDS("DATA/resultsA")
#resultsA

## Bring in baseline cluster results object
results <- readRDS("DATA/results")

## 24/05/2019
#data=read.csv("OUTPUTS/ShinyTemplateCompletedNWJB.csv",header=T,na.strings=c("NA", "-","?","<null>"),stringsAsFactors=F,check.names=FALSE)
#View(data)
#dim(data)
#str(data)
## Baseline sample - muted colours 
BaseCol <- colorFactor(c("#9999F8","#99FFFF","#9BDDE6","#F8DEF8","#D6ADEB","#99EB99","#D6FFD6","#E19999","#FF9999","#FFD199","#FFFF99","#E1E19A"), faunal.cluster$FaunalCluster)

## Baseline sample - full colours 
#BaseCol <- colorFactor(c("blue2","cyan1","#05aae1","plum2","darkorchid3","green3","palegreen1","#b40202","red1","darkorange","yellow","#b4b404"), faunal.cluster$FaunalCluster)


#####################################################
#v7
## Bring in raster data for phycluster
#phyclus = raster('DATA/FINAL_MODEL/PhysicalCluster/PhysicalClusterClip.tif')
phyclus = raster('DATA/PhysicalClusterClip.tif')

## Create layer for 'Licensed' polygons
#licence = readOGR("DATA","Aggregates_Licence_20151112")

#mcz = readOGR("DATA","DESIGNATED")
#####################################################

## Mean and sd for baseline cluster distances
meanA1dist=7.998403
sdA1dist=1.245949
meanA2adist=7.417848
sdA2adist=1.305278
meanA2bdist=6.955519
sdA2bdist=1.017589
meanB1adist=6.39505
sdB1adist=0.7995207
meanB1bdist=5.418755
sdB1bdist=0.8244385
meanC1adist=5.60087
sdC1adist=0.9520959
meanC1bdist=6.4334
sdC1bdist=0.9144225
meanD1dist=6.970568
sdD1dist=1.265555
meanD2adist=4.863829
sdD2adist=0.9588489
meanD2bdist=5.282591
sdD2bdist=0.7700981
meanD2cdist=2.963243
sdD2cdist=0.9613814
meanD2ddist=4.420252
sdD2ddist=0.906758
#############################################################
## 04/06/2019
## Bring in distances, z-scores and percentiles for baseline dataset
basedist=read.csv("DATA/DistancetoCentersTrain6.csv",header=T,na.strings=c("NA", "-","?","<null>"),stringsAsFactors=F,check.names=FALSE)

## Check baseline dataset and baseline distance from clustering have same length
dim(basedist)#[1] 27432    42
dim(faunal.cluster)# 27432     7
View(basedist)
names(basedist)

## Delete irrelevant values
basedist$zA1[basedist$FaunalCluster !=  "A1"] <- 0
basedist$zA2a[basedist$FaunalCluster !=  "A2a"] <- 0
basedist$zA2b[basedist$FaunalCluster !=  "A2b"] <- 0
basedist$zB1a[basedist$FaunalCluster !=  "B1a"] <- 0
basedist$zB1b[basedist$FaunalCluster !=  "B1b"] <- 0
basedist$zC1a[basedist$FaunalCluster !=  "C1a"] <- 0
basedist$zC1b[basedist$FaunalCluster !=  "C1b"] <- 0
basedist$zD1[basedist$FaunalCluster !=  "D1"] <- 0
basedist$zD2a[basedist$FaunalCluster !=  "D2a"] <- 0
basedist$zD2b[basedist$FaunalCluster !=  "D2b"] <- 0
basedist$zD2c[basedist$FaunalCluster !=  "D2c"] <- 0
basedist$zD2d[basedist$FaunalCluster !=  "D2d"] <- 0

basedist$pA1[basedist$FaunalCluster !=  "A1"] <- 0
basedist$pA2a[basedist$FaunalCluster !=  "A2a"] <- 0
basedist$pA2b[basedist$FaunalCluster !=  "A2b"] <- 0
basedist$pB1a[basedist$FaunalCluster !=  "B1a"] <- 0
basedist$pB1b[basedist$FaunalCluster !=  "B1b"] <- 0
basedist$pC1a[basedist$FaunalCluster !=  "C1a"] <- 0
basedist$pC1b[basedist$FaunalCluster !=  "C1b"] <- 0
basedist$pD1[basedist$FaunalCluster !=  "D1"] <- 0
basedist$pD2a[basedist$FaunalCluster !=  "D2a"] <- 0
basedist$pD2b[basedist$FaunalCluster !=  "D2b"] <- 0
basedist$pD2c[basedist$FaunalCluster !=  "D2c"] <- 0
basedist$pD2d[basedist$FaunalCluster !=  "D2d"] <- 0

## Add new cols for zscore and percentile

basedist$zscore <- do.call(`pmax`, basedist[16:27])
basedist$percentile <- do.call(`pmax`, basedist[28:39])
names(basedist)
## Take only required columns
basedist2=basedist[,c(1:15,41,42,40)]
View(basedist2)
names(basedist2)
## Change numeric columns to 1dp
is.num <- sapply(basedist2, is.numeric)
basedist2[is.num] <- lapply(basedist2[is.num], round, 1)

## Now add this distance info to the object faunal.cluster
faunal.cluster2=cbind(faunal.cluster,basedist2[,4:18])
names(faunal.cluster2)


#################
library(shiny)

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
    
    column(5,leafletOutput("plot2",height=800), 
           br(),
           downloadButton("downloadPlot", "Download plot"),style='border-left: 1px solid grey'),
    
    
    column(5,style='border-left: 1px solid grey',
           tabsetPanel(
             tabPanel("Results",div(DT::dataTableOutput("results"),style = 'font-size:90%'),br(),# Button
                      downloadButton("downloadData", "Download table")),
             tabPanel("Distances",div(DT::dataTableOutput("distances"),style = 'font-size:85%'),br()),
             tabPanel("Z-Scores",div(DT::dataTableOutput("zscores"),style = 'font-size:85%'),br()),
             tabPanel("Percentiles",div(DT::dataTableOutput("percentiles"),style = 'font-size:85%'),br()),
             tabPanel("About",
                      br(),
                      p("This tool matches new faunal data to the existing faunal cluster groups identified in Cooper and Barry (2017, http://rdcu.be/wi6C). The template includes all families from the baseline dataset, and matching is achieved using the 'predict' function.")))
           
    )))

textAlign = 'center'


############### SERVER ####################################################################################


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
  
  
  
  ################
  ###############
  
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
    
    ## Baseline sample - muted colours 
    #BaseCol <- colorFactor(c("#9999F8","#99FFFF","#9BDDE6","#F8DEF8","#D6ADEB","#99EB99","#D6FFD6","#E19999","#FF9999","#FFD199","#FFFF99","#E1E19A"), faunal.cluster$FaunalCluster)
    
    ## Baseline sample - full colours 
    #BaseCol <- colorFactor(c("blue2","cyan1","#05aae1","plum2","darkorchid3","green3","palegreen1","#b40202","red1","darkorange","yellow","#b4b404"), faunal.cluster$FaunalCluster)
    
    
    TestCol<-colorFactor(cols,faunal.cluster.test$FaunalCluster)
    
    ## List of items needed for maps
    res$k_st <- list( faunal.cluster.test, faunal.cluster,   TestCol, BaseCol, box)
    
    observe(leafletProxy("plot2",data=faunal.cluster.test)%>%clearMarkers() %>%
              #addProviderTiles(providers$Esri.OceanBasemap,options = providerTileOptions(noWrap = TRUE))%>%
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
                "<b>Z-score: </b>", faunal.cluster2$zscore,"<br>",
                "<b>Percentile: </b>", faunal.cluster2$percentile),
                radius = 3,stroke = F, color = "black",weight = 1,fill = TRUE, fillColor =~BaseCol(FaunalCluster),fillOpacity = 1,group = "Baseline")%>%
              addLegend(
                position = "bottomright",
                colors = c("#0000EE","#00FFFF","#05aae1","#EEAEEE","#9A32CD","#00CD00","#9AFF9A","#B40202","#FF0000","#FF8C00","#FFFF00","#b4b404"),# NB have to use hex cols
                labels = c("A1","A2a","A2b","B1a","B1b","C1a","C1b","D1","D2a","D2b","D2c","D2d"),        
                opacity = 1,
                title = "Faunal Cluster"
              )%>%
              addLayersControl(
                #baseGroups=("Baseline"),
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
      
      #addProviderTiles(providers$Esri.WorldImagery,options = providerTileOptions(noWrap = TRUE))%>%
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
      
      #####################################################
      #v7
      ## Get  phy cluster groupo from raster
      Phy <- extract(phyclus,  pos.test[,3:2])
      
      
      
      #####################################################
      
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
      
      faunal.cluster.test[,c(1,2,3,5,6,7)]#,6
    } else {
      
      
      pos.test=data()[,1:3]
      
      ## Change labels for Lat and Long
      names(pos.test)[2]<-paste("Lat")
      names(pos.test)[3]<-paste("Long")
      
      pos.test
    }
  })
  
  ###############################################################################
  ###############################################################################
  ## 24/05/2019
  output$distances <- DT::renderDataTable({
    
    if ( !is.null(res$k_st) )  {
      
      ## Split off faunal data
      ShinyTemplate3=data()[,4:706]
      #ShinyTemplate3=data[,4:706]#24/05
      ## Transform faunal data
      ShinyTemplate4=ShinyTemplate3^(0.25)
      
      ## Create a df 'pos.test' for Sample, Latitude_WGS84 and Longitude_WGS84 
      pos.test=data()[,1:3]
      #pos.test=data[,1:3]#24/05
      ## Now use predict function to predict cluster groups for test data.
      pred_test <- predict(resultsA, newdata=ShinyTemplate4)
      
      #####################################################
      #v7
      ## Get  phy cluster groupo from raster
      Phy <- extract(phyclus,  pos.test[,3:2])
      
      
      
      #####################################################
      
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
      
      #str(DistancetoCentersTest)
      
      ## Add column for faunal cluster group
      DistancetoCentersTest2=cbind(DistancetoCentersTest[,1],faunal.cluster.test$Fauna,DistancetoCentersTest[,2:13])
      colnames(DistancetoCentersTest2)[2]="FaunalCluster"
      colnames(DistancetoCentersTest2)[1]="Sample"
      
      ## Create a copy of'DistancetoCentersTrain3'
      DistancetoCentersTest3=DistancetoCentersTest2
      
      # Change cols into correct format
      DistancetoCentersTest3$Sample <- as.character(as.character(DistancetoCentersTest3$Sample))
      DistancetoCentersTest3$FaunalCluster <- as.character(as.character(DistancetoCentersTest3$FaunalCluster))
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
      #str(DistancetoCentersTest3)
      
      ## Change numeric columns to 1dp
      is.num <- sapply(DistancetoCentersTest3, is.numeric)
      DistancetoCentersTest3[is.num] <- lapply(DistancetoCentersTest3[is.num], round, 1)
      
      DistancetoCentersTest3
      
      #View(DistancetoCentersTest3)
      #class(DistancetoCentersTest3)
    } else {
      
      
      #pos.test=data()[,1:3]
      
      ## Change labels for Lat and Long
      #names(pos.test)[2]<-paste("Lat")
      #names(pos.test)[3]<-paste("Long")
      
      #pos.test
    }
  })
  
  ###############################################################################
  ###############################################################################
  ## 03/06/2019
  output$zscores <- DT::renderDataTable({
    
    if ( !is.null(res$k_st) )  {
      
      ## Split off faunal data
      ShinyTemplate3=data()[,4:706]
      #ShinyTemplate3=data[,4:706]#24/05
      ## Transform faunal data
      ShinyTemplate4=ShinyTemplate3^(0.25)
      
      ## Create a df 'pos.test' for Sample, Latitude_WGS84 and Longitude_WGS84 
      pos.test=data()[,1:3]
      #pos.test=data[,1:3]#24/05
      ## Now use predict function to predict cluster groups for test data.
      pred_test <- predict(resultsA, newdata=ShinyTemplate4)
      
      #####################################################
      #v7
      ## Get  phy cluster groupo from raster
      Phy <- extract(phyclus,  pos.test[,3:2])
      
      
      
      #####################################################
      
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
      
      #str(DistancetoCentersTest)
      
      ## Add column for faunal cluster group
      DistancetoCentersTest2=cbind(DistancetoCentersTest[,1],faunal.cluster.test$Fauna,DistancetoCentersTest[,2:13])
      colnames(DistancetoCentersTest2)[2]="FaunalCluster"
      colnames(DistancetoCentersTest2)[1]="Sample"
      
      ## Create a copy of'DistancetoCentersTrain3'
      DistancetoCentersTest3=DistancetoCentersTest2
      
      # Change cols into correct format
      DistancetoCentersTest3$Sample <- as.character(as.character(DistancetoCentersTest3$Sample))
      DistancetoCentersTest3$FaunalCluster <- as.character(as.character(DistancetoCentersTest3$FaunalCluster))
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
      #str(DistancetoCentersTest3)
      
      ## Calculate z-score
      DistancetoCentersTest3$zA1=(DistancetoCentersTest3$A1-meanA1dist)/sdA1dist
      DistancetoCentersTest3$zA2a=(DistancetoCentersTest3$A2a-meanA2adist)/sdA2adist
      DistancetoCentersTest3$zA2b=(DistancetoCentersTest3$A2b-meanA2bdist)/sdA2bdist
      DistancetoCentersTest3$zB1a=(DistancetoCentersTest3$B1a-meanB1adist)/sdB1adist
      DistancetoCentersTest3$zB1b=(DistancetoCentersTest3$B1b-meanB1bdist)/sdB1bdist
      DistancetoCentersTest3$zC1a=(DistancetoCentersTest3$C1a-meanC1adist)/sdC1adist
      DistancetoCentersTest3$zC1b=(DistancetoCentersTest3$C1b-meanC1bdist)/sdC1bdist
      DistancetoCentersTest3$zD1=(DistancetoCentersTest3$D1-meanD1dist)/sdD1dist
      DistancetoCentersTest3$zD2a=(DistancetoCentersTest3$D2a-meanD2adist)/sdD2adist
      DistancetoCentersTest3$zD2b=(DistancetoCentersTest3$D2b-meanD2bdist)/sdD2bdist
      DistancetoCentersTest3$zD2c=(DistancetoCentersTest3$D2c-meanD2cdist)/sdD2cdist
      DistancetoCentersTest3$zD2d=(DistancetoCentersTest3$D2d-meanD2ddist)/sdD2ddist
      
      ## Take only the z-scores
      
      DistancetoCentersTest4=DistancetoCentersTest3[,c(1,2,15:26)]
      ## Change numeric columns to 1dp
      is.num <- sapply(DistancetoCentersTest4, is.numeric)
      DistancetoCentersTest4[is.num] <- lapply(DistancetoCentersTest4[is.num], round, 1)
      
      ## Update column names
      colnames(DistancetoCentersTest4)=c("Sample","FaunalCluster","A1","A2a","A2b","B1a","B1b","C1a","C1b","D1","D2a","D2b","D2c","D2d")
      
      ## Delete irrelevant values
      DistancetoCentersTest4$A1[DistancetoCentersTest4$FaunalCluster !=  "A1"] <- NA
      DistancetoCentersTest4$A2a[DistancetoCentersTest4$FaunalCluster !=  "A2a"] <- NA
      DistancetoCentersTest4$A2b[DistancetoCentersTest4$FaunalCluster !=  "A2b"] <- NA
      DistancetoCentersTest4$B1a[DistancetoCentersTest4$FaunalCluster !=  "B1a"] <- NA
      DistancetoCentersTest4$B1b[DistancetoCentersTest4$FaunalCluster !=  "B1b"] <- NA
      DistancetoCentersTest4$C1a[DistancetoCentersTest4$FaunalCluster !=  "C1a"] <- NA
      DistancetoCentersTest4$C1b[DistancetoCentersTest4$FaunalCluster !=  "C1b"] <- NA
      DistancetoCentersTest4$D1[DistancetoCentersTest4$FaunalCluster !=  "D1"] <- NA
      DistancetoCentersTest4$D2a[DistancetoCentersTest4$FaunalCluster !=  "D2a"] <- NA
      DistancetoCentersTest4$D2b[DistancetoCentersTest4$FaunalCluster !=  "D2b"] <- NA
      DistancetoCentersTest4$D2c[DistancetoCentersTest4$FaunalCluster !=  "D2c"] <- NA
      DistancetoCentersTest4$D2d[DistancetoCentersTest4$FaunalCluster !=  "D2d"] <- NA
      
      DistancetoCentersTest4
      
      #View(DistancetoCentersTest3)
      #class(DistancetoCentersTest3)
    } else {
      
      
      #pos.test=data()[,1:3]
      
      ## Change labels for Lat and Long
      #names(pos.test)[2]<-paste("Lat")
      #names(pos.test)[3]<-paste("Long")
      
      #pos.test
    }
  }) 
  
  #############################################################################
  ##############################################################################
  ## 03/06/2019
  output$percentiles <- DT::renderDataTable({
    
    if ( !is.null(res$k_st) )  {
      
      ## Split off faunal data
      ShinyTemplate3=data()[,4:706]
      #ShinyTemplate3=data[,4:706]#24/05
      ## Transform faunal data
      ShinyTemplate4=ShinyTemplate3^(0.25)
      
      ## Create a df 'pos.test' for Sample, Latitude_WGS84 and Longitude_WGS84 
      pos.test=data()[,1:3]
      #pos.test=data[,1:3]#24/05
      ## Now use predict function to predict cluster groups for test data.
      pred_test <- predict(resultsA, newdata=ShinyTemplate4)
      
      #####################################################
      #v7
      ## Get  phy cluster groupo from raster
      Phy <- extract(phyclus,  pos.test[,3:2])
      
      
      
      #####################################################
      
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
      
      #str(DistancetoCentersTest)
      
      ## Add column for faunal cluster group
      DistancetoCentersTest2=cbind(DistancetoCentersTest[,1],faunal.cluster.test$Fauna,DistancetoCentersTest[,2:13])
      colnames(DistancetoCentersTest2)[2]="FaunalCluster"
      colnames(DistancetoCentersTest2)[1]="Sample"
      
      ## Create a copy of'DistancetoCentersTrain3'
      DistancetoCentersTest3=DistancetoCentersTest2
      
      # Change cols into correct format
      DistancetoCentersTest3$Sample <- as.character(as.character(DistancetoCentersTest3$Sample))
      DistancetoCentersTest3$FaunalCluster <- as.character(as.character(DistancetoCentersTest3$FaunalCluster))
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
      #str(DistancetoCentersTest3)
      
      ## Calculate z-score
      DistancetoCentersTest3$zA1=(DistancetoCentersTest3$A1-meanA1dist)/sdA1dist
      DistancetoCentersTest3$zA2a=(DistancetoCentersTest3$A2a-meanA2adist)/sdA2adist
      DistancetoCentersTest3$zA2b=(DistancetoCentersTest3$A2b-meanA2bdist)/sdA2bdist
      DistancetoCentersTest3$zB1a=(DistancetoCentersTest3$B1a-meanB1adist)/sdB1adist
      DistancetoCentersTest3$zB1b=(DistancetoCentersTest3$B1b-meanB1bdist)/sdB1bdist
      DistancetoCentersTest3$zC1a=(DistancetoCentersTest3$C1a-meanC1adist)/sdC1adist
      DistancetoCentersTest3$zC1b=(DistancetoCentersTest3$C1b-meanC1bdist)/sdC1bdist
      DistancetoCentersTest3$zD1=(DistancetoCentersTest3$D1-meanD1dist)/sdD1dist
      DistancetoCentersTest3$zD2a=(DistancetoCentersTest3$D2a-meanD2adist)/sdD2adist
      DistancetoCentersTest3$zD2b=(DistancetoCentersTest3$D2b-meanD2bdist)/sdD2bdist
      DistancetoCentersTest3$zD2c=(DistancetoCentersTest3$D2c-meanD2cdist)/sdD2cdist
      DistancetoCentersTest3$zD2d=(DistancetoCentersTest3$D2d-meanD2ddist)/sdD2ddist
      
      
      ## Calculate z-scores percentiles
      DistancetoCentersTest3$pA1=round(pnorm(DistancetoCentersTest3$zA1)*100,1)
      DistancetoCentersTest3$pA2a=round(pnorm(DistancetoCentersTest3$zA2a)*100,1)
      DistancetoCentersTest3$pA2b=round(pnorm(DistancetoCentersTest3$zA2b)*100,1)
      DistancetoCentersTest3$pB1a=round(pnorm(DistancetoCentersTest3$zB1a)*100,1)
      DistancetoCentersTest3$pB1b=round(pnorm(DistancetoCentersTest3$zB1b)*100,1)
      DistancetoCentersTest3$pC1a=round(pnorm(DistancetoCentersTest3$zC1a)*100,1)
      DistancetoCentersTest3$pC1b=round(pnorm(DistancetoCentersTest3$zC1b)*100,1)
      DistancetoCentersTest3$pD1=round(pnorm(DistancetoCentersTest3$zD1)*100,1)
      DistancetoCentersTest3$pD2a=round(pnorm(DistancetoCentersTest3$zD2a)*100,1)
      DistancetoCentersTest3$pD2b=round(pnorm(DistancetoCentersTest3$zD2b)*100,1)
      DistancetoCentersTest3$pD2c=round(pnorm(DistancetoCentersTest3$zD2c)*100,1)
      DistancetoCentersTest3$pD2d=round(pnorm(DistancetoCentersTest3$zD2d)*100,1)
      
      ## Take only the z-scores
      
      DistancetoCentersTest4=DistancetoCentersTest3[,c(1,2,27:38)]
      ## Change numeric columns to 1dp
      is.num <- sapply(DistancetoCentersTest4, is.numeric)
      DistancetoCentersTest4[is.num] <- lapply(DistancetoCentersTest4[is.num], round, 1)
      
      ## Update column names
      colnames(DistancetoCentersTest4)=c("Sample","FaunalCluster","A1","A2a","A2b","B1a","B1b","C1a","C1b","D1","D2a","D2b","D2c","D2d")
      
      
      ## Delete irrelevant values
      DistancetoCentersTest4$A1[DistancetoCentersTest4$FaunalCluster !=  "A1"] <- NA
      DistancetoCentersTest4$A2a[DistancetoCentersTest4$FaunalCluster !=  "A2a"] <- NA
      DistancetoCentersTest4$A2b[DistancetoCentersTest4$FaunalCluster !=  "A2b"] <- NA
      DistancetoCentersTest4$B1a[DistancetoCentersTest4$FaunalCluster !=  "B1a"] <- NA
      DistancetoCentersTest4$B1b[DistancetoCentersTest4$FaunalCluster !=  "B1b"] <- NA
      DistancetoCentersTest4$C1a[DistancetoCentersTest4$FaunalCluster !=  "C1a"] <- NA
      DistancetoCentersTest4$C1b[DistancetoCentersTest4$FaunalCluster !=  "C1b"] <- NA
      DistancetoCentersTest4$D1[DistancetoCentersTest4$FaunalCluster !=  "D1"] <- NA
      DistancetoCentersTest4$D2a[DistancetoCentersTest4$FaunalCluster !=  "D2a"] <- NA
      DistancetoCentersTest4$D2b[DistancetoCentersTest4$FaunalCluster !=  "D2b"] <- NA
      DistancetoCentersTest4$D2c[DistancetoCentersTest4$FaunalCluster !=  "D2c"] <- NA
      DistancetoCentersTest4$D2d[DistancetoCentersTest4$FaunalCluster !=  "D2d"] <- NA
      
      DistancetoCentersTest4
      #View(DistancetoCentersTest3)
      #class(DistancetoCentersTest3)
    } else {
      
      
      #pos.test=data()[,1:3]
      
      ## Change labels for Lat and Long
      #names(pos.test)[2]<-paste("Lat")
      #names(pos.test)[3]<-paste("Long")
      
      #pos.test
    }
  })
  
  #############################################################################
  ##############################################################################
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("download",".csv",sep="")#data2-",Sys.Date(),
    },
    content = function(file) {
      ######
      #### PREPARE TEST DATA ####
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
      
      ## Add cluster group from kmeans results file to df 'pos' which includes 'Sample',
      # 'Latitude_WGS84' and 'Longitude_WGS84'
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
      #str(faunal.cluster.test)
      faunal.cluster.test$Fauna=as.factor(faunal.cluster.test$Fauna)
      
      ## Concatenate Faunal and Physical cluster
      faunal.cluster.test$PhyFauna=paste(faunal.cluster.test$Phy,faunal.cluster.test$Fauna,sep="_")
      
      write.csv(faunal.cluster.test[,c(1,2,3,5,6,7)],file,row.names = F)
    })
  
  
  
  
}






shinyApp(ui, server)

