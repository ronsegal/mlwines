options(rgl.useNULL=TRUE) #fixes a warning message
library(shiny)
library(cluster)
library(HSAUR)
library(rainbow)
library(ggplot2)
library(fpc)

winedata<<-read.csv("data/wine.data", sep=",")
# Wine data from: http://archive.ics.uci.edu/ml/datasets/Wine

# Most of these variable names aren't actually used in the code below
names(winedata)<<- c("Type","Alcohol","Malic", "Ash", "Alcalinity", "Magnesium", "Phenols", "Flavanoids", "Nonflavanoids", "Proanthocyanins", "Colorintensity", "Hue", "Dilution", "Proline")  

wines<<-winedata
wines$Type<-NULL    # Remove the 'Type' variable for 3 growers, i.e. values 1,2, or 3
wines<-scale(wines) # Normalise the scale of the independent variables

# Function generates a ramp of color values from a vector
cRamp <<- function(x){
  if (diff(range(x)) > 0) {
    x <- (x-min(x))/diff(range(x))
  }
  cols <- colorRamp(rainbow(9))(x)
  apply(cols, 1, function(xt)rgb(xt[1], xt[2], xt[3], maxColorValue=255))
}

shinyServer(function(input, output, session) {

  totalClusters <- reactive({
       set.seed(input$seedval) # Get seed value from numeric input
       # Run k-means model
       km <<- kmeans(wines,input$clusters)
       # Run silhouette model to identify number of clusters
       # that achieve best fit as indicated by average width
       dissE <- daisy(wines) 
       dE2   <- dissE^2
       sk2   <- silhouette(km$cl, dE2)
       # Silhouette method is only valid for > 1 clusters
       if (input$clusters > 1) {
        sk2avwidth<<-summary(sk2)$avg.width
       } else {
        sk2avwidth<<-0 # value used to indicate an error
       }
  # Note - discrproj (from fpc) implements linear dimension reduction for classification
  # $proj contains coordinates of the original data points projected onto a 2d space
  # It has the same dimensions as the original with the first two dimensions
  # providing the best separation of the clusters
       
       xprojection<-discrproj(wines, km$cluster)$proj # See note above 
       xprojframe<<-data.frame(xprojection)
       colMin <<- sapply(xprojframe, min, na.rm = TRUE) # Get minimum column values plot positioning
       
       input$clusters # return the number of clusters from slider
  })
  

  output$plot1 <- renderPlot({
    numclusters<-totalClusters()
    par(mar = c(6.1, 5.1, 0, 1))
    
    # Show grower designators if check box ticked
    if (input$growers) { 
    plotgrowers <- geom_text(data=xprojframe, mapping=aes(x=X1, y=X2, label=winedata$Type), size=4,  color="black")
    } else {
    plotgrowers <- NULL}
    
    # Plot silhouette average width (scaled up) with value as label
    s1 <- geom_segment(aes(colMin["X1"],colMin["X2"]-1, xend=sk2avwidth*10, yend=colMin["X2"]-1),color="red",size=8, alpha=0.5)
    s2 <- geom_text(aes(x=sk2avwidth*10,y=colMin["X2"]-1, label=round(sk2avwidth,digits=2),hjust=1))
    s3 <- geom_text(aes(x=sk2avwidth*10,y=colMin["X2"]-1, label="Silhouette Av Width", vjust=-1, hjust=1))
    s4 <- geom_text(aes(x=colMin["X1"],y=colMin["X2"]-0.5, label="Silhouette Not Applicable with 1 Cluster", hjust=0))

# A width of 0 is used to indicate an error, generally when number of clusters < 2
    if (sk2avwidth > 0) { # Silhouette is valid so show
    plotsilhouette <- c(s1,s2,s3)
    } else {
    plotsilhouette <- s4} # Silhouette not valid
    
    ggplot() +
    # Scatterplot of k-means model
    geom_point(data=xprojframe, mapping=aes(x=X1, y=X2, fill=cRamp(km$cluster)),
               size=6, shape=21, color="black") +
    ggtitle("Machine Wine \"Tasting\" Clusters") +
        theme(plot.title = element_text(size=22, face="bold", colour="darkred")) +
      # Remove axes ticks and text
      theme(axis.ticks.x=element_blank(),
            axis.ticks.y=element_blank(),axis.text.x=element_blank(),
            axis.text.y=element_blank(),axis.title.x=element_blank(),
            axis.title.y=element_blank()) +
        # Modify legend
        scale_fill_discrete(name="Clusters", labels=seq(1,numclusters,1)) +
            plotgrowers +
                plotsilhouette
                
  })
  
  
}

)
 