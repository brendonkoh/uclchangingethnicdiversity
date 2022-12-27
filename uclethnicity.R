library(png)
library(plotly)
library(extrafont)

#Read in data
ethnicity.data <- read.csv("Ethnic Group Summarised.csv")

#Load .png file of Jeremy Bentham
img <- readPNG("bentham.png")

stackedbar_image <- function(image, cat_percentages, palette, 
                             show_scale = TRUE, colorbar_title = "", cat_names = NA,
                             width = NULL, height = NULL, font_family = "Arial", 
                             chart_title = "", bottom_title = "", label_color = "black"){
  
  #Rounding to ensure there's only 0 and 1s in the matrix
  img <- round(image, 0)
  
  #Finding height and width of image
  h <- dim(img)[1]
  w <- dim(img)[2]
  
  
  ##---Creating matrix used for the plot---##
  #Finding the rows where white pixels first appear and end (i.e. where feet starts and head ends)
  pos1 <- which(apply(img[,,1], 1, function(y) any(y==1)))
  bottom <- min(pos1)
  top <- max(pos1)
  h.figure <- top-bottom
  
  #Creating a matrix comprising pixels to be coloured
  col.matrix <- img[h:1,,1]
  pixel.matrix <- (col.matrix == 1)
  
  ncats <- length(cat_percentages)
  #Creating a vector storing the y-coordinates of the 'ceiling' of the bars for each category
  ylevels <- vector(mode = "logical", length = ncats)
  base <- bottom
  
  #Creating a vector storing the y-coordinates for where to place the percentage labels
  lab.position <- vector(mode = "logical", length = ncats)
  
  #For loop to recode values in matrix
  for(i in ncats:1){
    ylevels[i] <- round(h.figure*cat_percentages[i]) + base
    lab.position[i] <- (ylevels[i] - base)/2 + base - 2
    
    #Creating a matrix of all FALSE of equal dimension as our original image
    value.matrix <- matrix(rep(FALSE, h*w), nrow = h)
    #Filling all pixels between current ylevel and previous ylevel with a value
    value.matrix[ylevels[i]:base,] <- TRUE
    col.matrix[pixel.matrix & value.matrix] <- (ncats+1-i)/ncats
    
    base <- ylevels[i]
  }
  
  
  ##---Adjusting plot options (cosmetic changes to plot)---##
  
  #Appending 'transparent' to the palette to set the background colour as transparent.
  palette <- append(palette, "transparent", after = 0)
  
  #Function cuts 0-1 into equal divisions.
  colrS <- function(n){
    CUTS <- seq(0,1,length.out=n+1)
    rep(CUTS,ifelse(CUTS %in% 0:1,1,2))
  }
  
  #Creating color data frame
  colorpalette <- data.frame(z = colrS(ncats+1), col = rep(palette, each = 2), stringsAsFactors = F)
  
  #Creating tick positions
  tick.positions <- vector(length = ncats+1)
  nvalues <- length(tick.positions)
  for(i in 1:nvalues){
    tick.positions[i] <- (1/nvalues)*0.5 + (1/nvalues)*(i-1)
  }
  
  #Ensuring tick label for the transparent background colour does not appear on the color bar
  if(show_scale == T){
    cat_names <- rev(cat_names)
    cat_names <- append(cat_names, "", after = 0)
  }
  
  #Customising axis options: ensuring axes do not appear
  yax <- list(ticks="", showticklabels=FALSE, showgrid=FALSE, zeroline=FALSE)
  xax <- list(ticks="", showticklabels=FALSE, showgrid=FALSE, zeroline=FALSE, title = bottom_title)
  
  #Creating labels to be displayed on the chart
  pct.labs <- sapply(cat_percentages, function(x){paste0("<b>",round(x*100,1),"%</b>")})
  
  #Setting height and width of plot as the height and width of the image if not supplied by user
  if(is.null(height)){
    height <- h
  }
  if(is.null(width)){
    width <- w
  }
  
  ##---Creating plot---##
  
  plot_ly(z = col.matrix, 
          showscale = show_scale, 
          type="heatmap", 
          width = width,  
          height = height, 
          colorscale = colorpalette, 
          hoverinfo = "none") %>%
    
    layout(xaxis = xax,                        
           yaxis = yax, 
           title = list(text = chart_title,        #Specifying chart title
                        font = list(size = 15),
                        yanchor = "bottom"),
           margin = list(t = 45, b= 10),           #Adding top margin to plot 
           font = list(family = font_family)) %>%  #Customising font type for all text in the plot
    
    add_text(x = (w/2-20),       #Specifies the x-coordinates of the percentage labels
             y = lab.position,   #Specifies the y-coordinates of the percentage labels
             type = "heatmap", 
             mode = "text", 
             text = pct.labs,    #Specifies the text for the labels as defined above
             textfont = list(color = label_color), #Specifies label colors as defined by user
             showlegend = F, 
             inherit = F, 
             hoverinfo = "none") %>%
    
    colorbar(title = list(text = colorbar_title,
                          font = list(size = 16)),
             tickvals = tick.positions,
             ticktext = cat_names,
             tickfont = list(size = 15),
             ticks = "",
             outlinecolor = "transparent")
}

#Creating plots for 2021-22, 2014-15, and 2007-08
p2021_22 <- stackedbar_image(image = img, 
                             cat_percentages = ethnicity.data$X2021_22,
                             palette = c("#313628", "#F6931E", "#93B9E7", "ivory", "#181247"), 
                             show_scale = F, 
                             font_family = "Roboto Condensed", 
                             label_color = c("white", "black", "black", "black", "white"),
                             width = 690,
                             bottom_title = "<b>2021-22</b>")

p2014_15 <- stackedbar_image(image = img, 
                             cat_percentages = ethnicity.data$X2014_15,
                             palette = c("#313628", "#F6931E", "#93B9E7", "ivory", "#181247"), 
                             show_scale = F, 
                             font_family = "Roboto Condensed", 
                             label_color = c("white", "black", "black", "black", "white"),
                             width = 690,
                             bottom_title = "<b>2014-15</b>")

p2007_08 <- stackedbar_image(image = img, 
                             cat_percentages = ethnicity.data$X2007_08, 
                             palette = c("#313628", "#F6931E", "#93B9E7", "ivory", "#181247"),
                             cat_names = ethnicity.data$Ethnic.Group,
                             show_scale = T, 
                             font_family = "Roboto Condensed", 
                             label_color = c("white", "black", "black", "black", "white"),
                             width = 690,
                             colorbar_title = "<b>Ethnic Group</b>",
                             bottom_title = "<b>2007-08</b>")

#Combining the three plots into one figure
final.plot <- subplot(p2007_08, p2014_15, p2021_22, titleX = TRUE) %>%
  layout(title = list(text = "<b>Proportion of Ethnic Groups in UCL's student population over time</b>",
                      font = list(size = 20)))

final.plot