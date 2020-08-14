#PCA and tSNE

x = rnorm(100)
y = rnorm(100,x)
pca=prcomp(cbind(x,y))
names(pca)
#[1] "sdev"     "rotation" "center"   "scale"    "x"
#sdev- sd of Principal Components
#rotation - the matrix of variable loadings (i.e., a matrix whose columns contain the eigenvectors).
#The function princomp returns this in the element loadings.
#center T or F, whether variables should be shifted to be zero centered;
#scale T or F, scaling of the variables. By default is F, but advisable since will help to treat outliers;
#x - the matrix of every point with location on PC.
plot(x,y,pch=19,col='gray',xlim=range(x,y),ylim=range(x,y),main='Original coordinates')
points(pca$center[1],pca$center[2],cex=2)
b1 = pca$rotation[2,1]/pca$rotation[1,1]
abline(a=pca$center[2]-b1*pca$center[1],b=b1,col='red')
b2 = pca$rotation[2,2]/pca$rotation[1,2]
abline(a=pca$center[2]-b2*pca$center[1],b=b2,col='blue')
dev = round(pca$sdev/sum(pca$sdev)*100,1)
plot(-pca$x[,1],-pca$x[,2],pch=19,col='gray',main='PCA',xlab=
       paste('PC1(',dev[1],'%)',sep=''),ylab=paste('PC2 (',dev[2],'%)',sep=''),
     xlim=range(x,pca$x,y),ylim=range(x,y,pca$x))
abline(a=0,b=0,col='red')
abline(v=0,col='blue')
#NB! If you have bad variance explanation by PCA, try scale and center data. If you have undetermined clouds of data, use PCA-> t-SNE.
#If your data is shrinked through one of components- it's bad, because algorithm failed to find normal projection


set.seed(1)
N = 5000; D = 100; data.norm = matrix(rnorm(N * D, 2), N); groups.probs = runif(10)
groups = sample(1:10, N, TRUE, groups.probs/sum(groups.probs))
for (gp in unique(groups)) {dev = rep(1, D); dev[sample.int(D, 3)] = runif(3, -10, 10)
data.norm[which(groups == gp), ] = data.norm[which(groups ==
                                                     gp), ] %*% diag(dev)
}
info.norm = tibble(truth = factor(groups))


#PCA
pca.norm = prcomp(data.norm)
info.norm %<>% cbind(pca.norm$x[, 1:4])
ggplot(info.norm, aes(x = PC1, y = PC2, colour = truth)) +
  geom_point(alpha = 0.3) + theme_bw()

#TSNE
library(Rtsne)
tsne.norm = Rtsne(pca.norm$x, pca = FALSE)
info.norm %<>% mutate(tsne1 = tsne.norm$Y[, 1], tsne2 = tsne.norm$Y[,
                                                                    2])
ggplot(info.norm, aes(x = tsne1, y = tsne2, colour = truth)) +
  geom_point(alpha = 0.3) + theme_bw()

##############
####Clustering
##############
x = rnorm(100)
y = rnorm(100,x)
d=dist(cbind(x,y),method='euclidean')
h=hclust(d,method='complete')
plot(h)
t=2
abline(h=t,col='red')
cl = cutree(h,h=t)
plot(x,y,col=cl,pch=19,main='by level')
cl=cutree(h,k=4)
plot(x,y,col=cl,pch=19,main='by cluster count')
#Silhouette
library(cluster)
cl=pam(d,k=4) # Partitioning (clustering) of the data into k clusters “around medoids”, a more robust version of K-means.
slh =silhouette(cl$clustering,d) #Compute silhoutte
plot(slh)

###########
#Heatmaps

#Transfrom mtcars to matrix
data <- as.matrix(mtcars)
#Default Heatmap
heatmap(data)
heatmap(data, scale="column")

#Alternatively, you can use heatmap.2 function to adjust colors:
library(gplots)
heatmap.2(data,col='bluered',scale="column")

###########
###
###GGPLOT2
###
############

#GGPLOTS2. What is it and why is it needed at all.
#1. It is difficult in comparison with base plotics.
#2. Impressing functionality even without  additional packages.
#3. A bunch of extra packages for beauty.
#4. The output is just the very notorious plotics that can be sent without Nature to Photoshop, the data would be beautiful.

library (ggplot2)
ggplot (diamonds) + geom_point (aes (x = carat, y = price, color = cut)) + geom_smooth (aes (x = carat, y = price, color = cut))
ggplot (diamonds,aes (x=price, color = cut))+geom_histogram () #add histograms to the chart
ggplot (diamonds,aes (x=price, color = cut))+geom_boxplot () #add boxplot


#SYNTAX, and why is everything so confusing.

____________________________________________________________

data ("midwest", package = "ggplot2") # load the date.
head (midwest)
class (midwest)
tbl - the tibbles class, which is very convenient for working inside the console, because it duplicates the R-studio functionality about variables inside the object
# Let's create, in fact, a plot
ggplot (midwest, aes (x = area, y = poptotal)) #
#First we add data, then in the aes variable we indicate what, in fact, we want from the plot. That is, we limit the x and y axes
#The plot is empty because ggplot is waiting for you to specify.
#This must be indicated through additional functions, which are, in fact, layers and begin with geom_

ggplot (midwest, aes (x = area, y = poptotal)) + geom_point () # add points.

#ggplot2 does not know how to par (mfrow = c (*, *))!
ggplot (midwest, aes (x = area, y = poptotal)) + geom_point ()
plot (midwest$area, midwest$poptotal)
#To do this, put a separate library that knows how to handle it.
library (gridExtra)
plot1 = ggplot (midwest, aes (x = area, y = poptotal)) + geom_point ()
grid.arrange (plot1, plot1, ncol = 2)


#You can add as many layers to the plot as you like:
ggplot (midwest, aes (x = area, y = poptotal)) + geom_point () + geom_smooth (method = "lm")
lm = predict (lm (area ~ poptotal, data = midwest))
ggplot (midwest, aes (x = area, y = poptotal)) + geom_point () +
  geom_smooth (method = "loess") +
  geom_line () +
  geom_line (aes (y = lm))
#____________________________________________________________
#SCALING

#What if the chart has a poor scale?
#1. You can limit the range of x and y axes.
g <- ggplot (midwest, aes (x = area, y = poptotal)) + geom_point () + geom_smooth (method = "lm")
#An additional function, xlim and ylim, say that the boundaries of the plot
g + xlim (c (0, 0.1)) + ylim (c (0, 1,000,000))
#2. Zoom in the chart, saving all points using coord_cartesian
g1 <- g + coord_cartesian (xlim = c (0,0.1), ylim = c (0, 1000000)) # zooms in
plot (g1)

#NB! Note that the predicted curve in step 1 is different from curve 2.
#This is due to the fact that in the first case, we just throw out the data,
#after that we build straight, in the second case we simply limit the scope of tasks

#3. You can change the number of partitions and elevations on the axis using the scale_x_continuous function
g + scale_x_continuous (breaks = seq (0, 0.1, 0.01))
g + scale_x_continuous (breaks = seq (0, 0.1, 0.01), labels = letters [1:11]) +
  scale_y_continuous (labels = letters [1: 6])

#____________________________________________________________
#AXIS
g <- ggplot (midwest, aes (x = area, y = poptotal)) + geom_point () + geom_smooth (method = "lm") #
#There are 2 ways too: labs () and ggtitle ()
g1 <- g + coord_cartesian (xlim = c (0,0.1), ylim = c (0, 1000000))
g1 + labs (title = "Area Vs Population", subtitle = "From midwest dataset",
           y = "Population", x = "Area", caption = "Midwest Demoplotics")

g1 + ggtitle ("Area Vs Population", subtitle = "From midwest dataset") +
  xlab ("Area") + ylab ("Population")


ggplot (midwest, aes (x = area, y = poptotal)) +
  labs (title = "Let's change this title!") +
  labs (title = 'I doubt that') # you can modify the layers any number of identical functions, but the last one will prevail


#NB !!! Do not forget that g1 is an object. And you can handle it as an object.
class (g1)
attributes (g1)
labels = list ('Car', 'Dog')
names (labels) = c ('x', 'y')
g1$labels = labels


#AND ALL TOGETHER.
ggplot (midwest, aes (x = area, y = poptotal)) +
  geom_point () +
  geom_smooth (method = "lm") +
  coord_cartesian (xlim = c (0,0.1), ylim = c (0, 1,000,000)) +
  labs (title = "Area Vs Population", subtitle = "From midwest dataset",
        y = "Population", x = "Area", caption = "Midwest Demoplotics")


____________________________________________________________
#CUSTOM plot

#Change color and size.

library (ggplot2)
ggplot (midwest, aes (x = area, y = poptotal)) +
  geom_point (col = "steelblue", size = 3) + #  parameter col and size
  geom_smooth (method = "lm", col = "firebrick") + #  parameter col
  coord_cartesian (xlim = c (0, 0.1), ylim = c (0, 1,000,000)) +
  labs (title = "Area Vs Population", subtitle = "From midwest dataset",
        y = "Population", x = "Area", caption = "Midwest Demoplotics")


#You can make the data display "three-dimensional" and make the point color reflect the state, but for this the dependency column MUST BE A FACTOR!
gg = ggplot (midwest, aes (x = area, y = poptotal)) +
  geom_point (aes (col = state), size = 3) + # Now the color of the picture will reflect the state.
  geom_smooth (method = "lm", col = "firebrick", size = 2) +
  coord_cartesian (xlim = c (0, 0.1), ylim = c (0, 1,000,000)) +
  labs (title = "Area Vs Population", subtitle = "From midwest dataset", y = "Population", x = "Area", caption = "Midwest Demoplotics") # add custom axes


#In addition to the default col, you can specify the size, shape and fill.
#A legend is created automatically, if we do not want a legend, we must say this
gg + theme (legend.position = "None")


#If you need to change the colors for the legend, you can do this with one of the sets:
gg + scale_colour_brewer (palette = "Set1")
gg + scale_colour_brewer (palette = "Set2")
gg + scale_colour_brewer (palette = "Greens")

#Change the theme of the plot globally
gg
gg + theme_bw () + labs (subtitle = "BW Theme")
gg + theme_bw ()
gg + theme_classic () + labs (subtitle = "Classic Theme")


____________________________________________________________
#PNG in the background.

#What for?
packs <- c ("png", "grid", 'gridExtra')
lapply (packs, require, character.only = TRUE)

#Upload our image
img <- readPNG ("Why.png")
img <- rasterGrob (img, interpolate = TRUE)
g <- ggplot (midwest, aes (x = area, y = poptotal)) +
  annotation_custom (img, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  geom_point () + geom_smooth (method = "lm")

#NB! Pay attention to the order of the layers:

g1 = ggplot (midwest, aes (x = area, y = poptotal)) +
  geom_point (col = 'red') + geom_smooth (method = "lm") + # chart first
  annotation_custom (img, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) # now picture

g2 = ggplot (midwest, aes (x = area, y = poptotal)) +
  annotation_custom (img, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) + # first picture
  geom_point (col = 'red') + geom_smooth (method = "lm") # now the plot

grid.arrange (g1, g2, ncol = 2)
____________________________________________________________

gg <- ggplot (midwest, aes (x = area, y = poptotal)) +
  geom_point (aes (col = state, size = popdensity)) +
  geom_smooth (method = "loess", se = F) + xlim (c (0, 0.1)) + ylim (c (0, 500000)) +
  labs (title = "Area Vs Population", y = "Population", x = "Area", caption = "Source: midwest")

#THEME

#vjust the vertical distance between the text and the plot
#hjust the horizontal distance between the text and the plot
#family font name
#face font type

gg + theme (plot.title = element_text (size = 20,
                                       face = "bold",
                                       family = "American Typewriter",
                                       color = "tomato",
                                       hjust = 0.5,
                                       lineheight = 1.2), plot.subtitle = element_text (size = 15,
                                                                                        family = "American Typewriter",
                                                                                        face = "bold",
                                                                                        hjust = 0.5),
            plot.caption = element_text (size = 15),
            axis.title.x = element_text (vjust = 10,
                                         size = 15),
            axis.title.y = element_text (size = 15),
            axis.text.x = element_text (size = 10,
                                        angle = 30,
                                        vjust = .5),
            axis.text.y = element_text (size = 10))

#  ________________________________________________
#  MUCH MORE ABOUT LEGENDS

#  You can change the signatures on our legend like this:
#  Please note that the colors of lened are determined by the unique values ​​of this column in data.frame, which we submitted to the input:
gg + scale_color_manual (name = "State",
                         labels = c ("Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin"), # change signatures
                         values ​​= c ("IL" = "blue", "IN" = "red", "MI" = "green", "OH" = "brown", "WI" = "orange")) # change the colors

#  You can change the sequence of legends in the figure like this:
gg + guides (color = guide_legend (order = 2),
             size = guide_legend (order = 1))
#  theme () defines almost everything. You can adjust the legend for yourself.
gg + theme (legend.title = element_text (size = 12, color = "firebrick"),
            legend.text = element_text (size = 10),
            legend.key = element_rect (fill = 'springgreen')) +
  guides (color = guide_legend (override.aes ​​= list (size = 2, stroke = 1.5)))
#  You can remove it:
gg + theme (legend.position = "None") + labs (subtitle = "No Legend")
#  You can move left:
gg + theme (legend.position = "left") + labs (subtitle = "Legend on the Left")
#  Or under the schedule:
gg + theme (legend.position = "bottom", legend.box = "horizontal") + labs (subtitle = "Legend at Bottom")
#  Or even inside the chart:
gg + theme (legend.title = element_text (size = 12, color = "salmon", face = "bold"),
            legend.justification = c (1,0),
            legend.position = c (0.95, 0.05),
            legend.background = element_blank (),
            legend.key = element_blank ()) +
  labs (subtitle = "Legend: Bottom-Right Inside the Plot")


#  ________________________________________________


#ANNOTATION AND SIGNATURES ON PLOT
#Add text to the plot. Lets create a date frame that will contain only data for counties with a population> 300k
midwest_sub <- midwest [midwest $ poptotal> 300000,]
midwest_sub $ large_county <- ifelse (midwest_sub $ poptotal> 300000, midwest_sub $ county, "")

#  Build the chart:
gg <- ggplot (midwest, aes (x = area, y = poptotal)) +
  geom_point (aes (col = state, size = popdensity)) +
  geom_smooth (method = "loess", se = F) + xlim (c (0, 0.1)) + ylim (c (0, 500000)) +
  labs (title = "Area Vs Population", y = "Population", x = "Area", caption = "Source: midwest")

#  And add the text.
gg + geom_text (aes (label = large_county), size = 2, data = midwest_sub) +
  labs (subtitle = "With ggplot2 :: geom_text") + theme (legend.position = "None") # text

library (ggrepel)
gg + geom_text_repel (aes (label = large_county), size = 2, data = midwest_sub, force = 10) +
  labs (subtitle = "With ggrepel :: geom_text_repel") + theme (legend.position = "None") #force add "takeaway" signatures
#  Thats ALL!

######################
USEFUL functions
Once again succinctly:
  ggplot () + layers. Layered individual functions that accept either their date or, default, the date given in ggplot ().
The layer entered later determines the type of chart and cancels the “previous” assignment.
Pictures "overlap" each other in the order in which you registered them.

ggplot () creates an activity field
aes () assign a range of x and y values. In addition, you can add additional parameters. Such as color and size
geom_point () adds points to the chart
geom_smooth () adds curves to the plot
geom_hist () add histograms to the chart
geom_line () add lines to the chart
geom_boxplot () add boxplot
geom_bar () add barplot
geom_density () add a plot of the probability density distribution of the sample
xlim () ylim () coord_cartesian () limit the axis limit.
NB! xlim () ylim () ejects data from the drawing, which often entails undesirable things, while coord_cartesian () honestly limits the area of ​​construction.
scale_x_continuous () scale_y_continuous () axis risk manipulation
labs () ggtitle () xlab () ylab () text on axes. labs () duplicates ggtitle () xlab () ylab () functions
theme_bw () theme_classic () - change plot themes
scale_colour_brewer () change, for example, the color of the legend
scale_color_manual () change the color of the legend
theme () allows you to control most of the variables. A kind of analog par
geom_text () add custom text to the plot.
USEFUL LIBRARIES
grid.arrange () multiple plots in one space
grid () similarly, allows you to place multiple plots in one space
ggrepel () an optional package for signatures in the figure
