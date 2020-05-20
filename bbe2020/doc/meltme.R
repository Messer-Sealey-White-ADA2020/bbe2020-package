## ----setup, warning=FALSE, message=FALSE--------------------------------------
library(readr) # to enable tidying of data
library(dplyr) #""
library(tibble) #""
library(stats) #""
library(reshape2) # to use with melt function to convert matrix to objects igraph can plot
library(igraph) # to make an igraph object ie. network visualization of matrix values
library(visNetwork) # to make your igraph object prettier using visNetwork
library(bbe2020) # adds functionality to melt by converting a matrix generated in other software or elsewhere and converting to a form easily used by igraph

## ---- message=FALSE-----------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
# read in data matrix for aerial treatment
aerial <- read_csv("blacktreatments_heads2020f.csv", col_names=TRUE)
# store the first column, which should be "names" for later use
aerialnames <- as.character(aerial$names)
# read in data matrix for tactile treatment 
tactile <- read_csv("tactiletreatments_heads2020f.csv", col_names=TRUE)
# store the first column, which should be "names" for later use
tactilenames <- as.character(tactile$names)
## Please Note: you must ensure that your dataframe has a first column named "names" that refers to the labels for the unique row and column data points 

## -----------------------------------------------------------------------------
# melt_matrix_4_ig_visV <- function(x){
#   colnames(x) <- c("names", 1:nrow(x))
#   x <- select(x, -names) %>%
#   as.matrix()
#   x[lower.tri(x, diag=TRUE)] <- NA
#   x <- melt(x)
#   x <- setNames(x, c('ind1', 'ind2', 'values'))
#   x <- filter(x, !is.na(values))
#   return(x)
# }
#meltaerial <- melt_matrix_4_ig_visV(aerial)
# melting data matrix for aerial treatment
#melttactile <- melt_matrix_4_ig_visV(tactile)

## -----------------------------------------------------------------------------
#Using "melteme" function, designed to melt the matrices
meltaerial <- meltme(aerial)
head(meltaerial, 5)
# melting data matirx for tactile treatment 
melttactile <- meltme(tactile)
#melttactile <- melt_matrix_4_ig_visV(tactile)
head(melttactile, 5)

## -----------------------------------------------------------------------------
# iGraph network plot of aerial treatment
#aerialplot <- graph_from_data_frame(meltaerial, directed = FALSE, vertices = NULL)
aerialplot <- igraphme(meltaerial)
E(aerialplot)$weight <- meltaerial$values
# print iGraph using a sphere layout 
plot(aerialplot, vertex.label = NA, layout = layout_on_sphere, main="Aerial treatment")

# iGraph network plot of tactile treatment
#tactileplot <- graph_from_data_frame(melttactile, directed = FALSE, vertices = NULL)
tactileplot <- igraphme(melttactile)
E(tactileplot)$weight <- melttactile$values
# print iGraph using a force-directed layout (positions the nodes so that the edges have similar lengths and there are as few crossing edges as possible)
plot(tactileplot, vertex.label = NA, layout = layout_with_fr, main="Tactile treatment")
# converting igraph to visnetwork for aerial treatment
aerialdata <- toVisNetworkData(aerialplot)
## copy column "weight" to new column "value" in list "edges"
aerialdata$edges$width <- 20*aerialdata$edges$value 
## line color
aerialdata$edges$color <- "gray" 
## node labels
##refer to "aerialnames" to do this
aerialdata$nodes$label <- aerialnames
## print plot, zoom in for detail!
visNetwork(nodes = aerialdata$nodes, edges = aerialdata$edges, height = "500px") %>%
visIgraphLayout(layout = "layout_with_sugiyama")

# converting igraph to visnetwork for tactile treatment
tactiledata <- toVisNetworkData(tactileplot)
## copy column "weight" to new column "value" in list "edges"
tactiledata$edges$width <- 20*tactiledata$edges$value 
## line color
tactiledata$edges$color <- "gray" 
## print plot
visNetwork(nodes = tactiledata$nodes, edges = tactiledata$edges, height = "500px") %>%
visIgraphLayout(layout = "layout_with_sugiyama")

## -----------------------------------------------------------------------------
# aerial treatment 
centralityaerialplot <- evcent(aerialplot)$vector
centralityaerialplot
# tactile treatment 
centralitytactileplot <- evcent(tactileplot)$vector
centralitytactileplot

## -----------------------------------------------------------------------------
# First, we need to combine our centrality measures for both the aerial and tactical treatment conditions into one dataframe
# names and centraility score for aerial treatment condition
Acent <- bind_cols(name = aerialnames, number = centralityaerialplot)
# names and centriality score for tactile treatment condition
Tcent <- bind_cols(name = tactilenames, number = centralitytactileplot)
# naming the different centrality measures for each treatment condition
acondition = 'aerial'
tcondition = 'tactile'
Acent <- mutate(Acent, "condition" = paste(acondition))
Acent
Tcent <- mutate(Tcent, "condition" = paste(tcondition))
Tcent
# Joining aerial and tactile treatments with their centralities and snakenames into one tibble
snake_centrality <- full_join(Tcent, Acent, by = "name")
snake_centrality
snake_centrality <- as.data.frame(snake_centrality)
head(snake_centrality, 5)
## Re-naming the columns after cmbining the columns 
snake_centrality <- snake_centrality %>%
  rename(
    centrality_t = number.x,
    centrality_a = number.y
  )
head(snake_centrality, 5)
## running the wilcoxon analysis on our updated dataframe to compare snake centrality in the tactile and aerial treatment conditions  
wilcox_centrality <- wilcox.test(snake_centrality$centrality_t, snake_centrality$centrality_a, paired=TRUE)
wilcox_centrality

## -----------------------------------------------------------------------------
# Read in another dataset, here we use monkeys 
Monkeys <- read_csv("EM_monkey.csv", col_names=TRUE)
Sname <- c(Monkeys$name)
Sno <- as.numeric(1:nrow(Monkeys))
Monkeys <- meltme(Monkeys)
#Monkeys <- melt_matrix_4_ig_visV(Monkeys)
head(Monkeys, 5)
# This is how you can replace numbers with names 
Source <- bind_cols(name = Sname, number = Sno)
Source
final <- inner_join(Monkeys, Source, by = c("ind1" = "number"))
final <- inner_join(final, Source, by = c("ind2" = "number"))
final <- select(final, name.x, name.y, values)
head(final, 5)
# Visualization of Monkey interactions 
Monkeysp <- graph_from_data_frame(Monkeys, directed = FALSE, vertices = NULL)
plot(aerialplot, vertex.label = NA, layout = layout_with_fr, main="Aerial treatment") 
datam <- toVisNetworkData(Monkeysp)
visNetwork(nodes = datam$nodes, edges = datam$edges, height = "500px") %>%
visIgraphLayout(layout = "layout_with_sugiyama")
# New monkey graph to work out the centrality of monkeys. 
# igraph
monk = graph.data.frame(final[,c('name.x','name.y')], directed = FALSE, vertices = NULL)
E(monk)$weight = final$values
plot(monk, vertex.label = NA, layout = layout_with_fr, main="Monkeys") 
# working out centrality 
centrality <- evcent(monk)$vector
head(centrality) 

