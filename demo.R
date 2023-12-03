CURRENT_WORKING_DIR <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(CURRENT_WORKING_DIR)
getwd()


install.packages("igraph")
install.packages("readxl")

library(igraph)
library(readxl)

edge <- read_excel("survey_data.xlsx")
node <- read_excel("school_nodes.xlsx")

network_data <- graph_from_data_frame(edge)
network_data
plot(network_data)

network_data2 <- graph_from_data_frame(d = edge, vertices = node)
plot(network_data2)

### Adjusting the size of nodes and arrows
plot(network_data2, 
     vertex.size=5,
     edge.arrow.size=0.3)

### Changing label size and font
windowsFonts()
plot(network_data2,
     vertex.size=5, 
     edge.arrow.size=0.4, 
     vertex.label.cex=0.6,
     vertex.label.family="sans")

### Changing colors according to node attributes
node_colors <- ifelse(V(network_data2)$out_school == "out", "lightblue", "tomato")
plot(network_data2,
     vertex.size=5, 
     edge.arrow.size=.4, 
     vertex.label.cex=0.6,
     vertex.label.family="sans",
     vertex.color=node_colors)

### Removing node borders and changing label colors
plot(network_data2,
     vertex.size=5, 
     edge.arrow.size=0.4, 
     vertex.label.cex=0.6,
     vertex.label.family="sans",
     vertex.color=node_colors,
     vertex.frame.color=NA,
     vertex.label.color="black")

### Changing shapes according to node attributes
node_shapes <- ifelse(V(network_data2)$position == "leader", "square", "circle")
plot(network_data2,
     vertex.size=5, 
     edge.arrow.size=0.4, 
     vertex.label.cex=0.6,
     vertex.label.family="sans",
     vertex.color=node_colors,
     vertex.frame.color=NA,
     vertex.label.color="black",
     vertex.shape=node_shapes)

### Fixing visualization and adding legends
set.seed(1115)
plot(network_data2,
     vertex.size=5, 
     edge.arrow.size=0.3, 
     vertex.label.cex=0.6,
     vertex.label.family="sans",
     vertex.color=node_colors,
     vertex.frame.color=NA,
     vertex.label.color="black",
     vertex.shape=node_shapes,
     main="Network Visualization",
     #vertex.label = NA
     #layout=layout_with_kk,
     #vertex.label.dist=1
     )

legend("topright", 
       legend=c("In School", "Out of School", "", "Teacher", "Formal Leader"),
       col=c("tomato", "lightblue", NA, "black", "black"),
       pch=c(21, 21, NA, 21, 22),
       pt.bg=c("tomato", "lightblue", NA, NA, NA),
       pt.cex=2,
       cex=0.8,
       bty="n")


### Calculating density and centrality

edge_density(network_data2)

degree(network_data2, normalized = TRUE)
degree(network_data2, mode="in", normalized = TRUE)
degree(network_data2, mode="out", normalized = TRUE)
closeness(network_data2, mode = "all")
betweenness(network_data2)

names(degree(network_data2))[which(degree(network_data2) == max(degree(network_data2)))]
names(betweenness(network_data2))[which(betweenness(network_data2) == max(betweenness(network_data2)))]
names(closeness(network_data2, mode="all"))[which(closeness(network_data2, mode="all") == max(closeness(network_data2, mode="all"), na.rm = TRUE))]


Degree <- degree(network_data2, normalized = TRUE)
InDegree <- degree(network_data2, mode="in", normalized = TRUE)
OutDgree <- degree(network_data2, mode="out", normalized = TRUE)
Closeness <- closeness(network_data2, mode = "all")
Betweenness <- betweenness(network_data2)

centralities <- cbind(Degree, InDegree, OutDgree, Closeness, Betweenness)
write.csv(centralities, file="centralities.csv")


### Reference Material 
# R igraph Manual Page: https://igraph.org/r/html/latest/
# Other R network analysis packages: network, sna, statnet, tidygraph...
# Visualization: ggplot2, ggraph, ggnet2, ggnetwork...

