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
plot(network_data)
network_data

network_data2 <- graph_from_data_frame(d = edge, vertices = node)
plot(network_data2)

### 노드와 화살표 사이즈 조절
plot(network_data2, 
     vertex.size=5,
     edge.arrow.size=0.3)

### 라벨 사이즈와 폰트 변경
windowsFonts()
plot(network_data2,
     vertex.size=5, 
     edge.arrow.size=0.4, 
     vertex.label.cex=0.6,
     vertex.label.family="sans")

### 노드 속성에 따른 색상 변경
node_colors <- ifelse(V(network_data2)$out_school == "out", "lightblue", "tomato")
plot(network_data2,
     vertex.size=5, 
     edge.arrow.size=.4, 
     vertex.label.cex=0.6,
     vertex.label.family="sans",
     vertex.color=node_colors)

### 노드 테두리 삭제 및 라벨 색 변경
plot(network_data2,
     vertex.size=5, 
     edge.arrow.size=0.4, 
     vertex.label.cex=0.6,
     vertex.label.family="sans",
     vertex.color=node_colors,
     vertex.frame.color=NA,
     vertex.label.color="black")

### 노드 속성에 따른 모양 변경
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

### 시각화 고정과 범례 추가
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



### community detection
wc <- cluster_walktrap(network_data2)
plot(wc, network_data2)
plot(wc,
     network_data2,
     vertex.size=5, 
     edge.arrow.size=.5, 
     vertex.label.cex=0.7,
     vertex.label.family="sans",
     vertex.frame.color=NA,
     vertex.label.color="black",
     vertex.shape=node_shapes,
     main="Community Detection")


### 밀도 및 중심성 산출

edge_density(network_data2)

degree(network_data2)
degree(network_data2, mode="in")
degree(network_data2, mode="out")
closeness(network_data2, mode = "all")
betweenness(network_data2)

max(degree(network_data2))
names(degree(network_data2))[which(degree(network_data2) == max(degree(network_data2)))]
names(betweenness(network_data2))[which(betweenness(network_data2) == max(betweenness(network_data2)))]
names(closeness(network_data2, mode="all"))[which(closeness(network_data2, mode="all") == max(closeness(network_data2, mode="all"), na.rm = TRUE))]


Degree <- degree(network_data2)
InDegree <- degree(network_data2, mode="in")
OutDgree <- degree(network_data2, mode="out")
Closeness <- closeness(network_data2, mode = "all")
Betweenness <- betweenness(network_data2)

centralities <- cbind(Degree, InDegree, OutDgree, Closeness, Betweenness)
write.csv(centralities, file="centralities.csv")


### 참고자료 
# R igraph Manual Page: https://igraph.org/r/html/latest/
# 다른 R 네트워크 분석 패키지: network, sna, statnet, tidygraph...
# 시각화: ggplot2, ggraph, ggnet2, ggnetwork...













# centr_degree(g)$centralization
# centr_clo(g, mode="all")$centralization
# centr_eigen(g, directed=FALSE)$centralization
# centr_degree(g3)$centralization
# centr_degree(g3,mode="in")$centralization
# centr_degree(g3,mode="out")$centralization
# centralization.betweenness(g3)
# centr_betw(g3, directed = TRUE)$centralization
# centr_clo(g3, mode="all")$centralization