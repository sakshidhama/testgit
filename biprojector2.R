library(igraph)
library(reshape2)
ov<-read.csv('comm.txt', header = F, sep = "\t")
incidence_matrix<-table(melt(ov, id.var="V1")[-2])
g<-graph_from_incidence_matrix(incidence_matrix)
#
# create a bipartite graph from g
V(g)$type<-bipartite.mapping(g)$type
#
#now we have it as a bipartite graph,
#so let's do the projection:
gp<-bipartite.projection(g, which=F)
# note that E(gp)$weight contains the weight of the edges
#
# create the complementer graph:
cg<-graph.complementer(gp)
#
# optional: get the edgelist as a data frame:
as.matrix.data.frame(get.edgelist(cg))
#plot(gp)
#plot(g)
gp$deg<-degree(gp)
# define color and shape mappings.
col <- c("steelblue", "orange")
shape <- c("circle", "square")
l<-layout.bipartite(g)
#plot(g, layout = l[, c(2,1)],vertex.color = col[as.numeric(V(g)$type)+1],
 #    vertex.shape = shape[as.numeric(V(g)$type)+1])
#plot(gp)
components_list<-components(gp)
dg <- decompose.graph(gp)
#plot(graph.complementer(dg[[1]]))
#Creating a list that stores the maximum elements from the graph component
print(which.max(V(dg[[1]])))
seed_list<-list()
graph_list<-list()#stores the comliment of the component
#Step:1 Collecting the nodes with highest degree from each component
for(i in 1:length(dg))
{
  print(paste0("componenet",i))
  print(dg[i])
  graph_list[[i]]<-graph.complementer(dg[[i]])
  # plot(graph.complementer(dg[[i]]))
  seed_list<-append(seed_list,names(which.max(degree(dg[[i]]))))
 #seed_list[[i]][2]<-which.max(degree(dg[[i]]))
}
print("printing here the real stuff")
print(which.max(degree(dg[[1]])))
print("max degree element")
print("seed_list is ")
print(seed_list)
#print(graph_list[[1]])
#Step 2: Taking the compliment of the components and selecting the useful components based the criteria 
#        that the initial selection of seed nodes results in seed node as  a component in itself
#        Useful Components:Complemented Components where the first selected seed nodes have degree 1 or greater than 1
print("allnodes of component 1 ")
#print(V(graph_list[[1]]))
print(degree(graph_list[[3]]))
i<-1
seed_comp<-list()
sorted_comp_list<-list()
#seed_comp<-99
print("degree of seednodes list1")
#print(typeof(seed_list[[3]]))
x<-degree(graph_list[[3]],v=seed_list[[3]])
print(x[[1]])
k<-1
for (j in 1:length(seed_list)) {
  x<-degree(graph_list[[j]],v=seed_list[[j]])
  if(x[[1]]!=0)#Step 3 choosing only those componenets which belong to
  {
    #print(names(which.max(degree(graph_list[[j]]))))
  #  print(paste0("neighbors of ",seed_list[[j]]))
    #print(neighbors(dg[[j]], seed_list[[j]], mode = "total"))
    print(paste0("neigborhood of ",seed_list[[j]]))
    print(ego(dg[[j]],10, nodes =seed_list[[j]], mode = c("all", "out", "in"),
        mindist = 2))
    seed_comp<-append(seed_comp,names(which.max(degree(graph_list[[j]]))))
    sorted_comp_list[[k]]<-order(degree(graph_list[[j]]),decreasing = FALSE)
    #sorted_comp_list[[k]]<-order(degree(ego(dg[[j]],10, nodes =seed_list[[j]], mode = c("all", "out", "in"),
   #                                         mindist = 2)),decreasing = FALSE)
    k<-k+1
  }
  }
print("printing seed comp after step 2:")
print(seed_comp)
print("order of sorted degree list")
print(sorted_comp_list)