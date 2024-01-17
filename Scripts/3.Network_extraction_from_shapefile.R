#install.packages("sf", repos = "https://r-spatial.r-universe.dev")
#remotes::install_github("r-spatial/sf")#need to install this sf version to avoid error when trying to load .shp
#remotes::install_github("luukvdmeer/sfnetworks") #for latest version with bug fixes to_spatial_contacted

library(sf) 
library(dplyr) 
library(sfheaders)
library(plotly)
library(igraph)
library(sfnetworks)
library(tidygraph)
library(tidyverse)
library(dbscan)
library(plyr)
library(ggplot2)
library(shp2graph)
library(sp)
library(readxl)
library(openxlsx)

# # sessionInfo()
# # R version 4.1.0 (2021-05-18)
# # Platform: x86_64-w64-mingw32/x64 (64-bit)
# # Running under: Windows 10 x64 (build 19045)
# # 
# # Matrix products: default
# # 
# # locale:
# #   [1] LC_COLLATE=German_Germany.1252  LC_CTYPE=German_Germany.1252    LC_MONETARY=German_Germany.1252 LC_NUMERIC=C                    LC_TIME=German_Germany.1252    
# # 
# # attached base packages:
# #   [1] stats     graphics  grDevices utils     datasets  methods   base     
# # 
# # other attached packages:
# #   [1] plyr_1.8.7       plotly_4.10.1    ggplot2_3.4.0    shp2graph_0-5    igraph_1.3.5     dplyr_1.0.9      sfnetworks_0.6.1 sf_1.0-7        
# # 
# # loaded via a namespace (and not attached):
# #   [1] tidyselect_1.1.2   remotes_2.4.2      purrr_0.3.4        lattice_0.20-44    tcltk_4.1.0        colorspace_2.0-3   vctrs_0.5.2        generics_0.1.3    
# # [9] htmltools_0.5.2    viridisLite_0.4.1  yaml_2.3.5         utf8_1.2.2         rlang_1.0.6        e1071_1.7-11       pillar_1.8.1       foreign_0.8-81    
# # [17] glue_1.6.2         withr_2.5.0        DBI_1.1.3          sp_1.5-0           lifecycle_1.0.3    munsell_0.5.0      gtable_0.3.1       htmlwidgets_1.5.4 
# # [25] fastmap_1.1.0      crosstalk_1.2.0    maptools_1.1-4     class_7.3-19       fansi_1.0.3        tidygraph_1.2.2    Rcpp_1.0.9         KernSmooth_2.23-20
# # [33] scales_1.2.1       classInt_0.4-7     lwgeom_0.2-9       jsonlite_1.8.0     digest_0.6.29      grid_4.1.0         cli_3.4.1          tools_4.1.0       
# # [41] magrittr_2.0.3     lazyeval_0.2.2     proxy_0.4-27       tibble_3.1.7       crayon_1.5.2       tidyr_1.2.0        sfheaders_0.4.0    pkgconfig_2.0.3   
# # [49] ellipsis_0.3.2     Matrix_1.4-1       data.table_1.14.2  assertthat_0.2.1   httr_1.4.4         rstudioapi_0.14    R6_2.5.1           units_0.8-0       
# # [57] compiler_4.1.0    


#Data import ----
# Note: both shapefiles, i.e. net_3000m_15deg.shp and net_5500m_10deg.shp, need
# to be converted to graphs

net <- st_read("net_3000m_15deg.shp")

##Create nodelist and edgelist from shapefile (see documentation for shp2graph)----
### add new column id
net$number <- seq(1, nrow(net), by = 1)

### convert to SpatialDataFrame
hw_spatial <- as_Spatial(st_zm(net))
### create graph: ELComputed = return length of edges; Detailed = convert all points to 
### nodes, not only the endpoints; ea.prop = keep edge attributes
hw_nw <- readshpnw(hw_spatial, ELComputed = TRUE, Detailed = T, ea.prop = c(0,1))
nodelist <- hw_nw[[2]]
edgelist <- hw_nw[[3]]

#create network from nodelist and edgelist
graph_shp2graph <- nel2igraph(nodelist, edgelist, Directed = FALSE)

#convert edgelist to igraph format
##convert to data frame
edgelist_df <- as.data.frame(edgelist)
##remove id column
edgelist_igraph <- edgelist_df[,-1]
##change column names
names(edgelist_igraph) <- c("from", "to")

#convert nodelist to igraph format (columns: "id", "x-coordinate", "y-coordinate")
##extract x-coordinates
nodelist_unlist <- NULL
for (i in 1:nrow(nodelist)) {
  tmp <- unlist(nodelist[i,][[2]][1])
  nodelist_unlist <- rbind(nodelist_unlist, tmp)
}

##extract y-coordinates
nodelist_unlist2 <- NULL
for (i in 1:nrow(nodelist)) {
  tmp2 <- unlist(nodelist[i,][[2]][2])
  nodelist_unlist2 <- rbind(nodelist_unlist2, tmp2)
}

#merge into dataframe and add id-column
##merge the two columns for coordinates and convert to data frame
nodelist_igraph <- as.data.frame(cbind(nodelist_unlist, nodelist_unlist2))
##add id column
nodelist_igraph$number <- seq(1, nrow(nodelist_igraph), by = 1)
##change column names
names(nodelist_igraph) <- c("POINT_X", "POINT_Y", "number")
##change column order (otherwise igraph will not recognise the nodes properly)
nodelist_igraph <- nodelist_igraph[,c(3,1,2)]
nodelist_geom <- st_as_sf(nodelist_igraph, coords = c(2:3), crs = 32637, remove = FALSE)

nodeslist_buffer <- st_buffer(nodelist_geom, 1)

##read the networks and sites 
sites <- list.files(pattern = "Khabur_Ane_pred_Tuna_\\w+.shp$", full.names = TRUE)

##create list of shapefiles 
list_sites <- lapply(sites, st_read)

##add names to the shapefiles in the list to include only the period
names(list_sites) <- gsub(".shp", "", gsub("Khabur_Ane_pred_Tuna_", "sites_", list.files(pattern = "Khabur_Ane_pred_Tuna_\\w+.shp$", full.names = F)))

x = 1
for (site in list_sites) {
  print(paste("Starting sites for ", names(list_sites[x])))
  period <- gsub("sites_", "", names(list_sites[x])) 
  #add new column to check which site falls within which buffer
  site$number <- apply(st_intersects(nodeslist_buffer, site, sparse = FALSE), 2,
                       function(col) {
                         nodeslist_buffer[which(col), ]$number
                       })
  #change numeric(0) in id column to 0
  site$number[site$number == "numeric(0)"] <- 0
  #unlist id column
  site <- unnest(site, "number")
  #filter only those EBA1 sites whose id is not 0, i.e. those that are within a buffer of the nodes
  node <- site %>%
    filter(number != 0)
  #select the sites that do not coincide with nodes, i.e. those without edges
  no_node <- site %>%
    filter(number == 0)
  #select only the nodes that do not coincide with sites
  nodelist_geom_filter <- nodelist_geom %>%
    filter(!(number %in% node$number))
  #combine the endnodes of hollow ways and the sites and order them decreasingly
  nodes <- rbind.fill(nodelist_geom_filter, node)
  #to include all sites, even those without edges
  no_node$number <- seq(nrow(nodes) + 1, nrow(nodes) + nrow(no_node), by = 1)
  nodes_all <- rbind.fill(nodes, no_node)
  print(paste("Sites for ", period, " done"))
  #create and plot graph
  g <- graph_from_data_frame(edgelist_igraph, directed = FALSE, vertices = nodes_all)
  g <- igraph::simplify(g)
  write.graph(g, paste0("g_",period,"_comp_3000.txt"), "graphml")
  st_write(node, dsn = paste0("sites_",period,"_node_comp_3000.csv"))
  st_write(nodes_all, dsn = paste0("sites_",period,"_nodes_all_comp_3000.csv"))
  print(paste("Graph for ", period, " done"))
  x = x+1
}
