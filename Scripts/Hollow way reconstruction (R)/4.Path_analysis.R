library(igraph)
library(dplyr)
library(sf)
library(R.utils)
library(purrr)
library(xts)
library(network)

#Import the files----
##read the networks and sites
g1 <- read_graph("g_EBA1_comp.txt", "graphml")
g2 <- read_graph("g_EBA2_comp.txt", "graphml")
g3 <- read_graph("g_MBA_comp.txt", "graphml")
g4 <- read_graph("g_LBA_comp.txt", "graphml")
g5 <- read_graph("g_IA1_comp.txt", "graphml")
g6 <- read_graph("g_IA2_comp.txt", "graphml")

##create list of graphs
list_graphs <- list(g1, g2, g3, g4, g5, g6)

##add names to graphs in the list to include only the period
names(list_graphs) <- gsub(".txt", "", list.files(pattern = "g_\\w+.txt$", full.names = F))

##read the nodes
all_nodes <- list.files(pattern = "sites_\\w+_nodes_all_comp.csv$", full.names = TRUE)
site_nodes <- list.files(pattern = "sites_\\w+_node_comp.csv$", full.names = TRUE)

##create list of shapefiles 
list_all_nodes <- lapply(all_nodes, read.csv)
list_site_nodes <- lapply(site_nodes, read.csv)

##add names to the shapefiles in the list to include only the period
names(list_all_nodes) <- gsub(".csv", "", gsub("sites_", "", list.files(pattern = "sites_\\w+_nodes_all_comp.csv$", full.names = F)))
names(list_site_nodes) <- gsub(".csv", "", gsub("sites_", "", list.files(pattern = "sites_\\w+_node_comp.csv$", full.names = F)))

# extract paths ----
## calculate all simple paths ----
x = 1
for (nodes in 1:length(list_all_nodes)) {
  period <- gsub("_nodes_all_comp", "", names(list_all_nodes[x]))
  print(paste("Starting nodes for", period))
  ## extract ids of archaeological sites (as opposed to endpoints of lines)
  print(paste("Extracting site_ids for", period))
  site_ids <- list_site_nodes[[x]]$number
  for (i in 1:length(site_ids)) {
    print(paste("Calculating paths for site", site_ids[i]))
    ## USE THE RESPECTIVE GRAPH ##
    g <- list_graphs[grepl(period, names(list_graphs))]
    spaths <- all_simple_paths(g[[1]], V(g[[1]])[site_ids[i]], V(g[[1]])[site_ids], cutoff = 15)
    print(paste("Saving paths for site", site_ids[i]))
    saveRDS(spaths, file = paste0("paths_comp_5500_",period,"_",site_ids[i], ".rds"))
  }
  x = x+1
}

## create edgelist from paths ----
## import all rds file names
filelist = list.files(pattern = "paths_comp_\\w+.rds")
## read all rds files
all_rds <- lapply(filelist, readRDS)
## assign names, i.e. file name without .rds
names(all_rds) <- gsub(".rds", "", list.files(pattern = "paths_comp_\\w+.rds", full.names = FALSE), fixed = TRUE)

## add indivdual files to the global environment if they are not empty, i.e. if they include paths
invisible(lapply(names(all_rds), function(x) if (length(all_rds[[x]]) != 0) assign(x, all_rds[[x]], envir = .GlobalEnv)))

## add the objects back to a list (without the empty ones)
lst1 <- lapply(mget(ls(pattern = "paths_comp_EBA1_\\d+")), as.list)
lst2 <- lapply(mget(ls(pattern = "paths_comp_EBA2_\\d+")), as.list)
lst3 <- lapply(mget(ls(pattern = "paths_comp_MBA_\\d+")), as.list)
lst4 <- lapply(mget(ls(pattern = "paths_comp_LBA_\\d+")), as.list)
lst5 <- lapply(mget(ls(pattern = "paths_comp_IA1_\\d+")), as.list)
lst6 <- lapply(mget(ls(pattern = "paths_comp_IA2_\\d+")), as.list)

#Note: The lists must be alphabetically because the for loop looks at the path
#files in alphabetical order
lists_periods <- list(lst1, lst2, lst5, lst6, lst3,lst4)
names(lists_periods) <- c("EBA1", "EBA2","IA1", "IA2", "LBA", "MBA")

x = 1
for (plist in lists_periods) {
  y = 1
  period <- names(lists_periods[x])
  print(paste("Extracting site_ids for", period))
  site_ids <- list_site_nodes[[x]]$number
  edgelist <- data.frame()
  for (item in plist) {
    print(paste("Starting", names(plist[y])))
    for (path in item) {
      ## extract the unique nodes on the path
      #path_item <- lapply(1:length(path), function(x) as_ids(path))
      path_item_ids <- as.numeric(unique(unlist(path)))
      ## extract ids of archaeological sites (as opposed to endpoints of lines)
      ## find the sites on the path, i.e. those nodes that match an id on the 
      ## site_ids list
      print(paste("Extracting sitesOnPath for", names(plist[y])))
      sitesOnPath <- match(site_ids, path_item_ids)
      sitesOnPath <- site_ids[!is.na(sitesOnPath)][order(na.omit(sitesOnPath))]
      print(paste("Creating edgelist for", names(plist[y])))
      from <- list()
      to <- list()
      if (length(sitesOnPath) == 2) {
        ## add start node to "from" list
        from <- append(from, sitesOnPath[1])
        ## add end node to "to" list
        to <- append(to, sitesOnPath[2])
        ## if there are more than two nodes on the path
      } else if (length(sitesOnPath) == 3) {
        ## add start node to "from" list
        from <- append(from, head(sitesOnPath, n = 1))
        from <- append(from, sitesOnPath[2])
        ## add end node to "to" list
        to <- append(to, sitesOnPath[2])
        to <- append(to, tail(sitesOnPath, n = 1))
      } else if (length(sitesOnPath) == 4) {
        ## add start node to "from" list
        from <- append(from, head(sitesOnPath, n = 1))
        from <- append(from, sitesOnPath[2])
        from <- append(from, sitesOnPath[3])
        ## add end node to "to" list
        to <- append(to, sitesOnPath[2])
        to <- append(to, sitesOnPath[3])
        to <- append(to, tail(sitesOnPath, n = 1))
      }
      if(length(from) != 0 && length(to) != 0) {
        lists <- list(from, to)
        if (nrow(edgelist) == 0) {
          edgelist <-as.data.frame(do.call(cbind, lists))
          names(edgelist) <- c("from", "to")
        } else {
          edgelist <- rbind(edgelist, lists)
        }
      }
    }
    if (nrow(edgelist) != 0) {
      ## remove duplicate paths
      edgelist[1] <- apply(edgelist[1], 2, function(y) sapply(y, function(x) paste(unlist(x))))
      edgelist[2] <- apply(edgelist[2], 2, function(y) sapply(y, function(x) paste(unlist(x))))
    }
    y = y + 1
  }
  print(paste("Creating edgelist for", period))
  edgelist <- distinct(edgelist)
  write.csv(edgelist, paste0("edgelist_comp_5500_", period, ".csv"), row.names = F)
  x = x+1
}

