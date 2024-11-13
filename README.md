# Introduction
This repository provides the data and scripts for the paper “Filling the gaps - Computational approaches to incomplete archaeological net-works” 
It contains the following files: 
##	Data
  *	Original site data, clipped to the extent of the research area if necessary, used as input for the script “Site data preaparation”
    + Ane_clip.shp
    +	BA_IA_Sites_Names_Sizes.shp
    +	Tuna_clip.shp
  *	Original hollow way data (hw_all.csv) and transformed into the correct coordinate system (HW_reform.csv)

  *	Input for the script “Connecting the hollow ways”
    +	HW_reform-csv
    +	Khabur_Ane_Chris_Tuna_32637.shp
  *	Input for the script “Descriptive network analysis”
    +	edgelists_manual_new
    +	EBA1_sites_id_log.csv
    +	IA2_sites_id_log.csv
    +	EBA2_sites_id_log.csv
    +	IA1_sites_id_log.csv
    +	LBA_sites_id_log.csv
    +	MBA_sites_id_log.csv
    +	edgelists_comp_5500_new.xlsx
    +	EBA1_sites_5500_id
    +	IA2_sites_5500_id.csv
    +	EBA2_sites_5500_id.csv
    +	IA1_sites_5500_id.csv
    +	LBA_sites_5500_id.csv
    +	MBA_sites_5500_id.csv
    +	edgelists_comp_3000_new.xlsx
    +	sites_EBA1_node_comp_3000.csv
    +	sites_IA2_node_comp_3000.csv
    +	sites_EBA2_node_comp_3000.csv
    +	sites_IA1_node_comp_3000.csv
    +	sites_LBA_node_comp_3000.csv
    +	sites_MBA_node_comp_3000.csv
    +	edgelists_hybrid.xlsx
    +	sites_EBA1_hybrid.csv
    +	sites_IA2_hybrid.csv
    +	sites_EBA2_hybrid.csv
    +	sites_IA1_hybrid.csv
    +	sites_LBA_hybrid.csv
    +	sites_MBA_hybrid.csv
    *	Input for “Site_prediction” – “main_programm”
    +	Khabur_ID_merge.csv
    +	Hw_all.csv

##	Scripts
  *	Site prediction
  *	Hollow way reconstruction
    +	1.Site data preparation
    +	2.Connecting_hollow_ways
    +	3.Network_extraction_from_shapefile
    +	4.Path_analysis
    +	5.Descriptive_network_analysis

# Overview
The script for the site algorithm needs to be run first to predict potential missing sites that can then be included in the hollow way algorithm. All files should be loaded into one folder. For those scripts, MatLab Version R2019b is required. All the .m files should be opened in one MatLab project. Main_programm.m is the main script which draws from the additional .m scripts and can be run either as a whole or in batches as desired. 
The scripts for the hollow way algorithm are numbered in their chronological order, i.e. “1.Site_data_preparation” should be run first and “4.Path_analysis” should be run last. “5.Descriptive_network_analysis” contains optional code to analyse the resulting data as presented in the paper but can be omitted or adjusted.

The input data for the individual scripts is provided in the folder “Data” in this repository. Input for the scripts “3.Network_extraction_from_shapefile” and “4.Path_analysis” is created in the respective previous script. 

In script “1.Site data preparation”, the available site data from three distinctive sources are formatted and adjusted so that they can be merged into one coherent file. Fur this purpose, attributes are aligned, duplicates removed and as much information for individual sites drawn into one file. 

In script “2.Connecting_hollow_ways”, the resulting site data from the previous step are used together with the original hollow way data to connect the hollow way segments into longer routes. In this process, sites and adjacent hollow ways are connected as well to form the basis for the network graphs. The hollow ways are stored as polylines.

In “3.Network_extraction_from_shapefile”, the networks created from connecting the hollow ways and the hollow ways and sites are converted from shapefiles to network graphs. This step is necessary to allow further analysis with R packages for network analysis, such as igraph or network.

In “4.Path_analysis”, an inherent issue of the networks is solved: as the computational procedure for the connection of the hollow ways leads to a high amount of superfluous connections (see paper), the result is messy and the analysis of the data becomes time- and resource-intensive. To clean the hollow way data set, a procedure was therefore developed to extract only the simple paths between all sites of the network, thereby reducing the hollow way data set significantly.

In “5.Descriptive_network_analysis”, the code for the analysis conducted in the paper is presented. However, depending on the requirements of the study and the research questions, this analysis can be either omitted, reduced or altered.  

