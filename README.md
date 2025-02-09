This repository consists of two R files.

The 1st file 03. cropland.R:
This computes the cropland weight for all the 961 IMD grids. This pertains to the idea that India is a heterogeneous landmass; not all regions in India are equally suitable for growing crops. The cropland weights, therefore, give weight to a particular IMD grid occupied by vegetation where agriculture is suitable. However, the cropland grids are much smaller than the IMD grids; hence, each cropland grid loops through IMD grids, thereby calculating the proportion of vegetation in each IMD grid. 

The 2nd file 04.dist_weight_bins.R: 
It uses the cropland weights created earlier and calculates the weighted temperature of each IMD grid. Finally, we want to map it to all the districts of India. For this purpose, we do district weighting in QGIS. District weighting essentially calculates the weights: the proportion of each district that falls in a particular IMD grid. Once we get the district weights, we aggregate the districts and calculate the union. Eventually, the cropland-weighted temperature data weighted by district area will be used to calculate the temperature bins in every district.
