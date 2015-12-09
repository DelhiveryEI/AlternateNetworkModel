#hard-coded parameters used in the code
package_source_file <- "~/input/packages.csv"
package_no_of_rows <- 1000000
bag_source_file <- "~/input/bags.csv"
bag_no_of_rows <- 1000000
source_pc <- "Hyderabad_Katedan_PC (Andhra Pradesh)"


#input package data.
#modify nrows parameter as desired
packages <- read.csv(package_source_file, header = TRUE, stringsAsFactors=FALSE, skipNul = FALSE, nrows = package_no_of_rows)

#input bag data.
#modify nrows parameter as desired
bags <- read.csv(bag_source_file, header = TRUE, stringsAsFactors=FALSE, skipNul = FALSE, nrows = bag_no_of_rows)

#get bag ID's that are mapped to the PC
bag_ids_hyd <- bags[which(bags$sl == source_pc ),"bs"]

#get the records for the Bags corresponding to the above Bag ID's
bags <- bags[which(bags$bs %in% bag_ids_hyd),]

#get package ID's that are mapped to the Bags from the PC
packages_ids_bag_ids_hyd <- packages[which(packages$pid %in% bag_ids_hyd),"wbn"]

#get all the records for the above package ID's
packages_filtered_for_package_ids <- packages[which(packages$wbn %in% packages_ids_bag_ids_hyd),]

#get the records only for the PC under consideration
packages_filtered_for_package_ids <- packages_filtered_for_package_ids[which(packages_filtered_for_package_ids$sl == source_pc),] 

#get only properly sorted packages
properly_sorted_packages <- packages_filtered_for_package_ids[which(grepl('SORT',toupper(packages_filtered_for_package_ids$u))),"wbn"]
packages_filtered_for_package_ids <- packages_filtered_for_package_ids[which(packages_filtered_for_package_ids$wbn %in% properly_sorted_packages),]

#get only the bag scan records for the above package ID's
package_bag_scans <- packages_filtered_for_package_ids[which(packages_filtered_for_package_ids$pid %in% bag_ids_hyd & packages_filtered_for_package_ids$act =="+B"),c("sd","wbn","pid")]

#function to convert input raw date format to proper POSIX format
date_convert <- function(input_dates) return(as.POSIXct(paste(substr(input_dates, 14, 23), substr(input_dates, 25, 32), sep=" "), format= "%Y-%m-%d %H:%M:%S"))

#get the bag records for the source pc
source_pc_bags <- bags[which(bags$sl == source_pc),]

#get the bag open and closed records for the above Bag ID's
bag_open_scans <- source_pc_bags[which(source_pc_bags$ss == "WIP" & source_pc_bags$sr == "Bag Created"),c("sd","bs")]
names(bag_open_scans) <- c("open_time","bag_ID")
bag_close_scans <- source_pc_bags[which(source_pc_bags$ss == "In Transit" & source_pc_bags$sr == "Bag Sealed"),c("sd","bs")]
names(bag_close_scans) <- c("close_time","bag_ID")
bag_open_close <- merge(bag_open_scans,bag_close_scans,by="bag_ID")
bag_open_close$open_time <- date_convert(bag_open_close$open_time)
bag_open_close$close_time <- date_convert(bag_open_close$close_time)
bag_open_close$time_for_close <- as.numeric(bag_open_close[,"close_time"] - bag_open_close[,"open_time"])/60
bag_open_close$time_for_close <- ifelse(bag_open_close$time_for_close > 0 , bag_open_close$time_for_close, 0)
time_bins <- seq(15,240,by=15)
bag_open_close$time_for_close_bin <- findInterval(bag_open_close$time_for_close, time_bins)*15


#check that bagged record for that package does not exist more than once 
package_bagging_counts <- aggregate(pid~wbn, package_bag_scans[,c("pid","wbn")],FUN=length)
package_bagging_counts <- package_bagging_counts[which(package_bagging_counts$pid > 1),]
if(nrow(package_bagging_counts) > 0 ) stop("multiple bagging exists for one or more packages in this dataset")

#get only the sort scan records for the above package ID's
package_sort_scans <- packages_filtered_for_package_ids[which(grepl('SORT',toupper(packages_filtered_for_package_ids$u)) & packages_filtered_for_package_ids$sr == "Weight Captured"),c("sd","wbn")]

#check that sorted record for that package does not exist more than once 
package_sorted_counts <- aggregate(sd~wbn, package_sort_scans[,c("sd","wbn")],FUN=length)
package_sorted_counts <- package_sorted_counts[which(package_sorted_counts$sd > 1),]
if(nrow(package_sorted_counts) > 0 ) stop("multiple sorting exists for one or more packages in this dataset")

#number of bags vs number of shipments in the bag
bag_package_counts <- aggregate(wbn~pid, data =package_bag_scans[,c("pid","wbn")], FUN=length)[,2]
bag_package_density <- aggregate(bag_package_counts,data.frame(bag_package_counts),FUN=length)

#plot_1
plot(bag_package_density,type="l")

#convert package bag time to proper format
package_bag_scans$bagtime <- as.POSIXct(paste(substr(package_bag_scans$sd, 14, 23), substr(package_bag_scans$sd, 25, 32), sep=" "), format= "%Y-%m-%d %H:%M:%S")
package_bag_scans$bagdate <-  as.Date(package_bag_scans$bagtime)
bagdates <- sort(unique(package_bag_scans$bagdate))
package_bag_scans <- package_bag_scans[,-1]

#convert sort time to proper format
package_sort_scans$sorttime <- as.POSIXct(paste(substr(package_sort_scans$sd, 14, 23), substr(package_sort_scans$sd, 25, 32), sep=" "), format= "%Y-%m-%d %H:%M:%S")
package_sort_scans$sortdate <-  as.Date(package_sort_scans$sorttime)
sortdates <- sort(unique(package_sort_scans$sortdate))
package_sort_scans <- package_sort_scans[,-1]

#merge sorttime and bag time against the wbn
package_pivoted <- merge(package_sort_scans,package_bag_scans,by=c("wbn"))

#sort the times so that cumulative count can be easily obtained
package_pivoted <- package_pivoted[order(package_pivoted$sorttime),]

unbagged_shipments <- 0
for(i in sortdates){
  #packages sorted today
  package_sort_time <- data.frame(sorttime=sort(package_pivoted[package_pivoted$sortdate == i,c("sorttime")]))
  package_sort_time$no_of_packages <- c(1:nrow(package_sort_time)) 
  
  #packages bagged today
  package_bag_time <- data.frame(bagtime=sort(package_pivoted[package_pivoted$sortdate == i,c("bagtime")]))
  package_bag_time$no_of_packages <- c(1:nrow(package_bag_time)) + unbagged_shipments
  
  #plot_3a Date-wise
  plot(package_sort_time[,1],package_sort_time[,2], type="l")
  
  #plot_3b Date-wise
  lines(package_bag_time[,1],package_bag_time[,2], type="l")
  
}

#number of shipments in each bag
bag_no_packages <- aggregate(wbn~pid,package_pivoted[,c("wbn","pid")],FUN=length)
names(bag_no_packages) <- c("bag_ID","no_packages")
bag_summary <- merge(bag_open_close,bag_no_packages,by="bag_ID")
bag_summary <- bag_summary[,c(-2,-3)]
