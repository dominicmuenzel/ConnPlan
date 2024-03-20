#'Create a boundary file from migration tracks
#'
#' `create_boundary_from_track` determines the spatial overlap between one or more migration tracks and a planning unit grid. It then generates a boundary file for use in conservation planning. Adjacent planning units which have a migration track running through them have shared boundary. The resulting boundary file can be in the format of an edge list or matrix.
#'
#' @param track_points XY coordinates of migration tracks. An sf points object in a  projected coordinate system. The sf data frame may contain a column 'id' giving the identity of the individual. The sf data frame may contain a column 'date' in YYYY-MM-DD format to order the sampling locations.
#' @param pu_grid A planning unit grid as an sf polygon object in a projected coordinate system
#'
#' @section Details:
#' 'edge_list' has columns \emph{id1}, \emph{id2}, \emph{boundary}, and \emph{boundarylength} \cr
#' \itemize{
#'   \item \emph{id1} is the ID of the first planning unit
#'   \item \emph{id2} is the ID of the connected planning unit
#'   \item \emph{boundary} is 1, representing a logical TRUE connection between the pair of planning units
#'   \item \emph{boundarylength} is the length of the shared boundary in units of the projected coordinate system
#' }
#'
#' @return A planning unit boundary file as an edge list.
#' @export
#'
create_boundary_from_track<-function(track_points,pu_grid){

  #Check that both are sf objects
  stopifnot(is(track_points,"sf"))
  stopifnot(is(pu_grid,"sf"))

  #Transform coordinate system of points if not already matching
  if(sf::st_crs(track_points)!=sf::st_crs(pu_grid)){
    sf::st_crs(track_points)<-sf::st_crs(pu_grid)
  }

  #Sort by date if available
  if("Date" %in% colnames(track_points) | "date" %in% colnames(track_points)){
    track_points<-track_points[order(as.Date(track_points$date)),]
  }

  #Create a data frame to store results
  pu_intersection_df<-data.frame()

  #In a loop, take two consecutive tracks and make the edge list
  for(track.i in 1:(nrow(track_points)-1) ){

    #Create lines
    track_lines<-sf::st_linestring(sf::st_coordinates(track_points[track.i:(track.i+1),]))

    #Create sf
    track_lines<-sf::st_sfc(track_lines)

    #Set the crs to match the points
    sf::st_crs(track_lines)<-sf::st_crs(track_points)
    #plot(track_lines)

    suppressWarnings(
    # Perform intersection to get IDs of PUS which track crosses
    pu_crossing <- sf::st_drop_geometry(sf::st_intersection(pu_grid, track_lines) )
    )

    #Get the intersection of the Pus which the track crosses
    pu_intersection<-sf::st_intersection( pu_grid[which(pu_grid$Id%in%pu_crossing$Id),])

    #Subset to pairs of touching PUs
    pu_intersection<-pu_intersection[which(pu_intersection$n.overlaps==2),]

    #Get the type of geometry
    geom_types<-sf::st_geometry_type(pu_intersection)

    #Remove corner edges touching
    pu_intersection<-pu_intersection[which(geom_types != "POINT"),]

    #Get the length of boundary
    pu_intersection_lengths<-sf::st_length(pu_intersection)

    for(i in 1:nrow(pu_intersection)){

      pu_intersection_df<-rbind(pu_intersection_df,
                                data.frame("id1"=pu_crossing$Id[unlist(pu_intersection$origins[i])[1]],
                                           "id2"=pu_crossing$Id[unlist(pu_intersection$origins[i])[2]],
                                           "boundary"=1,
                                           "boundarylengthm"=as.numeric(pu_intersection_lengths[i]))
      )
    }


  }

  return(pu_intersection_df)
}

#
# #Sample data individual 2
# pu_intersection_df2<-pu_intersection_df[33:36,]
#
# #Merge the matching pairs
# pu_intersection_df_sum<- merge(pu_intersection_df, pu_intersection_df2, by = c("id1", "id2"), all = TRUE)
#
# #Sum the boundaries
# pu_intersection_df_sum$boundary<-rowSums(pu_intersection_df_sum[,3:4],na.rm=T)
#
# #Keep only the summed boundary
# pu_intersection_df_sum<-pu_intersection_df_sum[,c("id1","id2","boundary")]
#
#
# #Export the file
# write.table(pu_intersection_df,file=paste0(oDir,"/MarxanData/input/bound_example.csv"),sep=",",row.names = F)
