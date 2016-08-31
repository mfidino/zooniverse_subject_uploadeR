#########################
#
#
# zooniverse_subject_uploadeR.R
#
#
#
#
#

#####################################################
# User specified functions
#####################################################

### package_load

# This function loads appropriate packages when given a character vector
# of packages in the 'packages` arguement.  If you do not have it
# it downloads these packages.

package_load<-function(packages = NULL, quiet=TRUE, verbose=FALSE, warn.conflicts=FALSE){
  
  # download required packages if they're not already
  
  pkgsToDownload<- packages[!(packages  %in% installed.packages()[,"Package"])]
  if(length(pkgsToDownload)>0)
    install.packages(pkgsToDownload, repos="http://cran.us.r-project.org", quiet=quiet, verbose=verbose)
  
  # then load them
  for(i in 1:length(packages))
    require(packages[i], character.only=T, quietly=quiet, warn.conflicts=warn.conflicts)
}


### pwq (paste with quotes)

# This fucntion paste a character string with escpated quotes around it
# to account for spaces in file names when making
# a system command from R (e.g. 'Program Files')

pwq <- function(x,space = TRUE){
  if(space){
    paste0("\"",x,"\" ")
  }else{
    paste0("\"",x,"\"")
  }
}


# load packages
if(length(packages_required) == 0 |is.character(packages_required)==FALSE ){
  stop("This script requires 'packages_required' as a character string,
        run that portion of code in upload_photos_to_zooniverse.R")
}
package_load(packages_required)

# get file paths
if(length(folder_to_resize) == 0 |is.character(folder_to_resize)==FALSE ){
  stop("This script requires 'folder_to_resize' as a character string,
        run that portion of code in upload_photos_to_zooniverse.R")
}

# get the path names of the directory and all subdirectories
# if search_subdirs = TRUE

file_paths <- list.files(path = folder_to_resize, 
           pattern = photo_file_type, # currently only takes JPG files
           recursive = search_subdirs, # we want files in the sub-directories as well
           full.names = TRUE, # get the entire file names
           ignore.case = TRUE) #get .jpg or .JPG

# collect just the names of the photos from file_paths
photo_names <- strsplit(file_paths, "/") %>% sapply(FUN = function(x){x[length(x)]})

# now, we want to copy the files over ~ 1000 files over to 
# a temporary directory. First we determine how many
# iterations of 1k photos we need to take through
# photo_names

  # integer division 
if(length(photo_names)>= 1000){
  n_iters <- length(photo_names) %/% 1000
  }else{
    n_iters <- 1
  }


# set up to grab the first 1000 photos, these
# objects get updated throughout the for loop below.
start <- 1
end <- 1000

# check to make sure they specified im
if(length(im) == 0 |is.character(im)==FALSE ){
  stop("This script requires 'im' as a character string,
        run that portion of code in upload_photos_to_zooniverse.R")
  }
# and make sure it actually leads to convert.exe
if(file.exists(im) == FALSE){
  stop("The location you specified to convert.exe is wrong
        in object im is wrong.")
  }

# create tmp_dir if it does not already exist
if (file.exists(tmp_dir)) {
  cat("tmp_dir exists in mainDir and is a directory")
    } else if (file.exists(tmp_dir)) {
      cat("tmp_dir exists as a file! Look into this.")
      # you will probably want to handle this separately
        } else {
          cat("tmp_dir does not exist - creating")
          dir.create(file.path(tmp_dir))
        }
if(crop_drop){
  im_call <- " -crop 0x0+0-100 -resize 900x600 -quality 100 -interlace Plane -sampling-factor 4:2:0 -define jpeg:dct-method-float "
    }else{
      im_call <- " -resize 900x600 -quality 100 -interlace Plane -sampling-factor 4:2:0 -define jpeg:dct-method-float "
  }
# for loop to iterate through photos
for(i in 1:n_iters){
  paste0("Resizing batch ", i, " of ",n_iters, " batches." )
  # make 1000 unique ids for all i less than n_iters
  if(i<n_iters){
    id <- seq(start,end,by=1 )
      }else{
        # make length(photo_names) unique ids when i == n_iter
        id <- seq(start,length(photo_names),by = 1)
        end <- length(photo_names)
      } # close ifelse statement
  # make the manifest
  manifest <- data.frame(id = id, 
                         image_name = photo_names[id],
                         file_path = file_paths[id])
  
  manifest_file_path <- paste0(tmp_dir,"/manifest_",i,".csv")
  
  
  # write the manifest to the tmp_dir
  write.csv(manifest, manifest_file_path , row.names = FALSE)
  if(resize){
  # make a progress bar
  pb <- txtProgressBar(min =start, max = end, style = 3)
  for(photo in 1:length(id)){
    
    # crop out the bushnell stuff, which takes up
    # 100 pixels on the bottom
    system(paste0(pwq(im), pwq(file_paths[id[photo]]), im_call,
                  pwq(paste0(tmp_dir, "/", photo_names[id[photo]]), space = FALSE)))
    setTxtProgressBar(pb, id[photo])
  }
  # close the progress bar
  close(pb)
  }
  
  # fill the parameters we need for uploading the photos
  if(upload){
  panop <- "panoptes-subject-uploader "

  
  # make the system call
  node_call <- paste0(panop, manifest_file_path,
                      " --username ", username, " --password ", password,
                      " --project ", project, " --workflow ", workflow,
                      " --subject-set ", subject_set)
  Sys.setenv(NODE_ENV="production")
  system(node_call)
  
  if(delete_resized_post_upload){
    paste("Deleting resized photos (not originals)")
    file.remove(list.files(tmp_dir, full.names = TRUE))
    }
  }

  # modify the start and ends for the next batch to upload
  start <- end + 1
  end <- end + 1000
}


