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

# upload the packages you need based off of the photo trigger rate

packages_required <- c("dplyr", "magrittr", "exifr")

# load packages

package_load(packages_required)

# get file paths
if(length(folder_to_resize) == 0 |is.character(folder_to_resize)==FALSE ){
  stop("This script requires 'folder_to_resize' as a character string,
        run that portion of code in upload_photos_to_zooniverse.R")
}

# get the path names of the directory and all subdirectories
# if search_subdirs = TRUE
cat("collecting file paths\n\n")
file_paths <- unlist(list.files(path = folder_to_resize, 
           pattern = photo_file_type, # currently only takes JPG files
           recursive = search_subdirs, # we want files in the sub-directories as well
           full.names = TRUE, # get the entire file names
           ignore.case = TRUE)) #get .jpg or .JPG

# remove the subfolders that are in subfolders to skip
if(length(subfolders_to_skip)>0){
file_paths <- file_paths[-grep(paste(as.character(subfolders_to_skip),
                                    collapse = "|"), file_paths)]
}


# convert double slash to single if present
file_paths <-gsub("//", "/", file_paths)
# collect just the names of the photos from file_paths
photo_names <- strsplit(file_paths, "/") %>% 
  sapply(FUN = function(x){x[length(x)]})

# get date and time from each of these images
date_time <- exifr(file_paths, exiftoolargs = "-DateTimeOriginal")

# determine what splits the date time stuff
num_split <- gsub('[[:digit:]]+', '', date_time$DateTimeOriginal[1])
# how formatted
hf<- unlist(strsplit(num_split, ""))
# make character object that describes how
# the date / time is encoded on a camera trap
pic_format <-paste0("%Y",hf[1],"%m",hf[2],"%d",hf[3],"%H",hf[4],"%M",hf[5],"%S")

# convert to posix
date_time_psx <- as.POSIXct(date_time$DateTimeOriginal, 
  format = pic_format)

# sort the photos by time instead of by file name
time_order <- order(date_time_psx)
if(!all(time_order == 1:length(time_order))){
  date_time_psx <- date_time_psx[time_order]
  file_paths <- file_paths[time_order]
  photo_names <- photo_names[time_order]
}

# number of photos in upload
n_batch <- length(date_time_psx)

#if multiple photos are taken with each trigger
if(n_photos_when_triggered>1) {
# where does the first photo start
unq_batch <- c(0,which(diff(date_time_psx)>5)) + 1

# holds the new_paths
new_paths <- vector("list", length(unq_batch))
for(i in 1:length(unq_batch)){
  if(i == length(unq_batch)){
    new_paths[[i]] <- file_paths[unq_batch[i]:n_batch]
  } else {
  new_paths[[i]] <- file_paths[unq_batch[i]:c(unq_batch[i+1]-1)]
  }
}

# the location of the extra triggers
#  Extra triggers are times when there are more than n_photos_when_triggered
#  photos.
extra_trig <- rev(which(lengths(new_paths)> n_photos_when_triggered))
if(length(extra_trig)>0){
for(i in 1:length(extra_trig)){
    # this is the number of triggering events that should
    # have happened
    n_triggers <- ceiling(length(new_paths[[extra_trig[i]]]) / 
        n_photos_when_triggered)
    # make a vector and split it into equal n_photos_when_triggered parts
    # if there are leftovers (e.g., a 2 photo batch when there should be 3)
    # then the last trigger is assumed to be the issue. This will likely
    # be the case (considering the camera would have to finish it's
    # first triggering event before a second one started)
    trigger_batches <- split(seq(1, n_photos_when_triggered * n_triggers), 
      ceiling(seq(1, n_photos_when_triggered * n_triggers)/
          n_photos_when_triggered))
    # temporary list to hold the file paths
    temp <- vector("list", length = n_triggers)
    for(j in 1:length(temp)){
    temp[[j]] <- new_paths[[extra_trig[i]]][trigger_batches[[j]]]
    }
    # sneak temp into the correct location in new_paths
    new_paths <- c(new_paths[1:c(extra_trig[i]-1)], 
      temp, new_paths[c(extra_trig[i]+1):length(new_paths)])
 }
}

#n actual photos per trigger
if(!all(lengths(new_paths) == n_photos_when_triggered)){
 new_paths <- lapply(new_paths, 'length<-', max(lengths(new_paths))) 
 to_na <- function(x){
   if(sum(is.na(x)>0)){
     x[is.na(x)] <- "/NA"
   }
   return(x)
 }
 new_paths <- lapply(new_paths, to_na)
}


 


# splits by the forward slash
# used for image names on zooniverse
new_photo_names <- lapply(new_paths, strsplit, "/") 
# we need the last element from a list in a list
# this function can be used within sapply to get it
fn <- function(x) sapply(x, function(y){y[length(y)]})
# this keeps it in the same structure as new_paths
new_photo_names <- t(sapply(new_photo_names, fn))
} else {
  # if we just have 1 photo per trigger
  new_paths <- file_paths
  new_photo_names <- data.frame(photo_names, stringsAsFactors = FALSE)
}

# now, we want to copy the files over ~ 1000 files over to 
# a temporary directory. First we determine how many
# iterations of 1k photos we need to take through
# photo_names

  # integer division 
if(length(new_paths)>= 1000){
  n_iters <- length(new_paths) %/% 1000
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
        in object im.")
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
  im_call <- " -crop 0x0+0-100 -resize 900x600 -quality 96 -interlace Plane -sampling-factor 4:2:0 -define jpeg:dct-method-float "
    }else{
      im_call <- " -resize 900x600 -quality 96 -interlace Plane -sampling-factor 4:2:0 -define jpeg:dct-method-float "
    }

# for loop to iterate through photos
for(i in 1:n_iters){
  # make 1000 unique ids for all i less than n_iters
  if(i<n_iters){
    id <- seq(start,end,by=1 )
      }else{
        # make length(photo_names) unique ids when i == n_iter
        id <- seq(start,length(new_paths),by = 1)
        end <- length(new_paths)
      } # close ifelse statement
  # make the manifest
  # we need a number of columns equal to 1 + n_photos_when_triggers
  manifest <- data.frame(matrix(id, nrow = length(id), 
    ncol = 1 + n_photos_when_triggered))
  # id and then the file names
  colnames(manifest) <- c("id", 
    paste0(rep("image_", n_photos_when_triggered), 
      1:n_photos_when_triggered))
  # put the new_photo_names in the manifest
  manifest[,-1] <- new_photo_names[id,]
  
  # name of manifest file
  manifest_file_path <- paste0(tmp_dir,"/manifest_",i,".csv")
  
  # write the manifest to the tmp_dir
  write.csv(manifest, manifest_file_path , row.names = FALSE)
  if(resize){
  # make a progress bar
  pb <- txtProgressBar(min =start, max = end, style = 3)
  
  # go through and resize each photo and subject
  # if multiple photos per trigger
  if(n_photos_when_triggered>1){
  for(subject in 1:length(id)){
    for(photo in 1:n_photos_when_triggered){
    
    # crop out the bushnell stuff, which takes up
    # 100 pixels on the bottom 
      if(new_paths[id[subject]][[1]][photo] == "/NA") next
    system(paste0(pwq(im), pwq(new_paths[id[subject]][[1]][photo]), im_call,
                  pwq(paste0(tmp_dir, "/", new_photo_names[subject,photo]), 
                    space = FALSE)))
    setTxtProgressBar(pb, id[subject])
    }
  }
  } else { # if only one photo per trigger
    for(subject in 1:length(id)){
      system(paste0(pwq(im), pwq(new_paths[id[subject]]), im_call,
        pwq(paste0(tmp_dir, "/", new_photo_names[id[subject],]), 
          space = FALSE)))
      setTxtProgressBar(pb, id[subject])
      
    }
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


