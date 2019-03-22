#########################
#
#
# zooniverse_subject_uploadeR.R
#
#
#
#
#

# check to see if imagemagick downloaded

im <- system("where /r \"C:\\Program Files\"  convert.exe", intern = TRUE)
if(length(im) == 0){
  stop("convert.exe not found in imageMagick.")
}


# Error checks

# check if panoptes is downloaded
.p_out <- system('panoptes --version', intern = TRUE)
if(length(.p_out) != 1 |
   substr(.p_out, 1, 8) != "Panoptes"){
  err_mess <- paste0('\nPanoptes not downloaded.\n',
                     'Follow instructions on:',
                     '\nhttps://github.com/zooniverse/panoptes-cli') 
  stop(err_mess)
}

# check if there is an internet connection
havingIP <- function() {
  if (.Platform$OS.type == "windows") {
    ipmessage <- system("ipconfig", intern = TRUE)
  } else {
    ipmessage <- system("ifconfig", intern = TRUE)
  }
  validIP <- "((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)[.]){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)"
  any(grep(validIP, ipmessage))
}

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

# ask for username


pwq <- function(x,space = TRUE){
  if(space){
    paste0("\"",x,"\" ")
  }else{
    paste0("\"",x,"\"")
  }
}

# upload the packages you need based off of the photo trigger rate

packages_required <- c("dplyr", "magrittr", "exifr", "getPass")

# load packages

package_load(packages_required)

# get file paths
if(length(folder_to_resize) == 0 |is.character(folder_to_resize)==FALSE ){
  stop("This script requires 'folder_to_resize' as a character string,
        run that portion of code in upload_photos_to_zooniverse.R")
}


#cat("\nPlease enter your zooniverse username and password (without quotes).\n")
#.username <- readline(prompt="Enter zooniverse username: ")
#if(hide_password){
#.password <- getPass(msg="Enter zooniverse password: ")
#} else {
#.password <- readline(prompt="Enter zooniverse password: ")
#}
#
## try to log on to panoptes cli
#system('panoptes configure', 
#       input = c( 'https://www.zooniverse.org',.username,.password),
#       show.output.on.console = FALSE)
#.try_out <- system(paste('panoptes workflow ls -p ', project), intern = TRUE,
#                           show.output.on.console = FALSE)
#if(!is.null(attributes(.try_out)$status)){
#if(attributes(.try_out)$status == 1){
#  stop("Wrong username or password. Try again.")
#}
#}

# get the path names of the directory and all subdirectories
# if search_subdirs = TRUE

##################
# photo_regex
##################
photo_regex <- function(x){
  to_return <- switch(tolower(x),
                      'jpg'  = ".JPG$|.jpeg$",
                      'jpeg' = ".JPG$|.jpeg$",
                      'png'  = ".PNG|.png")
  if(is.null(to_return)){
    err <- paste0('file_info$photo_file_type must take one of the following values:\n',
                  "\t- 'jpg'\n",
                  "\t- 'jpeg'\n",
                  "\t- 'JPG'\n",
                  "\t- 'png'\n",
                  "\t- 'PNG'")
    stop(err)
  }
  return(to_return)
}

###########################
# get_paths
###########################

get_paths <- function(file_info = NULL){
  if(!is.list(file_info) | 
     any(!names(file_info) %in% c('folder_to_upload', 'photo_file_type', 'search_subdirs'))){
      err <- paste0('the structure of file_info is incorrect.',
                    '\n\nfile_info must be a list object that contains the following named elements:\n\n',
                    '\t - folder_to_upload: the file path to the photos to be uploaded\n',
                    '\t - photo_file_type:  the file type of the images (jpg or png)\n',
                    '\t - search_subdirs:   TRUE / FALSE on whether to recursively search folder_to_upload\n\n',
                    'EXAMPLE:\n\n',
                    "my_file_info <- list(folder_to_upload = 'file/path/to/photos/here',\n",
                    "                     photo_file_type = 'jpg',\n",
                    "                     search_subdirs = TRUE)")
    stop(err)
  }
  cat("Collecting file paths\n")
  file_paths <- unlist(list.files(path = file_info$folder_to_upload, 
                                  pattern = photo_regex(file_info$photo_file_type), 
                                  recursive = file_info$search_subdirs, 
                                  full.names = TRUE, 
                                  ignore.case = TRUE)) 
  
  # drop corrupted files that are 0 kb 
  file_sizes <- file.size(file_paths)
  if(any(file_sizes == 0)){
    file_paths <- file_paths[-which(file_sizes == 0)]
  }
  
  # convert double slash to single if present
  file_paths <-gsub("//", "/", file_paths)
  

  
  return(file_paths)
}

# remove the subfolders that are in subfolders to skip
#if(length(subfolders_to_skip)>0){
#file_paths <- file_paths[-grep(paste(as.character(subfolders_to_skip),
#                                    collapse = "|"), file_paths)]
#}

# get file sizes



if(.username == "mason_uwi"){
if(class(human_images) == "data.frame"){
  
  human <- human_images[human_images$has_human == TRUE,]
  
  # get just the photo_name
  p_name_human <- strsplit(human$human_photos, "/") %>% 
    sapply(., function(x) x[length(x)]) %>% tolower
  
  p_name_images <- strsplit(file_paths , "/") %>% 
    sapply(., function(x) x[length(x)]) %>% tolower
  
  human_to_go <- which(p_name_images %in% p_name_human)
  if(length(human_to_go)> 0){
    file_paths <- file_paths[-human_to_go]
  }
  
  
}
}

#################################
# get_site_names
#################################

get_site_names <- function(file_paths = NULL, file_info = NULL){
  # check file_paths
  if(is.null(file_paths)){
    stop("please supply file_paths to this function.")
  }
  if(!is.character(file_paths)){
    err <- paste0('file_paths must be a character vector.\n\n',
                  'file_paths can easily be generated with get_paths().\n\n',
                  'EXAMPLE:\n\n',
                  '# create file_info\n',
                  "my_file_info <- list(folder_to_upload = 'file/path/to/photos/here',\n",
                  "                     photo_file_type = 'jpg',\n",
                  "                     search_subdirs = TRUE)\n\n",
                  '# collect file paths\n',
                  'my_files <- get_paths(my_file_info)\n\n',
                  '# collect site names\n',
                  'my_sites <- get_sites(my_files, my_file_info)')
    stop(err)
  }
  # check file_info
  if(is.null(file_info)){
    stop("please supply file_info to this function.")
  }
  if(!is.list(file_info) | 
     any(!names(file_info) %in% c('folder_to_upload', 'photo_file_type', 'search_subdirs'))){
    err <- paste0('the structure of file_info is incorrect.',
                  '\n\nfile_info must be a list object that contains the following named elements:\n\n',
                  '\t - folder_to_upload: the file path to the photos to be uploaded\n',
                  '\t - photo_file_type:  the file type of the images (jpg or png)\n',
                  '\t - search_subdirs:   TRUE / FALSE on whether to recursively search folder_to_upload\n\n',
                  'EXAMPLE:\n\n',
                  "my_file_info <- list(folder_to_upload = 'file/path/to/photos/here',\n",
                  "                     photo_file_type = 'jpg',\n",
                  "                     search_subdirs = TRUE)")
    stop(err)
  }

  # get just the file names
  photo_names <- strsplit(file_paths, "/") %>% 
    sapply(FUN = function(x){x[length(x)]})
  
  site_names <- gsub(photo_regex(file_info$photo_file_type), "", photo_names)
  
  # drop numerics if the trail like 'site_0001', 'site_0002'
  if(length(grep("_\\w+$", site_names)) > 0 ){
    site_names <- gsub("_\\w+$","",site_names)
  }
  
  # drop () if they are named that way
  if(length(grep("\\s\\(\\w+\\)?$", site_names)) > 0) {
    site_names <- gsub("\\s\\(\\w+\\)?$","",site_names)
  }
  
  # create a summary of the sites
  site_summary <- t(t(sort(table(site_names), decreasing = TRUE)))
  site_summary <- tibble::tibble(site_names = row.names(site_summary),
                             count = site_summary[,1])
  row.names(site_summary) <- 1:nrow(site_summary)
  
  print(site_summary)
  
  return(list(names = site_names, 
              summary = site_summary))
}

get_datetime <- function(file_paths = NULL, site_names = NULL){
  # check file_paths
  if(is.null(file_paths)){
    stop("please supply file_paths to this function.")
  }
  if(!is.character(file_paths)){
    err <- paste0('file_paths must be a character vector.\n\n',
                  'file_paths can easily be generated with get_paths().\n\n',
                  'EXAMPLE:\n\n',
                  '# create file_info\n',
                  "my_file_info <- list(folder_to_upload = 'file/path/to/photos/here',\n",
                  "                     photo_file_type = 'jpg',\n",
                  "                     search_subdirs = TRUE)\n\n",
                  '# collect file paths\n',
                  'my_files <- get_paths(my_file_info)\n\n',
                  '# collect site names\n',
                  'my_sites <- get_sites(my_files, my_file_info)')
    stop(err)
  }
  cat('Collecting date time information from photos\n')
  date_time <- read_exif(file_paths, tags = "DateTimeOriginal")
  
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
  #  if site_names is available sort by site first
  if(is.null(site_names)){
    time_order <- order(date_time_psx)
  } else {
    time_order <- order(site_names$names, date_time_psx)
  }
  
    date_time_psx <- as.character(date_time_psx[time_order])
    file_paths <- file_paths[time_order]
    if(!is.null(site_names)){
      site_names_vec <- site_names$names[time_order]
    }
  
  # bundle everything together
  if(is.null(site_names)){
    date_time <- tibble::tibble(file_paths = file_paths,
                                DateTimeOriginal = date_time_psx)
  } else {
    date_time <- tibble::tibble(file_paths = file_paths,
                                site_names = site_names_vec,
                                DateTimeOriginal = date_time_psx)
  }
  
  
  
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
    new_paths[[i]] <- date_time[unq_batch[i]:n_batch,]
  } else {
  new_paths[[i]] <- date_time[unq_batch[i]:c(unq_batch[i+1]-1),]
  }
}

# the location of the extra triggers
#  Extra triggers are times when there are more than n_photos_when_triggered
#  photos.
extra_trig <- rev(which(sapply(new_paths, nrow)> n_photos_when_triggered))
if(length(extra_trig)>0){
for(i in 1:length(extra_trig)){
    # this is the number of triggering events that should
    # have happened
    n_triggers <- ceiling(nrow(new_paths[[extra_trig[i]]]) / 
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
    temp[[j]] <- new_paths[[extra_trig[i]]][trigger_batches[[j]],]
    }
    # sneak temp into the correct location in new_paths
    if(extra_trig[i] == length(new_paths)){
      new_paths <- c(new_paths[1:c(extra_trig[i]-1)], 
                     temp)
    } else {
    new_paths <- c(new_paths[1:c(extra_trig[i]-1)], 
      temp, new_paths[c(extra_trig[i]+1):length(new_paths)])
    }
}
}
if(any(sapply(new_paths, is.null))){
  new_paths <- new_paths[!sapply(new_paths, is.null)]
}

# add blank rows if needed
.add_blank <- function(x, n_pho = n_photos_when_triggered){
  if(nrow(x) == n_pho){
    return(x)
  } else {
    difference <- n_pho- nrow(x)
    if(difference < 0) {
      stop("You have an error with the way your batch photos are being sorted.")
    }
    x[nrow(x) + difference, ] <- NA
    return(x)
  }
}
#n actual photos per trigger
if(!all(unlist(lapply(new_paths, nrow)) == n_photos_when_triggered)){
  new_paths <- lapply(new_paths, .add_blank)
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
new_photo_names <- lapply(new_paths, function(x)strsplit(x$file_paths, "/") ) 
# we need the last element from a list in a list
# this function can be used within sapply to get it
fn <- function(x) sapply(x, function(y){y[length(y)]})
# this keeps it in the same structure as new_paths
new_photo_names <- t(sapply(new_photo_names, fn))
# we need to collect the date time stuff in the same format
new_photo_dates <- t(sapply(new_paths, function(x) x$DateTimeOriginal))

} else {
  # if we just have 1 photo per trigger
  new_paths <- file_paths
  new_photo_names <- data.frame(photo_names, stringsAsFactors = FALSE)
  new_photo_dates <- matrix(date_time$DateTimeOriginal, ncol = 1, nrow = nrow(date_time))
}
# change /NA to NA again for photo dates
if(any(new_photo_dates == "/NA")){
  new_photo_dates[new_photo_dates == "/NA"] <- NA
}


# now, we want to copy the files over ~ 1000 files over to 
# a temporary directory. First we determine how many
# iterations of 1k photos we need to take through
# photo_names

  # integer division 
if(length(new_paths)>= 1000){
  n_iters <- ceiling(length(new_paths) / 1000)
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
  im_call <- " -background #808080  -crop 0x0+0-100 -resize 900x600 -quality 96 -interlace Plane -sampling-factor 4:2:0 -define jpeg:dct-method-float "
    }else{
      im_call <- " -resize 900x600 -quality 96 -interlace Plane -sampling-factor 4:2:0 -define jpeg:dct-method-float "
    }

if(.username == 'mason_uwi'){
  im_call <- " -background #808080   -crop 0x0+0-100 -resize 900x600! -quality 96 -interlace Plane -sampling-factor 4:2:0 -define jpeg:dct-method-float -splice 35x35 -font arial -pointsize 30 -fill black -annotate +185+30 1 -annotate +485+30 2 -annotate +785+30 3 -fill #cccccc -annotate +335+25 | -annotate +635+25 | -fill black -annotate +8+135 A -annotate +8+335 B -annotate +8+535 C -fill #cccccc -annotate 90x90+10+235 | -annotate 90x90+10+435 | "
}

# for loop to iterate through photos

# save these files so we can use them again
#  in the event that not everything gets uploaded.
saveRDS(new_photo_names, paste0(tmp_dir,"/photo_names.rds"))
saveRDS(new_photo_dates, paste0(tmp_dir,"/photo_dates.rds"))
saveRDS(new_paths, paste0(tmp_dir,"/photo_paths.rds"))


for(i in 1:n_iters){
  if(!havingIP()){
    stop("no internet connection")
  }
  cat(paste("\nBatch", i, "of", n_iters, "\n"))
  # make 1000 unique ids for all i less than n_iters
  if(i<n_iters){
    id <- seq(start,end,by=1 )
      }else{
        # make length(photo_names) unique ids when i == n_iter
        id <- seq(start,length(new_paths),by = 1)
        end <- length(new_paths)
      } # close ifelse statement
  # make the manifest
  # we need a number of columns equal to 1 + n_photos_when_triggers + 
  #  length of the datetime_columns object
  datetime_columns <- paste("#datetime", 1:n_photos_when_triggered, sep = "_")
  manifest <- data.frame(matrix(id, nrow = length(id), 
    ncol = 1 + n_photos_when_triggered + length(datetime_columns)))
  # id and then the file names
  
  colnames(manifest) <- c("id", 
    paste0(rep("image_", n_photos_when_triggered), 
      1:n_photos_when_triggered), datetime_columns)
  # put the new_photo_names in the manifest
  manifest[,grep("^image_\\w+$", colnames(manifest))] <- new_photo_names[id,]
  # put the date-time in the manifest
  manifest[,grep("^#datetime_\\w+$", colnames(manifest))] <- new_photo_dates[id,]
  
  # name of manifest file
  manifest_file_path <- paste0(tmp_dir,"/manifest_",i,".csv")
  
  # write the manifest to the tmp_dir
  write.csv(manifest, manifest_file_path , row.names = FALSE)
  if(resize){
  # make a progress bar
  pb <- txtProgressBar(min =start, max = end, style = 3)
  
  # go through and resize each photo and subject
  # if multiple photos per trigger
  cat('Resizing images\n')
  if(n_photos_when_triggered>1){
  for(subject in 1:length(id)){
    for(photo in 1:n_photos_when_triggered){
    
    # crop out the bushnell stuff, which takes up
    # 100 pixels on the bottom 
      if(new_paths[id[subject]][[1]][photo, "file_paths"] == "/NA") next
    system(paste0(pwq(im), pwq(new_paths[id[subject]][[1]][photo,"file_paths"]), im_call,
                  pwq(paste0(tmp_dir, "/", new_photo_names[id[subject],photo]), 
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
    

  # log in to 
    cat(paste0("\nCalling panoptes CLI (command line interface).\n",
              "Panoptes CLI does not have a progress bar for the upload process.\n"))
  system('panoptes configure', 
         input = c('https://www.zooniverse.org',.username ,.password),
         show.output.on.console = FALSE)
  
  # make the system call
  node_call <- paste0('panoptes subject-set upload-subjects --allow-missing ',
                      '-m image/jpg ',subject_set, ' ',
                      manifest_file_path)
  system(node_call)
  cat(paste0('\nSubjects uploaded. Batch ', i, ' of ',n_iters,' complete.\n'))

  
  if(delete_resized_post_upload){
    paste("Deleting resized photos (not originals)")
    file.remove(list.files(tmp_dir, full.names = TRUE))
    }
  }

  # modify the start and ends for the next batch to upload
  start <- end + 1
  end <- end + 1000
}


