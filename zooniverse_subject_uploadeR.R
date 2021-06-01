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


###########################
# get_fileinfo
###########################


get_fileinfo <- function(folder_to_upload = NULL,
                         photo_file_type = NULL,
                         search_subdirs = TRUE,
                         max_group = NULL,
                         seconds_between_triggers = 5){
  # check that the folder exists
  if(!file.exists(folder_to_upload)){
    stop(paste(folder_to_upload, "does not exist. Change input given to 'folder_to_upload'."))
  }
  photo_file_type <- switch(tolower(photo_file_type),
                      'jpg'  = ".JPG$|.jpeg$",
                      'jpeg' = ".JPG$|.jpeg$",
                      'png'  = ".PNG|.png")
  if(is.null(photo_file_type)){
    err <- paste0('fileinfo$photo_file_type must take one of the following values:\n',
                  "\t- 'jpg'\n",
                  "\t- 'jpeg'\n",
                  "\t- 'JPG'\n",
                  "\t- 'png'\n",
                  "\t- 'PNG'")
    stop(err)
  }
  if(!is.logical(search_subdirs)){
    stop("search_subdirs must be a logical statement (T/F or TRUE/FALSE).\nCurrent entry: ", search_subdirs)
  }
  if(!is.numeric(max_group)){
    stop("max_group must be numeric.\n Current entry: ", max_group)
  }
  if(max_group > 9){
    stop("max_group cannot be greater than 9.")
  }

  return(list(folder_to_upload = folder_to_upload,
              photo_file_type = photo_file_type,
              max_group = max_group,
              search_subdirs = search_subdirs,
              seconds_between_triggers = seconds_between_triggers))


}


###########################
# get_paths
###########################

get_paths <- function(fileinfo = NULL){
  if(!is.list(fileinfo) |
     any(
       !c('folder_to_upload', 'photo_file_type',
            'search_subdirs', 'max_group') %in%
       names(fileinfo)
     )
  ){
    err <- paste0('Use get_fileinfo() to construct fileinfo argument.\n\n',
                  'EXAMPLE:\n\n',
                  "my_fileinfo <- get_fileinfo(folder_to_upload = 'file/path/to/photos/here',\n",
                  "                     photo_file_type = 'jpg',\n",
                  "                     max_group = 1,\n",
                  "                     search_subdirs = TRUE)")
    stop(err)
  }
  if(!fileinfo$photo_file_type %in% c(".JPG$|.jpeg$", ".PNG|.png")){
    stop("fileinfo$photo_file_type incorrect. Use get_fileinfo() to make fileinfo argument." )
  }
  file_paths <- unlist(list.files(path = fileinfo$folder_to_upload,
                                  pattern = fileinfo$photo_file_type,
                                  recursive = fileinfo$search_subdirs,
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



#if(.username == "mason_uwi"){
#if(class(human_images) == "data.frame"){
#
#  human <- human_images[human_images$has_human == TRUE,]
#
#  # get just the photo_name
#  p_name_human <- strsplit(human$human_photos, "/") %>%
#    sapply(., function(x) x[length(x)]) %>% tolower
#
#  p_name_images <- strsplit(file_paths , "/") %>%
#    sapply(., function(x) x[length(x)]) %>% tolower
#
#  human_to_go <- which(p_name_images %in% p_name_human)
#  if(length(human_to_go)> 0){
#    file_paths <- file_paths[-human_to_go]
#  }
#
#
#}
#}

#################################
# get_sitenames
#################################

get_sitenames <- function(file_paths = NULL, fileinfo = NULL){
  # check file_paths
  if(is.null(file_paths)){
    stop("please supply file_paths to this function.")
  }
  if(!is.character(file_paths)){
    err <- paste0('file_paths must be a character vector.\n\n',
                  'file_paths can easily be generated with get_paths().\n\n',
                  'EXAMPLE:\n\n',
                  '# create fileinfo\n',
                  "my_fileinfo <- get_fileinfo(folder_to_upload = 'file/path/to/photos/here',\n",
                  "                     photo_file_type = 'jpg',\n",
                  "                     max_group = 1,\n",
                  "                     search_subdirs = TRUE)\n\n",
                  '# collect file paths\n',
                  'my_files <- get_paths(my_fileinfo)\n\n',
                  '# collect site names\n',
                  'my_sites <- get_sites(my_files, my_fileinfo)')
    stop(err)
  }
  # check fileinfo
  if(is.null(fileinfo)){
    stop("please supply fileinfo to this function.")
  }
  if(!is.list(fileinfo) |
     any(
       !c('folder_to_upload', 'photo_file_type',
          'search_subdirs', 'max_group') %in%
       names(fileinfo)
     )
  ){
    err <- paste0('Use get_fileinfo() to construct fileinfo argument.\n\n',
                  'EXAMPLE:\n\n',
                  "my_fileinfo <- get_fileinfo(folder_to_upload = 'file/path/to/photos/here',\n",
                  "                     photo_file_type = 'jpg',\n",
                  "                     max_group = 1,\n",
                  "                     search_subdirs = TRUE)")
    stop(err)
  }

  # get just the file names
  photo_names <- strsplit(file_paths, "/") %>%
    sapply(FUN = function(x){x[length(x)]})

  site_names <- gsub(fileinfo$photo_file_type, "", photo_names)

  # drop numerics if the trail like 'site_0001', 'site_0002'

  if(length(grep("_\\w+$", site_names)) > 0 ){
    site_names <- gsub("_(?!.*_)\\w+$","",site_names, perl = TRUE)
  }

  # drop () if they are named that way
  if(length(grep("\\s\\(\\w+\\)?$", site_names)) > 0) {
    site_names <- gsub("\\s\\(\\w+\\)?$","",site_names)
  }

  # create a summary of the sites
  site_summary <- t(t(sort(table(site_names), decreasing = TRUE)))
  site_summary <- tibble::tibble(site_names = row.names(site_summary),
                             count = site_summary[,1])

  print(site_summary)

  return(list(names = site_names,
              summary = site_summary))
}

##############################
# get_datetime
##############################

get_datetime <- function(file_paths = NULL, site_names = NULL){
  # check file_paths
  if(is.null(file_paths)){
    stop("please supply file_paths to this function.")
  }
  if(!is.character(file_paths)){
    err <- paste0('file_paths must be a character vector.\n\n',
                  'file_paths can easily be generated with get_paths().\n\n',
                  'EXAMPLE:\n\n',
                  '# create fileinfo\n',
                  "my_fileinfo <- get_fileinfo(folder_to_upload = 'file/path/to/photos/here',\n",
                  "                            photo_file_type = 'jpg',\n",
                  "                            search_subdirs = TRUE\n",
                  "                            max_group = 1)\n\n",
                  '# collect file paths\n',
                  'my_files <- get_paths(my_fileinfo)\n\n',
                  '# collect site names\n',
                  'my_sites <- get_sites(my_files, my_fileinfo)')
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
  return(date_time)
}

# number of photos in upload
#n_batch <- length(date_time_psx)

##########################
# bundle photos
##########################

bundle_photos <- function(date_time = NULL, fileinfo = NULL){
  if(!is.list(fileinfo) |
     any(!c('folder_to_upload', 'photo_file_type',
            'search_subdirs', 'max_group') %in%
         names(fileinfo)
     )
  ){
    err <- paste0('Use get_fileinfo() to construct fileinfo argument.\n\n',
                  'EXAMPLE:\n\n',
                  "my_fileinfo <- get_fileinfo(folder_to_upload = 'file/path/to/photos/here',\n",
                  "                     photo_file_type = 'jpg',\n",
                  "                     max_group = 1,\n",
                  "                     search_subdirs = TRUE)")
    stop(err)
  }
  # where does the first photo start
  date_time_psx <- diff(as.POSIXct(date_time$DateTimeOriginal))
  units(date_time_psx) <-  'secs'
  date_time_psx <- as.numeric(date_time_psx)

  # all photos not within 5 seconds of one another are
  #  grouped together
  unq_batch <- c(0,which(date_time_psx>fileinfo$seconds_between_triggers | date_time_psx <0)) + 1

  new_paths <- vector("list", length(unq_batch))
  for(i in 1:length(unq_batch)){
    if(i == length(unq_batch)){
      new_paths[[i]] <- date_time[unq_batch[i]:nrow(date_time),]
    } else {
      new_paths[[i]] <- date_time[unq_batch[i]:c(unq_batch[i+1]-1),]
    }
  }
  # check if we need to split bundles
  if(any(sapply(new_paths, nrow) > fileinfo$max_group)){
    new_paths <- split_bundles(new_paths = new_paths, fileinfo = fileinfo)
  }
  # check if we need to add any NA
  if(!all(sapply(new_paths, nrow) == fileinfo$max_group)){
    .add_blank <- function(x, n_pho = fileinfo$max_group){
      if(nrow(x) == n_pho){
        return(x)
      } else {
        difference <- n_pho - nrow(x)
        if(difference < 0) {
          stop("You have an error with the way your batch photos are being sorted.")
        }
        x[nrow(x) + difference, ] <- NA
        return(x)
      }
    }
    new_paths <- lapply(new_paths, .add_blank)
  }

  # splits by the forward slash
  # used for image names on zooniverse
  new_photo_names <- lapply(new_paths, function(x)strsplit(x$file_paths, "/") )
  # we need the last element from a list in a list
  # this keeps it in the same structure as new_paths
  new_photo_names <- sapply(new_photo_names, function(x) sapply(x, function(y){y[length(y)]})) %>%
    matrix(., ncol = fileinfo$max_group, nrow = length(new_paths), byrow = TRUE)

  # we need to collect the date time stuff in the same format
  new_photo_dates <- sapply(new_paths, function(x) as.character(x$DateTimeOriginal)) %>%
    matrix(., ncol = fileinfo$max_group, nrow = length(new_paths), byrow = TRUE)

  to_return <- list(paths = lapply(new_paths, function(x)x$file_paths),
                    names = new_photo_names,
                    times = new_photo_dates)
  class(to_return) <- "prepped_paths"
  return(to_return)
}

##################################
# split_bundles
##################################

split_bundles <- function(new_paths = NULL, fileinfo = NULL){

  # it is imperative we do this backwards via rev so that our
  #  placement of these new bundles can be correctly indexed.
  extra_trig <- rev(which(sapply(new_paths, nrow)> fileinfo$max_group))
    for(extra in 1:length(extra_trig)){
      # this is the number of triggering events that should
      # have happened
      n_triggers <- ceiling(nrow(new_paths[[extra_trig[extra]]]) / fileinfo$max_group)
      # make a vector and split it into equal max_group parts
      # if there are leftovers (e.g., a 2 photo batch when there should be 3)
      # then the last trigger is assumed to be the issue. This will likely
      # be the case (considering the camera would have to finish it's
      # first triggering event before a second one started)
      trigger_batches <- split(seq(1, fileinfo$max_group * n_triggers),
                               ceiling(seq(1, fileinfo$max_group * n_triggers)/
                                         fileinfo$max_group))
      # temporary list to hold the file paths
      temp <- vector("list", length = n_triggers)
      for(j in 1:length(temp)){
        temp[[j]] <- new_paths[[extra_trig[extra]]][trigger_batches[[j]],]
      }
      # sneak temp into the correct location in new_paths
      # if it's the last record put it in the end
      if(extra_trig[extra] == length(new_paths)){
        new_paths <- c(new_paths[1:c(extra_trig[extra]-1)],
                       temp)
      } else if(extra_trig[extra] == 1){
        # if it is the first record put it at the start
        new_paths <- c(temp, new_paths[2:length(new_paths)])
      } else{
        # otherwise, sneak it in
        new_paths <- c(new_paths[1:c(extra_trig[extra]-1)],
                       temp, new_paths[c(extra_trig[extra]+1):length(new_paths)])
      }
    }


  if(any(sapply(new_paths, is.null))){
    new_paths <- new_paths[!sapply(new_paths, is.null)]
  }

  # add blank rows if needed

  return(new_paths)
}




# now, we want to copy the files over ~ 1000 files over to
# a temporary directory. First we determine how many
# iterations of 1k photos we need to take through
# photo_names

  # integer division


resize_photos <- function(to_resize = NULL, fileinfo = NULL, output = NULL,
                          crop_drop = FALSE, border = FALSE){
  im <- system("where /r \"C:\\Program Files\"  convert.exe", intern = TRUE)
  if(length(im) == 0){
    stop("convert.exe not found in imageMagick.")
  }
  if(class(to_resize) != "prepped_paths"){
    err <- paste0("to_resize must be of class 'prepped_paths'.\n\n",
                  "Create to_resize with bundle_photos().\n\n",
                  "EXAMPLE:\n\n",
                  'my_resize <- bundle_photos(date_time = my_dates, fileinfo = my_fileinfo)\n',
                  'resize_photos(my_resize, my_fileinfo)')
    stop(err)
  }
  if(is.null(output)){
    err <- paste0("the output argument is required. ",
      "Specify file path for resized photos and manifests.")
    stop(err)
  }

  # create tmp_dir if it does not already exist
  output <- normalizePath(output, mustWork = FALSE)
  if (file.exists(output)) {
    cat("output folder exists")
  } else if (file.exists(output)) {
    stop("output exists as a file. You cannot write photos into a file.")
    # you will probably want to handle this separately
  } else {
    cat("output folder does not exist - creating")
    dir.create(file.path(output))
  }


  # figure out which image magick call to make.
  if(crop_drop & !border){
    # only crop drop
    im_call <- " -background #808080  -crop 0x0+0-100 -resize 900x600! -quality 96 -interlace Plane -sampling-factor 4:2:0 -define jpeg:dct-method-float "
  }else if(!crop_drop & !border){
    # just resize
    im_call <- " -resize 900x600! -quality 96 -interlace Plane -sampling-factor 4:2:0 -define jpeg:dct-method-float "
  }  else if(!crop_drop & border){
    # no crop, add border
    im_call <- " -background #808080 -resize 900x600! -quality 96 -interlace Plane -sampling-factor 4:2:0 -define jpeg:dct-method-float -splice 35x35 -font arial -pointsize 30 -fill black -annotate +185+30 1 -annotate +485+30 2 -annotate +785+30 3 -fill #cccccc -annotate +335+25 | -annotate +635+25 | -fill black -annotate +8+135 A -annotate +8+335 B -annotate +8+535 C -fill #cccccc -annotate 90x90+10+235 | -annotate 90x90+10+435 | "
  } else if(crop_drop & border){
    im_call <- " -background #808080   -crop 0x0+0-100 -resize 900x600! -quality 96 -interlace Plane -sampling-factor 4:2:0 -define jpeg:dct-method-float -splice 35x35 -font arial -pointsize 30 -fill black -annotate +185+30 1 -annotate +485+30 2 -annotate +785+30 3 -fill #cccccc -annotate +335+25 | -annotate +635+25 | -fill black -annotate +8+135 A -annotate +8+335 B -annotate +8+535 C -fill #cccccc -annotate 90x90+10+235 | -annotate 90x90+10+435 | "
  }

  # resizing images

  # figure out how many batches to do

  if(length(to_resize$paths)>= 500){
    n_iters <- ceiling(length(to_resize$paths) / 500)
  }else{
    n_iters <- 1
  }

  # set up to grab the first 500 photos, these
  # objects get updated throughout the for loop below.
  start <- 1
  end <- 500

  for(i in 1:n_iters){
    cat(paste("\nPreparing manifest", i, "of", n_iters, "\n"))
    # make 1000 unique ids for all i less than n_iters
    if(i<n_iters){
      id <- seq(start,end,by=1 )
    }else{
      # make length(photo_names) unique ids when i == n_iter
      id <- seq(start,length(to_resize$paths),by = 1)
      end <- length(to_resize$paths)
    } # close ifelse statement
    # make the manifest
    # we need a number of columns equal to 1 + n_photos_when_triggers +
    #  length of the datetime_columns object
    datetime_columns <- paste("#datetime", 1:fileinfo$max_group, sep = "_")
    manifest <- data.frame(matrix(id, nrow = length(id),
                                  ncol = 1 + fileinfo$max_group + length(datetime_columns)))
    # id and then the file names

    colnames(manifest) <- c("id",
                            paste0(rep("image_", fileinfo$max_group),
                                   1:fileinfo$max_group), datetime_columns)
    # put the new_photo_names in the manifest
    manifest[,grep("^image_\\w+$", colnames(manifest))] <- to_resize$names[id,]
    # put the date-time in the manifest
    manifest[,grep("^#datetime_\\w+$", colnames(manifest))] <- to_resize$times[id,]

    # name of manifest file
    manifest_file_path <- paste0(output,"/manifest_",i,".csv")

    # write the manifest to the tmp_dir
    write.csv(manifest, manifest_file_path , row.names = FALSE)
      # go through and resize each photo and subject
      # if multiple photos per trigger
      if(fileinfo$max_group>1){
        for(subject in 1:length(id)){
          for(photo in 1:fileinfo$max_group){

            # crop out the bushnell stuff, which takes up
            # 100 pixels on the bottom
            if(is.na(to_resize$paths[id[subject]][[1]][photo])) next
            system(paste0(pwq(im), pwq(to_resize$paths[id[subject]][[1]][photo]), im_call,
                          pwq(paste0(output, "/", to_resize$names[id[subject],photo]),
                              space = FALSE)))
          }
        }
      } else { # if only one photo per trigger
        for(subject in 1:length(id)){
          system(paste0(pwq(im), pwq(to_resize$paths[id[subject]]), im_call,
                        pwq(paste0(output, "/", to_resize$names[id[subject],]),
                            space = FALSE)))
        }
      }
      # modify the start and ends for the next batch to upload
      start <- end + 1
      end <- end + 500
    }
  }
# make it a bat file
# FOR %i IN (manifest_*) DO panoptes subject-set upload-subjects --allow-missing -m image/jpg 74127 %i

cleanup_manifests <- function(output){
  # get the manifests
  m <- list.files(
    output,
    pattern = "manifest_",
    full.names = TRUE
  )
  my_pb <- txtProgressBar(0, length(m))
  # loop through them and modify as needed.
  for(i in 1:length(m)){
    setTxtProgressBar(my_pb, value = i)
    tmp <- read.csv(
      m[i],
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    # get the count of number of images in the file
    nimages <- rowSums(
      !is.na(
        tmp[,grep("image_", colnames(tmp))]
      )
    )
    # go to the next manifest if all of them have the
    #  same number of images.
    if(length(unique(nimages)) == 1){
      next
    } else {
    # split the dataframe based on the count
    new_manifest <- split(
      tmp,
      factor(nimages)
    )
    # go through and rename the files so they are all unique
    for(j in 1:length(new_manifest)){
      new_path <- gsub(
        "_(\\w+?)",
        paste0("_\\1_",j),
        m[i]
      )
      write.csv(
        new_manifest[j],
        new_path,
        row.names = FALSE
      )
    }
    file.remove(m[i])
    }
  }

}

upload_photos <- function(output = NULL, subject_set = NULL){
  output <- normalizePath(output, winslash = "/")
  sink('panoptes_windows.bat')
  cat(
  paste0("@ECHO OFF\n",
         "cd %1\n",
         "ECHO This is a batch script to use panoptes cli.\n",
         #"panoptes configure\n",
         "FOR %%i IN (%1/manifest_*) DO panoptes subject-set upload-subjects --allow-missing -m image/jpg %2 %%i" ),
  fill = TRUE, sep = ""
  )
  sink()
  shell(paste('start cmd.exe @cmd /k panoptes_windows.bat', output, subject_set))
}





