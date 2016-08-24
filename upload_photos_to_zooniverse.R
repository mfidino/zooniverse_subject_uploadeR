#############################################
#
# Objects to change to run upload_photos_to_zooniverse.R
#
#
#

# the packages needed for zooniverse_subject_uploadeR.R to run
packages_required <- c("dplyr", "magrittr")

# the location of the folder of photos to resize.
folder_to_resize <- "Z:/TransectTrailCamPics/FA14/JNT" 

# Do you want to look through all nested folders within folder_to_resize?
search_subdirs <- TRUE

# REGEXP to search for jpg files
photo_file_type <- ".JPG$|.jpeg$" 

# location of convert.exe in ImageMagick on PC
im <- "C:\\Program Files\\ImageMagick-7.0.2-Q16\\convert.exe"

# temporary directory to to store resized photos,
# it will create the directory if it does not already exist.
tmp_dir <- "C:/tmp_resize"

# Drop 100 bottom pixels from photos? 
# useful for removing date/time stamps from Bushnell camera traps.
crop_drop  <- TRUE

# Do you want to upload the photos?
upload <- TRUE

# Do you want to delete resized photos after uploading?
# would be a good idea if you are uploading a huge amount of photos
# that you do not want to store on your hard drive. This effectively
# clears the temporary directory each batch.
delete_resized_post_upload <- FALSE

#####################################
### for panoptes-subject-uploader ###
#####################################

# all of the numbers associated to subject-set, workflow, etc.
# can be gathered on your project builder page for a project.

# project # is in the top left of pretty much every page
# subject set # is a header at the top of a subject_set you click on.
# workflow # is a header of the workflow you click on.

# your username
username <- "mason_UWI"
# and passowrd
password <- "Parulida3!"
# project number number
project <- 2990
# your workflow number
workflow <- 2334
# subject set, applies to all photos you are uploading
subject_set <- 5801

# run zooniverse_subject_uploaderR.R
source("zooniverse_subject_uploader.R")

