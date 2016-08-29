#############################################
#
# Objects to change to run upload_photos_to_zooniverse.R
#
#
#

# the packages needed for zooniverse_subject_uploadeR.R to run
packages_required <- c("dplyr", "magrittr")

# the location of the folder of photos to resize.
folder_to_resize <- "put/path/name/to/folder/here" 

# Do you want to look through all nested folders within folder_to_resize?
search_subdirs <- TRUE

# REGEXP to search for jpg files
photo_file_type <- ".JPG$|.jpeg$" 

# Do you want to resize photos and copy them to a 
# temporary folder?
resize <- FALSE

# location of convert.exe in ImageMagick on PC
# Change this if convert.exe is located in some
# other folder.  This would be the default
# location if you download ImageMagick-7.0.2-Q16
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
# that you do not want to store on your hard drive. This
# clears the temporary directory after each upload of
#1000 photos.
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
username <- "your_username"
# and password
password <- "your_password"
# project number number
project <- NA
# your workflow number
workflow <- NA
# subject set, applies to all photos you are uploading
subject_set <- NA

# run zooniverse_subject_uploaderR.R
source("zooniverse_subject_uploader.R")


