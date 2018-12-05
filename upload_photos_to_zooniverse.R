#############################################
#
# Objects to change to run upload_photos_to_zooniverse.R
#
#
#




# How many photos are taken when a camera trap is triggered?
# This will determine if photos should be grouped within a subject
# on zooniverse.
n_photos_when_triggered <- 3

# If there are multiple photos per trigger the images will need to be
# sorted by site and then date / time. If you include the site name
# in the file name of the jpg and want to pull it from there set
# this to true. Note that the function that pulls this is slow
# because each file name must be compared to one another. 
pull_site_from_photo <- TRUE


# the location of the folder of photos to resize. This will search through
# folders nested inside of 'folder_to_resize' so long as search_subdirs
# is TRUE (default value). For example, the season folder FA12 may have
# multiple site folders inside of it. Set 'folder_to_resize' to the site
# folder if you want to upload all of those sites.
folder_to_resize <- "put/path/to/folder/here" 

# Do you want to look through all nested folders within folder_to_resize?
search_subdirs <- TRUE

# are there any sub-folders that you want to skip?
# provide a character vector of the site names you
# would like to skip

subfolders_to_skip <- NULL



# REGEXP to search for jpg files
photo_file_type <- ".JPG$|.jpeg$" 

# Do you want to resize photos and copy them to a 
# temporary folder?
resize <- TRUE

# location of convert.exe in ImageMagick on PC
# Change this if convert.exe is located in some
# other folder.  This would be the default
# location if you download ImageMagick-7.0.2-Q16
im <- "C:\\Program Files\\ImageMagick-7.0.2-Q16\\convert.exe"

# temporary directory to to store resized photos,
# it will create the directory if it does not already exist.
tmp_dir <- "C:/tmp_resize_test"

# Drop 100 bottom pixels from photos? 
# useful for removing date/time stamps from Bushnell camera traps.
crop_drop  <- FALSE

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


