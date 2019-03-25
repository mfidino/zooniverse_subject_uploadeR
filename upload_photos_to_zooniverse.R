#############################################
#
# Objects to change to run upload_photos_to_zooniverse.R
#
# load functions from zooniverse_subject_uploaderR.R
source("zooniverse_subject_uploader.R")

# Get the file information you want to upload to zooniverse
my_file_info <- get_fileinfo(folder_to_upload = "path/to/your/photos/folder",
                             photo_file_type = 'JPG',
                             max_group = 1,
                             search_subdirs = TRUE)

# collect the file paths of images within my_file_info$folder_to_upload
file_paths <- get_paths(my_file_info)

# If you have multiple triggers for capture event on a camera, you need
#  to collect the site names from the image.
site_names <- get_sitenames(file_paths, my_file_info)

# Collect the date / time information from photo exif data
my_dates <- get_datetime(file_paths, site_names)

# Bundle photos together for resizing
to_resize<- bundle_photos(my_dates, my_file_info)

# resize the photos
resize_photos(to_resize, my_file_info, 
              output = "output/folder/location", 
              crop_drop = TRUE, border = FALSE)

# upload the photos, this will open up a command prompt and execute
#  a batch file to upload each manifest file in your output folder.
upload_photos(output = "output/folder/location",
              subject_set = PUT_NUMERIC_ID_HERE)





