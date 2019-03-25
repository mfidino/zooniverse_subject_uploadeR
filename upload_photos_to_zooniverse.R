#############################################
#
# Objects to change to run upload_photos_to_zooniverse.R
#

my_file_info <- get_fileinfo(folder_to_upload = "C:/Users/mfidino/Desktop/cww_test",
                             photo_file_type = 'JPG',
                             max_group = 1,
                             search_subdirs = TRUE)

file_paths <- get_paths(my_file_info)
site_names <- get_sitenames(file_paths, my_file_info)
my_dates <- get_datetime(file_paths, site_names)

to_resize<- bundle_photos(my_dates, my_file_info)

resize_photos(to_resize, my_file_info, 
              output = "C:/users/mfidino/folder_test", crop_drop = TRUE, border = FALSE)



# run zooniverse_subject_uploaderR.R
source("zooniverse_subject_uploader.R")


