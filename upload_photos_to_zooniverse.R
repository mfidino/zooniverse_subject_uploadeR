#############################################
#
# Objects to change to run upload_photos_to_zooniverse.R
#

my_file_info <- list(folder_to_upload = "Z:/TransectTrailCamPics/SP17/DPT/D09-VTC1",
                     photo_file_type = 'JPG',
                     search_subdirs = TRUE,
                     max_group = 1)

file_paths <- get_paths(my_file_info)
site_names <- get_site_names(file_paths, my_file_info)
my_dates <- get_datetime(file_paths, site_names)

to_resize<- bundle_photos(my_dates, my_file_info)

resize_photos(to_resize, my_file_info, 
              output = "C:/users/mfidino/folder_test", crop_drop = TRUE, border = FALSE)



# run zooniverse_subject_uploaderR.R
source("zooniverse_subject_uploader.R")


