# zooniverse_subject_uploadeR

These scripts function as convenience wrappers for preparing images to be uploaded to a Zooniverse project via [panoptes-cli](https://github.com/zooniverse/panoptes-cli)

## Notes
- This is currently written to work for PCs 
- This will work for multiple photos per triggering event (i.e., you set a camera trap to take multiple photos when an animal comes across it)
- For multiple photos per triggering event pictures are grouped together if they occur within 5 seconds of each other. If no delay is set between possible triggering events there is the possibility that a large number of photos can be grouped together. When this occurs, they are split apart based off of how many photos per trigger should be expected, which the user supplies to the `max_groups` object when using `get_fileinfo()`.
- For multiple photos per triggering event we assume that the name of the image contains information about which site it was taken. An
example of this would be the photo name `"CHIL - HUP1 - FA17_00001.JPG"`. The program will iterate through all the photo names to find
 the longest matching string in photos names that do not end with numeric information. In the above example that would be `"CHIL - HUP1 - FA17"`. The trailing numbers will be removed if they end with `"_NUMBERS"` or `"(NUMBERS)"`.
- This will accomodate instances when there is variation in the number of photos per triggering event (e.g., 2 photos per trigger can be uploaded alongside 3 photos per trigger). Set `n_photos_when_triggered` to the maximum expected photos per trigger. For example, if you have between 1 and 3 photos per triggering event, set `n_photos_when_triggered` to 3. The maximum value that this can take is 9.

## Setup
panoptes-cli is written in Python, so it must be installed on your computer. Installing from [here](https://www.python.org/downloads/) will also download pip, which is also required.

You will also need to download [ImageMagick](http://www.imagemagick.org/script/index.php). Make sure to click the 'Install legacy utilities (e.g. convert)' box when you install it.  Version 7.0.2 was used during the creation of these scripts.

zooniverse_subject_uploadeR uses [Exiftool](https://sno.phy.queensu.ca/~phil/exiftool/) via the [exifr](https://github.com/paleolimbot/exifr) R package. As [Exiftool](https://sno.phy.queensu.ca/~phil/exiftool/) is written in Perl, you will need to download it if you have not. This can be done from [Strawberry Perl](http://strawberryperl.com/):[64-bit](http://strawberryperl.com/download/5.26.0.1/strawberry-perl-5.26.0.1-64bit.msi) (most users) or [32-bit](http://strawberryperl.com/download/5.26.0.1/strawberry-perl-5.26.0.1-32bit.msi) (advanced users).

## Use

The general flow of using the functions in `zooniverse_subject_uploadeR` follows as such:
```
# load functions from zooniverse_subject_uploaderR.R
source("zooniverse_subject_uploader.R")

# Get the file information you want to upload to zooniverse
my_file_info <- get_fileinfo(folder_to_upload = "path/to/your/photos/folder",
                             photo_file_type = 'JPG',
                             max_group = 1,
                             search_subdirs = TRUE,
                             seconds_between_triggers = 5)

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
              crop_drop = FALSE, border = FALSE)

# upload the photos, this will open up a command prompt and execute
#  a batch file to upload each manifest file in your output folder.
#  You need to collect the subject_set id from your zooniverse
#  project builder page. This batch script will also require you to enter
#  your zooniverse username and password.
upload_photos(output = "output/folder/location",
              subject_set = PUT_SUBJECT_SET_NUMBER_HERE)
```


## Finding the apporopriate subject ID

Along the lefthand side of the Project Builder page click on the appropriate subject set (or create a new one) under the SUBJECT SETS section. The subject sets number will be in the header of the page that is loaded.

![Imgur](http://i.imgur.com/JRDVYTA.png)





