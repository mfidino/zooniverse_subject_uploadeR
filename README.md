# zooniverse_subject_uploadeR

An R script that resizes camera trap photos via ImageMagick and uploads them as subjects for projects built on zooniverse.org (Panoptes) via [panoptes-subject-uploader](https://github.com/zooniverse/panoptes-subject-uploader).  

## Notes
- This is written to work for PCs 
- This will now work for multiple photos per triggering event (i.e., you set a camera trap to take multiple photos when an animal comes across it)
- For multiple photos per triggering event pictures are grouped together if they occur within 5 seconds of each other. If no delay is set between possible triggering events there is the possibility that a large number of photos can be grouped together. When this occurs, they are split apart based off of how many photos per trigger should be expected, which the user supplies to the `n_photos_when_triggered` object in `upload_photos_to_zooniverse.R`.
- This will accomodate instances when there is variation in the number of photos per triggering event (e.g., 2 photos per trigger can be uploaded alongside 3 photos per trigger). Set `n_photos_when_triggered` to the maximum expected photos per trigger. For example, if you have between 1 and 3 photos per triggering event, set `n_photos_when_triggered` to 3.

## Setup
panoptes-subject-uploader requires you to download Node.js (any version >=0.10.36). [Link to node.js site](https://nodejs.org/en/). 

You will also need to download [ImageMagick](http://www.imagemagick.org/script/index.php). Make sure to click the 'Install legacy utilities (e.g. convert)' box when you install it.  Version 7.0.2 was used during the creation of these scripts.

zooniverse_subject_uploadeR uses [Exiftool](https://sno.phy.queensu.ca/~phil/exiftool/) via the [exifr](https://github.com/paleolimbot/exifr) R package. As [Exiftool](https://sno.phy.queensu.ca/~phil/exiftool/) is written in Perl, you will need to download it if you have not. This can be done from [Strawberry Perl](http://strawberryperl.com/):[64-bit](http://strawberryperl.com/download/5.26.0.1/strawberry-perl-5.26.0.1-64bit.msi) (most users) or [32-bit](http://strawberryperl.com/download/5.26.0.1/strawberry-perl-5.26.0.1-32bit.msi) (advanced users).

Aside from [exifr](https://github.com/paleolimbot/exifr), this software also uses the [dplyr](https://github.com/tidyverse/dplyr) and [magrittr](https://github.com/tidyverse/magrittr) packages. If you do not have these R packages downloaded zooniverse_subject_uploadeR will do it for you.

Following this, you can either fork this repository or copy and paste these scripts into your favorite text editor and save them.

## Use

The main script that will be changed in order to upload photos is `upload_photos_to_zooniverse.R`. There are a number of objects in there that you will have to alter for your project that are then plugged into the `zooniverse_subject_uploadeR.R` script.  The purpose of each of the objects in `upload_photos_to_zooniverse.R` are commented out so you can determine what they do. Do not change the object names or `zooniverse_subject_uploadeR.R` will not work (just change the objects values).

After reassinging the objects in `upload_photos_to_zooniverse.R` you can source `zooniverse_subject_uploadeR.R` (the last line of code in `upload_photos_to_zooniverse.R`). Assuming that both scripts are in your current working directory R will commence the image resizing and upload process. If there are any missing objects that are needed for uploading it should spit back a relevant error.

The first progress bar you see in the `R` console is the progress made on resizing a batch of photos. The additional data printed in the `R` console following this is from the upload process via `panoptes-subject-uploader`. This script will batch process any number of photos, but does it in multiples of 1000 (the last batch is whatever is leftover).

Before uploading photos you may want to resize a test batch to make sure that the size of photos is < 600 kb. The current ImageMagick call does a sufficient job for camera trap images collected with Bushnells. 

## Finding the apporopriate subject id's, workflows, etc.

These numbers need to be added to `upload_photos_to_zooniverse.R` as a numeric object. (e.g. `workflow <- 1234`).  You can find the appropriate numbers on that particular projects Project Builder homepage.

### Project number
On your Project Builder homepage for a particular project. The project number is in the top left corner (e.g. Chicago Wildlife Watch is project 2990).

![Imgur](http://i.imgur.com/1ofQgDu.png)

### Workflow number
Along the lefthand side of the Project Builder page click on the appropriate workflow under the WORKFLOWS section that you want to upload subjects to. The workflow number will be in the header of the page that is loaded.

![Imgur](http://i.imgur.com/HpFCu1h.png)

### Subject id
Along the lefthand side of the Project Builder page click on the appropriate subject set (or create a new one) under the SUBJECT SETS section. The subject sets number will be in the header of the page that is loaded.

![Imgur](http://i.imgur.com/JRDVYTA.png)





