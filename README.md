# zooniverse_subject_uploadeR
initial commit
*Zooniverse subject uploadeR*

An R script that resizes camera trap photos via ImageMagick and uploads them as subjects for projects built on zooniverse.org (Panoptes) via [panoptes-subject-uploader](https://github.com/zooniverse/panoptes-subject-uploader).  

## Notes
- This is currently built for 1 photo per subject
- This is written to work for PCs (sorry, I don't have a Mac)

## Setup
panoptes-subject-uploader requires you to download a previous version of Node.js (version >=0.10.36). [Link to version 0.10.35](https://nodejs.org/download/release/v0.10.35/). For Windows you will want to download the .msi file.

You will also need to download [ImageMagick](http://www.imagemagick.org/script/index.php). Make sure to click the Install legacy utilities box (e.g. `convert`) when you install it.  Version 7.0.2 was used during the creation of these scripts.

Following this, you can either [fork](https://help.github.com/articles/fork-a-repo/) this repository or copy and paste these scripts into your favorite text editor and save them.

## Use

The main script you to work with to upload photos is `upload_photos_to_zooniverse.R`. There are a number of objects in there that you will have to alter for your project that are then plugged into the `zooniverse_subject_uploadeR.R` script.  The purpose of each of the objects in `upload_photos_to_zooniverse.R` are commented out so you can determine what they do.

After reassinging the obhjects in `upload_photos_to_zooniverse.R` you can source `zooniverse_subject_uploadeR.R` (the last line of code in `upload_photos_to_zooniverse.R`) assuming that it is in your current working directory to start the image resizing and upload process. If there are any missing objects that are needed for uploading it should spit back a relevant error.

The first progress bar is for resizing a batch of photos. The additional data printed in the `R` console is from the upload process via `panoptes-subject-uploader`. This script will batch process any number of photos, but does it in multiples of 1000 (the last batch is whatever is leftover).

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





