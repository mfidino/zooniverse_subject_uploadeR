# zooniverse_subject_uploadeR
initial commit
*Zooniverse subject uploadeR*

An R script that resizes camera trap photos via ImageMagick and uploads them as subjects for projects built on zooniverse.org (Panoptes) via panoptes-subject-uploader.  

## Notes
- This is currently built for 1 photo per subject
- This is written to work for PCs (sorry, I don't have a Mac)

## Setup
panoptes-subject-uploader requires you to download a previous version of Node.js (version >=0.10.36). [Link to version 0.10.35](https://nodejs.org/download/release/v0.10.35/). For Windows you will want to download the .msi file.

You will also need to download [ImageMagick](http://www.imagemagick.org/script/index.php). Make sure to click the Install legacy utilities box (e.g. `convert`) when you install it.
