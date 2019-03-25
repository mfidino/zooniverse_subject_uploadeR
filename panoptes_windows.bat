@ECHO OFF
cd %1
ECHO This is a batch script to use panoptes cli.
panoptes configure
FOR %%i IN (%1/manifest_*) DO panoptes subject-set upload-subjects --allow-missing -m image/jpg %2 %%i
