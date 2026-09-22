# hmd (development version)

## hmd Package Changelog
### Version 0.0.0.1 (2026-06-03)
* Package creation date
* Added license, namespace, description file, news
* Added many functions
* Created documentation using roxygen2
* Added package dependencies
* Initial upload to github

### Version 0.0.0.2 (2026-06-04)
* Minor update to classifyHMD()
* Update to trackFun() to replace movement metrics each time (instead of adding columns)

### Version 0.0.0.3 (2026-06-05)
* Added new function createBldgTiles() to download building tiles from Microsoft's planetary computer
* Fixed syntax error in DESCRIPTION file

### Version 0.0.0.4 (2026-08-21)
* Updated documentation and script formatting
* Fixed bugs in flagRemoval() related to data.table conversion of functions

### Version 0.0.0.5 (2026-09-04)
* Modified trackFun to calculate non-movements as having a turn angle of 0
* Slight other improvements to trackFun
* Updated trackFun documentation to reflect changes
* Updated classifyHMD to incorporate changes to trackFun

### Version 0.0.0.6 (2026-09-16)
* Modified workflow to ensure that a unique OID is maintained from flagAssignment to classifyHMD functions
* Added OID.POINT to flagAssignment function to ensure unique ID is maintained
* Added the calculateIndices function for calculating unique IDs
* Continued modification of functions for alignment with data.table syntax, especially in flagAssignment
* Added start and end times to flagAssignment and flagRemoval
* Speed and efficiency improvements to classifyHMD, especially relating to initial data loading

### Version 0.0.0.7 (2026-09-17)
* Additional fixes to classifyHMD to fix bugs created by previous changes
* Fixes to enable parallel processing
* Added additional text to diagnose parallel processing issues (these will be removed in a future update)

### Version 0.0.0.8 (2026-09-17)
* The parallel processing issue may be related to how rasters are handled in the terra package and its interaction with parallel processing. 
* As such, raster loading is added inside the parallel processing loop. 
* This fixed the issues. Removing additional text to diagnose issues and updating parallel processing documentation to state that it is now possible for up to 8 cores
* Added a file moving code to classifyHMD to remove cleaned files to classified folder
* Moved classifyHMD's internal functions outside of classifyHMD. This may break the function. Will need to check when needing to run again. 

### Version 0.0.0.9 (2026-09-18) 
* Added the summarizeHMD function and associated documentation
* summarizeHMD requires some additional modification including adding OIDs to summarized data.
* Added summarizeHMD's internal functions as standalone functions. They are untested but should work within the standard pipeline. 

### Version 0.0.0.10 (2026-09-22)
* Updated summarizeHMD function documentation to describe some of the metrics outputted. 
* Updated documentation to properly link to external functions. 
* Updated internal function calculateIndices to allow index calculation for summaries to be used within summarizeHMD. 
* Tested and fixed bugs in summarizeHMD internal functions. 
* Added the combineIndices function to create a bridging file for points and track data. 

