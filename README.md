# DiplomaThesis
Diploma Thesis (2017) in Archaeology at Masaryk University Brno, CZ.

The repository contains DATA and R SCRIPTS used in the diploma thesis.
Focus of my diploma thesis is morphometric analysis of neolithic stone tools shapes and 
the covariance of shape with other variables (like use-wear etc.).

Full text at official Uni repository (add link)

Folder Data contains input data, folder Scripts contains .r scripts and folder Archive contains backup data achieved through the analysis

### DATA

+ Db.BASE.csv - csv file with basic database, data not manipulated
+ Db.PRIME.csv - csv file with database used for most of the analysis, some data manipulation, some editing
+ Db.Lok.csv - csv of individual sites

### SCRIPTS

+ AllShapes - script containing most of the stuff done on "all shapes"
+ Adzes.r - script to perform morphometrics on adzes
+ IndSites.r - performing PCA and hierarchical clustering for individual sites
+ Mapa.r - script to create maps
+ Comp.r - comparison of two small datasets of shapes (created with diff. techniques: handdrawn (Orig) vs scanned (Lit))

### ARCHIVE

sub Adzes -- contains data for adzes
 + Dt.in files - files containig shape on import
 + Dt.out files - outlines of shape
 + Dt.ef files - efourier coefficients
 + Dt.pc files - files containing pca coefficients

License: CC-BY-4.0
https://creativecommons.org/licenses/by/4.0/
