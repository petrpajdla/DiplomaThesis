# DiplomaThesis
Diploma Thesis (2017) in Archaeology at Masaryk University Brno, CZ.
Topic: Early Neolithic polished stone tools analysis

The repository contains DATA and R SCRIPTS used in the diploma thesis.
Focus of my diploma thesis is morphometric analysis of neolithic stone tools shapes and 
the covariance of shape with other variables (like use-wear etc.)

Full text at official University repository: http://is.muni.cz/th/383410/ff_m/
Release 1.0.0 of this repository at Zenodo: doi 10.5281/zenodo.1044744 (http://dx.doi.org/10.5281/zenodo.1044744)

Folder Data contains input data, folder Scripts contains .r scripts and folder Archive contains morphometric data.

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
+ DiscreteVariables.r - analysis of discrete variables
+ ContinuousVariables.r - analysis of continuous variables (metric values)

### ARCHIVE

sub Adzes -- contains data for adzes
 + Dt.in files - files containig shape on import
 + Dt.out files - outlines of shape
 + Dt.ef files - efourier coefficients
 + Dt.pc files - files containing pca coefficients

License: CC-BY-4.0
https://creativecommons.org/licenses/by/4.0/
