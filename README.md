# KSO model testing and mapplotting

This repository contains scripts and example files to evaluate the performance of [Koster Seafloor Observatory](https://www.zooniverse.org/projects/victorav/the-koster-seafloor-observatory) (KSO, or [PLatform for Analysis of SUBSea IMages](https://oceandatafactory.se/plan-subsim/), PLAN-SUBSIM) species detection models, and to convert such model observations into GIS layers. It features custom-made R functions built around the KSO YOLO object detection models output, and Python scripts for geoprocessing in [QGIS](https://qgis.org/) (PyQGIS). Koster Seafloor Observatory is a project run by [University of Gothenburg](https://www.gu.se/en/), [Ocean Data Factory Sweden](https://oceandatafactory.se/), [Swedish Biodiversity Data Infrastructure](https://biodiversitydata.se/) and [Wildlife.ai](https://www.wildlife.ai/), which uses machine learning to map marine species from large volumes of image data. It was released in 2020 and has since developed both technologically and expanded its range of ecological applications, and this repository is the result of a student project along the way.

![Workflowchart](/images/Workflowchart.svg)

The consisting R functions are:

* `compare.film.by.film`: Calculate confusion matrix statistics for different species in different films at different confidence thresholds.
* `summarize.by.species`: Summarize the confusion matrices for all films into a single confusion matrix per species and parameter set.
* `annotate.grid.cells`: Annotate vector layer subunits of a timed route, like grid cells or segments of a line track, with the highest model confidence value for each species within that unit. Alternatively, annotate with the positive predictive value (PPV), or just presence/absence.
* `save.to.file`: Save the results (data frame or list of data frames) to a new XLSX workbook or CSV file.

The code relies on the _openxlsx_ R package to read and write XLSX files, but other than that everything works entirely in base-R. Instructions on how to use these scripts can be found in the [/r_scripts](/r_scripts) README.md file, and a more detailed explanation is also available as embedded comments within the scripts.

There are two geoprocessing Python scripts for QGIS 3. Both take an input layer of waypoints and create a timed route polyline layer where the length of each line segment...

* `LineRoute.py`: ...corresponds to a given number of seconds.
* `GridRoute.py`: ...is determined by the lines of a grid overlay, and enter/exit times for every crossed grid cell are calculated.

Instructions on how to use these scripts can be found in the [/geoprocessing](/geoprocessing) README.md file.

<img src="/images/Example_result_map.png" width=75% height=75%>

For more info on the technical aspects of KSO, please visit the [Github profile of Ocean Data Factory Sweden](https://github.com/ocean-data-factory-sweden), which hosts its official repositories. More info on the project in general can be found on the [KSO Zooniverse pages](https://www.zooniverse.org/projects/victorav/the-koster-seafloor-observatory), and in the first published scientific paper describing the system:

_Anton, V., Germishuys, J., Bergström, P., Lindegarth, M., & Obst, M. (2021). An open-source, citizen science and machine learning approach to analyse subsea movies. Biodiversity Data Journal, 9, e60548. https://doi.org/10.3897/BDJ.9.e60548_
