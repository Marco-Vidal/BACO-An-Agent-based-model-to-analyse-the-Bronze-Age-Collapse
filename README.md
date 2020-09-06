# BACO-An-Agent-based-model-to-analyse-the-Bronze-Age-Collapse


The Bronze Age Collapse model (BACO model) is written using free NetLogo software v.6.0.3. The purpose of using the BACO model is to develop a tool to identify and analyse 
the main factors that made the LBA and Early Iron Age socio-ecological system resilient or vulnerable in the face of the increased aridity recorded in the Aegean. 
The model explores the relationship between dependent and independent variables. Independent variables are: 
•	Inter-annual rainfall variability for the LBA and Early Iron Age
•	Intensity of raiding
•	Percentage of marine, agricultural and other calorie sources included in the diet; hereinafter referred to as ‘dietary patterns’
•	Soil erosion processes
•	Farming assets
•	Storage capacity 
Dependent variables are: 
•	Human pressure for land
•	Settlement patterns
•	Number of commercial exchanges
•	Demographic behaviour
•	Number of migrations


INITIALIZATION:


The world is a grid matrix obtained from an ASCII archive (BASEMAP.asc), which includes terrain elevation data from southern Greece. 
The model also needs data from annual rainfall evolution between 1350 and 930 B.C. from a document named “RAI.txt”. 
Before setup, both documents (“RAI.txt” and “BASEMAP.asc”) must be located in the same folder as the model itself. 
The cell size changes according to the map resolutions previously fixed by the user in the Cell-Size box. 
The elevation of each cell is interpolated by using the bicubic_2 method.
For details, see the ODD document.
