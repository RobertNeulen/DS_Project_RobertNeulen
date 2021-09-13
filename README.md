# DS_Project_RobertNeulen

Following files are included:
* *data_preparation_files*: 
  * *raw_data*: Prepares the raw data files
  * *prepared_data*: prepares the data for the use in the R Shiny App
* *function_files*: Includes the files with the functions for each tab in the App
* *app*: Loads the App
* *global_variables*: Loads all the necessary data files and calls all functions from *function_files*
* *ui*: Loads the user interface
* *server*: Loads all server functions

The folder
* *data*
  * *other*: Includes files like variable descriptions, etc.
  * *prepared_data*: Includes all data files generated by *data_preparation_files/prepared_data/...*
  * *raw_data*: Includes all raw data files from the server. The only files which is from an external data source is the geo data (*data_preparation_files/prepared_data/geo_data*) with the states/regions polygons

is not included due to file size issues.

To update the App, getting the new data into the respective sub-folders of the *data* folder is needed. The only manual control is with repsect to the translation of the NPI classes, which are partially done "by hand"in the following steps:
* Translated in the downloaded Excel-File *data/other/datensatzbeschreibung_massnahmen*
* Due to an updated, new NPIs were introduced and hence the translation was updated in the file *data_preparation_files/99_NPIDescription*
