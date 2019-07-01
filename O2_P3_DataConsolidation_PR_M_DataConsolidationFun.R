
data_consolidation <- function(BrandName){

#Identify the Main--sheet and pull it
BrandFolderLocation = paste("//192.168.2.32/GoogleDrive/TDot_Brands/",as.character(BrandName), sep = "", collapse = NULL)
setwd(BrandFolderLocation)
MS <- Sys.glob("main--*.csv")
OldMain_OldLocation = paste(BrandFolderLocation, MS, sep = "/", collapse = NULL)
mainsheet =read.csv(MS , header = TRUE)
MS_From = paste(BrandFolderLocation, MS, sep="/", collapse = NULL)
brand_filter = as.character(subset(mainsheet, type == "simple", select = "brand_filter")[1,1])
attribute_set = as.character(subset(mainsheet, type == "simple", select = "attribute_set")[1,1])


#Identify the data_consolidation--sheet and pull it
data_consol_Location = paste(BrandFolderLocation, "4a. R_Input_Files", sep = "/", collapse = NULL)
setwd(data_consol_Location)
DC <- Sys.glob("data_consolidation--*.csv")
data_consol = read.csv(DC , header = TRUE)


#Identify the discontinued_skus--sheet and pull it
discoSKU_Location = paste(BrandFolderLocation, "5. Brand_Info", sep = "/", collapse = NULL)
setwd(discoSKU_Location)
ODS <- Sys.glob("discontinued_skus--*.csv")
OldODS_OldLocation = paste(discoSKU_Location, ODS, sep = "/", collapse = NULL)
discontinued_skus = read.csv(ODS , header = TRUE)


columns = names(data_consol)[-1]

new = subset(data_consol, STATUS == "NEW", select = columns )
disco = subset(data_consol, STATUS == "DISCO", select = sku)
amad = subset(data_consol, STATUS == "AMAD", select = c("sku", "internal_sku"))


####BASIC####
new$delete = "N"
new$type = "simple"
new$brand_filter = brand_filter
new$attribute_set = attribute_set
new$websites = "Canada;USA"
new$qty = 9999
new$usa_show_currency = "N"
new$usa_shipping_destination = "USA"
new$ca_show_currency = "Y"
new$ca_shipping_destination = "Ontario;Quebec;Alberta;British Columbia;Manitoba;Saskatchewan;Newfoundland;PEI;New Brunswick;Nova Scotia;Yukon;NWT;Nunavut;USA"
new$taxable = "Y"
new$ca_authorized_dealer = "Y"
new$ca_proudly_canadian = "Y"
new$ca_shipped_from_canada = "N" 
new$ca_fitment_guarantee = "N"
new$ca_no_customs_duties = "Y"
new$ca_over_x_customers = "Y"
new$ca_lowest_price_guaranteed = "Y"
new$use_attributes_brief = "Y"
new$ca_easy_returns_no_fees = "N"
new$ca_no_restocking_fees = "N"
new$ca_tax_free_ordering = "N"
new$usa_proudly_canadian = "N"
new$usa_free_shipping = "Y"
new$usa_next_day_shipping = "Y"
new$usa_fitment_guarantee = "Y"
new$usa_tax_free_ordering = "Y"
new$usa_over_x_customers = "Y"
new$usa_authorized_dealer = "Y"
new$usa_easy_returns_no_fees = "N"
new$usa_no_restocking_fees = "N"

compiled_MS = merge(mainsheet, new, by = names(new), all= TRUE)

formatted_MS = data.frame(subset(compiled_MS, select = names(mainsheet)))
formatted_MS$delete = as.character(formatted_MS$delete)

OldDiscoSkus = subset(formatted_MS, delete == "Y")
NEW_mainsheet = subset(formatted_MS, delete != "Y")

NEW_discontinued_skus_DISCO = merge(discontinued_skus, OldDiscoSkus, by = names(OldDiscoSkus), all = TRUE)
NEW_discontinued_skus = merge(NEW_discontinued_skus_DISCO, amad , by = names(amad), all = TRUE)

for(i in 1:nrow(disco)){

	NEW_mainsheet[grep(disco [i, 1], NEW_mainsheet$sku), 1] = "Y"
}

OutputDate = format(Sys.time(), "%Y.%m.%d")
OutputFolderName = paste("Consol", OutputDate , sep = "_", collapse = NULL)

OutputLocation = paste("//192.168.2.32/GoogleDrive/TDot_Brands", BrandName, "4c. R_Output_Folder", sep = "/", collapse = NULL)
New_OutputLocation = paste(OutputLocation, OutputFolderName, sep = "/", collapse = NULL)
dir.create(paste(OutputLocation ,OutputFolderName , sep = "/", collapse = NULL)) 

ArchiveLocation = paste("//192.168.2.32/GoogleDrive/TDot_Brands", BrandName, "2. Archive/Consolidation_Archive", sep = "/", collapse = NULL)
New_ArchiveLocation = paste(ArchiveLocation, OutputFolderName, sep = "/", collapse = NULL)
dir.create(paste(ArchiveLocation ,OutputFolderName , sep = "/", collapse = NULL)) 


MainsheetName = paste("main", BrandName, sep = "--", collapse = NULL)

New_Main = paste(MainsheetName, "NOT_PRICED", OutputDate, "csv", sep = ".", collapse = NULL)
New_Main_OutputLocation = paste(New_OutputLocation, New_Main, sep = "/", collapse = NULL)
New_Main_ArchiveLocation = paste(New_ArchiveLocation, New_Main, sep = "/", collapse = NULL)
New_Main_BrandFolderLocation = paste(BrandFolderLocation, New_Main, sep = "/", collapse = NULL)
write.csv(NEW_mainsheet, file = New_Main_OutputLocation, na="", row.names = FALSE)
write.csv(NEW_mainsheet, file = New_Main_ArchiveLocation, na="", row.names = FALSE)
write.csv(NEW_mainsheet, file = New_Main_BrandFolderLocation, na="", row.names = FALSE)

Old_Main = paste("Old", MainsheetName, OutputDate , "csv", sep = ".", collapse = NULL)
Old_Main_OutputLocation = paste(New_OutputLocation ,Old_Main, sep = "/", collapse = NULL)
OldMain_NewLocation = paste(New_ArchiveLocation, MS, sep = "/", collapse = NULL)
Old_Main_ArchiveLocation = paste(New_ArchiveLocation ,Old_Main, sep = "/", collapse = NULL)
write.csv(mainsheet , file = Old_Main_OutputLocation, na="", row.names = FALSE)
file.rename(from = OldMain_OldLocation, to = Old_Main_OutputLocation)


discontinued_skusName = paste("discontinued_skus", BrandName, sep = "--", collapse = NULL)

New_discontinued_skus = paste(discontinued_skusName, OutputDate , "csv", sep = ".", collapse = NULL)
New_discontinued_skus_OutputLocation = paste(New_OutputLocation, New_discontinued_skus, sep = "/", collapse = NULL)
New_discontinued_skus_ArchiveLocation = paste(New_ArchiveLocation, New_discontinued_skus, sep = "/", collapse = NULL)
BrandDataLocation = paste("//192.168.2.32/GoogleDrive/TDot_Brands", BrandName, "5. Brand_Info",New_discontinued_skus, sep = "/", collapse = NULL)
write.csv(NEW_discontinued_skus, file = New_discontinued_skus_OutputLocation, na="", row.names = FALSE)
write.csv(NEW_discontinued_skus, file =New_discontinued_skus_ArchiveLocation, na="", row.names = FALSE)
write.csv(NEW_discontinued_skus, file =BrandDataLocation , na="", row.names = FALSE)

Old_discontinued_skus = paste("Old", discontinued_skusName, OutputDate , "csv", sep = ".", collapse = NULL)
Old_discontinued_skus_OutputLocation = paste(New_OutputLocation, Old_discontinued_skus, sep = "/", collapse = NULL)
Old_discontinued_skus_ArchiveLocation = paste(New_ArchiveLocation, Old_discontinued_skus, sep = "/", collapse = NULL)
write.csv(discontinued_skus, file = Old_discontinued_skus_OutputLocation, na="", row.names = FALSE)
write.csv(discontinued_skus, file = Old_discontinued_skus_ArchiveLocation, na="", row.names = FALSE)
OldODS_NewLocation = paste(New_ArchiveLocation, ODS, sep = "/", collapse = NULL)
file.rename(from = OldODS_OldLocation, to = OldODS_NewLocation)


}



data_consolidation(BrandName = "Gem_Tubes")


#Need to create with headers
test <- data.frame(matrix(ncol = ncol(mainsheet), nrow = 0))
names(test) = names(mainsheet)


####PRICING####
usa_price
usa_retail_price
usa_cost
usa_jobber_price
na_usa_shipping
usa_shipping_destination_values
na_exchange_rate
ca_price
ca_retail_price
ca_cost
ca_jobber_price
na_ca_shipping
ca_shipping_destination_values
na_supplier	
na_jobber_discount
ca_free_shipping

####CONTENT####
product_overview
usa_short_description
usa_description
ca_short_description
ca_description
return_info
usa_faq
ca_faq
meta_keyword
meta_description
meta_title
keywords
url
alttag_image1
alttag_image2
alttag_image3
alttag_image4

####BLANK####
ribbon
ribbon_start
ribbon_end
tdot_position
best_seller
news_from_date
news_to_date
delete_redirect































