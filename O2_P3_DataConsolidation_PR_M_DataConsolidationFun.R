
data_consolidation <- function(NSFile){

#Output File Data
OutputDate = format(Sys.time())
BrandName = strsplit(NSFile , "--")[[1]][2]
FileName = paste("PriceReady--", BrandName , "--", gsub(":", "-", as.character(Sys.time())), ".csv", sep = "", collapse = NULL)
TempLocation = paste("//192.168.2.32/Group/Data Team/Brand_Update_Location/2_Output_Folder", FileName , sep = "/", collapse = NULL)

#Load data_consolidation file for brand
BrandFile = paste(as.character(NSFile), "csv", sep = ".", collapse = NULL)
File_Location = paste("//192.168.2.32/Group/Data Team/Brand_Update_Location/1_Input_Folder", as.character(BrandFile ), sep = "/", collapse = NULL)
data_consol =read.csv(File_Location, header = TRUE, row.names=NULL)

#####################################
#Identify the Main--sheet and pull it
BrandFolderLocation = paste("//192.168.2.32/GoogleDrive/Completed Magento Uploads (v 1.0)/",as.character(BrandName), sep = "", collapse = NULL)
setwd(BrandFolderLocation)
MS <- Sys.glob("main--*.csv")
OldMain_OldLocation = paste(BrandFolderLocation, MS, sep = "/", collapse = NULL)
mainsheet =read.csv(MS , header = TRUE)
MS_From = paste(BrandFolderLocation, MS, sep="/", collapse = NULL)
brand_filter = as.character(subset(mainsheet, type == "simple", select = "brand_filter")[1,1])
attribute_set = as.character(subset(mainsheet, type == "simple", select = "attribute_set")[1,1])

#####################################
#Identify the desc--Series description  and pull it
SD <- Sys.glob("desc--*.csv")
SeriesData =read.csv(SD, header = TRUE, row.names=NULL)

ca_CompanyInfo_raw = as.character(subset(SeriesData, SeriesData$attribute_set == "CompanyInfo", select = ca_desc)[1,1])
if(nchar(ca_CompanyInfo_raw)>0){
	ca_CompanyInfo = paste(
		'<div id=\"company-info\" class=\"product-description\">', 
		'<p>', 
		ca_CompanyInfo_raw, 
		'</p></div>', 
		sep = "", collapse = NULL)
} else {ca_CompanyInfo=""}

usa_CompanyInfo_raw = as.character(subset(SeriesData, SeriesData$attribute_set == "CompanyInfo", select = us_desc)[1,1])
if(nchar(usa_CompanyInfo_raw)>0){
	usa_CompanyInfo = paste(
		'<div id=\"company-info\" class=\"product-description\">', 
		'<p>', 
		usa_CompanyInfo_raw, 
		'</p></div>', 
		sep = "", collapse = NULL)
} else {usa_CompanyInfo=""}

ca_WarrentyInfo_raw =  as.character(subset(SeriesData, SeriesData$attribute_set == "WarrentyInfo", select = ca_desc)[1,1])
if(nchar(ca_WarrentyInfo_raw)>0){
	ca_WarrentyInfo = paste(
		'<div id=\"company-info\" class=\"product-description\">', 
		'<h3 class=\"contentheader\">Warranty Information</h3>', 
		'<p>', 
		ca_WarrentyInfo_raw, 
		'</p></div>', 
		sep = "", collapse = NULL)
} else {ca_WarrentyInfo=""}

usa_WarrentyInfo_raw = as.character(subset(SeriesData, SeriesData$attribute_set == "WarrentyInfo", select = us_desc)[1,1])
if(nchar(usa_WarrentyInfo_raw)>0){
	usa_WarrentyInfo = paste(
		'<div id=\"company-info\" class=\"product-description\">', 
		'<h3 class=\"contentheader\">Warranty Information</h3>', 
		'<p>', 
		usa_WarrentyInfo_raw, 
		'</p></div>', 
		sep = "", collapse = NULL)
} else {usa_WarrentyInfo=""}

#Make New Skus DF
columns = names(data_consol)[-1]
new = subset(data_consol, STATUS == "NEW", select = columns )
new[is.na(new)] <- ""

new$image1 = as.character(new$image1)
new$image2 = as.character(new$image2)
new$image3 = as.character(new$image3)
new$image4 = as.character(new$image4)


####BASIC####
new$delete = "N-N"
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
new$usa_next_day_shipping = "N"
new$usa_fitment_guarantee = "Y"
new$usa_tax_free_ordering = "Y"
new$usa_over_x_customers = "N"
new$usa_authorized_dealer = "Y"
new$usa_easy_returns_no_fees = "N"
new$usa_no_restocking_fees = "N"


###Modify###
RawSku = gsub("#","",new$sku)
Rawproduct_name = new$product_name

new$product_name = paste(brand_filter, " ", RawSku, " - ", Rawproduct_name, sep = "", collaps=NULL)
new$meta_keyword = paste(brand_filter, " ", RawSku, " ", Rawproduct_name, sep = "", collaps=NULL)
new$meta_title = paste(brand_filter, " ", RawSku, " ", Rawproduct_name, sep = "", collaps=NULL)
new$keywords = paste(brand_filter, " ", RawSku, " ", Rawproduct_name, sep = "", collaps=NULL)
new$meta_description = paste("Great Prices On ", brand_filter, " ", RawSku, " ", Rawproduct_name, sep = "", collaps=NULL)
new$ca_short_description = as.character(new$ca_short_description)
new$usa_short_description = as.character(new$usa_short_description)

##########################################
for(i in 1:nrow(new)){

Series_Data = subset(SeriesData, as.character(new$series_parent[i]) == as.character(SeriesData$series_parent))
Series_Data[is.na(Series_Data)] <- ""
RowTest = nrow(Series_Data)

Start_Container = '<div class="content-container">'
End_Container = '</div>'

###Create Alt Image Tags
if(nchar(new$image1[i]) > 0){
	new$alttag_image1[i] = paste(brand_filter, " ", RawSku[i], " ", Rawproduct_name[i], " Image 1", sep = "", collaps=NULL)
} else { new$alttag_image1[i] = "" }

if(nchar(new$image2[i]) > 0){
	new$alttag_image2[i] = paste(brand_filter, " ", RawSku[i], " ", Rawproduct_name[i], " Image 2", sep = "", collaps=NULL)
} else { new$alttag_image2[i] = "" }

if(nchar(new$image3[i]) > 0){
	new$alttag_image3[i] = paste(brand_filter, " ", RawSku[i], " ", Rawproduct_name[i], " Image 3", sep = "", collaps=NULL)
} else { new$alttag_image3[i] = "" }

if(nchar(new$image4[i]) > 0){
	new$alttag_image4[i] = paste(brand_filter, " ", RawSku[i], " ", Rawproduct_name[i], " Image 4", sep = "", collaps=NULL)
} else { new$alttag_image4[i] = "" }

#Canada specifications Logic
if(nchar(new$ca_short_description[i])> 0){

	new$ca_short_description[i] = gsub("</li><li></li></ul>","</li></ul>", paste("<ul><li>", gsub(";","</li><li>",trimws(new$ca_short_description[i])), "</li></ul>", sep="", collaps=NULL))

		ca_Specs = paste(
		'<div id="specifications" class="product-key-features">',
		'<h3 class="contentheader">Specs:',
		'</h3>',
		###Product Features
		new$ca_short_description[i], 
		'</div>',
		sep = "", collapse = NULL)
} else {ca_Specs= ""; new$ca_short_description[i] = ""}

#USA specifications Logic
if(nchar(new$usa_short_description[i])>0){

	new$usa_short_description[i] = gsub("</li><li></li></ul>","</li></ul>", paste("<ul><li>", gsub(";","</li><li>",trimws(new$usa_short_description[i])), "</li></ul>", sep="", collaps=NULL))

		usa_Specs = paste(
		'<div id="specifications" class="product-key-features">',
		'<h3 class="contentheader">Specs:',
		'</h3>',
		###Product Features
		new$usa_short_description[i], 
		'</div>',
		sep = "", collapse = NULL)
} else {usa_Specs= ""; new$usa_short_description[i] = ""}

###UPC LOGIC
if(nchar(as.character(new$upc_code[i])) > 0){
		UPCInfo = paste('<tr>',
		'<td style="width:180px;">',
		'<b>UPC:',
		'</b>',
		'</td>',
		'<td style="width:180px;">',
		###UPC Number
		gsub("#","",new$upc_code[i]),
		'</td>',
		'</tr>',
		sep = "", collapse = NULL)
} else {UPCInfo=""}

#Create product table with brand name, sku and UPC
ProductTable =  paste(
		'<div class="additionaldetailbar">',
		'</div>',
		'<table>',
		'<tr>',
		'<td style="width:180px;">',
		'<b>Brand:',
		'</b>',
		'</td>',
		'<td style="width:180px;">',
		###brand_filter
		brand_filter, 
		'</td>',
		'</tr>',
		'<tr>',
		'<td style="width:180px;">',
		'<b>Part Number:',
		'</b>',
		'</td>',
		'<td style="width:180px;">',
		###Raw Sku
		gsub("#","",new$sku[i]), 
		'</td>',
		'</tr>',
		UPCInfo, 
		'</table>',
sep = "", collapse = NULL)

#################
	if(RowTest==0){
#################

new$ca_description[i] = paste( 

		Start_Container,
		ca_Specs,
		ca_CompanyInfo, 
		ca_WarrentyInfo, 
		ProductTable, 
		End_Container,
		sep = "", collapse = NULL)

###US DESCRIPTION
new$usa_description[i] = paste(

		Start_Container, 
		usa_Specs,
		usa_CompanyInfo,
		usa_WarrentyInfo, 
		ProductTable, 
		End_Container,
		sep = "", collapse = NULL)

#################
	} else if(RowTest==1){
#################

#ca_desc Logic
if(nchar(as.character(Series_Data$ca_desc)) > 0){
		ca_desc= paste(
		'<div id="description" class="product-description">',
		'<p>', 
		###Series Description
		Series_Data$ca_desc, 
		'</p>',
		'</div>',
		sep = "", collapse = NULL)
} else {ca_desc= ""}


#us_desc Logic
if(nchar(as.character(Series_Data$us_desc)) > 0){
		us_desc= paste(
		'<div id="description" class="product-description">',
		'<p>', 
		###Series Description
		Series_Data$us_desc, 
		'</p>',
		'</div>',
		sep = "", collapse = NULL)
}  else {us_desc= ""}


#YoutubeURL1 Logic
if(nchar(as.character(Series_Data$YoutubeURL1)) > 0){

		YoutubeURL1 = paste(
		'<div class="videocontainer">',
		'<div>',
		'<iframe width="560" height="315" src="',
		'https://www.youtube.com/embed/', 
		Series_Data$YoutubeURL1,
		'?rel=0', 
		'" frameborder="0" allowfullscreen>',
		'</iframe>',
		'</div>',
		'</div>',
		sep = "", collapse = NULL)
} else {YoutubeURL1 = ""}

#YoutubeURL1 Logic
if(nchar(as.character(Series_Data$YoutubeURL2)) > 0){

		YoutubeURL1 = paste(
		'<div class="videocontainer">',
		'<div>',
		'<iframe width="560" height="315" src="',
		'https://www.youtube.com/embed/', 
		Series_Data$YoutubeURL2,
		'?rel=0', 
		'" frameborder="0" allowfullscreen>',
		'</iframe>',
		'</div>',
		'</div>',
		sep = "", collapse = NULL)
} else {YoutubeURL2 = ""}

#YoutubeURL1 Logic
if(nchar(as.character(Series_Data$YoutubeURL3)) > 0){

		YoutubeURL1 = paste(
		'<div class="videocontainer">',
		'<div>',
		'<iframe width="560" height="315" src="',
		'https://www.youtube.com/embed/', 
		Series_Data$YoutubeURL3,
		'?rel=0', 
		'" frameborder="0" allowfullscreen>',
		'</iframe>',
		'</div>',
		'</div>',
		sep = "", collapse = NULL)
} else {YoutubeURL3 = ""}

#Combine all youtube URL into a collapsable container
VideoContainer = paste(
		'<div class="clearer">',
		'</div>',
		'<div id="videos" class="product-videos">',
		'<div class="video-thirds">',
		YoutubeURL1,
		'</div>',
		'<div class="video-thirds">',
		YoutubeURL2,
		'</div>',
		'<div class="video-finalthird">',
		YoutubeURL3,
		'</div>',
		'</div>',
		'<div class="clearer">',
		'</div>',
sep = "", collapse = NULL)

#Canada Features Logic
if(nchar(as.character(Series_Data$ca_features)) > 0){

		ca_Features= paste(
		'<div id="features" class="product-key-features">',
		'<h3 class="contentheader">Features:',
		'</h3>',
		###Series Features
		Series_Data$ca_features, 
		'</div>',
		sep = "", collapse = NULL)
} else {ca_Features= ""}

#USA Features Logic
if(nchar(as.character(Series_Data$usa_features)) > 0){

		usa_Features= paste(
		'<div id="features" class="product-key-features">',
		'<h3 class="contentheader">Features:',
		'</h3>',
		###Series Features
		Series_Data$usa_features, 
		'</div>',
		sep = "", collapse = NULL)
} else {usa_Features= ""}

###Canada DESCRIPTION
new$ca_description[i] = paste( 

		Start_Container,
		ca_desc,
		ca_Specs,
		ca_Features, 
		ca_CompanyInfo, 
		VideoContainer, 
		ca_WarrentyInfo, 
		ProductTable, 
		End_Container,
		sep = "", collapse = NULL)

###US DESCRIPTION
new$usa_description[i] = paste(

		Start_Container, 
		us_desc, 
		usa_Specs,
		usa_Features,
		usa_CompanyInfo,
		VideoContainer, 
		usa_WarrentyInfo, 
		ProductTable, 
		End_Container,
		sep = "", collapse = NULL)

	} else {

		new$ca_description[i] = "+@+@+@+@CLEAN UP desc--*.csv FILE+@+@+@+@"
		new$usa_description[i] = "+@+@+@+@CLEAN UP desc--*.csv FILE+@+@+@+@"
	}

}
####################################
#Merge mainsheet with New skus
compiled_MS = merge(mainsheet, new, by = names(new), all= TRUE)

####################################
#Adding New Series
Nseries = subset(data_consol, STATUS == "NEW_SERIES", select = columns )
Nseries[is.na(Nseries)] <- ""

if(nrow(Nseries)>0){
	Nseries$delete = "N-S"
	Nseries$type = "series"
	Nseries$attribute_set= attribute_set
	Nseries$websites = "Canada;USA"
	Nseries$usa_price= 0.01
	Nseries$ca_price= 0.01
	Nseries$ca_proudly_canadian = "Y"
	Nseries$ca_shipped_from_canada = "N" 
	Nseries$ca_fitment_guarantee = "N"
	Nseries$ca_no_customs_duties = "Y"
	Nseries$ca_over_x_customers = "Y"
	Nseries$ca_lowest_price_guaranteed = "Y"
	Nseries$ca_easy_returns_no_fees = "N"
	Nseries$ca_no_restocking_fees = "N"
	Nseries$ca_tax_free_ordering = "N"
	Nseries$usa_proudly_canadian = "N"
	Nseries$usa_free_shipping = "Y"
	Nseries$usa_next_day_shipping = "Y"
	Nseries$usa_fitment_guarantee = "Y"
	Nseries$usa_tax_free_ordering = "Y"
	Nseries$usa_over_x_customers = "Y"
	Nseries$usa_authorized_dealer = "Y"
	Nseries$usa_easy_returns_no_fees = "N"
	Nseries$usa_no_restocking_fees = "N"
	Nseries$ca_authorized_dealer = "Y"
	Nseries$ca_free_shipping = "Y"
	Nseries$meta_keyword = paste(Nseries$product_name, sep = "", collaps=NULL)
	Nseries$meta_title = paste(Nseries$product_name, sep = "", collaps=NULL)
	Nseries$keywords = paste(Nseries$product_name, sep = "", collaps=NULL)
	Nseries$meta_description = paste("Great Prices On ", Nseries$product_name, sep = "", collaps=NULL)


Nseries$image1 = as.character(Nseries$image1)
Nseries$image2 = as.character(Nseries$image2)
Nseries$image3 = as.character(Nseries$image3)
Nseries$image4 = as.character(Nseries$image4)


for(i in 1: nrow(Nseries)){

###Create Alt Image Tags
	if(nchar(Nseries$image1[i]) > 0 ){
		Nseries$alttag_image1[i] = paste(Nseries$product_name[i], " Image 1", sep = "", collaps=NULL)
	} else { Nseries$alttag_image1[i] = "" }

	if(nchar(Nseries$image2[i])  > 0){
		Nseries$alttag_image2[i] = paste(Nseries$product_name[i], " Image 2", sep = "", collaps=NULL)
	} else { Nseries$alttag_image2[i] = "" }

	if(nchar(Nseries$image3[i]) > 0){
		Nseries$alttag_image3[i] = paste(Nseries$product_name[i], " Image 3", sep = "", collaps=NULL)
	} else { Nseries$alttag_image3[i] = "" }

	if(nchar(Nseries$image4[i]) > 0){
		Nseries$alttag_image4[i] = paste(Nseries$product_name[i], " Image 4", sep = "", collaps=NULL)
	} else { Nseries$alttag_image4[i] = "" }

}


	compiled_MS_series = merge(compiled_MS, Nseries , by = names(Nseries), all = TRUE)

} else {compiled_MS_series = compiled_MS}


####################################
#Merge mainsheet with AMAD skus
amad = subset(data_consol, STATUS == "AMAD", select = c("sku", "internal_sku"))
if(nrow(amad)>0){
	amad$delete = "Y-P"
	amad$type = "simple"
	amad$product_name = paste(brand_filter, " ", gsub("#","",amad$sku) , sep="", collapse=NULL)
	amad$brand_filter= brand_filter
	amad$attribute_set= attribute_set
	amad$categories = paste("Brands/", brand_filter, sep = "", collapse=NULL)
	amad$series_parent = "Discontinued"
	amad$websites = "Canada;USA"
	amad$qty= 9999
	amad$usa_price= 0.01
	amad$ca_price= 0.01
	amad$ca_proudly_canadian = "Y"
	amad$ca_shipped_from_canada = "N" 
	amad$ca_fitment_guarantee = "N"
	amad$ca_no_customs_duties = "Y"
	amad$ca_over_x_customers = "Y"
	amad$ca_lowest_price_guaranteed = "Y"
	amad$ca_easy_returns_no_fees = "N"
	amad$ca_no_restocking_fees = "N"
	amad$ca_tax_free_ordering = "N"
	amad$usa_proudly_canadian = "N"
	amad$usa_free_shipping = "Y"
	amad$usa_next_day_shipping = "Y"
	amad$usa_fitment_guarantee = "Y"
	amad$usa_tax_free_ordering = "Y"
	amad$usa_over_x_customers = "Y"
	amad$usa_authorized_dealer = "Y"
	amad$usa_easy_returns_no_fees = "N"
	amad$usa_no_restocking_fees = "N"
	amad$ca_authorized_dealer = "Y"
	amad$ca_free_shipping = "Y"
	amad$meta_keyword = paste(amad$product_name, sep = "", collaps=NULL)
	amad$meta_title = paste(amad$product_name, sep = "", collaps=NULL)
	amad$keywords = paste(amad$product_name, sep = "", collaps=NULL)
	amad$meta_description = paste("Great Prices On ", amad$product_name, sep = "", collaps=NULL)

	compiled_MS_amad = merge(compiled_MS_series, amad , by = names(amad), all = TRUE)

} else {

	compiled_MS_amad = compiled_MS_series

}


####################################
compiled_MS_amad$delete = as.character(compiled_MS_amad$delete)

formatted_MS = data.frame(subset(compiled_MS_amad, select = names(mainsheet)))

#Discontinue disco skus on mainsheet
disco = subset(data_consol, STATUS == "DISCO", select = sku)

if(nrow(disco)>0){
	for(i in 1:nrow(disco)){

		Positions = grep(disco[i, 1], formatted_MS$sku)
		for(p in 1: length(Positions)){

			if(formatted_MS$delete[Positions[p]] == "N" & disco$sku[i] == formatted_MS$sku[Positions[p]]){
				formatted_MS[Positions[p], 1] = "Y-D"
			}

		}

	}
}
####################################

ActiveOld = nrow(subset(mainsheet, delete == "N", select = sku))
DiscoOld = nrow(subset(mainsheet,  delete == "Y", select = sku))
TotalOld = ActiveOld + DiscoOld
SeriesOld = nrow(subset(mainsheet, type == "series", select = sku)) 

ActiveNew = nrow(subset(data_consol, STATUS == "NEW", select = sku))
OnlyDiscoNew = nrow(subset(data_consol, STATUS == "DISCO", select = sku))
DiscoNew = nrow(subset(data_consol, STATUS == "DISCO", select = sku)) +  nrow(subset(data_consol, STATUS == "AMAD", select = sku)) 
AMADNew =  nrow(subset(data_consol, STATUS == "AMAD", select = sku)) 
TotalNew = ActiveOld + DiscoOld
SeriesNew = nrow(subset(data_consol, STATUS == "NEW_SERIES", select = sku)) 

ActiveCon = nrow(subset(formatted_MS, delete == "N-N", select = sku)) + nrow(subset(formatted_MS, delete == "N", select = sku))
DiscoCon = nrow(subset(formatted_MS, delete == "Y-D", select = sku)) + nrow(subset(formatted_MS, delete == "Y-P", select = sku))+ nrow(subset(formatted_MS, delete == "Y", select = sku))
TotalCon =  ActiveCon  + DiscoCon 
SeriesCon =  nrow(subset(formatted_MS, type == "series", select = sku))

if(SeriesOld + SeriesNew != SeriesCon){
message("Series Issue:: ", "Old:", SeriesOld, " + New:", SeriesNew, " should be ",SeriesOld+SeriesNew, " not ", SeriesCon)}

if(DiscoOld + DiscoNew != DiscoCon ){
message("Discontinued Issue:: ", "Old:", DiscoOld , " + New:", DiscoNew , " should be ",DiscoOld+DiscoNew, " not ", DiscoCon, "with difference of ", abs((DiscoOld+DiscoNew)- DiscoCon))}

if(ActiveNew + (ActiveOld - OnlyDiscoNew ) != ActiveCon ){
message("New Sku Issue:: There is a difference of ", abs((ActiveNew + (ActiveOld - OnlyDiscoNew )) - ActiveCon), " skus")}

if(DiscoOld + AMADNew + ActiveOld + ActiveNew != TotalCon ){
message("Mainsheet Sku Issue:: There is a difference of ", abs((DiscoOld + AMADNew + ActiveOld + ActiveNew) - TotalCon ), " skus")}


message("***Consolidation Complete***")
####################################
#Output formatted Mainsheet
write.csv(formatted_MS, file = TempLocation , na="", row.names=FALSE)
}

####################################
####################################
####################################
#data_consolidation(NSFile = "NS--Rigid")
#NSFile = "NS--Gem_Tubes"


####PRICING####
#usa_price
#usa_retail_price
#usa_cost
#usa_jobber_price
#na_usa_shipping
#usa_shipping_destination_values
#na_exchange_rate
#ca_price
#ca_retail_price
#ca_cost
#ca_jobber_price
#na_ca_shipping
#ca_shipping_destination_values
#na_supplier	
#na_jobber_discount
#ca_free_shipping






























