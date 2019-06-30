

FeatureStich = function(){

STICHER_File =read.csv("//192.168.2.32/Group/Data Team/Brand_Update_Location/1_Input_Folder/SERIES_FEATURE_IMAGE_STICHER.csv" , header = TRUE)

for(i in 1:nrow(STICHER_File)){

	ImageVal = nchar(as.character(STICHER_File[i,2]))

	x =  paste('<div class="img-feature-content">', 
		"<p>", 
		STICHER_File[i,1], 
		"</p>", 
		"</div>", 
		sep = "", collapse = NULL)

	if(ImageVal>1){

STICHER_File[i,3]= paste('<div class="feature-container">', 
			'<div class="img-feature">', 
			'<img src="{{media url="', 
			STICHER_File[i,2],
			'"}}" alt="', 
			STICHER_File[i,1],
			'"></div>',
		   	x, 
			'</div>', 
			sep = "", collapse = NULL)
	} else {

STICHER_File[i,3]= paste('<div class="feature-container">', 
		   	x, 
			'</div>', 
			sep = "", collapse = NULL)
	}
}

message(STICHER_File[,3])

}



#FeatureStich()







