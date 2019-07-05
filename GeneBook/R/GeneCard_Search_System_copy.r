
#*****************************************	GeneNet AutoNet for Biologist 	*****************************************#

###@@@	Author: LI Xu
###@@@	Version: V.1.0
###@@@	Date: 2019

###@@@	Require packages:
# require(RMySQL)


###@@@  Require data: test.csv


# requireNamespace("tidyverse")
# library(tidyverse)
#load("./data/genecard_miss.rda")
#load("./data/genecard_id.rda")
#load("./data/genecard_description_summary.rda")
load("./R/sysdata.rda")


# genecard_miss = read.csv("./data/genecard_miss.csv")
#genecard_miss = read.csv("./data/genecard_miss.csv",header = TRUE,fileEncoding = 'UTF-8')
# save(genecard_miss, file ="genecard_miss.rda", compress = 'xz' )

#genecard_id = read.csv("./data/genecard_id.csv", fileEncoding = 'UTF-8') %>%
# mutate(subname = as.character(subname)) %>%
# mutate(subname = shQuote(subname))  # string_detect
#save(genecard_id, file ="genecard_id.rda", compress = 'xz' )

#genecard_description_summary = read.csv("./data/genecard_description_summary.csv", fileEncoding = 'UTF-8')

#save(genecard_description_summary, file ="genecard_description_summary.rda", compress = 'xz')


# rio::export(genecard_miss, "./data/genecard_miss.rda")
# rio::export(genecard_id, "./data/genecard_id.rda")
GeneCard_ID_Convert = function(x){
# library(RMySQL)


#	   conn <- dbConnect(MySQL(), dbname = "genecard", username="ionadmin", password="ionadmin", host="10.9.17.6", port=3306)

#	  dbGetQuery(conn, paste0("SELECT *
#	                           FROM genecard_miss
#	                           where gene=","'", x,"'"))->MISS

MISS = dplyr::filter(genecard_miss, x==genecard_miss$gene)

	  if(nrow(MISS)==0){

	  #sql_genecard <- paste0("SELECT *
	  #                        FROM genecard_id
	  #                        where gene=","'", x,"'")
	  #res_genecard <- dbSendQuery(conn, sql_genecard)
	  #data <- dbFetch(res_genecard)
############ gene id
	    x1 = toupper(x) # ucase_transfer
	    x2 = tolower(x) #lcase_transfer
    data = rbind(dplyr::filter(genecard_id, x1==genecard_id$gene),
                 dplyr::filter(genecard_id, x2 ==genecard_id$gene))
		result = c()
			if(dim(data)[1]==0){# no direct match Symbol

			  #			  sql_genecard1 <- paste0("SELECT * FROM genecard_id where subname LIKE","'%", x,"%'")
			  #			  res_genecard1 <- dbSendQuery(conn, sql_genecard1)
			  #			  data1 <- dbFetch(res_genecard1)
			  #x1 = toupper(x) # ucase_transfer
			  #x2 = tolower(x) #lcase_transfer
			  ####  data1 = genecard_id[c(grep(x1, genecard_id$subnam), grep(x2, genecard_id$subnam,)),]
data1 = rbind(genecard_id[stringr::str_detect(genecard_id$subname,x1),], genecard_id[stringr::str_detect(genecard_id$subname,x2),])
				  if(dim(data1)[1]>1){ # multiple matched results
					  nchar(data1[,2])->num
					  which(num==min(num))->num1  # Predict best match
					  unique(data1[num1,1])->Symbol
					  Symbol = as.character(Symbol)

					  if(length(Symbol)>10){  #multiple matched results & results greater than 10 after ajutesed, label as missing
					  c(x, "Missing")->result
					  }

					  if(length(Symbol)>1 & length(Symbol)<=10){ #multiple matched results & results >1 &<-=19 after adjusted,only output the first one
					  cbind(Symbol[1],"Predict Suspicion")->result
					  }


					  if(length(Symbol)==1 & dim(data1)[1]<50 ){ #multiple mathched results, & only one trusted result after adjusted
					  c(Symbol,"Predict Trust")->result
					  }

					  if(length(Symbol)==1 & dim(data1)[1]>=50 ){  #multiple matched results & only one suspected result after adjusted
					  c(Symbol,"Predict Suspicion")->result
					  }

				  }

				 if(dim(data1)[1]==1){ #only one indirect symbol matched
				 c(as.character(data1[1,1]), "Trust")->result
				 }

				 if(dim(data1)[1]==0){ #no match
				 c(x, "Missing")->result
				 }


			}


			if(dim(data)[1]!=0){ # direct symbol match
				c(as.character(data[1,1]),"Trust")->result
				# mat = rbind(mat,c(x,result))
			}

#			 dbClearResult(res_genecard)
	}




	  if(nrow(MISS)!=0){
	  result<-c(x,"Trust Missing")
	  }
	##########################################################



		# dbDisconnect(conn)


	return(result)

}

####  GeneCard_Symbol_Details function
# genecard_description_summary = read.csv("./data/genecard_description_summary.csv", fileEncoding = "UTF-8") %>%
# mutate(gene = shQuote(gene))
# rio::export(genecard_description_summary, "./data/genecard_description_summary.rda")


GeneCard_Symbol_Details = function(x){
  # library(RMySQL)
  x = shQuote(x,type = c("cmd"))

  #  conn <- dbConnect(MySQL(), dbname = "genecard", username="ionadmin", password="ionadmin", host="10.9.17.6", port=3306)
  #	  sql_genecard <- paste0("SELECT * FROM genecard_description_summary where gene=","'", x,"'")
  #	  res_genecard <- dbSendQuery(conn, sql_genecard)
  #	  data <- dbFetch(res_genecard)

  #		 dbClearResult(res_genecard)
  #		 dbDisconnect(conn)
data = dplyr::filter(genecard_description_summary,genecard_description_summary$gene == x)


data$type = as.character(data$type)
data$description = as.character(data$description)
data$summary_entrez = as.character(data$summary_entrez)
data$summary_genecard = as.character(data$summary_genecard)
data$summary_uniport = as.character(data$summary_uniport)
data$summary_Tocris = as.character(data$summary_Tocris)
data$summary_CIViC = as.character(data$summary_CIViC)
# data = genecard_description_summary[str_detect(genecard_description_summary$gene, x),]
  return(data)
}


get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

#### function to replace choose.dir for Mac Users and Windows Users

dir.choose <- function() {
  system("osascript -e 'tell app \"RStudio\" to POSIX path of (choose folder with prompt \"Choose Folder:\")' > /tmp/R_folder",
    intern = FALSE, ignore.stderr = TRUE)
  p <- system("cat /tmp/R_folder && rm -f /tmp/R_folder", intern = TRUE)
  return(ifelse(length(p), p, NA))
}

###### function f.dir.create
requireNamespace("svDialogs")
 library(svDialogs)
f.dir.create = function() {
  if (get_os() == "osx") {
  xdir = dir.choose()
  } else {
  xdir = choose.dir()
  }
  #caption = "plasee choose a fold: ")
  ######### for Windows user:	 folder = winDialogString(message = "please enter a fold name: ", default = "")
  folder = svDialogs::dlg_input(message = "please enter a fold name: ", default = "")$res
  if (xdir != "" & folder != "") {
    # if ( !folder == "") {
    # if (!dir.create(file.path(folder))) {
    if (!dir.create(file.path(xdir, folder))) {
      svDialogs::dlg_message(message = "fail to create a new fold, please check the fold name! ")
      ######### for Windows userwinDialog(message = "fail to create a new fold, please check the fold name! ")
    }
    path_out = c()
    path_out$path <- file.path(xdir, folder)
    path_out$name <- folder
    return(path_out)
  }
}


GeneCard_main = function(){

  #input
  read.csv(file.choose())->test_file  #test.csv
  as.matrix(test_file)->test_file

  mat_id_convert = c()
  for(i in 1:nrow(test_file)){
    GeneCard_ID_Convert(test_file[i])->out
    mat_id_convert=rbind(mat_id_convert,out)
  }
  cbind(test_file, mat_id_convert)->test_file_symbol_result
  colnames(test_file_symbol_result)<-c("Previous ID","Symbol","Label")


  test_file_description_result = c()
  for(i in 1:nrow(test_file_symbol_result)){
    if(test_file_symbol_result[i,3]!="Missing" & test_file_symbol_result[i,3]!="Trust Missing"){
      GeneCard_Symbol_Details(test_file_symbol_result[i,2])->temp
      test_file_description_result = rbind(test_file_description_result, c(test_file_symbol_result[i,c(1,3)],temp))
    }
  }
  colnames(test_file_description_result)[1:3]<-c("Previous_ID","Label","Symbol")


  #layout
  output1 = c()
  output2 = c()
  output1$genecard_symbol_id_convert <- test_file_symbol_result
  output2$genecard_symbol_id_details <- test_file_description_result

  f.dir.create()->fd
  output_dir=paste(fd$path,"//",fd$name,"_",sep="")

  write.table(test_file_symbol_result , file = paste(output_dir,"genecard_symbol_id_convert.txt",sep=""), quote=F, sep="\t", row.names=F)
  write.table(test_file_description_result , file = paste(output_dir,"genecard_symbol_id_details.txt",sep=""), quote=F, sep="\t",  row.names=F)

  return(output1)
  return(output2)
}






