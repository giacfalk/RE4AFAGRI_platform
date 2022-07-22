# jp_folder = "https://drive.google.com/drive/folders/1TYvZuIqkHFBSXDZ2O0jqbv3Wb0i901Qq"
# folder_id = drive_get(as_id(jp_folder))
# files = drive_ls(folder_id)
# lapply(files$id, drive_rm)

# sizes <- vector()
# 
# for (i in 1:length(all_input_files)){
# sizes[i] <- file.info(all_input_files[i])$size
# }
# #
# sizes <- data.frame(all_input_files, sizes)

all_input_files <- list.files(path=input_folder, recursive = T, full.names = T)

all_input_files <- all_input_files[grep(exclude_countries, all_input_files,ignore.case=TRUE, invert = TRUE)]

all_input_files <- all_input_files[grep("\\.ini$|\\.docx$|\\.png$|\\.r$|\\.mat$|r_tmp_|results|\\.pyc$|\\.pdf$|\\.rds$|\\.rdata$|\\.dbf$|\\.xml$", all_input_files,ignore.case=TRUE, invert = TRUE)] 

all_input_files <- gsub("//", "/", all_input_files)

all_input_files_basename <- basename(all_input_files)

user.input <- function(prompt) {
  x= readline(prompt)
  return(x)
}


find_it <- function(X){
  
  out_file <- all_input_files[str_detect(all_input_files_basename, paste0('\\b', X, '\\b'))]
  
  if(length(out_file)>1){
    
    beep()
    print(out_file)
    pick_one <- user.input("Which one: ")
    return(out_file[as.numeric(pick_one)])
    
  } 
  
  if(length(out_file)==0){
    
    beep()
    stop("Cannot find file")
    
  } else {
    
    return(out_file)
    
  }}


all_input_files_stub <- gsub("H:/My Drive/MLED_database/", "", all_input_files)
all_input_files_stub <- gsub("//", "/", all_input_files_stub)

#

setwd("H:/My Drive/MLED_database")

sapply(file.path("H:/My Drive/MLED_database", dirname(all_input_files_stub)),
       dir.create, recursive = TRUE, showWarnings = FALSE)

googledrive::drive_auth()

out <- list()

for (i in 1:length(all_input_files_stub)){
  while(TRUE){
    
    print(i)
    
    table <- try(drive_upload(media = all_input_files_stub[i], path = paste0('~/MLED_database/',dirname(all_input_files_stub[i]), "/"), name=basename(all_input_files_stub[i]), overwrite = T))

if(!is(table, 'try-error')) break
  }
  print(i)
  out[[i]] <- table
  
}

  write_rds(out, "C:/Users/falchetta/Documents/GitHub/mled/download_data_index.rds")
write_rds(all_input_files_stub, "C:/Users/falchetta/Documents/GitHub/mled/download_data_index_stubs.rds")

setwd("C:/Users/falchetta/Documents/GitHub/mled")
