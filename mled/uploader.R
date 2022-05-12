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

all_input_files_stub <- gsub("F:/MLED_database/", "", all_input_files)
all_input_files_stub <- gsub("//", "/", all_input_files_stub)

#

setwd("F:/MLED_database/")

sapply(file.path("H:/Il mio Drive/MLED_database", dirname(all_input_files_stub)),
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

write_rds(out, "D:/OneDrive - IIASA/RE4AFAGRI_platform/mled/download_data_index.rds")
write_rds(all_input_files_stub, "D:/OneDrive - IIASA/RE4AFAGRI_platform/mled/download_data_index_stubs.rds")

setwd("D:/OneDrive - IIASA/RE4AFAGRI_platform/mled")
