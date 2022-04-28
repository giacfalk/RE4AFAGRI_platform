drive_auth(email="giacomo.falchetta@unive.it")

# jp_folder = "https://drive.google.com/drive/folders/1TYvZuIqkHFBSXDZ2O0jqbv3Wb0i901Qq"
# folder_id = drive_get(as_id(jp_folder))
# files = drive_ls(folder_id)
# lapply(files$id, drive_rm)

# sizes <- vector()

# for (i in 1:length(all_input_files)){
# sizes[i] <- file.info(all_input_files[i])$size
# }
#
# sizes <- data.frame(all_input_files, sizes)

#zip::zip(zipfile="F:/compressed_MLED_db.zip", files=all_input_files, mode = "cherry-pick")

for (i in 1:length(all_input_files)){
print(i)
drive_upload(all_input_files[i], path=as_id("1TYvZuIqkHFBSXDZ2O0jqbv3Wb0i901Qq"), overwrite = T)
}


#

