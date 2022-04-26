drive_auth(email="giacomo.falchetta@unive.it")

for (i in 1:length(all_input_files)){
print(i)
drive_upload(all_input_files[i], path=as_id("1TYvZuIqkHFBSXDZ2O0jqbv3Wb0i901Qq"), overwrite = T)
}


#

