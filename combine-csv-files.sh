#!/bin/bash

# To run file, type the following in the command line:
# chmod +x combine-csv-files.sh
# ./combine-csv-files.sh

mkdir data
# Combine all CSV files for a particular person if it exists
function combine_csv_files() {
	local user="$1" ##<< Name of user in CAPS
	local user_bzfiles=$(ls MJFF-Data | grep HumDynLog_$user)
	local csv_files="hdl_meta_*.csv":"hdl_audio_*.csv":"hdl_accel_*.csv":"hdl_batt_*.csv":"hdl_cmpss_*.csv":"hdl_gps_*.csv":"hdl_light_*.csv":"hdl_prox_*.csv"
	
	# Restore default IFS
	IFS=$' \t\n'

	for user_bzfile in $user_bzfiles
		do
			tar -xjf MJFF-Data/$user_bzfile # Unzip bzfile!
		done
	echo "Unzipping of files complete for $user!"
	mkdir $user
	mv HumDynLog_$user* $user/.
	echo "Moving of unzipped files complete for $user!"
	
	IFS=:
	for csv_file in $csv_files
	do
		cat $user/HumDynLog_$user*/$csv_file > data/$user-$csv_file
		echo "Combined csv file written to $user-$csv_file"
	done
  	# Restore default IFS
	IFS=$' \t\n'

	# Remove unzipped folders
	# rm -rf user_folders

	echo "Processing of files complete for $user!"
}

IFS=:
users="APPLE":"CHERRY":"CROCUS":"DAFODIL":"DAISEY":"DAISY":"FLOX":"IRIS":"LILLY":"LILY":"MAPLE":"ORANGE":"ORCHID":"PEONY":"ROSE":"SUNFLOWER":"SWEETPEA":"TESTCLIQ":"VIOLET"

for user in $users
do
	combine_csv_files $user
done
# Restore default IFS
IFS=$' \t\n'
