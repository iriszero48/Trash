$src = $args[0]
$dest = "$src.mkv"
Write-Output "-> ffmpeg -i $src -movflags use_metadata_tags -map_metadata 0 -c copy -c:a libopus -b:a 32k -c:v libsvtav1 -preset 0 -y $dest"
ffmpeg.exe -i $src -movflags use_metadata_tags -map_metadata 0 -c copy -c:a libopus -b:a 32k -c:v libsvtav1 -preset 0 -y $dest
