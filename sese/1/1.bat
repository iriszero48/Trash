@REM ffmpeg -i %%v -movflags use_metadata_tags -map_metadata 0 -c copy -c:a libopus -c:v libaom-av1 -y %%v.mkv

@REM ffmpeg -i %%v -c copy -c:a libaom-av1 -y %%v.avif
@REM exiftool -TagsFromFile %%v "-all:all>all:all" %%v.avif

set work_dir=

set exiftool_exec=exiftool.bat

pushd %work_dir%
(@for /F "usebackq tokens=*" %%f in (`dir /b ^| findstr /i /m "^.*\.jpg$ ^.*\.png$"`) do (
    ffmpeg -i "%%f" -cpu-used 0 -y "%%f.avif"
    %exiftool_exec% -charset filename="" -tagsFromFile "%%f" "-all:all>all:all" "%%f.avif"
    del "%%f.avif_original"
    del "%%f"
)) & popd

@REM pushd %work_dir%
@REM (@for /F "usebackq tokens=*" %%f in (`dir /b ^| findstr /i /m "^.*\.mp4$ ^.*\.mov$"`) do (
@REM     ffmpeg -i "%%f" -movflags use_metadata_tags -map_metadata 0 -c copy -c:a libopus -b:a 32k -c:v libaom-av1 -cpu-used 0 -y "%%f.mkv"
@REM     del "%%f"
@REM )) & popd
