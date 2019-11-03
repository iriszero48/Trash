For x264：
vspipe --y4m script.vpy - | x264 --demuxer y4m - --output encoded.mkv
For FFmpeg:
vspipe --y4m script.vpy - | ffmpeg -i pipe: enc
