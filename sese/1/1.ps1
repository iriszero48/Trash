param ($d, $del_only, $ffmpeg_exec = 'ffmpeg.exe')

$ErrorActionPreference = "Stop"

$real_exec = 1
$skip_video = 0
$only_del_src = 0

# if ($argc.count -eq 0) {
#     Write-Error "Usage: `n`t-d work_dir`n`t-del_only`n"
# }

$work_dir = $d
$only_del_src = $del_only

Write-Output "d = $work_dir"
Write-Output "del_only = $del_only"
Write-Output "ffmpeg_exec = $ffmpeg_exec"

$exiftool_exec = 'exiftool.bat'
$_7z_exec = '7z.exe'

function call_rm($path) {
    Write-Output "-> rm $_"
    if ($real_exec) { Remove-Item -LiteralPath $path }
}

function call_7z($src, $dest) {
    Write-Output "-> 7z a -mx=9 -myx=9 -ms=on -mmtf=off -md=1g -mfb=273 $dest $src"
    if ($real_exec) { &$_7z_exec a -mx=9 -myx=9 -ms=on -mmtf=off -md=1g -mfb=273 $dest $src }
}

foreach ($_ in Get-ChildItem -File -LiteralPath $work_dir) {
    Write-Output "> $_"

    $file = Get-Item -LiteralPath $_
    $ext = $file.Extension

    if ($ext -match '^\.(avif|mkv|pdf)$') {
        Write-Output "... skip $_"
        continue
    }

    if ($only_del_src) {
        call_rm($_)
        continue
    }

    $tmp = "tmp$ext"

    Write-Output "-> cp $_ $tmp"
    Copy-Item -LiteralPath $_ -Destination $tmp

    if ($ext -match '^\.(jpg|png|bmp)$') {
        $dest_ext = ".avif"
        $tmp_res = "$tmp$dest_ext"

        Write-Output "-> ffmpeg -i $tmp -cpu-used 0 -y $tmp_res"
        if ($real_exec) { &$ffmpeg_exec -i $tmp -cpu-used 0 -y $tmp_res }

        Write-Output "-> exiftool -charset filename="""" -tagsFromFile $tmp ""-all:all>all:all"" $tmp_res"
        if ($real_exec) { &$exiftool_exec -charset filename="" -tagsFromFile $tmp "-all:all>all:all" $tmp_res }

        $exif_tmp = "$($tmp_res)_original"
        Write-Output "-> rm $exif_tmp"
        if (Test-Path $exif_tmp) {
            Remove-Item $exif_tmp
        }
    }
    elseif ($ext -match '^\.(mp4|mov|3gp)$') {
        $dest_ext = ".mkv"
        $tmp_res = "$tmp$dest_ext"

        Write-Output "-> ffmpeg -i $tmp -movflags use_metadata_tags -map_metadata 0 -c copy -c:a libopus -b:a 32k -c:v libsvtav1 -preset 0 -y $tmp_res"
        if ($skip_video) {}
        elseif ($real_exec) { &$ffmpeg_exec -i $tmp -movflags use_metadata_tags -map_metadata 0 -c copy -c:a libopus -b:a 32k -c:v libsvtav1 -preset 0 -y $tmp_res }
    }
    elseif ($ext -match '^\.txt$') {
        $dest_ext = ".pdf"
        $tmp_u8 = "$tmp.u8"
        $tmp_res = "$tmp$dest_ext"

        # call_7z $tmp $tmp_res
        Write-Output "-> (Get-Content -Encoding gbk -Raw $tmp).Replace(""``n"", ""``n``n"") | Set-Content -Encoding utf8 $tmp_u8"
        (Get-Content -Encoding gbk -Raw $tmp).Replace("`n", "`n`n") | Set-Content -Encoding utf8 $tmp_u8

        Write-Output "-> pandoc -f markdown -i $tmp_u8 --pdf-engine=xelatex -V CJKmainfont:LXGWWenKai-Regular.ttf -o $tmp_res"
        pandoc -f markdown -i $tmp_u8 --pdf-engine=xelatex -V CJKmainfont:LXGWWenKai-Regular.ttf -o $tmp_res

        call_rm $tmp_u8
    }
    else {
        throw "unknow ext: $ext"
    }

    Write-Output "-> cp $tmp_res $_$dest_ext"
    if ($real_exec) { Copy-Item $tmp_res -Destination "$_$dest_ext" }

    call_rm $tmp
    call_rm $tmp_res
    # call_rm($_)
}
