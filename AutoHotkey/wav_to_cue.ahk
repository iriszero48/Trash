#Requires AutoHotkey v2.0

MainGUi := Gui("Resize")

global FileList := ""
global CueData := ""
global ff_in := ""
global ff_fc := ""
global TrackIdx := 0

MainGUi.AddText(,"raw files path:")
PathInput := MainGUi.AddEdit("R1 section w600 -Wrap")

MainGUi.AddText(,"output file path:")
PathOutput := MainGUi.AddEdit("R1 section w600 -Wrap")

CueEditor := MainGUi.AddEdit("R30 section w600 -Wrap")

LoadBtn := MainGUi.AddButton("section Default w80", "加载")
LoadBtn_Click(GuiCtrlObj, Info) {
    CueData := "REM GENRE ...`n"
    CueData .= "REM DATE ...`n"
    CueData .= "PERFORMER `"`"`n"
    CueData .= "TITLE `"`"`n"
    CueData .= "FILE `"`" WAVE`n"

    FileList := ""
    global TrackIdx := 0
    global ff_in := ""
    global ff_fc := ""

    SetWorkingDir PathInput.Value
    shell := ComObject("WScript.Shell")

    Loop Files, Format("{}\*.*", PathInput.Value)
    {
        FileList .= A_LoopFileName "`n"
        global TrackIdx := TrackIdx + 1
        CueData .= Format("  TRACK {:02d} AUDIO`n", TrackIdx)
        CueData .= Format("    TITLE `"{}`"`n", A_LoopFileName)
        CueData .= "    PERFORMER `"`"`n"

        mm := 0
        ss := 0
        ff := 0
        if (TrackIdx > 1)
        {
            ; ff_in := StrReplace(FileList, "`n", "|")
            tmp_file := "tmp.wav"
            ff_conv_cmd := Format("ffmpeg {1} -filter_complex `"{2}concat=n={3}:v=0:a=1[outa]`" -map `"[outa]`" -y `"{4}`"", ff_in, ff_fc, TrackIdx - 1, tmp_file)
            ; CueData .= "-> " . ff_conv_cmd
            ff_conv := shell.Exec(ff_conv_cmd)
            ff_conv.StdOut.ReadAll()

            ff_ts_cmd := Format("ffprobe -v error -show_entries stream=duration_ts -of default=noprint_wrappers=1:nokey=1 {}", tmp_file)
            ff_ts := shell.Exec(ff_ts_cmd)
            duration_ts := StrReplace(ff_ts.StdOut.ReadAll(), "`r`n") + 0

            ff_ss_cmd := Format("ffprobe -v error -show_entries stream=sample_rate -of default=noprint_wrappers=1:nokey=1 {}", tmp_file)
            ff_ss := shell.Exec(ff_ss_cmd)
            sample_rate := StrReplace(ff_ss.StdOut.ReadAll(), "`r`n") + 0

            mm := Floor(duration_ts / sample_rate / 60)
            duration_ts -= mm * 60 * sample_rate
            ss := Floor(duration_ts / sample_rate)
            duration_ts -= ss * sample_rate
            ff := Round(duration_ts / (sample_rate / 75))

            FileDelete(Format("{1}\{2}", PathInput.Value, tmp_file))
        }
        CueData .= Format("    INDEX 01 {1:02d}:{2:02d}:{3:02d}`n", mm, ss, ff)

        global ff_in .= Format("-i `"{}`" ", A_LoopFileName)
        global ff_fc .= Format("[{:d}:a:0]", TrackIdx - 1)
    }
    CueEditor.Value := CueData
}
LoadBtn.OnEvent("Click", LoadBtn_Click)

ConvBtn := MainGUi.AddButton("section Default w80", "导出")
ConvBtn_Click(GuiCtrlObj, Info) {
    RegExMatch(CueEditor.Value, "FILE `"(.*)`" WAVE", &res)
    flac_filename := res[1]
    out_flac := Format("{1}\{2}", PathOutput.Value, flac_filename)
    SplitPath flac_filename,,,, &name_no_ext
    out_cue := Format("{1}\{2}.cue", PathOutput.Value, name_no_ext)

    shell := ComObject("WScript.Shell")
    ff_conv_cmd := Format("ffmpeg {1} -filter_complex `"{2}concat=n={3}:v=0:a=1[outa]`" -map `"[outa]`" -y `"{4}`"", ff_in, ff_fc, TrackIdx, out_flac)
    CueEditor.Value := Format("REM COMMENT `"{}`"`n", StrReplace(ff_conv_cmd, "`"", "'")) . CueEditor.Value
    ff_conv := shell.Exec(ff_conv_cmd)

    fs := FileOpen(out_cue, "w", "utf-8")
    fs.Write(CueEditor.Value)
    fs.Close()
}
ConvBtn.OnEvent("Click", ConvBtn_Click)

Gui_DropFiles(GuiObj, GuiCtrlObj, FileArray, X, Y) {
    GuiCtrlObj.Value := FileArray[1]
}
MainGUi.OnEvent("DropFiles", Gui_DropFiles)

Gui_Resize(thisGui, MinMax, Width, Height) {
    if MinMax = -1
        return

    PathInput.Move(,, Width - 20)
    PathOutput.Move(,, Width - 20)
    CueEditor.Move(,, Width - 20)
}
MainGUi.OnEvent("Size", Gui_Resize)

MainGUi.Show()
