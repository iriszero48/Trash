#Requires AutoHotkey v2.0

MyGui := Gui("Resize")

MyText1 := MyGui.AddText(, "要修改的图片:")
MyEdit1 := MyGui.AddEdit("R1 section w600 -Wrap")

MyText2 := MyGui.AddText(, "tag来源:")
MyEdit2 := MyGui.AddEdit("R1 section w600 -Wrap")

MyText3 := MyGui.AddEdit("R30 section w600")

MyBtn := MyGui.AddButton("Default w80", "处理")
MyBtn_Click(GuiCtrlObj, Info) {
    shell := ComObject("WScript.Shell")
    cmd := Format("exiftool.bat -tagsFromFile `"{1}`" `"{2}`" 2>&1", MyEdit2.Value, MyEdit1.Value)
    MyText3.Value := "-> " . cmd . "`n"
    exec := shell.Exec(cmd)
    MyText3.Value .= exec.StdOut.ReadAll() . "`n"
}
MyBtn.OnEvent("Click", MyBtn_Click)

Gui_DropFiles(GuiObj, GuiCtrlObj, FileArray, X, Y) {
    GuiCtrlObj.Value := FileArray[1]
}
MyGui.OnEvent("DropFiles", Gui_DropFiles)

MyGui.Show()
