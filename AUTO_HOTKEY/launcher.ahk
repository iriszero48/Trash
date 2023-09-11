main_gui := Gui("+Resize")

filter := main_gui.AddEdit("r1 w700 -Wrap")
viewer := main_gui.AddListView("xm r8 w700 Icon Redraw", ["Name", "full_path"])

small_icon_list := IL_Create(10)
big_icon_list := IL_Create(10, 10, true)

viewer.SetImageList(small_icon_list)
viewer.SetImageList(big_icon_list)

viewer.OnEvent("DoubleClick", (GuiCtrlObj, Info) => Run(viewer.GetText(Info, 2)))
filter.OnEvent("Change", (GuiCtrlObj, *) => apply_filter(GuiCtrlObj.Value))
main_gui.OnEvent("Size", windows_resize)

main_gui.Show()

apply_filter("")

apply_filter(filter_exp) {
    static icon_cache := Map()

    file_info_size := A_PtrSize + 688
    file_info := Buffer(file_info_size)

    viewer.Delete()

    ; viewer.Opt("-Redraw")
    loop files A_Desktop "\*", "R"
    {
        filename := A_LoopFilePath
        SplitPath filename, , &parent_path, , &filename_without_ext
        if StrLen(filter_exp) == 0 || InStr(filename_without_ext, filter_exp) {
            icon_id := icon_cache.Has(filename) ? icon_cache[filename] : 0
            if not icon_id
            {
                if not DllCall("Shell32\SHGetFileInfoW", "Str", filename
                    , "Uint", 0, "Ptr", file_info, "UInt", file_info_size, "UInt", 0x000000100)
                {
                    icon_id := 9999999
                }
                else
                {
                    icon_ptr := NumGet(file_info, 0, "Ptr")
                    icon_id := DllCall("ImageList_ReplaceIcon", "Ptr", small_icon_list, "Int", -1, "Ptr", icon_ptr) + 1
                    DllCall("ImageList_ReplaceIcon", "Ptr", big_icon_list, "Int", -1, "Ptr", icon_ptr)
                    DllCall("DestroyIcon", "Ptr", icon_ptr)
                    icon_cache[filename] := icon_id
                }
            }

            viewer.Add("Icon" . icon_id, filename_without_ext, A_LoopFilePath)
        }
    }
    ; viewer.Opt("+Redraw")
}

windows_resize(GuiObj, MinMax, Width, Height) {
    if MinMax = -1
        return

    filter.Move(, , Width - 20)

    viewer.Move(, , Width - 20, Height - 40)
    ; viewer.ModifyCol()
    viewer.ModifyCol(1, "AutoHdr")
}