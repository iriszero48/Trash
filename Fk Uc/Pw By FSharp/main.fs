open System
open System.IO

let RootPath = "36,)+Lk_uQLj[[GH[KeI9}SM[MKt-VAnA)e;;4%09_9IQ34Rg_GwPK`h"
let DonePath = "36,)+Lk_uQLj[[GH[KeI9}SM[MKt-VAnA)e;;4%09_9IQ34Rg_Gus9XHOh"

let GetKid (fullpath:string) =
    let path = Path.GetFileNameWithoutExtension(fullpath) in
    match path.IndexOf('[') with
    | 0 -> Some(path.Substring(1, path.IndexOf(']') - 1))
    | _ -> None

let rec GetAllFiles dir =
    seq { yield! Directory.EnumerateFiles(dir)
          for d in Directory.EnumerateDirectories(dir) do
              yield! GetAllFiles d }

let GetKinds _ =
    GetAllFiles RootPath
    |> Seq.choose GetKid
    |> Seq.distinct
    |> Seq.iter (printfn "%A")

//GetKinds()

let KindMap = dict [
"0uPqDcO7UWrk}ZfTW57M-CsF;MKjb6sLUJ@`k^a3SF7BlsOpmheAd8{VXD%RsMlHT19Gh"
"0uK$XB{4,BroxR{23O0).V)Wvs)xT3ewpRaYAJ+[sN`D$dzZ%i6zh"
"0uPt`e'nRWsN8qYU7S5i-Cr2}sk^TaZ`dnCfF)CjsN`D$dzZ%i6zh"
"0uPnDc0-R`sN`D$dzZ%l-CsF;MKjb6s,CDA`0z1STzpV5-}8Zh"
"0uPhjQhVE3s,6`Id{{zy-Cw{7fbL&{rSo(+dD5[wVtzl8rjd~aVO2MhaqhUasMlHT19Gh"
"0uPk.ckN'lsO^adSw^fU-Cq!a6zh"
"0uPnEar&Sls,Jmk`1]`n-Cw{9ar&Sls,Jmk`1dY=Vti,,sOpmheA]=z6zh"
"0uPhfT_KMJrSJ06e}N]G-Cwu^QLhC%rSJ,1e^-EyT{1rn-}8Zh"
"0uPhh]~layrSozJVlPDK-CwxcQLMN+rSozJVlW7G]~laysOpmheA]=z6zh"
"0uPklb3Gumrn$OMd_H!l-CsF;MKjb6rp;yh[Cu``b2qO_sPu$G`0s1d6zh"
"0uPk[^_VWMsK~j1d~0h(-CwxS^_VWMsK~j1d~7h+Vtzl8s19bB[YmA@6zh"
"0uPt-T_Sa8sL^Q!_19(J-Cx!fVtzl8rlhA7e#lOcT_Sa8sL^Q!_1D%HVX_@n-}8Zh"
"0uPhfeI8_xrm'4.VN}N=-CwxE[I52wrSKpiZGcos^C,LFsN`D$dzZ%i6zh"
"0uPhhe'vr2s)xm8d~0h(-Cwu`e'vr2s)xm8d~7gqTzpV5-}8Zh"
"0uPn=ZLP=Yt('rKUpuxU-Cw{3ZLP=Yt('rKUp}rRWqvA3sN`D$dzZ%i6zh"
"0uPk7Yk]eOsNk'_XiGyn-CwxJYknx9s)xT1USQ}6]FHTmt(QlEdzk,QQhVE!sm7aY19Gh"
"0uPk_V9)z&t&j'aY}h-C-CwxVV9)z&t&j'aY}o-.Y6M)Hh"
"0uPkIfFAF1rjo$#Wm2-r-CwxAfFAF1rjo$#Wm9-tVtzl8-}8Zh"
]

GetAllFiles RootPath
|> Seq.filter (Path.GetFileNameWithoutExtension >> String.IsNullOrEmpty >> not)
//|> Seq.filter (fun x -> x.Contains "rl`!PT545T_@@K_h")
//|> Seq.iter (printfn "%A")
//[]
|> Seq.map(fun x ->
    let pp = Path.GetDirectoryName x in
    let fn = Path.GetFileName x in
    let kid = GetKid fn in
    let ex = match kid with | Some kd -> KindMap.Item kd | None -> "" in
    (pp, fn, ex,
        match kid with
        | Some kd ->
            sprintf "%s_%s%s"
                (Path.GetFileNameWithoutExtension(fn).Replace(sprintf "[%s]" kd, ""))
                ex
                (Path.GetExtension fn)
        | _ -> ""))
|> Seq.iter (fun x ->
    let pp, fn, ex, nfn = x in
    printfn "%A" (fn, nfn)
    let fnfn = Console.ReadLine()
    let mvb = Path.Combine(pp, fn)
    let mva = Path.Combine(DonePath, if String.IsNullOrEmpty fnfn then nfn else (sprintf "%s_%s%s" fnfn ex (Path.GetExtension mvb)))
    printfn "%A -> %A" mvb mva
    File.Move(mvb, mva))
