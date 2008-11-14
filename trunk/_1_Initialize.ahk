; ****************************************************************
; Initialize some paths
; ****************************************************************

upxExe = c:\Program Files\UPX\upx.exe
sevenZipExe = c:\Program Files\7-Zip\7z.exe
nsisExe = C:\Program Files\NSIS\makensisw.exe
pafSourceDir = _PAF!\Other\Source
pafAppInfoDir = _PAF!\App\AppInfo
pafAppDir = _PAF!\App
releaseDir = _Sourceforge\Releases

FormatTime timeString, %A_Now%, yyyy-MM-dd_HH-mm-ss

releaseDirectory = _Release\%timeString%


; ****************************************************************
; Get the app version and short version (w.x.y.z) and (x.y)
; ****************************************************************

FileRead, appVersion, Resources\version.txt
StringSplit splitted, appVersion,.
appShortVersion = %splitted1%.%splitted2%