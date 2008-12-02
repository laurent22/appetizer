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
normalInstallerDir = _Installer

FormatTime timeString, %A_Now%, yyyy-MM-dd_HH-mm-ss

releaseDirectory = _Release\%timeString%


; ****************************************************************
; Get the app version and short version (w.x.y.z) and (x.y)
; ****************************************************************

FileRead, appVersion, Resources\version.txt
StringSplit splitted, appVersion,.
appShortVersion = %splitted1%.%splitted2%


; ****************************************************************
; A function to replace ___SHORT_VERSION___ and ___FULL_VERSION___
; by the short and full version numbers in a file
; ****************************************************************

ReplaceVersionNumbers(oldFile, newFile, appVersion, appShortVersion)
{
  FileDelete %newFile%
  FileRead fileContent, %oldFile%  
  StringReplace outputVar, fileContent, ___FULL_VERSION___, %appVersion%, All
  StringReplace finalContent, outputVar, ___SHORT_VERSION___, %appShortVersion%, All
  
  FileAppend %finalContent%, %newFile%
}