; ****************************************************************
; Initialize some paths
; ****************************************************************

upxExe = c:\Program Files\UPX\upx.exe
sevenZipExe = c:\Program Files\7-Zip\7z.exe
nsisExe = C:\Program Files\NSIS\makensisw.exe
pafSourceDir = _PAF!\Other\Source
pafAppInfoDir = _PAF!\App\AppInfo
releaseDir = _Sourceforge\Releases

FormatTime timeString, %A_Now%, yyyy-MM-dd_HH-mm-ss

releaseDirectory = _Release\%timeString%




; ****************************************************************
; Get the app version and short version (w.x.y.z) and (x.y)
; ****************************************************************

FileRead, appVersion, Resources\version.txt
StringSplit splitted, appVersion,.
appShortVersion = %splitted1%.%splitted2%




; ****************************************************************
; Copy the required files and folders in the release folder
; Basically eveything that is needed to run Appetizer minus the
; Settings folders, .svn folders, temporary files, etc.
; ****************************************************************

FileCreateDir %releaseDirectory%

FileCopy Appetizer.exe, %releaseDirectory%
FileCopyDir Data, %releaseDirectory%\Data

Loop %releaseDirectory%\.svn, 2, 1
{
  FileRemoveDir %A_LoopFileFullPath%, 1
}

FileRemoveDir %releaseDirectory%\Data\Settings, 1

Loop %releaseDirectory%\Data\Help\*.html, 0, 1
{
  FileDelete %A_LoopFileFullPath%
}

Loop %releaseDirectory%\Data\*.po~, 0, 1
{
  FileDelete %A_LoopFileFullPath%
}

Loop %releaseDirectory%\Data\*.pot, 0, 1
{
  FileDelete %A_LoopFileFullPath%
}

Loop %releaseDirectory%\Data\Help\images, 2, 1
{
  FileRemoveDir %A_LoopFileFullPath%, 1
}

; A ENLEVER UNE FOIS QUE LA TRADUC FRANCAISE EST PRETE
FileRemoveDir %releaseDirectory%\Data\Help\fr, 1

command = %upxExe% -9 %releaseDirectory%\Appetizer.exe
RunWait %command%

zipFileName = Appetizer_%appVersion%.zip
command = %sevenZipExe% a -tzip %zipFileName% *
RunWait %command%, %releaseDirectory%

FileMove %releaseDirectory%\%zipFileName%, %releaseDir%\%zipFileName%, 1




; ****************************************************************
; Build the installer files from templates, replacing
; ___FULL_VERSION___ and ___SHORT_VERSION___ by actual numbers
; ****************************************************************

ReplaceVersionNumbers(oldFile, newFile, appVersion, appShortVersion)
{
  FileDelete %newFile%
  FileRead fileContent, %oldFile%  
  StringReplace outputVar, fileContent, ___FULL_VERSION___, %appVersion%, All
  StringReplace finalContent, outputVar, ___SHORT_VERSION___, %appShortVersion%, All
  
  FileAppend %finalContent%, %newFile%
}

ReplaceVersionNumbers("_PAFRES\appinfo.tmpl.ini", "_PAFRES\appinfo.ini", appVersion, appShortVersion)
ReplaceVersionNumbers("_PAFRES\AppetizerLauncher.tmpl.nsi", "_PAFRES\AppetizerLauncher.nsi", appVersion, appShortVersion)
ReplaceVersionNumbers("_PAFRES\PortableApps.comInstallerConfig.tmpl.nsh", "_PAFRES\PortableApps.comInstallerConfig.nsh", appVersion, appShortVersion)

FileCopy _PAFRES\appinfo.ini, %pafAppInfoDir%\appinfo.ini, 1
FileCopy _PAFRES\AppetizerLauncher.nsi, %pafSourceDir%\AppetizerLauncher.nsi, 1
FileCopy _PAFRES\PortableApps.comInstallerConfig.nsh, %pafSourceDir%\PortableApps.comInstallerConfig.nsh, 1



; ****************************************************************
; Build the PA launcher and installer
; ****************************************************************

command = %nsisExe% AppetizerLauncher.nsi
RunWait %command%, %pafSourceDir%

command = %nsisExe% PortableApps.comInstaller.nsi
RunWait %command%, %pafSourceDir%