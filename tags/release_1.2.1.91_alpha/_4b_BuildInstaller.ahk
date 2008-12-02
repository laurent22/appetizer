#include _1_Initialize.ahk
#include _2_CopyToReleaseDir.ahk
#include _3_CopyToAppFolder.ahk

; ****************************************************************
; Build the installer files from templates, replacing
; ___FULL_VERSION___ and ___SHORT_VERSION___ by actual numbers
; ****************************************************************

ReplaceVersionNumbers("_PAFRES\NormalInstaller.tmpl.nsi", "_PAFRES\NormalInstaller.nsi", appVersion, appShortVersion)

FileCopy _PAFRES\NormalInstaller.nsi, %normalInstallerDir%\NormalInstaller.nsi, 1


; ****************************************************************
; Build the PA launcher and installer
; ****************************************************************

command = %nsisExe% NormalInstaller.nsi
RunWait %command%, %normalInstallerDir%
