#include _1_Initialize.ahk
#include _2_CopyToReleaseDir.ahk
#include _3_CopyToAppFolder.ahk

; ****************************************************************
; Build the installer files from templates, replacing
; ___FULL_VERSION___ and ___SHORT_VERSION___ by actual numbers
; ****************************************************************

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