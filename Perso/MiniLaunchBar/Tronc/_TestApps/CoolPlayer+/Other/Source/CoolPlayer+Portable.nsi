;Copyright (C) 2004-2008 John T. Haller of PortableApps.com

;Website: http://portableapps.com/CoolPlayerpPortable

;This software is OSI Certified Open Source Software.
;OSI Certified is a certification mark of the Open Source Initiative.

;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License
;as published by the Free Software Foundation; either version 2
;of the License, or (at your option) any later version.

;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.

;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

!define PORTABLEAPPNAME "CoolPlayer+ Portable"
!define APPNAME "CoolPlayer+"
!define NAME "CoolPlayer+Portable"
!define VER "1.5.9.2"
!define WEBSITE "PortableApps.com/CoolPlayerpPortable"
!define DEFAULTEXE "coolplayer+.exe"
!define DEFAULTAPPDIR "CoolPlayer+"

;=== Program Details
Name "${PORTABLEAPPNAME}"
OutFile "..\..\${NAME}.exe"
Caption "${PORTABLEAPPNAME} | PortableApps.com"
VIProductVersion "${VER}"
VIAddVersionKey ProductName "${PORTABLEAPPNAME}"
VIAddVersionKey Comments "Allows ${APPNAME} to be run from a removable drive.  For additional details, visit ${WEBSITE}"
VIAddVersionKey CompanyName "PortableApps.com"
VIAddVersionKey LegalCopyright "John T. Haller"
VIAddVersionKey FileDescription "${PORTABLEAPPNAME}"
VIAddVersionKey FileVersion "${VER}"
VIAddVersionKey ProductVersion "${VER}"
VIAddVersionKey InternalName "${PORTABLEAPPNAME}"
VIAddVersionKey LegalTrademarks "PortableApps.com is a Trademark of Rare Ideas, LLC."
VIAddVersionKey OriginalFilename "${NAME}.exe"
;VIAddVersionKey PrivateBuild ""
;VIAddVersionKey SpecialBuild ""

;=== Runtime Switches
CRCCheck On
WindowIcon Off
SilentInstall Silent
AutoCloseWindow True
RequestExecutionLevel user

; Best Compression
SetCompress Auto
SetCompressor /SOLID lzma
SetCompressorDictSize 32
SetDatablockOptimize On

;=== Include
!include "GetParameters.nsh"
!include "MUI.nsh"

;=== Program Icon
Icon "..\..\App\AppInfo\appicon.ico"

;=== Icon & Stye ===
!define MUI_ICON "..\..\App\AppInfo\appicon.ico"

;=== Languages
!insertmacro MUI_LANGUAGE "English"

LangString LauncherFileNotFound ${LANG_ENGLISH} "${PORTABLEAPPNAME} cannot be started. You may wish to re-install to fix this issue. (ERROR: $MISSINGFILEORPATH could not be found)"
LangString LauncherAlreadyRunning ${LANG_ENGLISH} "Another instance of ${APPNAME} is already running. Please close other instances of ${APPNAME} before launching ${PORTABLEAPPNAME}."
LangString LauncherAskCopyLocal ${LANG_ENGLISH} "${PORTABLEAPPNAME} appears to be running from a location that is read-only. Would you like to temporarily copy it to the local hard drive and run it from there?$\n$\nPrivacy Note: If you say Yes, your personal data within ${PORTABLEAPPNAME} will be temporarily copied to a local drive. Although this copy of your data will be deleted when you close ${PORTABLEAPPNAME}, it may be possible for someone else to access your data later."
LangString LauncherNoReadOnly ${LANG_ENGLISH} "${PORTABLEAPPNAME} can not run directly from a read-only location and will now close."

Var PROGRAMDIRECTORY
Var SETTINGSDIRECTORY
Var ADDITIONALPARAMETERS
Var EXECSTRING
Var PROGRAMEXECUTABLE
Var SECONDARYLAUNCH
Var MISSINGFILEORPATH
Var DISABLESPLASHSCREEN

Section "Main"
	;=== Check if already running
	System::Call 'kernel32::CreateMutexA(i 0, i 0, t "${NAME}") i .r1 ?e'
	Pop $0
	StrCmp $0 0 ReadINI
		StrCpy $SECONDARYLAUNCH "true"

	ReadINI:
		IfFileExists "$EXEDIR\${NAME}.ini" "" NoINI
		ReadINIStr $DISABLESPLASHSCREEN "$EXEDIR\${NAME}.ini" "${NAME}" "DisableSplashScreen"
		ReadINIStr $ADDITIONALPARAMETERS "$EXEDIR\${NAME}.ini" "${NAME}" "AdditionalParameters"
		ClearErrors
		Goto CheckForFile
		
	NoINI:
		;=== No INI file, so we'll use the defaults
		StrCpy "$ADDITIONALPARAMETERS" ""
		
	CheckForFile:
		StrCpy "$PROGRAMEXECUTABLE" "${DEFAULTEXE}"

		IfFileExists "$EXEDIR\App\${DEFAULTAPPDIR}\${DEFAULTEXE}" "" NoProgramEXE
			StrCpy "$PROGRAMDIRECTORY" "$EXEDIR\App\${DEFAULTAPPDIR}"
			StrCpy "$SETTINGSDIRECTORY" "$EXEDIR\Data\settings"
			GoTo EndINI

	EndINI:
		IfFileExists "$PROGRAMDIRECTORY\$PROGRAMEXECUTABLE" FoundProgramEXE

	NoProgramEXE:
		;=== Program executable not where expected
		StrCpy $MISSINGFILEORPATH $PROGRAMEXECUTABLE
		MessageBox MB_OK|MB_ICONEXCLAMATION `$(LauncherFileNotFound)`
		Abort
		
	FoundProgramEXE:
		;=== Check if running
		StrCmp $SECONDARYLAUNCH "true" ShowSplashScreen
		FindProcDLL::FindProc "${DEFAULTEXE}"
		StrCmp $R0 "1" WarnAnotherInstance ShowSplashScreen

	WarnAnotherInstance:
		MessageBox MB_OK|MB_ICONINFORMATION `$(LauncherAlreadyRunning)`
		Abort
		
	ShowSplashScreen:
		StrCmp $DISABLESPLASHSCREEN "true" GetPassedParameters
		;=== Show the splash screen while processing registry entries
		InitPluginsDir
		File /oname=$PLUGINSDIR\splash.jpg "${NAME}.jpg"
		newadvsplash::show /NOUNLOAD 1200 0 0 -1 /L $PLUGINSDIR\splash.jpg
	
	GetPassedParameters:
		;=== Get any passed parameters
		Call GetParameters
		Pop $0
		StrCmp "'$0'" "''" "" LaunchProgramParameters

		;=== No parameters
		StrCpy $EXECSTRING `"$PROGRAMDIRECTORY\$PROGRAMEXECUTABLE"`
		Goto AdditionalParameters

	LaunchProgramParameters:
		StrCpy $EXECSTRING `"$PROGRAMDIRECTORY\$PROGRAMEXECUTABLE" $0`

	AdditionalParameters:
		StrCmp $ADDITIONALPARAMETERS "" SettingsDirectory

		;=== Additional Parameters
		StrCpy $EXECSTRING `$EXECSTRING $ADDITIONALPARAMETERS`
	
	SettingsDirectory:
		;=== Set the settings directory if we have a path
		IfFileExists "$SETTINGSDIRECTORY\coolplayer.ini" CheckForSettings
			CreateDirectory $SETTINGSDIRECTORY
	
	CheckForSettings:
		StrCmp $SECONDARYLAUNCH "true" LaunchAndExit
		IfFileExists "$PROGRAMDIRECTORY\coolplayer.ini" ReconstructLastDir
		IfFileExists "$SETTINGSDIRECTORY\coolplayer.ini" MoveSettings
		IfFileExists "$EXEDIR\App\DefaultData\settings\coolplayer.ini" "" LaunchNow
			CopyFiles /SILENT "$EXEDIR\App\DefaultData\settings\*.*" "$PROGRAMDIRECTORY"
			WriteINIStr "$PROGRAMDIRECTORY\coolplayer.ini" "Skin" "SkinFile1" "$PROGRAMDIRECTORY\CoolPlayer+PortableSkin\CoolPlayer+Portable.ini"
			WriteINIStr "$PROGRAMDIRECTORY\coolplayer.ini" "Skin" "SkinFile2" "$PROGRAMDIRECTORY\CoolPlayer+PortableSkin\CoolPlayer+Portable_EQ.ini"
			WriteINIStr "$PROGRAMDIRECTORY\coolplayer.ini" "Skin" "SkinFile3" "$PROGRAMDIRECTORY\CoolPlayer+PortableSkin\CoolPlayer+Portable_shade.ini"
			
			ReadEnvStr $0 "PortableApps.comMusic"
			StrCmp $0 "" ManuallyCheckForDocuments
				IfFileExists "$0\*.*" "" ManuallyCheckForDocuments
				WriteINIStr "$PROGRAMDIRECTORY\coolplayer.ini" "LastDirectory" "Directory" "$0\"
				Goto ReconstructLastDir
				
			ManuallyCheckForDocuments:
				StrCpy $1 $EXEDIR 1 ;get current drive letter
				IfFileExists "$1:\Documents\Music\*.*" "" SetPathToRoot
				WriteINIStr "$PROGRAMDIRECTORY\coolplayer.ini" "LastDirectory" "Directory" "$1:\Documents\Music\"
				Goto ReconstructLastDir
			
			SetPathToRoot:
				WriteINIStr "$PROGRAMDIRECTORY\coolplayer.ini" "LastDirectory" "Directory" "$1:\"
				Goto ReconstructLastDir

	MoveSettings:
		Rename "$SETTINGSDIRECTORY\coolplayer.ini" "$PROGRAMDIRECTORY\coolplayer.ini"
		Rename "$SETTINGSDIRECTORY\default.m3u" "$PROGRAMDIRECTORY\default.m3u"

	ReconstructLastDir:
		ReadINIStr $0 "$PROGRAMDIRECTORY\coolplayer.ini" "LastDirectory" "Directory"
		StrCpy $1 $EXEDIR 1 ;get current drive letter
		StrCpy $2 $0 "" 1 ;get the last path without the drive letter
		WriteINIStr "$PROGRAMDIRECTORY\coolplayer.ini" "LastDirectory" "Directory" "$1$2"
		
		ReadINIStr $0 "$PROGRAMDIRECTORY\coolplayer.ini" "Skin" "SkinFile1"
		StrCpy $1 $EXEDIR 1 ;get current drive letter
		StrCpy $2 $0 "" 1 ;get the last path without the drive letter
		IfFileExists "$1$2" "" UseDefaultSkin
			WriteINIStr "$PROGRAMDIRECTORY\coolplayer.ini" "Skin" "SkinFile1" "$1$2"
			Goto ReconstructRemainingSkinDirs
		UseDefaultSkin:
			WriteINIStr "$PROGRAMDIRECTORY\coolplayer.ini" "Skin" "SkinFile1" "$PROGRAMDIRECTORY\CoolPlayer+PortableSkin\CoolPlayer+Portable.ini"
		
	ReconstructRemainingSkinDirs:
		ReadINIStr $0 "$PROGRAMDIRECTORY\coolplayer.ini" "Skin" "SkinFile2"
		StrCpy $1 $EXEDIR 1 ;get current drive letter
		StrCpy $2 $0 "" 1 ;get the last path without the drive letter
		WriteINIStr "$PROGRAMDIRECTORY\coolplayer.ini" "Skin" "SkinFile2" "$1$2"
		
		ReadINIStr $0 "$PROGRAMDIRECTORY\coolplayer.ini" "Skin" "SkinFile3"
		StrCpy $1 $EXEDIR 1 ;get current drive letter
		StrCpy $2 $0 "" 1 ;get the last path without the drive letter
		WriteINIStr "$PROGRAMDIRECTORY\coolplayer.ini" "Skin" "SkinFile3" "$1$2"

		ReadINIStr $0 "$PROGRAMDIRECTORY\coolplayer.ini" "Misc" "RememberLastSong"
		StrCpy $1 $EXEDIR 1 ;get current drive letter
		StrCpy $2 $0 "" 1 ;get the last path without the drive letter
		WriteINIStr "$PROGRAMDIRECTORY\coolplayer.ini" "Misc" "RememberLastSong" "$1$2"

	LaunchNow:
		ExecWait $EXECSTRING
		
	CheckRunning:
		Sleep 1000
		FindProcDLL::FindProc "${DEFAULTEXE}"                  
		StrCmp $R0 "1" CheckRunning

	;=== Put the settings file back
	Sleep 500
	Rename "$PROGRAMDIRECTORY\coolplayer.ini" "$SETTINGSDIRECTORY\coolplayer.ini"
	Rename "$PROGRAMDIRECTORY\default.m3u" "$SETTINGSDIRECTORY\default.m3u"
	Goto TheEnd
	
	LaunchAndExit:
		Exec $EXECSTRING

	TheEnd:
		newadvsplash::stop /WAIT
SectionEnd