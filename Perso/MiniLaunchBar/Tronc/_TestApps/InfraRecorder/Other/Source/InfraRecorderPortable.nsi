;Copyright (C) 2004-2008 John T. Haller
;Copyright (C) 2008 Travis Carrico

;Website: http://PortableApps.com/InfraRecorderPortable

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

!define PORTABLEAPPNAME "InfraRecorder Portable"
!define APPNAME "InfraRecorder"
!define NAME "InfraRecorderPortable"
!define VER "1.5.8.0"
!define WEBSITE "PortableApps.com/InfraRecorderPortable"
!define DEFAULTEXE "InfraRecorder.exe"
!define OTHEREXE "irExpress.exe"
!define DEFAULTAPPDIR "InfraRecorder"
!define DEFAULTSETTINGSPATH "settings"

;=== Program Details
Name "${PORTABLEAPPNAME}"
OutFile "..\..\${NAME}.exe"
Caption "${PORTABLEAPPNAME} | PortableApps.com"
VIProductVersion "${VER}"
VIAddVersionKey ProductName "${PORTABLEAPPNAME}"
VIAddVersionKey Comments "Allows ${APPNAME} to be run from a removable drive.  For additional details, visit ${WEBSITE}"
VIAddVersionKey CompanyName "PortableApps.com"
VIAddVersionKey LegalCopyright "PortableApps.com and contributers"
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
!include "Registry.nsh"
!include "TextFunc.nsh"
!insertmacro ConfigWrite
!insertmacro ConfigRead

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
Var OTHEREXECUTABLE
Var INIPATH
Var DISABLESPLASHSCREEN
Var ISDEFAULTDIRECTORY
Var SECONDARYLAUNCH
Var MISSINGFILEORPATH
Var KEY1
Var KEY2
Var KEY3
Var KEY4
Var KEY5
Var KEY6
Var KEY7
Var KEY8

Section "Main"
	;=== Check if already running
	System::Call 'kernel32::CreateMutexA(i 0, i 0, t "${NAME}2") i .r1 ?e'
	Pop $0
	StrCmp $0 0 CheckForINI
		StrCpy $SECONDARYLAUNCH "true"
	
	CheckForINI:
	;=== Find the INI file, if there is one
		IfFileExists "$EXEDIR\${NAME}.ini" "" NoINI
			StrCpy "$INIPATH" "$EXEDIR"
			Goto ReadINI

	ReadINI:
		;=== Read the parameters from the INI file
		ReadINIStr $0 "$INIPATH\${NAME}.ini" "${NAME}" "${APPNAME}Directory"
		StrCpy "$PROGRAMDIRECTORY" "$EXEDIR\$0"
		ReadINIStr $0 "$INIPATH\${NAME}.ini" "${NAME}" "SettingsDirectory"
		StrCpy "$SETTINGSDIRECTORY" "$EXEDIR\$0"

		;=== Check that the above required parameters are present
		IfErrors NoINI

		ReadINIStr $0 "$INIPATH\${NAME}.ini" "${NAME}" "AdditionalParameters"
		StrCpy "$ADDITIONALPARAMETERS" $0
		ReadINIStr $0 "$INIPATH\${NAME}.ini" "${NAME}" "${APPNAME}Executable"
		StrCpy "$PROGRAMEXECUTABLE" $0
		ReadINIStr $0 "$INIPATH\${NAME}.ini" "${NAME}" "DisableSplashScreen"
		StrCpy "$DISABLESPLASHSCREEN" $0

	;CleanUpAnyErrors:
		;=== Any missing unrequired INI entries will be an empty string, ignore associated errors
		ClearErrors

		;=== Correct PROGRAMEXECUTABLE if blank
		StrCmp $PROGRAMEXECUTABLE "" "" CheckForProgramINI
			StrCpy "$PROGRAMEXECUTABLE" "${DEFAULTEXE}"
			StrCpy "$OTHEREXECUTABLE" "${OTHEREXE}"
			Goto CheckForProgramINI
			
	CheckForProgramINI:
		IfFileExists "$PROGRAMDIRECTORY\$PROGRAMEXECUTABLE" FoundProgramEXE NoProgramEXE

	NoINI:
		;=== No INI file, so we'll use the defaults
		StrCpy "$ADDITIONALPARAMETERS" ""
		StrCpy "$PROGRAMEXECUTABLE" "${DEFAULTEXE}"
		StrCpy "$DISABLESPLASHSCREEN" "false"

		IfFileExists "$EXEDIR\App\${DEFAULTAPPDIR}\${DEFAULTEXE}" "" NoProgramEXE
			StrCpy "$PROGRAMDIRECTORY" "$EXEDIR\App\${DEFAULTAPPDIR}"
			StrCpy "$SETTINGSDIRECTORY" "$EXEDIR\Data\${DEFAULTSETTINGSPATH}"
			StrCpy "$ISDEFAULTDIRECTORY" "true"
			GoTo FoundProgramEXE

	NoProgramEXE:
		;=== Program executable not where expected
		StrCpy $MISSINGFILEORPATH $PROGRAMEXECUTABLE
		MessageBox MB_OK|MB_ICONEXCLAMATION `$(LauncherFileNotFound)`
		Abort
		
	FoundProgramEXE:
		;=== Check if already running
		StrCmp $SECONDARYLAUNCH "true" CheckForSettings
		FindProcDLL::FindProc "$PROGRAMEXECUTABLE"                 
		StrCmp $R0 "1" WarnAnotherInstance
		FindProcDLL::FindProc "$OTHEREXECUTABLE"                 
		StrCmp $R0 "1" WarnAnotherInstance CheckForSettings

	WarnAnotherInstance:
		MessageBox MB_OK|MB_ICONINFORMATION `$(LauncherAlreadyRunning)`
		Abort
	
	CheckForSettings:
		IfFileExists "$SETTINGSDIRECTORY\Settings.xml" SettingsFound
		StrCmp $SECONDARYLAUNCH "true" SettingsFound
		;=== No settings found
		StrCmp $ISDEFAULTDIRECTORY "true" CopyDefaultSettings
		CreateDirectory $SETTINGSDIRECTORY
		Goto SettingsFound
	
	CopyDefaultSettings:
		CreateDirectory "$EXEDIR\Data"
		CreateDirectory "$EXEDIR\Data\settings"
		CopyFiles /SILENT $EXEDIR\App\DefaultData\settings\*.* $EXEDIR\Data\settings
		Goto SettingsFound

	SettingsFound:
		StrCmp $DISABLESPLASHSCREEN "true" AdjustTempPath
			;=== Show the splash screen before processing the files
			InitPluginsDir
			File /oname=$PLUGINSDIR\splash.jpg "${NAME}.jpg"	
			newadvsplash::show /NOUNLOAD 1200 0 0 -1 /L $PLUGINSDIR\splash.jpg
	
	AdjustTempPath:
		CreateDirectory "$TEMP\${NAME}"
		;=== convert the settings file from unicode-16be to ansi settings file to be able to edit it
		unicode::FileUnicode2Ansi "$SETTINGSDIRECTORY\Settings.xml" "$SETTINGSDIRECTORY\Settings-ascii.xml" "UTF-16LE"
		Pop $R0
		${ConfigWrite} "$SETTINGSDIRECTORY\Settings-ascii.xml" "			<TempPath>" "$TEMP\${NAME}\</TempPath>" $R0
		;=== Get the last path
		${ConfigRead} "$SETTINGSDIRECTORY\Settings-ascii.xml" "			<ShellDir>" $0
		StrCmp $0 "DEFAULT</ShellDir>" "" AdjustShellDir
			ReadEnvStr $1 "PortableApps.comDocuments"
			StrCmp $1 "" SetRootDirectoryAsDefault
				${ConfigWrite} "$SETTINGSDIRECTORY\Settings-ascii.xml" "			<ShellDir>" "$1\</ShellDir>" $R0
				Goto ResetWindowPosition
				
		SetRootDirectoryAsDefault:
			StrCpy $1 $EXEDIR 2
			${ConfigWrite} "$SETTINGSDIRECTORY\Settings-ascii.xml" "			<ShellDir>" "$1\</ShellDir>" $R0
			Goto ResetWindowPosition
			
		AdjustShellDir:
			StrCpy $1 $0 "" 1 ;Path of last shell minus drive letter
			StrCpy $2 $EXEDIR 1 ;drive letter
			StrCpy $3 "$2$1"
			${ConfigWrite} "$SETTINGSDIRECTORY\Settings-ascii.xml" "			<ShellDir>" "$3" $R0
			
		ResetWindowPosition:
			${ConfigWrite} "$SETTINGSDIRECTORY\Settings-ascii.xml" "			<WindowLeft>" "</WindowLeft>" $R0
			${ConfigWrite} "$SETTINGSDIRECTORY\Settings-ascii.xml" "			<WindowRight>" "</WindowRight>" $R0
			${ConfigWrite} "$SETTINGSDIRECTORY\Settings-ascii.xml" "			<WindowTop>" "</WindowTop>" $R0
			${ConfigWrite} "$SETTINGSDIRECTORY\Settings-ascii.xml" "			<WindowBottom>" "</WindowBottom>" $R0
			Goto ConvertSettingsBackToUnicode
		
		ConvertSettingsBackToUnicode:
			Delete "$SETTINGSDIRECTORY\Settings.xml"
			unicode::FileAnsi2Unicode "$SETTINGSDIRECTORY\Settings-ascii.xml" "$SETTINGSDIRECTORY\Settings.xml" "UTF-16LE"
			Pop $R0	
			Delete "$SETTINGSDIRECTORY\Settings-ascii.xml"
			Goto GetPassedParameters

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
		StrCmp $ADDITIONALPARAMETERS "" CheckKey1

		;=== Additional Parameters
		StrCpy $EXECSTRING `$EXECSTRING $ADDITIONALPARAMETERS`
	
	CheckKey1:
		${registry::KeyExists} "HKEY_CURRENT_USER\Software\Cygnus Solutions" $R0
		StrCmp $R0 "0" CheckKey2
		StrCpy $KEY1 $R0
		Goto CheckKey5
	
	CheckKey2:
		${registry::KeyExists} "HKEY_CURRENT_USER\Software\Cygnus Solutions\Cygwin" $R0
		StrCmp $R0 "0" CheckKey3
		StrCpy $KEY2 $R0
		Goto CheckKey5
		
	CheckKey3:
		${registry::KeyExists} "HKEY_CURRENT_USER\Software\Cygnus Solutions\Cygwin\mounts v2" $R0
		StrCmp $R0 "0" CheckKey4
		StrCpy $KEY3 $R0
		
	CheckKey4:
		${registry::KeyExists} "HKEY_CURRENT_USER\Software\Cygnus Solutions\Cygwin\Program Options" $R0
		StrCmp $R0 "0" CheckKey5
		StrCpy $KEY4 $R0
		
	CheckKey5:
		userInfo::getAccountType
		Pop $0
		StrCmp $0 "Admin" "" MoveSettings
		${registry::KeyExists} "HKEY_LOCAL_MACHINE\Software\Cygnus Solutions" $R0
		StrCmp $R0 "0" CheckKey6
		StrCpy $KEY5 $R0
		Goto MoveSettings
		
	CheckKey6:
		${registry::KeyExists} "HKEY_LOCAL_MACHINE\Software\Cygnus Solutions\Cygwin" $R0
		StrCmp $R0 "0" CheckKey7
		StrCpy $KEY6 $R0
		Goto MoveSettings
		
	CheckKey7:
		${registry::KeyExists} "HKEY_LOCAL_MACHINE\Software\Cygnus Solutions\Cygwin\mounts v2" $R0
		StrCmp $R0 "0" CheckKey8
		StrCpy $KEY7 $R0
	
	CheckKey8:
		${registry::KeyExists} "HKEY_LOCAL_MACHINE\Software\Cygnus Solutions\Cygwin\Program Options" $R0
		StrCmp $R0 "0" MoveSettings
		StrCpy $KEY8 $R0

	MoveSettings:
		StrCmp $SECONDARYLAUNCH "true" LaunchAndExit
		Sleep 100
		Rename "$SETTINGSDIRECTORY\Settings.xml" "$PROGRAMDIRECTORY\Settings.xml"
		Sleep 100
		Goto LaunchNow
	
	LaunchNow:
		Sleep 100
		ExecWait $EXECSTRING
		
	CheckRunning:
		Sleep 1000
		FindProcDLL::FindProc "${DEFAULTEXE}"                  
		StrCmp $R0 "1" CheckRunning
		FindProcDLL::FindProc "${OTHEREXE}"
		StrCmp $R0 "1" CheckRunning
		Goto MoveBack
		
	MoveBack:
		Rename "$PROGRAMDIRECTORY\Settings.xml" "$SETTINGSDIRECTORY\Settings.xml"
		Delete "$PROGRAMDIRECTORY\Devices.xml"
		RMDir /r "$TEMP\${NAME}"

		StrCmp $KEY1 "-1" "" DeleteKey2
		${registry::DeleteKeyEmpty} "HKEY_CURRENT_USER\Software\Cygnus Solutions\Cygwin\mounts v2" $R0
		Sleep 100
		${registry::DeleteKeyEmpty} "HKEY_CURRENT_USER\Software\Cygnus Solutions\Cygwin\Program Options" $R0
		Sleep 100
		${registry::DeleteKeyEmpty} "HKEY_CURRENT_USER\Software\Cygnus Solutions\Cygwin" $R0
		Sleep 100
		${registry::DeleteKeyEmpty} "HKEY_CURRENT_USER\Software\Cygnus Solutions" $R0
		Sleep 100
		Goto DeleteKey5

	DeleteKey2:
		StrCmp $KEY2 "-1" "" DeleteKey3
		${registry::DeleteKeyEmpty} "HKEY_CURRENT_USER\Software\Cygnus Solutions\Cygwin\mounts v2" $R0
		Sleep 100
		${registry::DeleteKeyEmpty} "HKEY_CURRENT_USER\Software\Cygnus Solutions\Cygwin\Program Options" $R0
		Sleep 100
		${registry::DeleteKeyEmpty} "HKEY_CURRENT_USER\Software\Cygnus Solutions\Cygwin" $R0
		Sleep 100
		Goto DeleteKey5
	
	DeleteKey3:
		StrCmp $KEY3 "-1" "" DeleteKey4
		${registry::DeleteKeyEmpty} "HKEY_CURRENT_USER\Software\Cygnus Solutions\Cygwin\mounts v2" $R0
		Sleep 100
	
	DeleteKey4:
		StrCmp $KEY4 "-1" "" DeleteKey5
		${registry::DeleteKeyEmpty} "HKEY_CURRENT_USER\Software\Cygnus Solutions\Cygwin\Program Options" $R0
		Sleep 100
	
	DeleteKey5:
		userInfo::getAccountType
		Pop $0
		StrCmp $0 "Admin" "" TheEnd
		StrCmp $KEY5 "-1" "" DeleteKey6
		${registry::DeleteKeyEmpty} "HKEY_LOCAL_MACHINE\Software\Cygnus Solutions\Cygwin\mounts v2" $R0
		Sleep 100
		${registry::DeleteKeyEmpty} "HKEY_LOCAL_MACHINE\Software\Cygnus Solutions\Cygwin\Program Options" $R0
		Sleep 100
		${registry::DeleteKeyEmpty} "HKEY_LOCAL_MACHINE\Software\Cygnus Solutions\Cygwin" $R0
		Sleep 100
		${registry::DeleteKeyEmpty} "HKEY_LOCAL_MACHINE\Software\Cygnus Solutions" $R0
		Sleep 100
		Goto TheEnd
	
	DeleteKey6:
		StrCmp $KEY6 "-1" "" DeleteKey7
		${registry::DeleteKeyEmpty} "HKEY_LOCAL_MACHINE\Software\Cygnus Solutions\Cygwin\mounts v2" $R0
		Sleep 100
		${registry::DeleteKeyEmpty} "HKEY_LOCAL_MACHINE\Software\Cygnus Solutions\Cygwin\Program Options" $R0
		Sleep 100
		${registry::DeleteKeyEmpty} "HKEY_LOCAL_MACHINE\Software\Cygnus Solutions\Cygwin" $R0
		Sleep 100
		Goto TheEnd
	
	DeleteKey7:
		StrCmp $KEY7 "-1" "" DeleteKey8
		${registry::DeleteKeyEmpty} "HKEY_LOCAL_MACHINE\Software\Cygnus Solutions\Cygwin\mounts v2" $R0
		Sleep 100
	
	DeleteKey8:
		StrCmp $KEY8 "-1" "" TheEnd
		${registry::DeleteKeyEmpty} "HKEY_LOCAL_MACHINE\Software\Cygnus Solutions\Cygwin\Program Options" $R0
		Sleep 100
		Goto TheEnd
	
	LaunchAndExit:
		Exec $EXECSTRING
	
	TheEnd:
		${registry::Unload}
		newadvsplash::stop /WAIT
SectionEnd