;Copyright (C) 2004-2007 John T. Haller

;Website: http://PortableApps.com/FileZillaPortable

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

!define PORTABLEAPPNAME "FileZilla Portable"
!define APPNAME "FileZilla"
!define NAME "FileZillaPortable"
!define VER "1.5.7.0"
!define WEBSITE "PortableApps.com/FileZillaPortable"
!define DEFAULTEXE "FileZilla.exe"
!define DEFAULTAPPDIR "filezilla"
!define DEFAULTSETTINGSDIR "settings"

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
!include "FileFunc.nsh"
!insertmacro GetRoot
!include "ReplaceInFile.nsh"
!include "StrRep.nsh"
!include "MUI.nsh"
!include "TextFunc.nsh"
!insertmacro ConfigRead
!insertmacro ConfigWrite

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
Var INIPATH
Var DISABLESPLASHSCREEN
Var SECONDARYLAUNCH
Var MISSINGFILEORPATH
Var LASTDRIVE
Var CURRENTDRIVE
Var APPLANGUAGE

Section "Main"
	;=== Check if already running
	System::Call 'kernel32::CreateMutexA(i 0, i 0, t "${NAME}") i .r1 ?e'
	Pop $0
	StrCmp $0 0 CheckINI
		StrCpy $SECONDARYLAUNCH "true"

	CheckINI:
		;=== Find the INI file, if there is one
		IfFileExists "$EXEDIR\${NAME}.ini" "" NoINI
			StrCpy $INIPATH "$EXEDIR"

		;=== Read the parameters from the INI file
		ReadINIStr $0 "$INIPATH\${NAME}.ini" "${NAME}" "${APPNAME}Directory"
		StrCpy $PROGRAMDIRECTORY "$EXEDIR\$0"
		ReadINIStr $0 "$INIPATH\${NAME}.ini" "${NAME}" "SettingsDirectory"
		StrCpy $SETTINGSDIRECTORY "$EXEDIR\$0"

		;=== Check that the above required parameters are present
		IfErrors NoINI

		ReadINIStr $0 "$INIPATH\${NAME}.ini" "${NAME}" "AdditionalParameters"
		StrCpy $ADDITIONALPARAMETERS $0
		ReadINIStr $0 "$INIPATH\${NAME}.ini" "${NAME}" "${APPNAME}Executable"
		StrCpy $PROGRAMEXECUTABLE $0
		ReadINIStr $0 "$INIPATH\${NAME}.ini" "${NAME}" "DisableSplashScreen"
		StrCpy $DISABLESPLASHSCREEN $0

		;=== Any missing unrequired INI entries will be an empty string, ignore associated errors
		ClearErrors
		IfFileExists "$PROGRAMDIRECTORY\$PROGRAMEXECUTABLE" FoundProgramEXE NoProgramEXE

	NoINI:
		;=== No INI file, so we'll use the defaults
		StrCpy $ADDITIONALPARAMETERS ""
		StrCpy $PROGRAMEXECUTABLE "${DEFAULTEXE}"
		StrCpy $DISABLESPLASHSCREEN "false"

		IfFileExists "$EXEDIR\App\${DEFAULTAPPDIR}\${DEFAULTEXE}" "" NoProgramEXE
			StrCpy $PROGRAMDIRECTORY "$EXEDIR\App\${DEFAULTAPPDIR}"
			StrCpy $SETTINGSDIRECTORY "$EXEDIR\Data\${DEFAULTSETTINGSDIR}"
			GoTo FoundProgramEXE

	NoProgramEXE:
		;=== Program executable not where expected
		StrCpy $MISSINGFILEORPATH $PROGRAMEXECUTABLE
		MessageBox MB_OK|MB_ICONEXCLAMATION `$(LauncherFileNotFound)`
		Abort
		
	FoundProgramEXE:
		StrCmp $SECONDARYLAUNCH "true" GetPassedParameters
		StrCmp $DISABLESPLASHSCREEN "true" SkipSplashScreen
			;=== Show the splash screen before processing the files
			InitPluginsDir
			File /oname=$PLUGINSDIR\splash.jpg "${NAME}.jpg"
			newadvsplash::show /NOUNLOAD 1300 0 0 -1 /L "$PLUGINSDIR\splash.jpg"

	SkipSplashScreen:
		;=== Check for data files
		IfFileExists "$SETTINGSDIRECTORY\*.*" CheckSettingsVersion
		
		;=== Create settings directory
		CreateDirectory $SETTINGSDIRECTORY

	CheckSettingsVersion:
		IfFileExists "$SETTINGSDIRECTORY\FileZilla.xml" "" AdjustPaths
		IfFileExists "$SETTINGSDIRECTORY\sitemanager.xml" AdjustPaths
		IfFileExists "$SETTINGSDIRECTORY\filters.xml" AdjustPaths
		IfFileExists "$SETTINGSDIRECTORY\layout.xml" AdjustPaths
		IfFileExists "$SETTINGSDIRECTORY\queue.xml" AdjustPaths
		;=== No FileZilla 3 files exist, FileZilla 2 upgrade
		CreateDirectory "$SETTINGSDIRECTORY\old"
		Rename "$SETTINGSDIRECTORY\FileZilla.xml" "$SETTINGSDIRECTORY\old\FileZilla2.xml"
		MessageBox MB_OK|MB_ICONINFORMATION `Your configuration file is for an older version of FileZilla and can not be used.  Your old settings have been saved to:$\n$\n$SETTINGSDIRECTORY\old\FileZilla2.xml$\n$\nTo import your old settings into this new version of FileZilla, select EDIT and then IMPORT from the main menu and open the old settings file.$\nOnce imported successfully, you can delete your old settings file and the 'old' directory.$\nDetails on importing your old configuration file are also available in FileZilla Portable's help file and on PortableApps.com.`	

	AdjustPaths:
		ReadINIStr $LASTDRIVE "$SETTINGSDIRECTORY\${NAME}Settings.ini" "${NAME}Settings" "LastDrive"
		${GetRoot} $EXEDIR $CURRENTDRIVE
		StrCmp $LASTDRIVE $CURRENTDRIVE GetAppLanguage
		IfFileExists "$SETTINGSDIRECTORY\FileZilla.xml" "" GetAppLanguage
			${ReplaceInFile} `$SETTINGSDIRECTORY\FileZilla.xml` `>$LASTDRIVE\` `>$CURRENTDRIVE\`
			Delete "$SETTINGSDIRECTORY\FileZilla.xml.old"
			
	GetAppLanguage:
		ReadEnvStr $APPLANGUAGE "PortableApps.comLocaleglibc"
		StrCmp $APPLANGUAGE "" RememberPath ;if not set, move on
		StrCmp $APPLANGUAGE "en_US" "" GetCurrentLanguage
			StrCpy $APPLANGUAGE "en"
		
	GetCurrentLanguage:
		${ConfigRead} `$SETTINGSDIRECTORY\FileZilla.xml` `        <Setting name="Language Code" type="string">` $0
		StrCmp `$APPLANGUAGE</Setting>` $0 RememberPath ;if the same, move on
		StrCmp $APPLANGUAGE "en" SetAppLanguage ;english is built in, so skip locale file check
		IfFileExists "$PROGRAMDIRECTORY\locales\$APPLANGUAGE\*.*" SetAppLanguage RememberPath

	SetAppLanguage:
		${ConfigWrite} `$SETTINGSDIRECTORY\FileZilla.xml` `        <Setting name="Language Code" type="string">` `$APPLANGUAGE</Setting>` $R0
	
	RememberPath:
		WriteINIStr "$SETTINGSDIRECTORY\${NAME}Settings.ini" "${NAME}Settings" "LastDrive" "$CURRENTDRIVE"
	
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
		StrCmp $ADDITIONALPARAMETERS "" LaunchNow

		;=== Additional Parameters
		StrCpy $EXECSTRING `$EXECSTRING $ADDITIONALPARAMETERS`

	LaunchNow:
		System::Call 'Kernel32::SetEnvironmentVariableA(t, t) i("FILEZILLA_PORTABLE_SETTINGS", "$SETTINGSDIRECTORY").r0'
		StrCmp $SECONDARYLAUNCH "true" LaunchAndExit
		ExecWait $EXECSTRING

	CheckRunning:
		Sleep 1000
		FindProcDLL::FindProc "$PROGRAMEXECUTABLE"
		StrCmp $R0 "1" CheckRunning
		Goto TheEnd
	
	LaunchAndExit:
		Exec $EXECSTRING

	TheEnd:
		newadvsplash::stop /WAIT
SectionEnd