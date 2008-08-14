;Copyright 2007-2008 John T. Haller

;Website: http://PortableApps.com/

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

;EXCEPTION: Can be used with non-GPL apps distributed by PortableApps.com

;=== BEGIN: BASIC INFORMATION
!define NAME "FileZilla Portable"
!define SHORTNAME "FileZillaPortable"
!define VERSION "3.0.11.1"
!define FILENAME "FileZilla_Portable_3.0.11.1"
!define CHECKRUNNING "FileZillaPortable.exe"
!define CLOSENAME "FileZilla Portable"
!define ADDONSDIRECTORYPRESERVE "NONE"
!define PORTABLEAPPSINSTALLERVERSION "0.9.9.4"
!define INSTALLERCOMMENTS "For additional details, visit PortableApps.com"
!define INSTALLERADDITIONALTRADEMARKS "" ;end this entry with a period and a space if used
!define INSTALLERLEGALCOPYRIGHT "PortableApps.com and contributors"
;== License.  For no license agreement, comment out the next line by placing a semicolon at the start of it
;!define LICENSEAGREEMENT "eula.rtf"
;== Multi-Installer.  If making an installer with no options (like additional languages), comment out the next line by placing a semicolon at the start of it
!define MAINSECTIONTITLE "FileZilla Portable (English) [Required]"
!ifdef MAINSECTIONTITLE
	!define MAINSECTIONDESCRIPTION "Install the portable app"
	!define OPTIONALSECTIONTITLE "Additional Languages"
	!define OPTIONALSECTIONDESCRIPTION "Add multilingual support for this app"
	!define OPTIONALSECTIONSELECTEDAPPINFOSUFFIX "(Multilingual)"
	!define OPTIONALSECTIONNOTSELECTEDAPPINFOSUFFIX "(English)"
!endif
;=== END: BASIC INFORMATION

;=== Program Details
Name "${NAME}"
OutFile "..\..\..\${FILENAME}.paf.exe"
InstallDir "\${SHORTNAME}"
Caption "${NAME} | PortableApps.com Installer"
VIProductVersion "${VERSION}"
VIAddVersionKey ProductName "${NAME}"
VIAddVersionKey Comments "${INSTALLERCOMMENTS}"
VIAddVersionKey CompanyName "PortableApps.com"
VIAddVersionKey LegalCopyright "${INSTALLERLEGALCOPYRIGHT}"
VIAddVersionKey FileDescription "${NAME}"
VIAddVersionKey FileVersion "${VERSION}"
VIAddVersionKey ProductVersion "${VERSION}"
VIAddVersionKey InternalName "${NAME}"
VIAddVersionKey LegalTrademarks "${INSTALLERADDITIONALTRADEMARKS}PortableApps.com is a Trademark of Rare Ideas, LLC."
VIAddVersionKey OriginalFilename "${FILENAME}.paf.exe"
VIAddVersionKey PortableApps.comInstallerVersion "${PORTABLEAPPSINSTALLERVERSION}"
;VIAddVersionKey PrivateBuild ""
;VIAddVersionKey SpecialBuild ""

;=== Runtime Switches
SetCompress Auto
SetCompressor /SOLID lzma
SetCompressorDictSize 32
SetDatablockOptimize On
CRCCheck on
AutoCloseWindow True
RequestExecutionLevel user

;=== Include
!include MUI.nsh
!include FileFunc.nsh
!include LogicLib.nsh
!insertmacro DriveSpace
!insertmacro GetOptions
!insertmacro GetDrives
!insertmacro GetRoot
!insertmacro GetSize
!insertmacro GetParent
!include TextFunc.nsh
!insertmacro ConfigRead
!insertmacro ConfigReadS
!insertmacro ConfigWrite
!insertmacro ConfigWriteS

;=== Program Icon
Icon "..\..\App\AppInfo\appicon.ico"

;=== Icon & Stye ===
!define MUI_ICON "..\..\App\AppInfo\appicon.ico"
BrandingText "PortableApps.com - Your Digital Life, Anywhere™"

;=== Pages
!define MUI_WELCOMEFINISHPAGE_BITMAP "PortableApps.comInstaller.bmp"
!define MUI_WELCOMEPAGE_TITLE "${NAME}"
!define MUI_WELCOMEPAGE_TEXT "$(welcome)"
!define MUI_COMPONENTSPAGE_SMALLDESC
!insertmacro MUI_PAGE_WELCOME
!ifdef LICENSEAGREEMENT
	!define MUI_LICENSEPAGE_CHECKBOX
	!insertmacro MUI_PAGE_LICENSE "${LICENSEAGREEMENT}"
!endif
!ifdef MAINSECTIONTITLE
	!insertmacro MUI_PAGE_COMPONENTS
!endif
!define MUI_DIRECTORYPAGE_VERIFYONLEAVE
!define MUI_PAGE_CUSTOMFUNCTION_LEAVE LeaveDirectory
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
!define MUI_FINISHPAGE_TEXT "$(finish)"
!insertmacro MUI_PAGE_FINISH

;=== Languages
!insertmacro MUI_LANGUAGE "English"
!include PortableApps.comInstallerLANG_ENGLISH.nsh

;=== Variables
Var FOUNDPORTABLEAPPSPATH
!ifdef MAINSECTIONTITLE
	Var OPTIONAL1DONE
!endif

Function .onInit
	${GetOptions} "$CMDLINE" "/DESTINATION=" $R0

	IfErrors CheckLegacyDestination
		StrCpy $INSTDIR "$R0${SHORTNAME}"
		Goto InitDone

	CheckLegacyDestination:
		ClearErrors
		${GetOptions} "$CMDLINE" "-o" $R0
		IfErrors NoDestination
			StrCpy $INSTDIR "$R0${SHORTNAME}"
			Goto InitDone

	NoDestination:
		ClearErrors
		${GetDrives} "HDD+FDD" GetDrivesCallBack
		StrCmp $FOUNDPORTABLEAPPSPATH "" DefaultDestination
			StrCpy $INSTDIR "$FOUNDPORTABLEAPPSPATH\${SHORTNAME}"
			Goto InitDone
		
	DefaultDestination:
		StrCpy $INSTDIR "\${SHORTNAME}"

	InitDone:
FunctionEnd

Function LeaveDirectory
	GetInstDirError $0
  
	;=== Does it already exist? (upgrade)
	IfFileExists "$INSTDIR" "" CheckInstallerError
		;=== Check if app is running?
		StrCmp ${CHECKRUNNING} "NONE" CheckInstallerError
			FindProcDLL::FindProc "${CHECKRUNNING}"
			StrCmp $R0 "1" "" CheckInstallerError
				MessageBox MB_OK|MB_ICONINFORMATION `$(runwarning)`
				Abort
  
	CheckInstallerError:
		${Switch} $0
		    ${Case} 0 ;=== Valid directory and enough free space
				${Break}
		    ${Case} 1
				MessageBox MB_OK `$(invaliddirectory)`
				Abort
				${Break}
		    ${Case} 2
				IfFileExists `$INSTDIR` "" NotEnoughSpaceNoUpgrade ;=== Is upgrade
					SectionGetSize ${SecApp} $1 ;=== Space Required for App
					!ifdef MAINSECTIONTITLE
						StrCmp $OPTIONAL1DONE "true" NoOptions
							SectionGetSize ${SecOptional1} $2 ;=== Space Required for App
							IntOp $1 $1 + $2
						NoOptions:
					!endif
					${GetRoot} `$INSTDIR` $2
					${DriveSpace} `$2\` "/D=F /S=K" $3 ;=== Space Free on Device
					${GetSize} `$INSTDIR` "/M=*.* /S=0K /G=1" $4 $5 $6 ;=== Current installation size
					IntOp $3 $3 + $4 ;=== Space Free + Current Install Size
					IfFileExists `$INSTDIR\Data` "" CheckPluginsDirectory
						${GetSize} `$INSTDIR\Data` "/M=*.* /S=0K /G=1" $4 $5 $6 ;=== Size of Data directory
						IntOp $3 $3 - $4 ;=== Remove the data directory from the free space calculation

				CheckPluginsDirectory:
					StrCmp `${ADDONSDIRECTORYPRESERVE}` "NONE" CalculateSpaceLeft
						IfFileExists `$INSTDIR\${ADDONSDIRECTORYPRESERVE}` "" CalculateSpaceLeft
							${GetSize} `$INSTDIR\${ADDONSDIRECTORYPRESERVE}` "/M=*.* /S=0K /G=1" $4 $5 $6 ;=== Size of Data directory
							IntOp $3 $3 - $4 ;=== Remove the plugins directory from the free space calculation

				CalculateSpaceLeft:
					IntCmp $3 $1 NotEnoughSpaceNoUpgrade NotEnoughSpaceNoUpgrade
					Goto EndNotEnoughSpace

				NotEnoughSpaceNoUpgrade:
					MessageBox MB_OK `$(notenoughspace)`
					Abort

				EndNotEnoughSpace:
				${Break}
		${EndSwitch}
FunctionEnd

Function GetDrivesCallBack
	;=== Skip usual floppy letters
	StrCmp $8 "FDD" "" CheckForPortableAppsPath
	StrCmp $9 "A:\" End
	StrCmp $9 "B:\" End
	
	CheckForPortableAppsPath:
		IfFileExists "$9PortableApps" "" End
			StrCpy $FOUNDPORTABLEAPPSPATH "$9PortableApps"

	End:
		Push $0
FunctionEnd

!ifdef MAINSECTIONTITLE
	Section "${MAINSECTIONTITLE}" SecApp
!else
	Section "!App Portable (required)" SecApp
!endif
	SetOutPath $INSTDIR
	
	;=== BEGIN: PRE-INSTALL CODE
	;3.0.6 upgrade
	Delete "$INSTDIR\App\FileZilla\dbghelp.dll"
	Delete "$INSTDIR\App\FileZilla\FileZilla.chm"
	Delete "$INSTDIR\App\FileZilla\FileZilla.xml.default"
	Delete "$INSTDIR\App\FileZilla\Fz*.dll"
	Delete "$INSTDIR\App\FileZilla\legal.htm"
	Delete "$INSTDIR\App\FileZilla\libeay32.dll"
	Delete "$INSTDIR\App\FileZilla\puttylicense.html"
	Delete "$INSTDIR\App\FileZilla\readme.htm"
	Delete "$INSTDIR\App\FileZilla\ssleay32.dll"
	RMDir /r "$INSTDIR\Other\FileZillaPortableSource"
	RMDir /r "$INSTDIR\Other\FileZillaSource"
	;3.0.7 upgrade
	RMDir /r "$INSTDIR\App\FileZilla\locales\ca_ES"
	RMDir /r "$INSTDIR\App\FileZilla\locales\de_DE"
	RMDir /r "$INSTDIR\App\FileZilla\locales\el_GR"
	RMDir /r "$INSTDIR\App\FileZilla\locales\es_ES"
	RMDir /r "$INSTDIR\App\FileZilla\locales\fr_FR"
	RMDir /r "$INSTDIR\App\FileZilla\locales\it_IT"
	RMDir /r "$INSTDIR\App\FileZilla\locales\nl_NL"
	RMDir /r "$INSTDIR\App\FileZilla\locales\ru_RU"
	RMDir /r "$INSTDIR\App\FileZilla\locales\sv_SE"
	RMDir /r "$INSTDIR\App\FileZilla\locales\tr_TR"
	;3.0.8
	Delete "$INSTDIR\App\filezilla\resources\themes.xml"
	;3.0.11
	Delete "$INSTDIR\App\FileZilla\resources\empty.png"
	;=== END: PRE-INSTALL CODE
	
	File "..\..\*.*"
	SetOutPath $INSTDIR\App
	File /r "..\..\App\*.*"
	SetOutPath $INSTDIR\Other
	File /r "..\..\Other\*.*"
	CreateDirectory "$INSTDIR\Data"
	
	;=== BEGIN: POST-INSTALL CODE
	;=== END: POST-INSTALL CODE
	
	;=== Refresh PortableApps.com Menu (not final version)
	${GetParent} `$INSTDIR` $0
	;=== Check that it exists at the right location
	DetailPrint '$(checkforplatform)'
	IfFileExists `$0\PortableApps.com\App\PortableAppsPlatform.exe` "" TheEnd
		;=== Check that it's the real deal so we aren't hanging with no response
		MoreInfo::GetProductName `$0\PortableApps.com\App\PortableAppsPlatform.exe`
		Pop $1
		StrCmp $1 "PortableApps.com Platform" "" TheEnd
		MoreInfo::GetCompanyName `$0\PortableApps.com\App\PortableAppsPlatform.exe`
		Pop $1
		StrCmp $1 "PortableApps.com" "" TheEnd
		
		;=== Check that it's running
		FindProcDLL::FindProc "PortableAppsPlatform.exe"
		StrCmp $R0 "1" "" TheEnd
		
		;=== Send message for the Menu to refresh
		StrCpy $2 'PortableApps.comPlatformWindowMessageToRefresh$0\PortableApps.com\App\PortableAppsPlatform.exe'
		System::Call "user32::RegisterWindowMessage(t r2) i .r3"
		DetailPrint '$(refreshmenu)'
		SendMessage 65535 $3 0 0
	TheEnd:
SectionEnd

!ifdef MAINSECTIONTITLE
	Section /o "${OPTIONALSECTIONTITLE}" SecOptional1
		SetOutPath $INSTDIR
		File /r "..\..\..\${SHORTNAME}Optional1\*.*"
		StrCpy $OPTIONAL1DONE "true"
	SectionEnd

	Section "-Cleanup" SecCleanup
		StrCmp $OPTIONAL1DONE "true" SecCleanupOptionalSelected
			;=== BEGIN: OPTIONAL NOT SELECTED POST-INSTALL CODE
			RMDir /r "$INSTDIR\App\FileZilla\locales"
			;=== END: OPTIONAL NOT SELECTED POST-INSTALL CODE
			StrCmp ${OPTIONALSECTIONNOTSELECTEDAPPINFOSUFFIX} "" SecCleanupTheEnd
				ReadINIStr $0 "$INSTDIR\App\AppInfo\appinfo.ini" "Version" "DisplayVersion"
				WriteINIStr $INSTDIR\App\AppInfo\appinfo.ini" "Version" "DisplayVersion" "$0 ${OPTIONALSECTIONNOTSELECTEDAPPINFOSUFFIX}"
			Goto SecCleanupTheEnd

		SecCleanupOptionalSelected:
			StrCmp ${OPTIONALSECTIONSELECTEDAPPINFOSUFFIX} "" SecCleanupTheEnd
				ReadINIStr $0 "$INSTDIR\App\AppInfo\appinfo.ini" "Version" "DisplayVersion"
				WriteINIStr $INSTDIR\App\AppInfo\appinfo.ini" "Version" "DisplayVersion" "$0 ${OPTIONALSECTIONSELECTEDAPPINFOSUFFIX}"
		SecCleanupTheEnd:
	SectionEnd

	!insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
		!insertmacro MUI_DESCRIPTION_TEXT ${SecApp} "${MAINSECTIONDESCRIPTION}"
		!insertmacro MUI_DESCRIPTION_TEXT ${SecOptional1} "${OPTIONALSECTIONDESCRIPTION}"
	!insertmacro MUI_FUNCTION_DESCRIPTION_END
!endif