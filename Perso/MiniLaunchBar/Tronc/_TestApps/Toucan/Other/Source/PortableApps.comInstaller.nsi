;Copyright 2007 John T. Haller

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

;EXCEPTION: Can be used with non-GPLed apps distributed by PortableApps.com

!define NAME "Toucan"
!define SHORTNAME "Toucan"
!define VERSION "1.2.2.0"
!define FILENAME "Toucan_1.2.2"
!define CHECKRUNNING "Toucan.exe"
!define CLOSENAME "Toucan"
!define ADDONSDIRECTORYPRESERVE "NONE"
!define PORTABLEAPPSINSTALLERVERSION "0.9.1.0"

;=== Program Details
Name "${NAME}"
OutFile "..\..\..\${FILENAME}.paf.exe"
InstallDir "\${SHORTNAME}"
Caption "${NAME} | PortableApps.com Installer"
VIProductVersion "${VERSION}"
VIAddVersionKey ProductName "${NAME}"
VIAddVersionKey Comments "For additional details, visit PortableApps.com"
VIAddVersionKey CompanyName "PortableApps.com"
VIAddVersionKey LegalCopyright "PortableApps.com and contributors"
VIAddVersionKey FileDescription "${NAME}"
VIAddVersionKey FileVersion "${VERSION}"
VIAddVersionKey ProductVersion "${VERSION}"
VIAddVersionKey InternalName "${NAME}"
VIAddVersionKey LegalTrademarks "PortableApps.com is a Trademark of Rare Ideas, LLC."
VIAddVersionKey OriginalFilename "${FILENAME}.paf.exe"
VIAddVersionKey PortableApps.comInstallerVersion "${PORTABLEAPPSINSTALLERVERSION}"
;VIAddVersionKey PrivateBuild ""
;VIAddVersionKey SpecialBuild ""

;=== Runtime Switches
;SetDatablockOptimize on
;SetCompress off
SetCompressor /SOLID lzma
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

;=== Program Icon
Icon "..\..\App\AppInfo\appicon.ico"

;=== Icon & Stye ===
!define MUI_ICON "..\..\App\AppInfo\appicon.ico"
BrandingText "PortableApps.com - Your Digital Life, Anywhere™"

;=== Pages
!define MUI_WELCOMEFINISHPAGE_BITMAP "PortableApps.comInstaller.bmp"
!define MUI_WELCOMEPAGE_TITLE "${NAME}"
!define MUI_WELCOMEPAGE_TEXT "$(welcome)"
!insertmacro MUI_PAGE_WELCOME
;!define MUI_LICENSEPAGE_RADIOBUTTONS
;!insertmacro MUI_PAGE_LICENSE "EULA.rtf"
!define MUI_PAGE_CUSTOMFUNCTION_LEAVE LeaveDirectory
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
!define MUI_FINISHPAGE_TEXT "$(finish)"
!insertmacro MUI_PAGE_FINISH

;=== Languages
!insertmacro MUI_LANGUAGE "Estonian"
!insertmacro MUI_LANGUAGE "Spanish"
!insertmacro MUI_LANGUAGE "Dutch"
!insertmacro MUI_LANGUAGE "English"
!insertmacro MUI_LANGUAGE "French"
!insertmacro MUI_LANGUAGE "German"
!insertmacro MUI_LANGUAGE "Japanese"

!insertmacro MUI_RESERVEFILE_LANGDLL

LangString welcome ${LANG_ENGLISH} "This wizard will guide you through the installation of ${NAME}.\r\n\r\nIf you are upgrading an existing installation of ${NAME}, please close it before proceeding.\r\n\r\nClick Next to continue."
LangString finish ${LANG_ENGLISH} "${NAME} has been installed on your device.\r\n\r\nClick Finish to close this wizard."
LangString runwarning ${LANG_ENGLISH} "Please close all instances of ${CLOSENAME} and then click OK.  The portable app can not be upgraded while it is running."
LangString invaliddirectory ${LANG_ENGLISH} "The destination folder you selected is invalid.  Please choose a valid folder."
LangString notenoughspace ${LANG_ENGLISH} "The device you have selected to install to does not have enough free space for this app."
LangString checkforplatform ${LANG_ENGLISH} "Checking for PortableApps.com Platform"
LangString refreshmenu ${LANG_ENGLISH} "Refreshing PortableApps.com Menu"

LangString welcome ${LANG_FRENCH} "Cet assistant vous guidera tout au long de l'installation de ${NAME}.\r\n\r\nSi vous mettez à jour une installation existante de ${NAME}, veuillez l'arrêter avant de continuer.\r\n\r\nCliquez sur Next pour continuer."
LangString finish ${LANG_FRENCH} "${NAME} a été installé sur votre disque.\r\n\r\nCliquez sur Finish pour fermer cet assistant."
LangString runwarning ${LANG_FRENCH} "Veuillez fermer toutes les instances de ${CLOSENAME} et cliquez ensuite sur OK. L'application portable ne peut pas être mise à jour tant qu'il fonctionne."
LangString invaliddirectory ${LANG_FRENCH} "Le dossier de destination que vous avez sélectionné est invalide. Veuillez choisir un dossier valide."
LangString notenoughspace ${LANG_FRENCH} "Le disque que vous avez sélectionné pour l'installation, ne contient pas assez d'espace libre pour cette application."
LangString checkforplatform ${LANG_FRENCH} "Recherche de la Plateforme PortableApps.com"
LangString refreshmenu ${LANG_FRENCH} "Actualisation du Menu PortableApps.com"

LangString welcome ${LANG_GERMAN} "Dieser Assistent wird Sie bei der Installation von ${NAME} unterstützen.\r\n\r\nWenn Sie eine existierende Installation von ${NAME} aktualisieren wollen, schliessen Sie sie bitte, bevor Sie fortfahren.\r\n\r\nKlicken Sie bitte auf Weiter, um fortzusetzen."
LangString finish ${LANG_GERMAN} "${NAME} wurde auf Ihrem Laufwerk installiert.\r\n\r\nBitte Beenden klicken, um diesen Assistenten zu verlassen."
LangString runwarning ${LANG_GERMAN} "Bitte schliessen Sie alle Instanzen von ${CLOSENAME} und klicken Sie dann auf OK.  Die portable Anwendung kann nicht aktualisiert werden, solange sie läuft."
LangString invaliddirectory ${LANG_GERMAN} "Das ausgewählte Zielverzeichnis ist ungültig!.  Bitte wählen Sie ein gültiges Verzeichnis."
LangString notenoughspace ${LANG_GERMAN} "Das Laufwerk, welches Sie zur Installation ausgewählt haben, hat nicht genug freien Speicherplatz für diese Anwendung."
LangString checkforplatform ${LANG_GERMAN} "PortableApps.com Platform überprüfen"
LangString refreshmenu ${LANG_GERMAN} "PortableApps.com Menü neu laden"

LangString welcome ${LANG_DUTCH} "Deze wizard zal u door de installatie van ${NAME} begeleiden.\r\n\r\nIndien u een bestaande installatie wil vernieuwen van ${NAME}, sluit deze dan alstublieft af voordat u verder gaat.\r\n\r\nKlik Volgende om verder te gaan."
LangString finish ${LANG_DUTCH} "${NAME} is geinstalleerd op uw apparaat.\r\n\r\nKlik Eindigen om deze wizard te sluiten."
LangString runwarning ${LANG_DUTCH} "Sluit alstublieft alle processen van ${CLOSENAME} en klik dan op OK.  De draagbare toepassing kan niet vernieuwd worden zolang deze nog gebruikt wordt."
LangString invaliddirectory ${LANG_DUTCH} "De gekozen bestemmingsmap is ongeldig.  Kies een geldige map."
LangString notenoughspace ${LANG_DUTCH} "Het toestel dat u geselecteerd heeft, heeft onvoldoende beschikbare schijfruimte voor deze toepassing."
LangString checkforplatform ${LANG_DUTCH} "Controleert op PortableApps.com Platform"
LangString refreshmenu ${LANG_DUTCH} "Vernieuwt PortableApps.com Menu"

LangString welcome ${LANG_SPANISH} "Este asistente le guiará durante la instalación de ${NAME}.\r\n\r\nSi está actualizando una instalación existente de ${NAME}, ciérrela antes de continuar.\r\n\r\nHaga clic en Siguiente para continuar."
LangString finish ${LANG_SPANISH} "${NAME} se ha instalado en su dispositivo.\r\n\r\nHaga clic en Finalizar para cerrar este asistente."
LangString runwarning ${LANG_SPANISH} "Cierre todas las instancias de ${CLOSENAME} y haga clic en Aceptar.  La aplicación portátil no se puede actualizar mientras se está ejecutando."
LangString invaliddirectory ${LANG_SPANISH} "La carpeta de destino que ha seleccionado no es válida.  Elija una carpeta válida."
LangString notenoughspace ${LANG_SPANISH} "El dispositivo que ha selecionado para la instalación no dispone de espacio suficiente para esta aplicación."
LangString checkforplatform ${LANG_SPANISH} "Comprobando la Plataforma PortableApps.com"
LangString refreshmenu ${LANG_SPANISH} "Actualizando el menú de PortableApps.com"

LangString welcome ${LANG_ESTONIAN} "Paigaldusnõustaja juhib sind läbi ${NAME} paigaldusprotsessi.\r\n\r\nKui sa uuendad ${NAME}, palun sulge programm enne jätkamist.\r\n\r\nJätkamiseks klõpsa Edasi."
LangString finish ${LANG_ESTONIAN} "${NAME} on paigaldatud.\r\n\r\nPaigaldusnõustaja sulgemiseks klõpsa Lõpeta."
LangString runwarning ${LANG_ESTONIAN} "Palun sulge ${CLOSENAME} ja vajuta Sobib. Programmi ei saa uuendada kui programm töötab."
LangString invaliddirectory ${LANG_ESTONIAN} "Valitud sihtkaust on vigane. Palun vali õige kaust."
LangString notenoughspace ${LANG_ESTONIAN} "Valitud seadmel ei ole programmi paigaldamiseks piisavalt vaba ruumi."
LangString checkforplatform ${LANG_ESTONIAN} "Kontrollitakse PortableApps.com olemasolu"
LangString refreshmenu ${LANG_ESTONIAN} "Värskendatakse PortableApps.com Menu"

!include "PortableApps.comInstallerLANG_JAPANESE.nsi"


;=== Variables
Var FOUNDPORTABLEAPPSPATH
Var PortableApps.comLocaleID

Function .onInit
	ReadEnvStr $PortableApps.comLocaleID "PortableApps.comLocaleID"
	StrCmp $PortableApps.comLocaleID "1061" SetLanguageFromEnvironment ;Estonian
	StrCmp $PortableApps.comLocaleID "3082" SetLanguageFromEnvironment ;Spanish
	StrCmp $PortableApps.comLocaleID "1043" SetLanguageFromEnvironment ;Dutch
	StrCmp $PortableApps.comLocaleID "1031" SetLanguageFromEnvironment ;German
	StrCmp $PortableApps.comLocaleID "1036" SetLanguageFromEnvironment ;French
	StrCmp $PortableApps.comLocaleID "1033" SetLanguageFromEnvironment ShowLanguageSelector ;English
	
	SetLanguageFromEnvironment:
		StrCpy $LANGUAGE $PortableApps.comLocaleID
		Goto GetCommandLineOptions

	ShowLanguageSelector:
		!insertmacro MUI_LANGDLL_DISPLAY

	GetCommandLineOptions:
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
					SectionGetSize ${SectionMain} $1 ;=== Space Required for App
					${GetRoot} `$INSTDIR` $2
					${DriveSpace} `$2\` "/D=F /S=K" $3 ;=== Space Free on Device
					${GetSize} `$INSTDIR` "/M=*.* /S=0K /G=1" $4 $5 $6 ;=== Current installation size
					IntOp $7 $3 + $4 ;=== Space Free + Current Install Size
					IfFileExists `$INSTDIR\Data` "" CheckPluginsDirectory
						${GetSize} `$INSTDIR\Data` "/M=*.* /S=0K /G=1" $4 $5 $6 ;=== Size of Data directory
						IntOp $7 $7 - $4 ;=== Remove the data directory from the free space calculation

				CheckPluginsDirectory:
					StrCmp `${ADDONSDIRECTORYPRESERVE}` "NONE" CalculateSpaceLeft
						IfFileExists `$INSTDIR\${ADDONSDIRECTORYPRESERVE}` "" CalculateSpaceLeft
							${GetSize} `$INSTDIR\${ADDONSDIRECTORYPRESERVE}` "/M=*.* /S=0K /G=1" $4 $5 $6 ;=== Size of Data directory
							IntOp $7 $7 - $4 ;=== Remove the plugins directory from the free space calculation

				CalculateSpaceLeft:
					IntCmp $7 $1 NotEnoughSpaceNoUpgrade NotEnoughSpaceNoUpgrade
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

Section "!App Portable (required)"
	SetOutPath $INSTDIR
    	Delete '$INSTDIR\Help.chm'
	File /r "..\..\*.*"

	;=== Set Language
	StrCmp $LANGUAGE "1041" 0 +3 ;Japanese
	WriteINIStr $INSTDIR\Data\Settings.ini "General" "Language" "wxLANGUAGE_JAPANESE"
	Goto RefreshPortableAppsMenu
	StrCmp $LANGUAGE "1061" 0 +3 ;Estonian
	WriteINIStr $INSTDIR\Data\Settings.ini "General" "Language" "wxLANGUAGE_ESTONIAN"
	Goto RefreshPortableAppsMenu
	StrCmp $LANGUAGE "3082" 0 +3 ;Spanish
	WriteINIStr $INSTDIR\Data\Settings.ini "General" "Language" "wxLANGUAGE_SPANISH"
	Goto RefreshPortableAppsMenu
	StrCmp $LANGUAGE "1043" 0 +3 ;Dutch
	WriteINIStr $INSTDIR\Data\Settings.ini "General" "Language" "wxLANGUAGE_DUTCH"
	Goto RefreshPortableAppsMenu
	StrCmp $LANGUAGE "1031" 0 +3 ;German
	WriteINIStr $INSTDIR\Data\Settings.ini "General" "Language" "wxLANGUAGE_GERMAN"
	Goto RefreshPortableAppsMenu
	StrCmp $LANGUAGE "1036" 0 +3 ;French
	WriteINIStr $INSTDIR\Data\Settings.ini "General" "Language" "wxLANGUAGE_FRENCH"
	Goto RefreshPortableAppsMenu
	WriteINIStr $INSTDIR\Data\Settings.ini "General" "Language" "wxLANGUAGE_ENGLISH" ;=== Fallback to English

	RefreshPortableAppsMenu:	
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