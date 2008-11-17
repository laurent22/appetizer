;NSIS Modern User Interface
;Basic Example Script
;Written by Joost Verburg

;--------------------------------
;Include Modern UI

  !include "MUI2.nsh"

;--------------------------------
;General

!define APP_NAME "Appetizer"
!define VERSION "1.0.2.43"
!define SOURCE_FOLDER "..\_PAF!\App\Appetizer"

  ;Name and file
  Name "${APP_NAME}"
  OutFile "..\_Sourceforge\Releases\${APP_NAME}_${VERSION}.exe"

  ;Default installation folder
  InstallDir "$PROGRAMFILES\${APP_NAME}"
  
  ;Get installation folder from registry if available
  InstallDirRegKey HKCU "Software\${APP_NAME}" ""

  ;Request application privileges for Windows Vista
  RequestExecutionLevel user

;--------------------------------
;Interface Settings

  !define MUI_ABORTWARNING

;--------------------------------
;Pages

  !insertmacro MUI_PAGE_LICENSE "${SOURCE_FOLDER}\Data\License.txt"
  !insertmacro MUI_PAGE_COMPONENTS
  !insertmacro MUI_PAGE_DIRECTORY
  !insertmacro MUI_PAGE_INSTFILES
  
  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES
  
;--------------------------------
;Languages
 
  !insertmacro MUI_LANGUAGE "English"

;--------------------------------
;Installer Sections

Section "Appetizer" AppetizerSection

  SectionIn RO

  SetOutPath "$INSTDIR"
  
  File "${SOURCE_FOLDER}\${APP_NAME}.exe"
  File /r "${SOURCE_FOLDER}\Data"
  
  ;Store installation folder
  WriteRegStr HKCU "Software\${APP_NAME}" "" $INSTDIR
  
  ;Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall.exe"

SectionEnd

Section "Start Menu Shortcuts"

  CreateDirectory "$SMPROGRAMS\${APP_NAME}"
  CreateShortCut "$SMPROGRAMS\${APP_NAME}\Uninstall.lnk" "$INSTDIR\uninstall.exe" "" "$INSTDIR\uninstall.exe" 0
  CreateShortCut "$SMPROGRAMS\${APP_NAME}\${APP_NAME}.lnk" "$INSTDIR\${APP_NAME}.exe" "" "$INSTDIR\${APP_NAME}.exe" 0
  
SectionEnd

Section "Desktop Shortcut"

  CreateShortCut "$DESKTOP\${APP_NAME}.lnk" "$INSTDIR\${APP_NAME}.exe" "" "$INSTDIR\${APP_NAME}.exe" 0
  
SectionEnd

;--------------------------------
;Descriptions

  ;Language strings
  LangString DESC_AppetizerSection ${LANG_ENGLISH} "${APP_NAME}"

  ;Assign language strings to sections
  !insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
    !insertmacro MUI_DESCRIPTION_TEXT ${AppetizerSection} $(DESC_AppetizerSection)
  !insertmacro MUI_FUNCTION_DESCRIPTION_END

;--------------------------------
;Uninstaller Section

Section "Uninstall"

  ;ADD YOUR OWN FILES HERE...

  Delete "$INSTDIR\Uninstall.exe"
  Delete "$INSTDIR\${APP_NAME}.exe"
  Delete "$INSTDIR\Data\License.txt"
  RMDir /r "$INSTDIR\Data\Help"
  RMDir /r "$INSTDIR\Data\Locales"
  RMDir /r "$INSTDIR\Data\Skin"
  RMDir /r "$SMPROGRAMS\${APP_NAME}"
  Delete "$DESKTOP\${APP_NAME}.lnk"

  DeleteRegKey /ifempty HKCU "Software\${APP_NAME}"

SectionEnd