;Copyright (C) 2004-2008 John T. Haller

;Website: http://app.etizer.org

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

!define NAME "Appetizer"
!define FRIENDLYNAME "Appetizer"
!define APP "Appetizer"
!define VER "___FULL_VERSION___"
!define WEBSITE "http://app.etizer.org"

;=== Program Details
Name "${NAME}"
OutFile "..\..\${NAME}.exe"
Caption "${FRIENDLYNAME} | PortableApps.com"
Icon "..\..\App\AppInfo\appicon.ico"
VIProductVersion "${VER}"
VIAddVersionKey ProductName "${FRIENDLYNAME}"
VIAddVersionKey CompanyName "PortableApps.com"
VIAddVersionKey LegalCopyright "Laurent Cozic"
VIAddVersionKey FileDescription "${FRIENDLYNAME}"
VIAddVersionKey FileVersion "${VER}"
VIAddVersionKey ProductVersion "${VER}"
VIAddVersionKey InternalName "${FRIENDLYNAME}"
VIAddVersionKey LegalTrademarks "PortableApps.com is a Trademark of Rare Ideas, LLC."
VIAddVersionKey OriginalFilename "${NAME}.exe"
;VIAddVersionKey PrivateBuild ""
;VIAddVersionKey SpecialBuild ""

;=== Include
;(Standard NSIS)
!include TextFunc.nsh
!insertmacro GetParameters

;=== Runtime Switches
CRCCheck On
WindowIcon Off
SilentInstall Silent
AutoCloseWindow True
RequestExecutionLevel user

Section "Main"
	${GetParameters} $0
	StrCmp $0 "" LaunchWithoutParameters LaunchWithParameters	
	
	LaunchWithoutParameters:
	
		newadvsplash::show /NOUNLOAD 1200 0 0 -1 "$EXEDIR\Other\Source\Splash.jpg"
		SetOutPath $EXEDIR\App\Appetizer
		ExecShell "open" "$EXEDIR\App\Appetizer\Appetizer.exe" "/d ..\..\Data" SW_HIDE
		Goto TheEnd

	LaunchWithParameters:
	
		newadvsplash::show /NOUNLOAD 1200 0 0 -1 "$EXEDIR\Other\Source\Splash.jpg"
		SetOutPath $EXEDIR\App\Appetizer
		ExecShell "open" "$EXEDIR\App\Appetizer\Appetizer.exe" "/d ..\..\Data" SW_HIDE
		Goto TheEnd

	TheEnd:
SectionEnd
