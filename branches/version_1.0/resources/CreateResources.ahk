; Copyright (C) 2008 Laurent Cozic. All right reserved.
; Use of this source code is governed by a GNU/GPL license that can be
; found in the LICENSE file.

; Run this script or CreateResources.exe to create resources.rc from
; the template (resources_template.rc). Currently, the only purpose of this script
; is to auto-increment the application build number


#NoEnv ; Recommended for performance and compatibility with future AutoHotkey releases.
SendMode Input ; Recommended for new scripts due to its superior speed and reliability.

versionFilePath = %A_ScriptDir%\version.txt
versionSuffixFilePath = %A_ScriptDir%\version_suffix.txt
resourceTemplateFilePath = %A_ScriptDir%\resources_template.rc
resourceFilePath = %A_ScriptDir%\resources.rc

FileRead, versionText, %versionFilePath%
if ErrorLevel
{
	MsgBox Could not read version number
	ExitApp
}

FileRead, versionSuffixText, %versionSuffixFilePath%

FileRead, resourceText, %resourceTemplateFilePath%
if ErrorLevel
{
	MsgBox Could not read resource template
	ExitApp
}

; Split the version string
StringSplit, splittedVersion, versionText, .

; Get the build number
buildNumber := splittedVersion%splittedVersion0%
buildNumberLength := strLen(buildNumber)

if buildNumber is integer
{			
	buildNumber := buildNumber + 1
	
	; Build the new version number
	versionItemCount = %splittedVersion0%
	versionItemCount := versionItemCount - 1
	
	newVersionNumber =
	
	Loop, %versionItemCount%
	{
		n := splittedVersion%a_index%
		if (newVersionNumber <> "")
		{
			newVersionNumber = %newVersionNumber%.
		}
		newVersionNumber = %newVersionNumber%%n%
	}
	
	if (newVersionNumber <> "")
	{
		newVersionNumber = %newVersionNumber%.
			
		newVersionNumber = %newVersionNumber%%buildNumber%
	}
}

if (newVersionNumber = "")
{
	MsgBox Could not generate new version number
	ExitApp
}

StringReplace, newVersionNumberComa, newVersionNumber, ., `,, All

StringReplace, resourceText, resourceText, ____VERSION_COMA____, %newVersionNumberComa%, All
StringReplace, resourceText, resourceText, ____VERSION____, %newVersionNumber% %versionSuffixText%, All

FileDelete %resourceFilePath%
FileAppend %resourceText%, %resourceFilePath%

FileDelete %versionFilePath%
FileAppend %newVersionNumber%, %versionFilePath%