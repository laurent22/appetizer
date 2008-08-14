InfraRecorder Portable Launcher
===============================
Copyright 2004-2006 John T. Haller
Copyright 2008 Travis Carrico

Website: http://PortableApps.com/InfraRecorderPortable

This software is OSI Certified Open Source Software.
OSI Certified is a certification mark of the Open Source Initiative.

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.


ABOUT InfraRecorder PORTABLE
======================
The InfraRecorder Portable Launcher allows you to run InfraRecorder from a removable drive whose letter changes as you move it to another computer.  The file archiver and the settings can be entirely self-contained on the drive and then used on any Windows computer.


LICENSE
=======
This code is released under the GPL.  Within the InfraRecorderPortableSource directory you will find the code (InfraRecorderPortable.nsi) as well as the full GPL license (License.txt).  If you use the launcher or code in your own product, please give proper and prominent attribution.


INSTALLATION / DIRECTORY STRUCTURE
==================================
By default, the program expects one of these directory structures:

-\ <--- Directory with InfraRecorderPortable.exe
	+\App\
	    +\InfraRecorder\
	+\Data\
		+\settings\


It can be used in other directory configurations by including the InfraRecorderPortable.ini file in the same directory as InfraRecorderPortable.exe and configuring it as details in the INI file section below.  The INI file may also be placed in a subdirectory of the directory containing InfraRecorderPortable.exe called InfraRecorderPortable or 2 directories deep in PortableApps\InfraRecorderPortable or Data\InfraRecorderPortable.  All paths in the INI should remain relative to the EXE and not the INI.


InfraRecorderPortable.INI CONFIGURATION
=================================
The InfraRecorder Portable Launcher will look for an ini file called InfraRecorderPortable.ini within its directory (see the paragraph above in the Installation/Directory Structure section).  If you are happy with the default options, it is not necessary, though.  There is an example INI included with this package to get you started.  The INI file is formatted as follows:

[InfraRecorderPortable]
InfraRecorderDirectory=App\InfraRecorder
SettingsDirectory=Data\settings
InfraRecorderExecutable=irExpress.exe
AdditionalParameters=
DisableSplashScreen=false


The InfraRecorderDirectory and SettingsDirectory entries should be set to the *relative* path to the directories containing InfraRecorder.exe and your settings from the current directory.  All must be a subdirectory (or multiple subdirectories) of the directory containing InfraRecorderPortable.exe.  The default entries for these are described in the installation section above.

The SettingsFile entry is the name of your InfraRecorder settings file within the SettingsDirectory.

The InfraRecorderExecutable entry allows you to give an alternate filename for the InfraRecorder executable.

The AdditionalParameters entry allows you to specify additional parameters to be passed to InfraRecorder on the command line.

The DisableSplashScreen entry allows you to run the InfraRecorder Portable Launcher without the splash screen showing up.  The default is false.


PROGRAM HISTORY / ABOUT THE AUTHORS
===================================
This launcher is loosely based on the Firefox Portable launcher, which contains methods suggested by mai9 and tracon of the mozillaZine.org forums.