x := (A_ScreenWidth - 400) / 2
y := (A_ScreenHeight - 50) / 2	

splashTextTitle = Building all...
splashTextMessage = Initializing...

SplashTextOn 400, 50, %splashTextTitle%, %splashTextMessage%
WinMove, %splashTextMessage%, %x%, %y%

#include _1_Initialize.ahk

SplashTextOn 400, 50, %splashTextTitle%, Copying release directory...
#include _2_CopyToReleaseDir.ahk

SplashTextOn 400, 50, %splashTextTitle%, Copying to app directory...
#include _3_CopyToAppFolder.ahk

;SplashTextOn 400, 50, %splashTextTitle%, Building PAF...
;#include _4_BuildPAF.ahk

SplashTextOn 400, 50, %splashTextTitle%, Building Windows installer...
#include _4b_BuildInstaller.ahk

SplashTextOff