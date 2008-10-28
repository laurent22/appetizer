@echo off

mkdir _Release
del _Release\*.* /Q /S
rmdir _Release\*.* /Q /S

mkdir "_Unicode Release\Data"
mkdir "_Unicode Debug\Data"
xcopy Data "_Unicode Release\Data" /Y/E/S
xcopy Data "_Unicode Debug\Data" /Y/E/S

mkdir _Release\Standard
mkdir _Release\Standard\Data
copy "_Unicode Release\MiniLaunchBar.exe" "_Release\Standard"
xcopy Data _Release\Standard\Data /Y/E/S
del _Release\Standard\Data\Settings\*.* /Q
rmdir _Release\Standard\Data\Help /Q /S

mkdir _Release\Debug
mkdir _Release\Debug\Data
copy "_Unicode Debug\MiniLaunchBar.exe" "_Release\Debug"
xcopy Data _Release\Debug\Data /Y/E/S
del _Release\Debug\Data\Settings\*.* /Q
rmdir _Release\Debug\Data\Help /Q /S