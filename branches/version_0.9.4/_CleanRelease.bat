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
copy "_Unicode Release\*.exe" "_Release\Standard"
xcopy Data _Release\Standard\Data /Y/E/S
del _Release\Standard\Data\Settings\*.* /Q
del _Release\Standard\Data\*.odt /S/Q

mkdir _Release\Debug
mkdir _Release\Debug\Data
copy "_Unicode Debug\*.exe" "_Release\Debug"
xcopy Data _Release\Debug\Data /Y/E/S
del _Release\Debug\Data\Settings\*.* /Q
del _Release\Standard\Data\*.odt /S/Q

upx -9 _Release\Standard\Appetizer.exe