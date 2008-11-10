@echo off

mkdir _Release
del _Release\*.* /Q/S
rmdir _Release\*.* /Q/S

mkdir "_Unicode Release\Data"
mkdir "_Unicode Debug\Data"
xcopy Data "_Unicode Release\Data" /Y/E/S
xcopy Data "_Unicode Debug\Data" /Y/E/S

mkdir _Release\Standard
mkdir _Release\Standard\Data
copy "Appetizer.exe" "_Release\Standard"
xcopy Data _Release\Standard\Data /Y/E/S
del _Release\Standard\Data\Settings\*.* /Q
del _Release\Standard\Data\Help\*.html /S/Q
del _Release\Standard\Data\*.po~ /S/Q
del _Release\Standard\Data\*.pot /S/Q
rmdir _Release\Standard\Data\Help\en\images /Q/S
rmdir _Release\Standard\Data\Help\fr\images /Q/S

upx -9 _Release\Standard\Appetizer.exe