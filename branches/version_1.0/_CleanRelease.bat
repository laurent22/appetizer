@echo off

REM Copy all the files needed for a release in the "_Release" folder

SET RELEASE_DIR=_Release\%date:~6,4%-%date:~3,2%-%date:~0,2%_%time:~0,2%-%time:~3,2%-%time:~6,2%

mkdir %RELEASE_DIR%
mkdir %RELEASE_DIR%\Data
copy "Appetizer.exe" "%RELEASE_DIR%"
xcopy Data %RELEASE_DIR%\Data /Y/E/S

del %RELEASE_DIR%\Data\IconCache\*.* /Q
del %RELEASE_DIR%\Data\Help\*.html /S/Q
del %RELEASE_DIR%\Data\*.po~ /S/Q
del %RELEASE_DIR%\Data\*.pot /S/Q

rmdir %RELEASE_DIR%\Data\Settings /Q/S
rmdir %RELEASE_DIR%\Data\Help\en\images /Q/S
rmdir %RELEASE_DIR%\Data\Help\fr\images /Q/S

REM A enlever :
rmdir %RELEASE_DIR%\Data\Help\fr /Q/S

upx -9 %RELEASE_DIR%\Appetizer.exe