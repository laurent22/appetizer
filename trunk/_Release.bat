mkdir _Release
mkdir _Release\Data
del _Release\*.* /Q /S
rmdir _Release\*.* /Q /S
copy MiniLaunchBar.exe _Release
xcopy Data _Release\Data /Y/E/S
del _Release\Data\Settings\*.* /Q
rmdir _Release\Data\Help /Q /S