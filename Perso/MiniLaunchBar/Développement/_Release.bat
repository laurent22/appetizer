mkdir _Release
mkdir _Release\Data
copy MiniLaunchBar.exe _Release
xcopy Data _Release\Data /S /E /Y
del _Release\Settings\*.* /Q
