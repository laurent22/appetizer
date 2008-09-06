mkdir _Release
mkdir _Release\Data
del _Release\*.* /Q /S
copy MiniLaunchBar.exe _Release
xcopy Data _Release\Data /S /E /Y
del _Release\Data\Settings\*.* /Q
