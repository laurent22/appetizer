SET HHC_PATH=c:\Program Files\HTML Help Workshop\hhc.exe
SET HELP_ROOT_DIR=s:\Docs\PROGS\C++\MiniLaunchBar\trunk\Data\Help
_Tools\ProcesadorHtml\ProcesadorHtml\bin\Release\ProcesadorHtml.exe /g /c "%HHC_PATH%" /o "%HELP_ROOT_DIR%\fr\Appetizer.chm" "%HELP_ROOT_DIR%\fr\Appetizer.html"

_Tools\ProcesadorHtml\ProcesadorHtml\bin\Release\ProcesadorHtml.exe /g /c "%HHC_PATH%" /o "%HELP_ROOT_DIR%\en\Appetizer.chm" "%HELP_ROOT_DIR%\en\Appetizer.html"
SET HHC_PATH
SET HELP_ROOT_DIR