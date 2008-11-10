
SET HHC_PATH=c:\Program Files\HTML Help Workshop\hhc.exe
SET HELP_ROOT_DIR=s:\Docs\PROGS\C++\MiniLaunchBar\trunk\Data\Help
SET HELP_TITLE=Appetizer

_Tools\ProcesadorHtml\ProcesadorHtml\bin\Release\ProcesadorHtml.exe /t "%HELP_TITLE%" /g /c "%HHC_PATH%" /o "%HELP_ROOT_DIR%\fr\Appetizer.chm" "%HELP_ROOT_DIR%\fr\Appetizer.html"
_Tools\ProcesadorHtml\ProcesadorHtml\bin\Release\ProcesadorHtml.exe /t "%HELP_TITLE%" /g /c "%HHC_PATH%" /o "%HELP_ROOT_DIR%\en\Appetizer.chm" "%HELP_ROOT_DIR%\en\Appetizer.html"

SET HHC_PATH
SET HELP_ROOT_DIR
SET HELP_TITLE