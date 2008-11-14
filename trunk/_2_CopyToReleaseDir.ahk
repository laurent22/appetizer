#include _1_Initialize.ahk

; ****************************************************************
; Copy the required files and folders in the release folder
; Basically eveything that is needed to run Appetizer minus the
; Settings folders, .svn folders, temporary files, etc.
; ****************************************************************

FileCreateDir %releaseDirectory%

FileCopy Appetizer.exe, %releaseDirectory%
FileCopyDir Data, %releaseDirectory%\Data

Loop %releaseDirectory%\.svn, 2, 1
{
  FileRemoveDir %A_LoopFileFullPath%, 1
}

FileRemoveDir %releaseDirectory%\Data\Settings, 1

Loop %releaseDirectory%\Data\Help\*.html, 0, 1
{
  FileDelete %A_LoopFileFullPath%
}

Loop %releaseDirectory%\Data\*.po~, 0, 1
{
  FileDelete %A_LoopFileFullPath%
}

Loop %releaseDirectory%\Data\*.pot, 0, 1
{
  FileDelete %A_LoopFileFullPath%
}

Loop %releaseDirectory%\Data\Help\images, 2, 1
{
  FileRemoveDir %A_LoopFileFullPath%, 1
}

; A ENLEVER UNE FOIS QUE LA TRADUC FRANCAISE EST PRETE
FileRemoveDir %releaseDirectory%\Data\Help\fr, 1

command = %upxExe% -9 %releaseDirectory%\Appetizer.exe
RunWait %command%

zipFileName = Appetizer_%appVersion%.zip
command = %sevenZipExe% a -tzip %zipFileName% *
RunWait %command%, %releaseDirectory%

FileMove %releaseDirectory%\%zipFileName%, %releaseDir%\%zipFileName%, 1