#include _1_Initialize.ahk
#include _2_CopyToReleaseDir.ahk

; ****************************************************************
; Copy Appetizer files to PAF "App" folder
; ****************************************************************

FileRemoveDir %pafAppDir%\Appetizer, 1
Sleep 100
FileCreateDir %pafAppDir%\Appetizer

FileCopy %releaseDirectory%\*.*, %pafAppDir%\Appetizer
FileCopyDir %releaseDirectory%\Data, %pafAppDir%\Appetizer\Data