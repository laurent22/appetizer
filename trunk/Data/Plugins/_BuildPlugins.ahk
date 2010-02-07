targetFilePath = %A_ScriptDir%\Securizer.zpl
sourcePath = %A_ScriptDir%\Securizer
GoSub BuildPlugin

targetFilePath = %A_ScriptDir%\AddToGroupOnRightClick.zpl
sourcePath = %A_ScriptDir%\AddToGroupOnRightClick
GoSub BuildPlugin

targetFilePath = %A_ScriptDir%\CloseAfterLaunchingAnApp.zpl
sourcePath = %A_ScriptDir%\CloseAfterLaunchingAnApp
GoSub BuildPlugin

targetFilePath = %A_ScriptDir%\RevealShortcutTarget.zpl
sourcePath = %A_ScriptDir%\RevealShortcutTarget
GoSub BuildPlugin

ExitApp





BuildPlugin:	
	
FileDelete %targetFilePath%

cmd = 7z.exe a -tzip "%targetFilePath%" "%sourcePath%"
RunWait %cmd%,, Hide

cmd = 7z d -tzip "%targetFilePath%" .svn -r
RunWait %cmd%,, Hide

cmd = 7z d -tzip "%targetFilePath%" plugin.po -r
RunWait %cmd%,, Hide

cmd = 7z d -tzip "%targetFilePath%" *.po~ -r
RunWait %cmd%,, Hide

cmd = 7z d -tzip "%targetFilePath%" plugin.pot -r
RunWait %cmd%,, Hide

return