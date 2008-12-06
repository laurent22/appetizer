dummyClassesDir = %A_ScriptDir%\DummyClasses



CreateAsFile(sourceFile, targetDir)
{
	SplitPath, sourceFile, name, dir, ext, name_no_ext, drive
	
	className := SubStr(name_no_ext, 3)

	fileContent := ""

	classCommentBlock := ""
  currentCommentBlock := ""
	currentParameters := ""
	currentReturnType := "void"
	isFirstComment := 1
	
  parsingComment := 0

  Loop, Read, %sourceFile%
  {
		firstChars := SubStr(A_LoopReadLine, 1, 3)

	  if (parsingComment = 0)
	  {        
	    if (firstChars = "/**")
	    {
				currentCommentBlock := firstChars
				parsingComment := 1
			} else {
	
				if (currentCommentBlock = "") 
				{
					continue
				}
	
				regex := "i)(.*)::(.*)\(lua_State \*L\)"
				found := RegExMatch(A_LoopReadLine, regex, output)
										
				if (found > 0 and output1 = output2)
				{
					; CONSTRUCTOR
					fileContent = %fileContent%`n%currentCommentBlock%
					fileContent = %fileContent%`npublic function %className%(%currentParameters%):void {}`n
					currentCommentBlock := ""
					currentParameters := ""
					currentReturnType := "void"
					isFirstComment := 0
					
				} else {
					regex := "i)int .*::(.*)\(lua_State \*L\)"
					found := RegExMatch(A_LoopReadLine, regex, output)
					
					if (found > 0)
					{
						; FUNCTION
						fileContent = %fileContent%`n%currentCommentBlock%
						fileContent = %fileContent%`npublic function %output1%(%currentParameters%):%currentReturnType% {
						
						if (currentReturnType = "void")
						{
							fileContent = %fileContent% return;
						} else {
							if (currentReturnType = "Number")
							{
								fileContent = %fileContent% return 0;
							} else {
								fileContent = %fileContent% return null;
							}
						}
												
						fileContent = %fileContent% }`n
						
						currentCommentBlock := ""
						currentParameters := ""
						currentReturnType := "void"
						isFirstComment := 0
																	
					} else {
						
						; CLASS
						
						if (isFirstComment = 1)
						{
							classCommentBlock := currentCommentBlock
							
						}
						
						currentCommentBlock := ""	
						currentParameters := ""
						currentReturnType := "void"	
						isFirstComment := 1
						
					}
				}		
			}
		
    } else {
			lineToWrite := A_LoopReadLine			
	
			if (firstChars = " */")
			{
			
				parsingComment := 0
				
			} else {
				regex := "iU) \* @param ([^\s]+) ([^\s]+) (.*)$"
				found := RegExMatch(A_LoopReadLine, regex, output)
				
				if (found > 0)
				{
					; PARAMETERS
					
					paramType = %output1%
					paramName = %output2%
					paramDesc = %output3%
					
					if (currentParameters <> "") 
					{
						currentParameters = %currentParameters%,%A_Space%
					}
					
					currentParameters = %currentParameters%%paramName%:%paramType%			
						
					regex := "iU)(.*)\s\(default\s(.*)\)$"
					found := RegExMatch(paramDesc, regex, output)	
						
					if (found > 0)
					{
						; There is a default parameter
						currentParameters = %currentParameters% = %output2%
						paramDesc = %output1%
					}
					
					lineToWrite = %A_Space%* @param %paramName% %paramDesc%
										
				}
				
				regex := "i)[\s]*\* @return ([^\s]+) (.*)"
				found := RegExMatch(lineToWrite, regex, output)
				
				if (found > 0)
				{
					; RETURN
					lineToWrite := " * @return "output2
					currentReturnType = %output1%
					
				}
				
			}
			
			; * <table class=innertable>
			; * <tr><th>Name</th><th>Description</th></tr>
			; * <tr><td><a href="events/IconMenuOpening.html">trayIconMenuOpening</a></td><td>Description</td></tr>
			; * </table> 	
			
			if (lineToWrite = " * @beginEventTable")
			{
				lineToWrite = <p>This class dispatches the following events:</p>
				lineToWrite = %lineToWrite% * <table class=innertable>
				lineToWrite = %lineToWrite% * <tr><th>Name</th><th>Type</th><th>Dispatched...</th></tr>
			}
	
			if (lineToWrite = " * @endEventTable")
				lineToWrite := " * </table>"
					
			regex := "i)[\s]*\* @event ([^\s]+) ([^\s]+) (.*)$"
			found := RegExMatch(lineToWrite, regex, output)
				
			if (found > 0)
			{				
				eventType = %output1%
				eventName = %output2%
				eventDesc = %output3%
				
				lineToWrite = %A_Space%* <tr><td><code>%eventName%</code></td><td><a href="events/%eventType%.html">%eventType%</a></td><td>%eventDesc%</td></tr>
			}
		
			currentCommentBlock = %currentCommentBlock%%A_Space%%A_Space%`n%lineToWrite%
		
		}
	
  }
	
	fileContent = package {`n`n%classCommentBlock%`npublic class %className% {`n%fileContent%
	
	fileContent = %fileContent%`n}`n}
	filePath = %targetDir%\%className%.as
	
	FileDelete %filePath%
	FileAppend %fileContent%, %filePath%
}
	


FileCreateDir %dummyClassesDir%

Loop, %A_ScriptDir%\..\lua_glue\az*.cpp
{
	if (A_LoopFileName = "azWrapper.cpp")
		continue
	if (A_LoopFileName = "azGlobal.cpp")
		continue
		
	CreateAsFile(A_LoopFileFullPath, dummyClassesDir)
}



CopyFilesAndFolders(SourcePattern, DestinationFolder, DoOverwrite = false)
; Copies all files and folders matching SourcePattern into the folder named DestinationFolder and
; returns the number of files/folders that could not be copied.
{
    ; First copy all the files (but not the folders):
    FileCopy, %SourcePattern%, %DestinationFolder%, %DoOverwrite%
    ErrorCount := ErrorLevel
    ; Now copy all the folders:
    Loop, %SourcePattern%, 2  ; 2 means "retrieve folders only".
    {
        FileCopyDir, %A_LoopFileFullPath%, %DestinationFolder%\%A_LoopFileName%, %DoOverwrite%
        ErrorCount += ErrorLevel
        if ErrorLevel  ; Report each problem folder by name.
            MsgBox Could not copy %A_LoopFileFullPath% into %DestinationFolder%.
    }
    return ErrorCount
}


sourceFolder = %A_ScriptDir%\ExtraClasses\*
CopyFilesAndFolders(sourceFolder, dummyClassesDir, true)


Loop, %dummyClassesDir%\*, 1, 1
{			
	SplitPath, A_LoopFileFullPath, name, dir, ext, name_no_ext, drive
	
	if (name = ".svn")
	{
		FileRemoveDir %A_LoopFileFullPath%, 1
	}
}



RunWait %A_ScriptDir%\BuildAsDoc.bat, %A_ScriptDir%