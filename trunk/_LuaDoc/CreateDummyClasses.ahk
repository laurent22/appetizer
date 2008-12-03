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
							fileContent = %fileContent% return null;
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
				;regex := "iU) \* @param (.*) (.*) "
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
		
	CreateAsFile(A_LoopFileFullPath, dummyClassesDir)
}



Loop, %A_ScriptDir%\ExtraClasses\*.as
{		
	SplitPath, A_LoopFileFullPath, name, dir, ext, name_no_ext, drive
	FileDelete %dummyClassesDir%\%name%
	FileCopy %A_LoopFileFullPath%, %dummyClassesDir%
}



RunWait %A_ScriptDir%\BuildAsDoc.bat, %A_ScriptDir%