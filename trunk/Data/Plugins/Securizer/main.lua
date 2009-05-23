-- Copyright (C) 2008 Laurent Cozic. All right reserved.
-- Use of this source code is governed by a GNU/GPL license that can be
-- found in the LICENSE file available at:
-- http://appetizer.svn.sourceforge.net/viewvc/appetizer/trunk/LICENSE


-- *****************************************************************
--
-- Utility functions
--
-- *****************************************************************

function getFileExtension(file)
	local result = "";
	
	local foundDot = false
	
	for i = file:len(), 1, -1 do
		c = file:sub(i, i)
		
		if c == ":" or c == "/" or c == "\\" then
			break
		end
		
		if c == "." then
			foundDot = true
			break
		end
		
		result = c .. result
	end
	
	if not foundDot then
		result = ""
	end
	
	return result
end


function getFilename(file, includeExtension)	
	local result = ""

	for i = file:len(), 1, -1 do
		c = file:sub(i, i)
		if c == ":" or c == "/" or c == "\\" then
			break
		else
			result = c .. result
		end		
	end
	
	if not includeExtension then
		local ext = getFileExtension(result)
		if ext:len() > 0 then
			result = result:sub(1, result:len() - ext:len() - 1)
		end
	end
	
	return result
end


function getFilePath(file)
	local filename = getFilename(file, true)
	local endIndex = file:len() - filename:len() - 1
	if endIndex < 1 then
		return ''
	end
	return file:sub(1, endIndex)
end


function string_split(str, pat)
   local t = {}  -- NOTE: use {n = 0} in Lua-5.0
   local fpat = "(.-)" .. pat
   local last_end = 1
   local s, e, cap = str:find(fpat, 1)
   while s do
      if s ~= 1 or cap ~= "" then
	 table.insert(t,cap)
      end
      last_end = e+1
      s, e, cap = str:find(fpat, last_end)
   end
   if last_end <= #str then
      cap = str:sub(last_end)
      table.insert(t, cap)
   end
   return t
end


function string_trim(s)
  -- from PiL2 20.4
  return (s:gsub("^%s*(.-)%s*$", "%1"))
end



function table_includesElement(t, e)
	for i, v in ipairs(t) do
		if v == e then
			return true
		end
	end
	
	return false
end


-- *****************************************************************
--
-- Register preferences
--
-- *****************************************************************

preferences:registerPreference({
	type = "TextArea",
	name = "includedFiles",
	defaultValue = "$(Drive)\\*.txt\n$(Drive)\\*.odt\n$(Drive)\\*.doc\n$(Drive)\\*.docx",
	title = _("Included files:"),
	description = _("List of files to be encrypted. Put one item per line. Wildcard such as '*' or '?' are supported. Use the special variable $(Drive) to specify the drive Appetizer is installed on.")
})

preferences:registerPreference({
	type = "TextArea",
	name = "excludedFiles",
	defaultValue = "",
	title = _("Excluded files:"),
	description = _("List of files to be excluded.")
})

preferences:registerPreference({
	type = "CheckBox",
	name = "encryptOnClose",
	defaultValue = true,
	title = _("Ask me to encrypt my files when I close Appetizer")
})

preferences:registerPreference({
	type = "CheckBox",
	name = "decryptOnOpen",
	defaultValue = true,
	title = _("Ask me to decrypt my files when I open Appetizer")
})


-- *****************************************************************
--
-- Decrypts or encrypts a file using ccrypt
--
-- *****************************************************************


function cryptFile(filePath, password, encrypt)
	local e = ''
	
	if (encrypt) then
		e = '--encrypt'
	else
		e = '--decrypt'
	end
	
	local command = plugin:getPath() .. '/Resources/ccrypt.exe ' .. e .. ' --key ' .. password .. ' --suffix .ccrypt1_7 --force --quiet --strictsuffix "' .. filePath .. '"'
	local output = system:runCommand(command)
end



-- *****************************************************************
--
-- Converts a "\n" separated list of paths to a table
-- Also resolves wildcards such as "*"
--
-- *****************************************************************


function getFullFilePaths(fileString, resolvePatterns)
	local files = string_split(fileString, "\n")
	local output = {}
	
	local applicationFilePath = appetizer:getFilePath()
	local applicationDirectory = appetizer:getDirectory()
	local applicationDataDir = appetizer:getDataDirectory()
		
	for i, file in ipairs(files) do
		local fullPath = system:resolvePath(file, true)
		
		if resolvePatterns then
		
			local pattern = getFilename(file, true)
			local filePath = getFilePath(fullPath)
			
			if (filePath ~= applicationFilePath) and (filePath:sub(1, applicationDataDir:len()) ~= applicationDataDir) then -- always skip Appetizer files and directory
						
				if filePath == '' then
					filePath = appetizer:getDrive()
				end
						
				local fullFilePaths = system:getDirectoryContents(filePath, true)
					
					
								
				for j, fullFilePath in ipairs(fullFilePaths) do
					
					if system:fileMatchesPattern(fullFilePath, pattern) then
						table.insert(output, fullFilePath)
					end
				end
				
			end
			
		else
		
			table.insert(output, fullPath)
		
		end
		
	end
	
	return output	
end


-- *****************************************************************
--
-- Main function - does the actual encryption / decryption
-- based on user's preferences.
--
-- *****************************************************************


function doCrypt(password, encrypt)

	local errorCodes = {}
	errorCodes[0] = "Success"
	errorCodes[1] = "Illegal command line"	
	errorCodes[2] = "Out of memory"
	errorCodes[3] = _("Fatal i/o error (was the file already open?)")
	errorCodes[4] = _("Incorrect password (or wrong file format)")
	errorCodes[6] = "Interrupted"
	errorCodes[7] = "Mistyped password in --timid mode"
	errorCodes[8] = _("Non-fatal i/o error (file is missing, or not readable)")

	local errorStrings = {}

	local includedFiles = getFullFilePaths(preferences:getValue('includedFiles'), true)
	local excludedFiles = getFullFilePaths(preferences:getValue('excludedFiles'), false)
		
	for i, includedFile in ipairs(includedFiles) do
	
		local includedFilePath = getFilePath(includedFile)
		local includedFilename = getFilename(includedFile, true)
		local includedFileExt = getFileExtension(includedFile)
		
		
				
		if ((includedFileExt == 'ccrypt1_7') and (encrypt)) or ((includedFileExt ~= 'ccrypt1_7') and (not encrypt)) then
		
			-- nothing
		
		else
		
			local doIt = true
		
			for j, excludedFile in ipairs(excludedFiles) do
			
				local excludedFilePath = getFilePath(excludedFile)
				local excludedPattern = getFilename(excludedFile, true)
				
				if (excludedFilePath ~= '') and (excludedFilePath ~= includedFilePath) then
				
					-- nothing
					
				else
					local doesMatch = system:fileMatchesPattern(includedFilename, excludedPattern)
					if doesMatch then
						doIt = false
						break
					end
				end
				
			end
			
			if doIt then
				cryptFile(includedFile, password, encrypt)
				local errorCode = system:getLastCommandErrorCode()
				
				if (errorCode ~= 0) and (#errorStrings < 10) then
					local errorString = includedFilename .. ": " .. errorCodes[errorCode]
					table.insert(errorStrings, errorString)
				end				
				
			end	
			
		end
		
	end
	
	return errorStrings
end


-- *****************************************************************
--
-- Checks that the password contains the right characters
-- and is not too long nor too short
--
-- *****************************************************************


function validatePassword(password)
	if password:len() < 4 or password:len() > 20 then
		return _("The password must be between 4 and 20 characters.")
	end

	local allowedChars = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
	
	for i = 1, password:len(), 1 do
		local c = password:sub(i,i)
		if string.find(allowedChars, c) == nil then
			return _("The password must only contain letters and numbers.")
		end
	end
	
	return "ok"	
end


-- *****************************************************************
--
-- Creates and displays a dialog box asking for the user's
-- password. Optionally, also asks to confirm the password.
--
-- *****************************************************************


function askForPassword(confirm)
	local controls = {}
	
	table.insert(controls, {
		type = "Text",
		name = "password",
		secure = true,
		title = _("Password:"),
		description = _("Please type-in a password between 4 and 20 characters. Only letters and numbers are accepted.")
	})
	
	if confirm then
		table.insert(controls, {
			type = "Text",
			name = "passwordConfirm",
			secure = true,
			title = _("Confirm:"),
			description = _("Please confirm the password")
		})
	end
	
	local result = dialogs:showForm(controls, _("Type-in your password"))
	
	if not result then
		return ""
	end
	
	local errorMessage = validatePassword(result.password)
	
	if errorMessage ~= "ok" then
		dialogs:showMessage(errorMessage, "ok", "error")
		return ""
	end

	if not confirm then
		return result.password
	end
	
	if result.password ~= result.passwordConfirm then
		dialogs:showMessage(_("Passwords do not match."), "ok", "error")
		return ""
	end
	
	return result.password
end



-- *****************************************************************
--
-- Asks for the user's password and encrypt / decrypt
--
-- *****************************************************************


function askForPasswordAndCrypt(encrypt)
	local password = askForPassword(encrypt)
	if password == "" then
		return
	end
	
	appetizer:disable()
	
	local message = ""
	if encrypt then
		message = _("Encryption in progress. Please wait...")
	else
		message = _("Decryption in progress. Please wait...")
	end
	
	dialogIndex = dialogs:showSplashForm(message, _("Please wait..."))
	
	local errorStrings = doCrypt(password, encrypt)
	
	dialogs:closeSplashForm(dialogIndex)
	
	if #errorStrings > 0 then
		local errorString = ''
		for i, v in ipairs(errorStrings) do
			errorString = errorString .. "\n" .. v
		end
		
		dialogs:showMessage(_("Some errors occured during the operation:") .. "\n" .. errorString, "ok", "warning") 
	end
	
	appetizer:enable()
end


-- *****************************************************************
--
-- Creates and setups option button
--
-- *****************************************************************


function popupMenu_encrypt()
	askForPasswordAndCrypt(true)
end


function popupMenu_decrypt()
	askForPasswordAndCrypt(false)
end


function popupMenu_configuration()
	dialogs:showPreferences()
end


function cryptButton_click(event)
	local menu = Menu:new("Securizer")
	
	menuItem = MenuItem:new(_("Start encryption"))
	menuItem:setOnSelected("popupMenu_encrypt")
	menu:append(menuItem)
	
	menuItem = MenuItem:new(_("Start decryption"))
	menuItem:setOnSelected("popupMenu_decrypt")
	menu:append(menuItem)
	
	menuItem = MenuItem:new(_("Configuration"))
	menuItem:setOnSelected("popupMenu_configuration")
	menu:append(menuItem)
	
	cryptButton:popupMenu(menu)
end
	

cryptButton = OptionButton:new()
cryptButton:setToolTip("Securizer")
cryptButton:setIconFile(plugin:getPath() .. '/Resources/ButtonIcon_Key.png')
cryptButton:addEventListener("click", "cryptButton_click")

optionPanel:addButton(cryptButton)


-- *****************************************************************
--
-- Listens to the app "earlyBird" and "close" events
--
-- *****************************************************************


function appetizer_earlyBird(event)
	if preferences:getValue("decryptOnOpen") then
		askForPasswordAndCrypt(false)
	end
end


function appetizer_close(event)
	if preferences:getValue("encryptOnClose") then
		askForPasswordAndCrypt(true)
	end
end


appetizer:addEventListener("earlyBird", "appetizer_earlyBird")
appetizer:addEventListener("close", "appetizer_close")