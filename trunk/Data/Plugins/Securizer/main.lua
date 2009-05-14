-- ************************************************
--
-- Utility functions
--
-- ************************************************

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


-- ************************************************
--
-- Register preferences
--
-- ************************************************

preferences:registerPreference({
	type = "TextArea",
	name = "includedFiles",
	defaultValue = "$(Drive)\\*.txt\n$(Drive)\\*.odt\n$(Drive)\\*.doc\n$(Drive)\\*.docx",
	title = "Included files:",
	description = "List of files to be encrypted. Put one item per line. Wildcard such as '*' or '?' are supported."
})

preferences:registerPreference({
	type = "TextArea",
	name = "excludedFiles",
	defaultValue = "",
	title = "Excluded files:",
	description = "List of files to be excluded."
})

preferences:registerPreference({
	type = "CheckBox",
	name = "encryptOnClose",
	defaultValue = true,
	title = "Ask me to encrypt my files when I close Appetizer"
})

preferences:registerPreference({
	type = "CheckBox",
	name = "decryptOnClose",
	defaultValue = true,
	title = "Ask me to decrypt my files when I open Appetizer"
})



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


function getFullFilePaths(fileString, resolvePatterns)
	local files = string_split(fileString, "\n")
	local output = {}
	
	for i, file in ipairs(files) do
		local fullPath = system:resolvePath(file)
		
		if resolvePatterns then
		
			local pattern = getFilename(fullPath, true)
			local filePath = getFilePath(fullPath)
			
			if filePath == '' then
				filePath = appetizer:getDrive()
			end
					
			local fullFilePaths = system:getDirectoryContents(filePath, true)
			
			for j, fullFilePath in ipairs(fullFilePaths) do
				if system:fileMatchesPattern(fullFilePath, pattern) then
					table.insert(output, fullFilePath)
				end
			end
			
		else
		
			table.insert(output, fullPath)
		
		end
		
	end
	
	return output	
end


function doCrypt(password, encrypt)

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
			end	
			
		end
		
	end
	
end



function askForPassword(confirm)
	local controls = {}
	
	table.insert(controls, {
		type = "Text",
		name = "password",
		secure = true,
		title = "Password:"
	})
	
	if confirm then
		table.insert(controls, {
			type = "Text",
			name = "passwordConfirm",
			secure = true,
			title = "Confirm:"
		})
	end
	
	local result = dialogs:showForm(controls, "Type-in your password")
	
	if not result then
		return ""
	end

	if not confirm then
		return string_trim(result.password)
	end
	
	if result.password ~= result.passwordConfirm then
		return ""
	end
	
	return string_trim(result.password)
end





-- Show dialog box
-- Show panel while operation in progress
-- Hook onApplicationStart
-- Hook onApplicationEnd -- ask user if he wants to encrypt his files