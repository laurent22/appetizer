preferences:registerPreference({
	type = "TextArea",
	name = "includedFiles",
	defaultValue = "*.txt\n*.odt\n*.doc\n*.docx",
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


function encryptFile(filePath, password)

end

result = system:runCommand('s:\\Docs\\PROGS\\C++\\MiniLaunchBar\\trunk\\Data\\Plugins\\Securizer\\Resources\\ccrypt.exe --encrypt --key abc --suffix .ccrypt1_7 "s:\\Docs\\PROGS\\C++\\MiniLaunchBar\\trunk\\Data\\Plugins\\Securizer\\Resources\\wx.chm"')

trace(result)

--'cmd /c "dir c: /On"')