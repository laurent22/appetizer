-- Copyright (C) 2008 Laurent Cozic. All right reserved.
-- Use of this source code is governed by a GNU/GPL license that can be
-- found in the LICENSE file.

function shortcutTest()
	trace("Creating new shortcut...")
	s = Shortcut:new(false)

	s:setPath("C:\\WINDOWS\\system32\\calc.exe")
	trace("Path = ", s:getPath())

	s:setName("Some random name")
	trace("Name (before autoSetName) = ", s:getName())
	s:autoSetName()
	trace("Name (after autoSetName) = ", s:getName())

	s:setParameters("-help")
	trace("Parameters = ", s:getParameters())
	s:setParameters("")

	trace("Belongs to MLG = ", s:belongsToMultiLaunchGroup())
	s:addToMultiLaunchGroup()
	trace("Belongs to MLG = ", s:belongsToMultiLaunchGroup())
	s:removeFromMultiLaunchGroup()
	trace("Belongs to MLG = ", s:belongsToMultiLaunchGroup())

	trace("Adding shortcut to root...")
	root = appetizer:getShortcutRoot()
	root:addChild(s)

	trace("Launching shortcut...")
	s:launch()

	trace("Number of children on root: ", root:childrenCount())

	trace("Getting first child")
	firstChild = root:getChildAt(0)

	trace("Getting parent")
	parent = firstChild:getParent()
	trace("Parent name is (should be root): ", parent:getName())

	trace("Inserting calc.exe shortcut at index 0")
	root:insertChildAt(s, 0)

	trace("Removing the child that was previously the first one: ", firstChild:getName())
	firstChild:removeFromParent()
end
	
	
	
function groupTest()
	trace("Creating new group...")
	group = Shortcut:new(true)	
	group:setName("test group")
	
	trace("Duplicating all the shortcuts on the dock, and adding them to the group")
	
	root = appetizer:getShortcutRoot()	
	childrenCount = root:childrenCount()
	
	for i = 0, (childrenCount - 1) do
		child = root:getChildAt(i)
		
		trace("Duplicating shortcut: ", child:getName())
	
		shortcut = Shortcut:new()
		shortcut:setPath(child:getPath())
		shortcut:autoSetName()
		
		group:addChild(shortcut)
	end	
	
	trace("Adding group to root")	
	root:addChild(group)	
end



function dialogTest()
	result = dialogs:showMessage("This is an error message with an ok button", "ok", "error")
	dialogs:showMessage("'"..result.."' was clicked")
	
	result = dialogs:showMessage("This is a warning message with yes / no buttons", "yesNo", "warning")
	dialogs:showMessage("'"..result.."' was clicked")	
	
	result = dialogs:showMessage("This is a confirmation message with yes / no / cancel buttons", "yesNoCancel", "confirmation")
	dialogs:showMessage("'"..result.."' was clicked")	
	
	dialogs:showMessage("Showing eject dialog...")	
	dialogs:showEjectDriveDialog()
	
	dialogs:showMessage("Showing 'new shortcut' dialog...")	
	dialogs:showNewShortcutDialog()	
	
	dialogs:showMessage("Showing 'config' dialog...")	
	dialogs:showConfigDialog()	
	
	dialogs:showMessage("Showing 'import' dialog...")	
	dialogs:showImportDialog()	
end



function applicationTest()
	dialogs:showMessage("Hidding application")
	appetizer:hide()	
	trace("Application visible = ", appetizer:isVisible())
	
	dialogs:showMessage("Showing application")
	appetizer:show()
	trace("Application visible = ", appetizer:isVisible())

	dialogs:showMessage("Closing option panel")
	appetizer:closeOptionPanel()		
	trace("Option panel is open = ", appetizer:isOptionPanelOpen())
	
	dialogs:showMessage("Opening option panel")
	appetizer:openOptionPanel()	
	trace("Option panel is open = ", appetizer:isOptionPanelOpen())
	
	currentOrientation = appetizer:getOrientation()
	dialogs:showMessage("Current orientation is: "..currentOrientation..". Rotating it...")
	
	if currentOrientation == "vertical" then
		appetizer:setOrientation("horizontal")
	else
		appetizer:setOrientation("vertical")
	end
	
	dialogs:showMessage("And back to normal...")
	appetizer:setOrientation(currentOrientation)
	
	result = dialogs:showMessage("Do you wish to install the autorun.inf file on "..appetizer:getDrive().."?", "yesNo", "confirmation")
	
	if result == "yes" then
		appetizer:installAutoRunFile()
	end
	
	dialogs:showMessage("Showing help file...")
	appetizer:showHelpFile()
	
	dialogs:showMessage("Doing Multilaunch (only works if there are items in the multilaunch group)")
	appetizer:doMultiLaunch()	
	
	dialogs:showMessage("Closing...")
	appetizer:close()
end



function optionButton_click(event)
	trace("Option button was clicked!")
	
	trace("Building menu to select test units...")
	
	menu = Menu:new("Test units")
	
	menuItem = {}
	menuItem.text = "Shortcut test"
	menuItem.onClick = "shortcutTest"
	menu:append(menuItem)
	
	menuItem = {}
	menuItem.text = "Group test"
	menuItem.onClick = "groupTest"
	menu:append(menuItem)
	
	menuItem = {}
	menuItem.text = "Dialog test"
	menuItem.onClick = "dialogTest"
	menu:append(menuItem)
	
	menuItem = {}
	menuItem.text = "Application test"
	menuItem.onClick = "applicationTest"
	menu:append(menuItem)
	
	trace("Get the object that has sent the event, in this case the option button")
	optionButton = event.sender
	
	trace("Showing menu")	
	optionButton:popupMenu(menu)
end



trace("Creating new button")	
button = OptionButton():new()

trace("Setting tooltip");
button:setToolTip("My plugin button")

trace("Adding button to option panel")
optionPanel:addButton(button)

button:addEventListener("click", "optionButton_click")



