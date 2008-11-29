function shortcutTest()
	azPrint("Creating new shortcut...")
	s = azShortcut:new(false)

	s:setPath("C:\\WINDOWS\\system32\\calc.exe")
	azPrint("Path = ", s:getPath())

	s:setName("Some random name")
	azPrint("Name (before autoSetName) = ", s:getName())
	s:autoSetName()
	azPrint("Name (after autoSetName) = ", s:getName())

	s:setParameters("-help")
	azPrint("Parameters = ", s:getParameters())
	s:setParameters("")

	azPrint("Belongs to MLG = ", s:belongsToMultiLaunchGroup())
	s:addToMultiLaunchGroup()
	azPrint("Belongs to MLG = ", s:belongsToMultiLaunchGroup())
	s:removeFromMultiLaunchGroup()
	azPrint("Belongs to MLG = ", s:belongsToMultiLaunchGroup())

	azPrint("Adding shortcut to root...")
	root = appetizer:getShortcutRoot()
	root:addChild(s)

	azPrint("Launching shortcut...")
	s:launch()

	azPrint("Number of children on root: ", root:childrenCount())

	azPrint("Getting first child")
	firstChild = root:getChildAt(0)

	azPrint("Getting parent")
	parent = firstChild:getParent()
	azPrint("Parent name is (should be root): ", parent:getName())

	azPrint("Inserting calc.exe shortcut at index 0")
	root:insertChildAt(s, 0)

	azPrint("Removing the child that was previously the first one: ", firstChild:getName())
	firstChild:removeFromParent()
end
	
	
	
function groupTest()
	azPrint("Creating new group...")
	group = azShortcut:new(true)	
	group:setName("test group")
	
	azPrint("Duplicating all the shortcuts on the dock, and adding them to the group")
	
	root = appetizer:getShortcutRoot()	
	childrenCount = root:childrenCount()
	
	for i = 0, (childrenCount - 1) do
		child = root:getChildAt(i)
		
		azPrint("Duplicating shortcut: ", child:getName())
	
		shortcut = azShortcut:new()
		shortcut:setPath(child:getPath())
		shortcut:autoSetName()
		
		group:addChild(shortcut)
	end	
	
	azPrint("Adding group to root")	
	root:addChild(group)	
end



function optionButton_click(event)
	azPrint("Option button was clicked!")
	
	azPrint("Building menu to select test units...")
	
	menu = azMenu:new("Test units")
	
	menuItem = {}
	menuItem.text = "Shortcut test";
	menuItem.onClick = "shortcutTest";	
	menu:append(menuItem)
	
	menuItem = {}
	menuItem.text = "Group test";
	menuItem.onClick = "groupTest";	
	menu:append(menuItem)
	
	azPrint("Showing menu")
	
	event.sender:popupMenu(menu)
end



azPrint("Creating new button")	
button = azOptionButton():new()

azPrint("Setting tooltip");
button:setToolTip("My plugin button");

azPrint("Adding button to option panel")
optionPanel:addButton(button)

button:addEventListener("click", "optionButton_click")



