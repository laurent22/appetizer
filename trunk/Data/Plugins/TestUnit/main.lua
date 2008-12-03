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



