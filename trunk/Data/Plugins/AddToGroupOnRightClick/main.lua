-- Copyright (C) 2008 Laurent Cozic. All right reserved.
-- Use of this source code is governed by a GNU/GPL license that can be
-- found in the LICENSE file.

-- This script allows adding a shortcut to a group
-- by right-clicking on it.
--
-- An "Add to group" sub-menu is added to each icon popup menu.
-- The user can then select to which group the shortcut should be added


function addToGroupPopupMenu_click(event) 
	-- Get the clicked menu item
	menuItem = event.menuItem

	-- Get the group
	group = appetizer:getShortcutById(menuItem:getId())
	-- Get the shortcut that must be added to the group
	shortcut = appetizer:getShortcutById(menuItem:getTag())
		
	-- Add the shortcut to the group
	group:addChild(shortcut)
end


-- This function is called when an icon is right-clicked,
-- before the popup menu is displayed
function application_iconMenuOpening(event)
	-- Get the icon
	icon = event.icon
	
	-- Get the icon popup menu and associated shortcut object
	menu = icon:getPopupMenu()	
	shortcut = icon:getShortcut()
	
	-- Get all the application groups, so that we
	-- can build a submenu from it
	root = appetizer:getShortcutRoot()	
	groups = root:getAllGroups()
	
	-- If no group exists, exit now
	if table.maxn(groups) == 0 then return 0; end
	
	-- Create the "Add to group" sub menu
	subMenu = Menu:new("Add to group")
	
	for i, group in pairs(groups) do
		-- Create a menu item with the group name as a label
		menuItem = MenuItem:new(group:getName())
		menuItem:setId(group:getId()) -- keep a reference to the group
		menuItem:setTag(shortcut:getId()) -- keep a reference to the shortcut
		menuItem:setOnSelected("addToGroupPopupMenu_click") -- this function is going to be called when/if the menu item is selected
				
		-- Add the menu item to the sub menu
		subMenu:append(menuItem)
	end	
	
	-- Add a separator and append the sub menu to the icon popup menu
	menu:appendSeparator()
	menu:appendSubMenu(subMenu)
end


-- Register a global event handler. "application_iconMenuOpening()" is going
-- to be called whenever an icon popup menu is about to be displayed
appetizer:addEventListener("iconMenuOpening", "application_iconMenuOpening")