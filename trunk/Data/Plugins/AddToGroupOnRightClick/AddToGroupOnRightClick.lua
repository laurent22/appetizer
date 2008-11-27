-- This script allows adding a shortcut to a group
-- by right-clicking on it.
--
-- An "Add to group" sub-menu is added to each icon popup menu.
-- The user can then select to which group the shortcut should be added


function addToGroupPopupMenu_click(event) 
	group = azGetShortcutById(event.menuItemId)
	shortcut = azGetShortcutById(event.menuItemTag)
		
	azShortcut_AddChild(group, shortcut)
end


-- This function is called when an icon is right-clicked,
-- before the popup menu is displayed
function icon_popupMenu(event)
	-- Get the icon
	icon = event.sender
	
	-- Get the icon popup menu
	menu = azIcon_GetPopupMenu(icon)
	
	-- Get the shortcut object associated with the icon
	iconShortcutObject = azIcon_GetShortcut(icon)
	
	-- Get all the application groups
	root = azGetShortcutsRoot()
	groups = azShortcut_GetAllGroups(root)
	
	-- If no group exists, exit now
	if table.maxn(groups) == 0 then return 0; end
	
	-- Create the "Add to group" sub menu
	subMenu = azNewMenu("Add to group  moi")		
	
	-- For each group, create a menu item
	for i, group in pairs(groups) do
		menuItem = {}
		menuItem.text = azShortcut_GetName(group);
		menuItem.id = azShortcut_GetId(group); -- keep a reference to the group
		menuItem.tag = azShortcut_GetId(iconShortcutObject); -- keep a reference to the shortcut
		menuItem.onClick = "addToGroupPopupMenu_click" -- this function is going to be called when/if the menu item is selected
				
		-- Add the menu item to the sub menu
		azMenu_Append(subMenu, menuItem)
	end
	
	-- Add a separator and append the sub menu to the icon popup menu
	azMenu_AppendSeparator(menu)
	azMenu_AppendSubMenu(menu, subMenu)
end


-- Register a global event handler. "icon_popupMenu()" is going
-- to be called whenever an icon popup menu is about to be displayed
azAddEventListener(azApp, azEvent_OnIconPopupMenu, "icon_popupMenu")