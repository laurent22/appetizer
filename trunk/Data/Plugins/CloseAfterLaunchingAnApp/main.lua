-- Copyright (C) 2008 Laurent Cozic. All right reserved.
-- Use of this source code is governed by a GNU/GPL license that can be
-- found in the LICENSE file.

-- This script listens to the "dockItemClick" event and close Appetizer when it occurs


-- This function is called when a shortcut is launched
function appetizer_dockItemClick(event)
	dockItem = event.dockItem
	
	if not dockItem:isGroup() then
		appetizer:close()
	end
end


-- Register a global event handler. "appetizer_dockItemClick()" is going
-- to be called whenever a dock item is clicked
appetizer:addEventListener("dockItemClick", "appetizer_dockItemClick")