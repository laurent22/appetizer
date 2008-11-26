
function icon_popupMenu(event)
	az_print("ouais!")
	
	--for key,value in pairs(data) do az_print(key,value) end
end

az_addEventListener(AZ_OBJECT_ANY_ICON, AZ_EVENT_POPUP_MENU_OPENING, "icon_popupMenu")