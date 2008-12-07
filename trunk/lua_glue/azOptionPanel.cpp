/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/


/**
 * The class represents the small panel that shows up when the user clicks on the arrow button. There
 * can be any number of buttons on it, and it is possible to add or remove buttons from it. Do not create
 * an instance of this class - instead use the global <code>optionPanel</code> object.
 * @see Global#optionPanel
 *
 */	


#include "../stdafx.h"

#include "azOptionPanel.h"
#include "azOptionButton.h"
#include "../MiniLaunchBar.h"


//*****************************************************************
//
// LUNAR DATA
//
//*****************************************************************


const char azOptionPanel::className[] = "OptionPanel";

#define method(class, name) {#name, &class::name}

Lunar<azOptionPanel>::RegType azOptionPanel::methods[] = {
  method(azOptionPanel, addButton),
  method(azOptionPanel, buttonCount),
  method(azOptionPanel, getButtonAt),
  method(azOptionPanel, removeButton),
  method(azOptionPanel, open),
  method(azOptionPanel, close),
  method(azOptionPanel, isOpen),
  {0,0}
};


//*****************************************************************
//
// NON-EXPORTED MEMBERS
//
//*****************************************************************


OptionPanel* azOptionPanel::Get() {
  return wxGetApp().GetMainFrame()->GetOptionPanel();
}


//*****************************************************************
//
// EXPORTED MEMBERS
//
//*****************************************************************



/**
 * Adds the given button to the panel.
 * @param OptionButton button The button to add to the panel.
 * @example The following code creates a new button and add it to the panel.
 * <listing version="3.0">
 * -- Create the button
 * button = OptionButton:new()
 * button:setName("my button")
 * 
 * -- Register an event listener
 * button:addEventListener("click", "button_click")
 *
 * -- Add it to the option panel
 * optionPanel:addButton(button)
 *
 * function button_click(event)
 *     dialogs:showMessage("'"..event.sender:getName().."' has been clicked!")
 * end
 * </listing>
 * 
 */	
int azOptionPanel::addButton(lua_State *L) {
  const azOptionButton* optionButton = Lunar<azOptionButton>::check(L, -1); 
  Get()->AddButton(optionButton->Get());

  return 0;
}


/**
 * Returns the number of buttons on the option panel.
 * @return Number Number of buttons.
 * @example This script loops through all the option buttons and returns the one with the given name.
 * <listing version="3.0">
 * function getButtonByName(buttonName)
 *     buttonCount = optionPanel:buttonCount()
 *     
 *     for i = 0, (buttonCount - 1) do
 *         button = optionPanel:getButtonAt(i)
 *         if buttonName == button:getName()
 *             return button
 *         end
 *     end
 * end
 *
 * helpButton = getButtonByName("Help")
 * </listing>
 * 
 */	
int azOptionPanel::buttonCount(lua_State *L) {
  lua_pushinteger(L, Get()->ButtonCount());

  return 1;
}


/**
 * Gets the button at the specified index.
 * @param Number index Button index.
 * @see #buttonCount
 * 
 */	
int azOptionPanel::getButtonAt(lua_State *L) {
  int index = lua_tointeger(L, 1);
  if (index >= Get()->ButtonCount()) luaL_error(L, "Index out of bounds");

  OptionButton* button = Get()->GetButtonAt(index);
  
  Lunar<azOptionButton>::push(L, new azOptionButton(button), true);

  return 1;
}


/**
 * Removes the specified button.
 * @param OptionButton button The button to remove from the panel.
 * @example This script removes the last button from the panel.
 * <listing version="3.0">
 * lastButton = optionPanel:getButtonAt(optionPanel:buttonCount() - 1)
 * optionPanel:removeButton(lastButton)
 * </listing>
 * 
 */	
int azOptionPanel::removeButton(lua_State *L) {
  const azOptionButton* optionButton = Lunar<azOptionButton>::check(L, 1); 
  Get()->RemoveButton(optionButton->Get());

  return 0;
}


/**
 * Opens the option panel.
 * 
 */	
int azOptionPanel::open(lua_State *L) {
  wxGetApp().GetMainFrame()->OpenOptionPanel(); return 0;
}


/**
 * Closes the option panel.
 * 
 */	
int azOptionPanel::close(lua_State *L) {
  wxGetApp().GetMainFrame()->CloseOptionPanel(); return 0;
}


/**
 * Returns <code>true</code> if the panel is open.
 * @return Boolean <code>true</code> if the panel is open.
 * @example This code toggles the option panel on and off:
 * <listing version="3.0">
 * function toggleOptionPanel()
 *     if optionPanel:isOpen() then
 *         optionPanel:close()
 *     else
 *         optionPanel:open()
 *     end
 * end
 *
 * toggleOptionPanel()
 * </listing>
 * 
 */	
int azOptionPanel::isOpen(lua_State *L) {
  lua_pushboolean(L, wxGetApp().GetMainFrame()->IsOptionPanelOpen()); return 1;
}

