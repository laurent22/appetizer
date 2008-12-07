/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/



/**
 * Option buttons are displayed on the option panel.
 *
 * @beginEventTable
 * @event Click click when the button is clicked
 * @endEventTable
 * @see OptionPanel
 *
 */	


#include "../stdafx.h"

#include "azOptionButton.h"
#include "azMenu.h"

#include "../ExtendedMenuItem.h"
#include "../MiniLaunchBar.h"
#include "LuaUtil.h"


//*****************************************************************
//
// LUNAR DATA
//
//*****************************************************************

const char azOptionButton::className[] = "OptionButton";

#define method(class, name) {#name, &class::name}

Lunar<azOptionButton>::RegType azOptionButton::methods[] = {
  method(azOptionButton, setToolTip),
  method(azOptionButton, addEventListener),
  method(azOptionButton, popupMenu),
  method(azOptionButton, getToolTip),
  method(azOptionButton, getName),
  method(azOptionButton, setName),
  {0,0}
};


//*****************************************************************
//
// NON-EXPORTED MEMBERS
//
//*****************************************************************


OptionButton* azOptionButton::Get() const {
  OptionButton* b = wxGetApp().GetMainFrame()->GetOptionPanel()->GetButtonById(buttonId_);
  if (b) return b;
  
  // The button may be on the null panel if it has been created but hasn't been
  // added to the option panel yet.
  b = (OptionButton*)(wxGetApp().GetMainFrame()->GetNullPanelObjectById(buttonId_));
  if (b) return b;

  return NULL;
}


void azOptionButton::OnMenuClick(wxCommandEvent& evt) {
  ExtendedMenuItem* menuItem = GetClickedMenuItem(evt);
  if (!menuItem) return;

  bool handled = wxGetApp().GetPluginManager()->HandleMenuItemClick(menuItem);
  
  if (!handled) evt.Skip();
}


//*****************************************************************
//
// EXPORTED MEMBERS
//
//*****************************************************************


/**
 * @copy Application#addEventListener()
 * @see Application#addEventListener()
 * 
 */	
int azOptionButton::addEventListener(lua_State *L) {
  wxString eventName = LuaUtil::ToString(L, 1);
  wxString functionName = LuaUtil::ToString(L, 2);
  Plugin* p = wxGetApp().GetPluginManager()->GetPluginByLuaState(L);
  p->AddEventListener(this->Get(), eventName, functionName);
  return 0;
}


/**
 * Constructor.
 * 
 */	
azOptionButton::azOptionButton(lua_State *L) {
  OptionButton* b = new OptionButton(wxGetApp().GetMainFrame()->GetNullPanel(), wxGetApp().GetUniqueInt());
  buttonId_ = b->GetId();
}


/**
 * 
 * Sets the tooltip that shows up when the user moves the mouse over the control.
 * @param String tooltip The tooltip.
 * 
 */	
int azOptionButton::setToolTip(lua_State *L) {
  CheckWrappedObject(L, Get());

  wxString toolTip = LuaUtil::ToString(L, 1);
  Get()->SetToolTip(toolTip);
  return 0;
}


/**
 * 
 * Pops up a menu below the button. This can be used to allow the user to choose
 * between multiple options when the button is clicked.
 * @param Menu menu The menu to display.
 * @see Menu
 * 
 */	
int azOptionButton::popupMenu(lua_State *L) {
  CheckWrappedObject(L, Get());

  const azMenu* menu = Lunar<azMenu>::check(L, 1);

  OptionButton* button = Get();

  int screenX = button->GetRect().GetX();
  int screenY = button->GetRect().GetY();

  wxWindow* parent = button->GetParent();
  if (!parent) luaL_error(L, "Cannot popup a menu on a button that does not have a parent");

  menu->Get()->Connect(
    wxID_ANY,
    wxEVT_COMMAND_MENU_SELECTED,
    wxCommandEventHandler(azOptionButton::OnMenuClick),
    NULL,
    this);

  parent->PopupMenu(menu->Get(), screenX, screenY + button->GetRect().GetHeight());

  return 0;
}


/**
 * 
 * Gets the tooltip associated with this control.
 * @return String The tooltip.
 * 
 */	
int azOptionButton::getToolTip(lua_State *L) {
  CheckWrappedObject(L, Get());

  wxToolTip* tooltip = Get()->GetToolTip();

  if (!tooltip) {
    LuaUtil::PushString(L, wxEmptyString);
  } else {
    LuaUtil::PushString(L, tooltip->GetTip());
  }
    
  return 1;
}


/**
 * 
 * Gets the control name.
 * @return String The control name.
 * 
 */	
int azOptionButton::getName(lua_State *L) { CheckWrappedObject(L, Get()); LuaUtil::PushString(L, Get()->GetName()); return 1; }

/**
 * 
 * Sets the control name.
 * @param String name The control name.
 * 
 */	
int azOptionButton::setName(lua_State *L) { CheckWrappedObject(L, Get());Get()->SetName(LuaUtil::ToString(L, 1)); return 0; }