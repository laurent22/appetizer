/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#include "azOptionButton.h"
#include "azMenu.h"

#include "../ExtendedMenuItem.h"
#include "../MiniLaunchBar.h"
#include "../utilities/LuaUtil.h"


const char azOptionButton::className[] = "azOptionButton";

#define method(class, name) {#name, &class::name}

Lunar<azOptionButton>::RegType azOptionButton::methods[] = {
  method(azOptionButton, setToolTip),
  method(azOptionButton, addEventListener),
  method(azOptionButton, popupMenu),
  {0,0}
};


azIMPLEMENT_EVENT_LISTENER_FUNCTION(azOptionButton)


azOptionButton::azOptionButton(lua_State *L) {
  button_ = new OptionButton(wxGetApp().GetMainFrame()->GetNullPanel(), wxGetApp().GetUniqueInt());
}


int azOptionButton::setToolTip(lua_State *L) {
  wxString toolTip = LuaUtil::ToString(L, 1);
  Get()->SetToolTip(toolTip);
  return 0;
}


void azOptionButton::OnMenuClick(wxCommandEvent& evt) {
  ExtendedMenuItem* menuItem = GetClickedMenuItem(evt);
  if (!menuItem) return;

  bool handled = wxGetApp().GetPluginManager()->HandleMenuItemClick(menuItem);
  
  if (!handled) evt.Skip();
}


int azOptionButton::popupMenu(lua_State *L) {
  const azMenu* menu = Lunar<azMenu>::check(L, 1);

  int screenX = Get()->GetRect().GetX();
  int screenY = Get()->GetRect().GetY();

  if (!Get()->GetParent()) return 0; // shouldn't happen

  menu->Get()->Connect(
    wxID_ANY,
    wxEVT_COMMAND_MENU_SELECTED,
    wxCommandEventHandler(azOptionButton::OnMenuClick),
    NULL,
    this);

  Get()->GetParent()->PopupMenu(menu->Get(), screenX, screenY + Get()->GetRect().GetHeight());

  return 0;
}