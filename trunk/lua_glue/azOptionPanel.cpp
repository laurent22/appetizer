/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
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


int azOptionPanel::addButton(lua_State *L) {
  const azOptionButton* optionButton = Lunar<azOptionButton>::check(L, -1); 
  Get()->AddButton(optionButton->Get());

  return 0;
}


int azOptionPanel::buttonCount(lua_State *L) {
  lua_pushinteger(L, Get()->ButtonCount());

  return 1;
}


int azOptionPanel::getButtonAt(lua_State *L) {
  int index = lua_tointeger(L, 1);
  if (index >= Get()->ButtonCount()) luaL_error(L, "Index out of bounds");

  OptionButton* button = Get()->GetButtonAt(index);
  
  Lunar<azOptionButton>::push(L, new azOptionButton(button), true);

  return 1;
}


int azOptionPanel::removeButton(lua_State *L) {
  const azOptionButton* optionButton = Lunar<azOptionButton>::check(L, -1); 
  Get()->RemoveButton(optionButton->Get());

  return 0;
}