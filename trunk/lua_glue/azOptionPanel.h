/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#ifndef __azOptionPanel_H
#define __azOptionPanel_H


#include "../OptionPanel.h"
#include "azWrapper.h"


class azOptionPanel : public azWrapper {

public:

  azOptionPanel() {}
  azOptionPanel(lua_State *L) {}

  int addButton(lua_State *L);
  int buttonCount(lua_State *L);
  int getButtonAt(lua_State *L);
  int removeButton(lua_State *L);
  
  static const char className[];
  static Lunar<azOptionPanel>::RegType methods[];

  OptionPanel* Get();

};


#endif // __azOptionPanel_H