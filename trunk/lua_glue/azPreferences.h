/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#ifndef __azPreferences_H
#define __azPreferences_H


#include "../PluginPreferences.h"
#include "azWrapper.h"


class azPreferences : public azWrapper {

public:

  azPreferences(PluginPreferences* preferences);
  azPreferences(lua_State *L);
  ~azPreferences();

  int registerPreference(lua_State *L);
  int getValue(lua_State *L);
  int setValue(lua_State *L);

  
  static const char className[];
  static Lunar<azPreferences>::RegType methods[];

  PluginPreferences* Get() const;

private:

  PluginPreferences* preferences_;

};


#endif // __azPreferences_H