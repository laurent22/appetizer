/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#ifndef __azMenu_H
#define __azMenu_H


#include "azWrapper.h"


class azMenu : public azWrapper {

public:

  azMenu(wxMenu* menu);
  azMenu(lua_State *L);
  ~azMenu();

  int appendSeparator(lua_State *L);
  int append(lua_State *L);
  int appendSubMenu(lua_State *L);
  
  static const char className[];
  static Lunar<azMenu>::RegType methods[];

  wxMenu* Get() const { return menu_; }

  void Set(wxMenu* menu) { menu_ = menu; }
  void ReleaseContent();
  bool IsOwningContent() { return ownContent_; }

  static void OnLuaScopeClose();

private:

  bool ownContent_;
  wxMenu* menu_; 
  
  static std::vector<azMenu*> createdObjects_;

};


#endif // __azMenu_H