/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#ifndef __azShortcut_H
#define __azShortcut_H


#include "../FolderItem.h"


class azShortcut : public wxObject {

public:

  azShortcut(FolderItemSP folderItem) { folderItem_ = folderItem; }
  azShortcut(lua_State *L) {}  

  int getAllGroups(lua_State *L);
  int getName(lua_State *L);
  int getId(lua_State *L);
  int addChild(lua_State *L);
  
  static const char className[];
  static Lunar<azShortcut>::RegType methods[];

  FolderItemSP folderItem_;  

};


#endif // __azShortcut_H