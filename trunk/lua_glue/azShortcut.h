/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#ifndef __azShortcut_H
#define __azShortcut_H


#include "../FolderItem.h"
#include "azWrapper.h"


class azShortcut : public azWrapper {

public:

  azShortcut(FolderItemSP folderItem) { folderItem_ = folderItem; }
  azShortcut(lua_State *L);

  int getAllGroups(lua_State *L);
  int getName(lua_State *L);
  int getId(lua_State *L);
  int addChild(lua_State *L);
  int setName(lua_State *L);
  int autoSetName(lua_State *L);
  int setPath(lua_State *L);
  int getPath(lua_State *L);
  int getResolvedPath(lua_State *L);
  int launch(lua_State *L);
  int setParameters(lua_State *L);
  int getParameters(lua_State *L);

  int addToMultiLaunchGroup(lua_State *L);
  int belongsToMultiLaunchGroup(lua_State *L);
  int removeFromMultiLaunchGroup(lua_State *L);
  
  int childrenCount(lua_State *L);
  int getChildAt(lua_State *L);
  int getParent(lua_State *L);
  int removeFromParent(lua_State *L);
  int insertChildAt(lua_State *L);
  
  static const char className[];
  static Lunar<azShortcut>::RegType methods[];

  FolderItemSP Get() const { return folderItem_; }

private:

  FolderItemSP folderItem_;  

};


#endif // __azShortcut_H