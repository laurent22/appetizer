/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#include "azGlobal.h"
#include "azShortcut.h"

#include "../MiniLaunchBar.h"
#include "LuaUtil.h"


const char azShortcut::className[] = "Shortcut";

#define method(class, name) {#name, &class::name}

Lunar<azShortcut>::RegType azShortcut::methods[] = {
  method(azShortcut, getAllGroups),
  method(azShortcut, getName),
  method(azShortcut, getId),
  method(azShortcut, addChild),
  method(azShortcut, setName),
  method(azShortcut, setPath),
  method(azShortcut, autoSetName),
  method(azShortcut, getPath),
  method(azShortcut, getResolvedPath),
  method(azShortcut, launch),
  method(azShortcut, setParameters),
  method(azShortcut, getParameters),
  method(azShortcut, addToMultiLaunchGroup),
  method(azShortcut, belongsToMultiLaunchGroup),
  method(azShortcut, removeFromMultiLaunchGroup),
  method(azShortcut, childrenCount),
  method(azShortcut, getChildAt),
  method(azShortcut, getParent),
  method(azShortcut, removeFromParent),
  method(azShortcut, insertChildAt),
  {0,0}
};


azShortcut::azShortcut(lua_State *L) {
  folderItem_ = FolderItem::CreateFolderItem(LuaUtil::ToBoolean(L, 1));
}


int azShortcut::getAllGroups(lua_State *L) {
  FolderItemVector allGroups = folderItem_->GetAllGroups();

  lua_createtable(L, allGroups.size(), 0);
  int tableIndex = lua_gettop(L);

  for (int i = 0; i < allGroups.size(); i++) {
    FolderItem* f = allGroups.at(i);

    lua_pushinteger(L, i + 1);
    Lunar<azShortcut>::push(L, new azShortcut(f), true);
    lua_settable(L, tableIndex);
  }

  return 1;
}


int azShortcut::getName(lua_State *L) {
  lua_pushstring(L, folderItem_->GetName(true).ToUTF8());
  
  return 1;
}


int azShortcut::getId(lua_State *L) {
  lua_pushinteger(L, folderItem_->GetId());
  
  return 1;
}


int azShortcut::addChild(lua_State *L) {
  if (!folderItem_->IsGroup()) return 0;

  const azShortcut* shortcut = Lunar<azShortcut>::check(L, -1); 
  if (!shortcut) return 0;

  folderItem_->AddChild(shortcut->Get());

  wxGetApp().FolderItems_CollectionChange();

  return 0;
}


int azShortcut::setName(lua_State *L) { wxString n = LuaUtil::ToString(L, 1); folderItem_->SetName(n); return 0; }
int azShortcut::autoSetName(lua_State *L) { folderItem_->AutoSetName(); return 0; }
int azShortcut::setPath(lua_State *L) { wxString n = LuaUtil::ToString(L, 1); folderItem_->SetFilePath(n); return 0; }
int azShortcut::getPath(lua_State *L) { LuaUtil::PushString(L, folderItem_->GetFilePath()); return 1; }
int azShortcut::getResolvedPath(lua_State *L) { LuaUtil::PushString(L, folderItem_->GetResolvedPath()); return 1; }
int azShortcut::setParameters(lua_State *L) { wxString n = LuaUtil::ToString(L, 1); folderItem_->SetParameters(n); return 0; }
int azShortcut::getParameters(lua_State *L) { LuaUtil::PushString(L, folderItem_->GetParameters()); return 1; }
int azShortcut::addToMultiLaunchGroup(lua_State *L) { folderItem_->AddToMultiLaunchGroup(); return 0; }
int azShortcut::belongsToMultiLaunchGroup(lua_State *L) { lua_pushboolean(L, folderItem_->BelongsToMultiLaunchGroup()); return 1; }
int azShortcut::removeFromMultiLaunchGroup(lua_State *L) { folderItem_->RemoveFromMultiLaunchGroup(); return 0; }
int azShortcut::childrenCount(lua_State *L) { lua_pushinteger(L, folderItem_->ChildrenCount()); return 1; }

int azShortcut::launch(lua_State *L) {
  int argc = lua_gettop(L);
  if (argc == 0) {
    folderItem_->Launch();
  } else {
    folderItem_->LaunchWithArguments(LuaUtil::ToString(L, 1));
  }
  return 0;
}

int azShortcut::getChildAt(lua_State *L) {
  if (!folderItem_->IsGroup()) return 0;
  int index = luaL_checkinteger(L, 1);
  if (index >= folderItem_->ChildrenCount()) return 0;

  Lunar<azShortcut>::push(L, new azShortcut(folderItem_->GetChildAt(index)), true);
  return 1;
}

int azShortcut::getParent(lua_State *L) {
  FolderItem* p = folderItem_->GetParent();
  if (!p) return 0;

  FolderItem* sp = FolderItem::GetFolderItemById(p->GetId());
  Lunar<azShortcut>::push(L, new azShortcut(sp), true);
  return 1;
}

int azShortcut::removeFromParent(lua_State *L) {
  FolderItem* p = folderItem_->GetParent();
  if (!p) return 0;
  
  p->RemoveChild(folderItem_);
  return 0;
}

int azShortcut::insertChildAt(lua_State *L) { 
  if (!folderItem_->IsGroup()) return 0;
  const azShortcut* shortcut = Lunar<azShortcut>::check(L, 1);
  int index = luaL_checkinteger(L, 2);  
  folderItem_->MoveChild(shortcut->Get(), index);
  return 0;
}