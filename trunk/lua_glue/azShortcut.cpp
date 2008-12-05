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


//*****************************************************************
//
// LUNAR DATA
//
//*****************************************************************

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


//*****************************************************************
//
// NON-EXPORTED MEMBERS
//
//*****************************************************************


azShortcut::azShortcut(FolderItem* folderItem) {
  folderItemId_ = folderItem->GetId();
}


FolderItem* azShortcut::Get() const {
  // This is very fast (50 milliseconds for 1000000 iterations) so no need to optimize
  return FolderItem::GetFolderItemById(folderItemId_);
}


//*****************************************************************
//
// EXPORTED MEMBERS
//
//*****************************************************************


azShortcut::azShortcut(lua_State *L) {
  FolderItem* folderItem = FolderItem::CreateFolderItem(LuaUtil::ToBoolean(L, 1));
  folderItemId_ = folderItem->GetId();
}


int azShortcut::getAllGroups(lua_State *L) {
  CheckWrappedObject(L, Get()); 

  FolderItemVector allGroups = Get()->GetAllGroups();

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
  CheckWrappedObject(L, Get()); 

  lua_pushstring(L, Get()->GetName(true).ToUTF8());
  
  return 1;
}


int azShortcut::getId(lua_State *L) {
  CheckWrappedObject(L, Get()); 

  lua_pushinteger(L, Get()->GetId());
  
  return 1;
}


int azShortcut::addChild(lua_State *L) {
  CheckWrappedObject(L, Get()); 

  if (!Get()->IsGroup()) luaL_error(L, "No child can be added to a non-group object");

  const azShortcut* shortcut = Lunar<azShortcut>::check(L, 1); 

  Get()->AddChild(shortcut->Get());

  wxGetApp().FolderItems_CollectionChange();

  return 0;
}


int azShortcut::setName(lua_State *L) { CheckWrappedObject(L, Get()); wxString n = LuaUtil::ToString(L, 1); Get()->SetName(n); return 0; }
int azShortcut::autoSetName(lua_State *L) { CheckWrappedObject(L, Get()); Get()->AutoSetName(); return 0; }
int azShortcut::setPath(lua_State *L) { CheckWrappedObject(L, Get()); wxString n = LuaUtil::ToString(L, 1); Get()->SetFilePath(n); return 0; }
int azShortcut::getPath(lua_State *L) { CheckWrappedObject(L, Get()); LuaUtil::PushString(L, Get()->GetFilePath()); return 1; }
int azShortcut::getResolvedPath(lua_State *L) { CheckWrappedObject(L, Get()); LuaUtil::PushString(L, Get()->GetResolvedPath()); return 1; }
int azShortcut::setParameters(lua_State *L) { CheckWrappedObject(L, Get()); wxString n = LuaUtil::ToString(L, 1); Get()->SetParameters(n); return 0; }
int azShortcut::getParameters(lua_State *L) { CheckWrappedObject(L, Get()); LuaUtil::PushString(L, Get()->GetParameters()); return 1; }
int azShortcut::addToMultiLaunchGroup(lua_State *L) { CheckWrappedObject(L, Get()); Get()->AddToMultiLaunchGroup(); return 0; }
int azShortcut::belongsToMultiLaunchGroup(lua_State *L) { CheckWrappedObject(L, Get()); lua_pushboolean(L, Get()->BelongsToMultiLaunchGroup()); return 1; }
int azShortcut::removeFromMultiLaunchGroup(lua_State *L) { CheckWrappedObject(L, Get()); Get()->RemoveFromMultiLaunchGroup(); return 0; }
int azShortcut::childrenCount(lua_State *L) { CheckWrappedObject(L, Get()); lua_pushinteger(L, Get()->ChildrenCount()); return 1; }


int azShortcut::launch(lua_State *L) {
  CheckWrappedObject(L, Get()); 

  int argc = lua_gettop(L);
  if (argc == 0) {
    Get()->Launch();
  } else {
    Get()->LaunchWithArguments(LuaUtil::ToString(L, 1));
  }
  return 0;
}

int azShortcut::getChildAt(lua_State *L) {
  CheckWrappedObject(L, Get()); 

  if (!Get()->IsGroup()) luaL_error(L, "This shortcut is not a group and therefore does not have any children");
  int index = luaL_checkinteger(L, 1);
  if (index >= Get()->ChildrenCount()) luaL_error(L, "Index out of bounds");

  Lunar<azShortcut>::push(L, new azShortcut(Get()->GetChildAt(index)), true);
  return 1;
}

int azShortcut::getParent(lua_State *L) {
  CheckWrappedObject(L, Get()); 

  FolderItem* p = Get()->GetParent();
  if (!p) return 0; // No error - just return nil

  FolderItem* sp = FolderItem::GetFolderItemById(p->GetId());
  Lunar<azShortcut>::push(L, new azShortcut(sp), true);
  return 1;
}

int azShortcut::removeFromParent(lua_State *L) {
  CheckWrappedObject(L, Get()); 

  FolderItem* p = Get()->GetParent();
  if (!p) return 0; // No error - just exit
  
  p->RemoveChild(Get());
  return 0;
}

int azShortcut::insertChildAt(lua_State *L) { 
  CheckWrappedObject(L, Get()); 

  if (!Get()->IsGroup()) luaL_error(L, "No child can be added to a non-group object");
  const azShortcut* shortcut = Lunar<azShortcut>::check(L, 1);
  int index = luaL_checkinteger(L, 2);  
  Get()->MoveChild(shortcut->Get(), index);
  return 0;
}