/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/


#include "stdafx.h"

#include "LuaWrapper.h"
#include "utilities/LuaUtil.h"
#include "Plugin.h"
#include "ExtendedMenuItem.h"
#include "FolderItemRenderer.h"
#include "Log.h"
#include "MiniLaunchBar.h"


wxObject* azAnyShortcut = new wxObject();


void luaWrapper_destroy() {
  wxDELETE(azAnyShortcut);
}


int azPrint(lua_State *L) {
  int argc = lua_gettop(L);

  wxString output;

  for (int n = 1; n <= argc; ++n) {
    const char* c = lua_tostring(L, n);
    if (!c) {
      output += _T("<null>");
      continue;
    }

    int cLen = strlen(c);
    wxString s;
    for (int j = 0; j < cLen; j++) {
      s += wxChar(c[j]);
    }
    output += s;
  }

  wxLogDebug(_T("%s %s"), _T("[Lua]"), output);

  return 0;
}


int azAddEventListener(lua_State *L) {
  void* object = (void*)lua_touserdata(L, 1);
  int eventId = lua_tointeger(L, 2);
  wxString functionName = LuaUtil::ToString(L, 3);
 
  PluginSP p = wxGetApp().GetPluginManager().GetPluginByLuaState(L);
  
  if (!p.get()) {
    elog("Could not find matching plugin for Lua state");
    return 0;
  }

  p->AddEventListener(object, eventId, functionName);

  return 0;
}


int azGetShortcutsRoot(lua_State *L) {
  FolderItemSP rootFolderItem = wxGetApp().GetUser()->GetRootFolderItem();
  lua_pushlightuserdata(L, rootFolderItem.get());

  return 1;
}


int azGetShortcutById(lua_State *L) {
  int folderItemId = lua_tointeger(L, 1);
  FolderItemSP folderItem = FolderItem::GetFolderItemById(folderItemId);
  lua_pushlightuserdata(L, folderItem.get());
  
  return 1;
}


int azShortcut_GetAllGroups(lua_State *L) {
  FolderItem* folderItem = (FolderItem*)lua_touserdata(L, 1);
  FolderItemVector allGroups = folderItem->GetAllGroups();

  lua_createtable(L, allGroups.size(), 0);
  int tableIndex = lua_gettop(L);

  for (int i = 0; i < allGroups.size(); i++) {
    FolderItemSP f = allGroups.at(i);

    lua_pushinteger(L, i + 1);
    lua_pushlightuserdata(L, f.get());
    lua_settable(L, tableIndex);
  }

  return 1;
}


int azIcon_GetPopupMenu(lua_State *L) {
  FolderItemRenderer* r = (FolderItemRenderer*)lua_touserdata(L, 1);
  if (!r) return 0;

  lua_pushlightuserdata(L, r->GetPopupMenu());

  return 1;
}


int azIcon_GetShortcut(lua_State *L) {
  FolderItemRenderer* r = (FolderItemRenderer*)lua_touserdata(L, 1);
  if (!r) return 0;

  FolderItemSP folderItem = r->GetFolderItem();
  if (!folderItem.get()) return 0;

  lua_pushlightuserdata(L, folderItem.get());

  return 1;
}


int azShortcut_AddChild(lua_State *L) {
  FolderItem* group = (FolderItem*)lua_touserdata(L, 1);
  if (!group) return 0;
  if (!group->IsGroup()) return 0;

  FolderItem* folderItem = (FolderItem*)lua_touserdata(L, 2);
  if (!folderItem) return 0;

  group->AddChild(FolderItem::GetFolderItemById(folderItem->GetId()));

  wxGetApp().FolderItems_CollectionChange();

  return 0;
}


int azShortcut_GetName(lua_State *L) {
  FolderItem* folderItem = (FolderItem*)lua_touserdata(L, 1);
  lua_pushstring(L, folderItem->GetName(true).mb_str());
  
  return 1;
}


int azShortcut_GetId(lua_State *L) {
  FolderItem* folderItem = (FolderItem*)lua_touserdata(L, 1);
  lua_pushinteger(L, folderItem->GetId());
  
  return 1;
}


int azNewMenu(lua_State *L) {
  wxString menuText = LuaUtil::ToString(L, 1);
  wxMenu* menu = new wxMenu(menuText);
  lua_pushlightuserdata(L, menu);

  return 1;
}


int azMenu_Append(lua_State *L) {
  wxMenu* menu = (wxMenu*)lua_touserdata(L, 1);

  wxString menuItemText = LuaUtil::GetStringFromTable(L, 2, _T("text"));
  wxString menuItemOnClick = LuaUtil::GetStringFromTable(L, 2, _T("onClick"));
  wxString menuItemId = LuaUtil::GetStringFromTable(L, 2, _T("id"));
  wxString menuItemTag = LuaUtil::GetStringFromTable(L, 2, _T("tag"));

  menuItemText.Trim(true).Trim(false);

  if (menuItemText == wxEmptyString) return 0;

  ExtendedMenuItem* menuItem = new ExtendedMenuItem(menu, wxGetApp().GetUniqueInt(), menuItemText);
  menuItem->SetMetadata(_T("plugin_menuItemId"), menuItemId);
  menuItem->SetMetadata(_T("plugin_onClick"), menuItemOnClick);
  menuItem->SetMetadataPointer(_T("plugin_luaState"), (void*)L);
  menuItem->SetMetadata(_T("plugin_menuItemTag"), menuItemTag);

  menu->Append(menuItem);

  return 0;
}


int azMenu_AppendSubMenu(lua_State *L) {
  wxMenu* menu = (wxMenu*)lua_touserdata(L, 1);
  if (!menu) return 0;

  wxMenu* subMenu = (wxMenu*)lua_touserdata(L, 2);
  if (!subMenu) return 0;

  menu->AppendSubMenu(subMenu, subMenu->GetTitle());

  return 0;
}


int azMenu_AppendSeparator(lua_State *L) {
  wxMenu* menu = (wxMenu*)lua_touserdata(L, 1);
  if (!menu) return 0;

  menu->AppendSeparator();

  return 0;
}