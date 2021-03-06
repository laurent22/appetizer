/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#include "Plugin.h"
#include "MiniLaunchBar.h"
#include "MessageBoxes.h"

#include "lua_glue/azGlobal.h"
#include "lua_glue/azApplication.h"
#include "lua_glue/azIcon.h"
#include "lua_glue/azOptionPanel.h"
#include "lua_glue/azOptionButton.h"
#include "lua_glue/azMenu.h"
#include "lua_glue/azMenuItem.h"
#include "lua_glue/azDockItem.h"



Plugin::Plugin() {
  enabled_ = true;
  initiallyEnabled_ = enabled_;
  
  L = NULL;
}


Plugin::~Plugin() {
  if (L) lua_close(L);

  std::map<std::pair<wxObject*, int>, wxArrayString*>::iterator it = eventRegister_.begin();

  for(; it != eventRegister_.end(); ++it) {
    wxArrayString* v = it->second;
    wxDELETE(v);
  }
}


void Plugin::SetInitiallyEnabled(bool enabled) {
  initiallyEnabled_ = enabled;
}


bool Plugin::WasInitiallyEnabled() { return initiallyEnabled_; }
wxString Plugin::GetName() { return name_; }
wxString Plugin::GetUUID() { return uuid_; }
bool Plugin::IsEnabled() { return enabled_; }
void Plugin::Disable() { Enable(false); }


void Plugin::Enable(bool enable) {
  if (enable == enabled_) return;

  enabled_ = enable;
}


void Plugin::OnLuaScopeClose() {
  azMenu::OnLuaScopeClose();
}


void Plugin::LoadPluginXml(const wxString& xmlFilePath) {
  ILOG(_T("Loading: ") + xmlFilePath);

  if (!wxFileName::FileExists(xmlFilePath)) {
    ILOG(_T("Plugin::LoadPluginXml: 'plugin.xml' is missing: ") + xmlFilePath + _T(" Using default settings"));
    return;
  }

  TiXmlDocument doc(xmlFilePath.mb_str());
  doc.LoadFile(TIXML_ENCODING_UTF8);

  TiXmlElement* root = doc.FirstChildElement("Plugin");
  if (!root) {
    ELOG(_T("Plugin::LoadPluginXml: Could not load XML. No 'Plugin' element found."));
    return;
  }

  TiXmlHandle handle(root);

  name_ = XmlUtil::ReadElementText(handle, "Name");
  uuid_ = XmlUtil::ReadElementText(handle, "UUID");
}


void Plugin::LoadMetadata(const wxString& folderPath) {
  wxString xmlFilePath = folderPath + wxFileName::GetPathSeparator() + _T("plugin.xml");
  LoadPluginXml(xmlFilePath);

  wxFileName folderPathFN(folderPath);
  if (name_ == wxEmptyString) name_ = folderPathFN.GetName();
  if (uuid_ == wxEmptyString) uuid_ = name_;
}


bool Plugin::Load(const wxString& folderPath) {
  wxString luaFilePath = folderPath + wxFileName::GetPathSeparator() + _T("main.lua");

  if (!wxFileName::FileExists(luaFilePath)) {
    ELOG(_T("Plugin::Load: 'main.lua' is missing for plugin: ") + folderPath);
    return false;
  }

  L = lua_open();

  luaopen_base(L);
  luaopen_table(L);
  luaopen_string(L);
  luaopen_math(L);

  lua_register(L, "trace", azPrint);

  const char* utf8bom = "\xef\xbb\xbf";
  char firstThree[4];
  std::ifstream f(luaFilePath.mb_str());
  f.get(firstThree, 4, NULL);

  int sizeRead = f.gcount();

  if (sizeRead >= 3 && strcmp(firstThree, utf8bom) == 0) {
    MessageBoxes::ShowError(wxString::Format(_T("%s\n\n%s"), _("Plugin script is not in the right format. Please encode it as UTF-8 NO BOM or ASCII. Applies to:"), luaFilePath));
    f.close();
    return false;
  }

  f.close();
  
  int error = luaL_loadfile(L, luaFilePath.mb_str());

  if (error) {
    const char* errorString = lua_tostring(L, -1);
    luaHost_logError(wxString(errorString, wxConvUTF8));
    OnLuaScopeClose();
    return false;
  }

  Lunar<azApplication>::Register(L);
  Lunar<azIcon>::Register(L);
  Lunar<azMenu>::Register(L);
  Lunar<azDockItem>::Register(L);
  Lunar<azOptionButton>::Register(L);
  Lunar<azOptionPanel>::Register(L);
  Lunar<azDialogs>::Register(L);
  Lunar<azSystem>::Register(L);
  Lunar<azMenuItem>::Register(L);

  PluginManager* pluginManager = wxGetApp().GetPluginManager();
  
  lua_pushliteral(L, "appetizer");
  Lunar<azApplication>::push(L, pluginManager->luaApplication);
  lua_settable(L, LUA_GLOBALSINDEX);

  lua_pushliteral(L, "optionPanel");
  Lunar<azOptionPanel>::push(L, pluginManager->luaOptionPanel);
  lua_settable(L, LUA_GLOBALSINDEX);

  lua_pushliteral(L, "dialogs");
  Lunar<azDialogs>::push(L, pluginManager->luaDialogs);
  lua_settable(L, LUA_GLOBALSINDEX);

  lua_pushliteral(L, "system");
  Lunar<azSystem>::push(L, pluginManager->luaSystem);
  lua_settable(L, LUA_GLOBALSINDEX);

  error = lua_pcall(L, 0, 0, 0);

  if (error) {
    const char* errorString = lua_tostring(L, -1);
    luaHost_logError(wxString(errorString, wxConvUTF8));
    OnLuaScopeClose();
    return false;
  }

  OnLuaScopeClose();

  return true;
}


void Plugin::AddEventListener(wxObject* object, int eventId, const wxString& functionName) {
  std::pair<wxObject*, int> pair(object, eventId);

  wxArrayString* functionNames = eventRegister_[pair];

  if (!functionNames) {
    functionNames = new wxArrayString();
    eventRegister_[pair] = functionNames;
  }

  functionNames->Add(functionName);
}


void Plugin::AddEventListener(wxObject* object, const wxString& eventName, const wxString& functionName) {
  int eventId = wxGetApp().GetPluginManager()->GetEventIdByName(eventName);
  AddEventListener(object, eventId, functionName);
}


void Plugin::DispatchEvent(wxObject* sender, const wxString& eventName, LuaHostTable& arguments) {
  int eventId = wxGetApp().GetPluginManager()->GetEventIdByName(eventName);
  DispatchEvent(sender, eventId, arguments);
}


void Plugin::DispatchEvent(wxObject* sender, int eventId, LuaHostTable& arguments) {
  std::pair<wxObject*, int> pair(sender, eventId);

  wxArrayString* functionNames = eventRegister_[pair];
  if (!functionNames) return;

  for (int i = 0; i < functionNames->Count(); i++) {
    wxString n = (*functionNames)[i];

    // ************************************************
    // Push the function to call onto the stack
    // ************************************************

    lua_getfield(L, LUA_GLOBALSINDEX, n.mb_str());

    // ************************************************
    // Push the "event" parameter onto the stack.
    // ************************************************

    int tableIndex = LuaUtil::ConvertAndPushLuaHostTable(L, arguments);

    // *********************************************************
    // Also add the sender as a property of the "event" object 
    // *********************************************************

    lua_pushstring(L, "sender");
    bool done = LuaUtil::DetectTypeAndPushAsWrapper(L, sender);
    if (!done) wxLogDebug(_T("[ERROR] Cannot detect type of sender"));
    lua_settable(L, tableIndex);   

    // *********************************************************
    // Call the function
    // *********************************************************

    int errorCode = lua_pcall(L, 1, 0, 0);

    if (errorCode) {
      const char* errorString = lua_tostring(L, -1);
      luaHost_logError(wxString(errorString, wxConvUTF8));
    }
    
  }

  OnLuaScopeClose();
}


bool Plugin::HandleMenuItemClick(ExtendedMenuItem* menuItem) {
  azMenuItem* luaMenuItem = (azMenuItem*)menuItem->GetMetadataPointer(_T("plugin_luaMenuItem"));
  if (!luaMenuItem) return false;
  
  wxString onSelectedHandler = luaMenuItem->Get()->onSelected;
  if (onSelectedHandler == wxEmptyString) return false;

  // Get the functions to be called
  lua_getfield(L, LUA_GLOBALSINDEX, onSelectedHandler.ToUTF8());

  // Push the arguments
  lua_createtable(L, 1, 0);
  int tableIndex = lua_gettop(L);

  lua_pushstring(L, "menuItem");
  Lunar<azMenuItem>::push(L, luaMenuItem);
  lua_settable(L, tableIndex);  

  int errorCode = lua_pcall(L, 1, 0, 0);

  if (errorCode) {
    const char* errorString = lua_tostring(L, -1);
    luaHost_logError(wxString(errorString, wxConvUTF8));
    // Don't return false here because even if there is an
    // error in the script, it still has processed the event
  }

  return true;
}


lua_State* Plugin::GetLuaState() {
  return L;
}