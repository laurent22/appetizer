/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#include "Plugin.h"
#include "MiniLaunchBar.h"
#include "FolderItemRenderer.h"
#include "OptionButton.h"
#include "MessageBoxes.h"

#include "lua_glue/azGlobal.h"
#include "lua_glue/azApplication.h"
#include "lua_glue/azIcon.h"
#include "lua_glue/azOptionPanel.h"
#include "lua_glue/azOptionButton.h"
#include "lua_glue/azMenu.h"
#include "lua_glue/azShortcut.h"



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
    luaHost_logError(wxString(errorString, wxConvUTF8), _T("Plugin::Load"));
    OnLuaScopeClose();
    return false;
  }

  Lunar<azApplication>::Register(L);
  Lunar<azIcon>::Register(L);
  Lunar<azMenu>::Register(L);
  Lunar<azShortcut>::Register(L);
  Lunar<azOptionButton>::Register(L);
  Lunar<azOptionPanel>::Register(L);
  Lunar<azDialogs>::Register(L);

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

  error = lua_pcall(L, 0, 0, 0);

  if (error) {
    const char* errorString = lua_tostring(L, -1);
    luaHost_logError(wxString(errorString, wxConvUTF8), _T("Plugin::Load"));
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


void Plugin::DispatchEvent(wxObject* sender, const wxString& eventName, LuaHostTable arguments) {
  int eventId = wxGetApp().GetPluginManager()->GetEventIdByName(eventName);
  DispatchEvent(sender, eventId, arguments);
}


template <class hostObjectT, class lunarObjectT>
bool luaConvertAndPushAsWrapper(lua_State* L, wxObject* o) {
  hostObjectT* asType = dynamic_cast<hostObjectT*>(o);
  if (asType) {
    Lunar<lunarObjectT>::push(L, new lunarObjectT(asType), true);
    return true;
  }
  return false;
}


template <class T>
bool luaPushAsWrapper(lua_State* L, wxObject* o) {
  T* asType = dynamic_cast<T*>(o);
  if (asType) {
    Lunar<T>::push(L, asType, true);
    return true;
  }
  return false;
}


bool DetectTypeAndPushAsWrapper(lua_State* L, wxObject* value) {
  // wxObject* must be any supported wxWidgets object EXCEPT for azWrappers
  // The function checks the type of "value" and try to find an associated wrapper.
  // If it does, the object is converted to an azWrapper and push onto the Lua stack.
  // If it doesn't, the function returns false.

  bool done = luaConvertAndPushAsWrapper<FolderItemRenderer, azIcon>(L, value);
  if (done) return true;

  done = luaConvertAndPushAsWrapper<wxMenu, azMenu>(L, value);
  if (done) return true;

  done = luaConvertAndPushAsWrapper<FolderItem, azShortcut>(L, value);
  if (done) return true;

  done = luaConvertAndPushAsWrapper<OptionButton, azOptionButton>(L, value);
  if (done) return true;

  done = luaConvertAndPushAsWrapper<FolderItem, azShortcut>(L, value);
  if (done) return true;

  if (dynamic_cast<MiniLaunchBar*>(value)) {
    Lunar<azApplication>::push(L, wxGetApp().GetPluginManager()->luaApplication, true);
    return true;
  }

  if (dynamic_cast<OptionPanel*>(value)) {
    Lunar<azOptionPanel>::push(L, wxGetApp().GetPluginManager()->luaOptionPanel, true);
    return true;
  }

  return false;
}


void Plugin::DispatchEvent(wxObject* sender, int eventId, LuaHostTable arguments) {
  std::pair<wxObject*, int> pair(sender, eventId);

  wxArrayString* functionNames = eventRegister_[pair];
  if (!functionNames) return;

  for (int i = 0; i < functionNames->Count(); i++) {
    wxString n = (*functionNames)[i];

    lua_getfield(L, LUA_GLOBALSINDEX, n.mb_str());
    lua_createtable(L, arguments.size(), 0);
    int tableIndex = lua_gettop(L);
    
    LuaHostTable::iterator it = arguments.begin();
    for(; it != arguments.end(); ++it) {
      wxString k = it->first;
      LuaHostTableItem* hostTableItem = it->second;

      wxObject* value = hostTableItem->value;
      LuaHostTableItemType valueType = hostTableItem->valueType;

      lua_pushstring(L, k.mb_str());

      bool done = true;

      if (valueType == LHT_boolean) {

        lua_pushboolean(L, *((wxString*)value) != _T("0"));

      } else if (valueType == LHT_integer) {

        wxString* s = (wxString*)value;
        long l; if (!s->ToLong(&l)) l = 0;
        lua_pushinteger(L, (int)l);

      } else if (valueType == LHT_string) {

        wxString* s = (wxString*)value;
        lua_pushstring(L, s->ToUTF8());

      } else if (valueType == LHT_wxObject) {

        done = DetectTypeAndPushAsWrapper(L, value);

      } else {

        done = false;

      }

      if (!done) wxLogDebug(_T("[ERROR] Cannot detect type of ") + k);

      lua_settable(L, tableIndex);      
    }

    lua_pushstring(L, "sender");

    bool done = DetectTypeAndPushAsWrapper(L, sender);

    if (!done) wxLogDebug(_T("[ERROR] Cannot detect type of sender"));

    lua_settable(L, tableIndex);   

    int errorCode = lua_pcall(L, 1, 0, 0);

    if (errorCode) {
      const char* errorString = lua_tostring(L, -1);
      luaHost_logError(wxString(errorString, wxConvUTF8), _T("Plugin::DispatchEvent"));
    }
    
  }

  OnLuaScopeClose();
}


bool Plugin::HandleMenuItemClick(ExtendedMenuItem* menuItem) {
  wxString onClickHandler = menuItem->GetMetadata(_T("plugin_onClick"));
  if (onClickHandler == wxEmptyString) return false;

  lua_getfield(L, LUA_GLOBALSINDEX, onClickHandler.mb_str());

  lua_createtable(L, 1, 0);
  int tableIndex = lua_gettop(L);

  lua_pushstring(L, "menuItemId");
  lua_pushstring(L, menuItem->GetMetadata(_T("plugin_menuItemId")).mb_str());
  lua_settable(L, tableIndex);

  lua_pushstring(L, "menuItemTag");
  lua_pushstring(L, menuItem->GetMetadata(_T("plugin_menuItemTag")).mb_str());
  lua_settable(L, tableIndex);  
  
  int errorCode = lua_pcall(L, 1, 0, 0);

  if (errorCode) {
    const char* errorString = lua_tostring(L, -1);
    luaHost_logError(wxString(errorString, wxConvUTF8), _T("Plugin::DispatchEvent"));
  }

  return true;
}


lua_State* Plugin::GetLuaState() {
  return L;
}