/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#include "PluginManager.h"
#include "FilePaths.h"
#include "FolderItem.h"
#include "OptionButton.h"
#include "MessageBoxes.h"
#include "FolderItemRenderer.h"
#include "lua_glue/LuaUtil.h"
#include "lua_glue/azOptionButton.h"
#include "lua_glue/azIcon.h"
#include "lua_glue/azMenu.h"
#include "lua_glue/azDockItem.h"


PluginManager::PluginManager() {
  luaApplication = NULL;
  luaOptionPanel = NULL;
  luaDialogs = NULL;
  luaSystem = NULL;

  initialized_ = false;
}


PluginManager::~PluginManager() {
  for (int i = 0; i < plugins_.size(); i++) wxDELETE(plugins_[i]);

  wxDELETE(luaApplication);
  wxDELETE(luaOptionPanel);
  wxDELETE(luaDialogs);
  wxDELETE(luaSystem);
}


bool PluginManager::InstallPluginPackage(const wxString& filePath) {
  if (!initialized_) return false;

  wxFileSystem fs;
  std::auto_ptr<wxZipEntry> entry(new wxZipEntry());

  wxFileInputStream in(filePath);
  if (!in.IsOk()) {
    MessageBoxes::ShowError(wxString::Format(_("Could not open %s"), filePath));
    return false;
  }

  wxString targetDir = FilePaths::GetPluginsDirectory();

  wxZipInputStream zip(in);
  
  while (entry.reset(zip.GetNextEntry()), entry.get() != NULL) {
    wxString name = entry->GetName();
    name = targetDir + wxFileName::GetPathSeparator() + name;

    if (entry->IsDir()) {
      int perm = entry->GetMode();
      wxFileName::Mkdir(name, perm, wxPATH_MKDIR_FULL);
    } else {
      zip.OpenEntry(*entry.get());

      if (!zip.CanRead()) {
        MessageBoxes::ShowError(wxString::Format(_("Can not read zip entry '%s'"), name));
        return false;
      }

      wxFileOutputStream file(name);

      if (!file) {
        MessageBoxes::ShowError(wxString::Format(_("Can not create file '%s'"), name));
        return false;
      }

      zip.Read(file);
    }

  }

  Plugin* p  = new Plugin();
  plugins_.push_back(p);
  p->LoadMetadata(targetDir);
  p->Enable(true);
  p->SetInitiallyEnabled(false);

  MessageBoxes::ShowInformation(wxString::Format(_("The plugin has been installed successfully. It will be activated the next time %s is started"), APPLICATION_NAME));

  return true;
}


void PluginManager::Initialize() {
  if (initialized_) return;

  eventNames_.Add(_T("iconMenuOpening"));
  eventNames_.Add(_T("click"));


  luaApplication = new azApplication();
  luaOptionPanel = new azOptionPanel();
  luaDialogs = new azDialogs();
  luaSystem = new azSystem();


  TiXmlDocument doc(FilePaths::GetPluginSettingsFile().mb_str());
  doc.LoadFile(TIXML_ENCODING_UTF8);
  TiXmlElement* pluginSettingsXml = doc.FirstChildElement("Plugins");
  if (!pluginSettingsXml) WLOG(_T("PluginManager::Initialize: Could not load XML. No Plugins element found."));


  wxString pluginPath = FilePaths::GetPluginsDirectory();
  wxDir pluginFolder;

  if (wxFileName::DirExists(pluginPath) && pluginFolder.Open(pluginPath)) {
    wxString folderName;
    bool success = pluginFolder.GetFirst(&folderName, wxALL_FILES_PATTERN, wxDIR_DIRS);
    
    while (success) {
      Plugin* p  = new Plugin();
      plugins_.push_back(p);

      wxString folderPath = pluginPath + wxFileName::GetPathSeparator() + folderName;

      p->LoadMetadata(folderPath);




      bool pluginIsEnabled = true;

      if (pluginSettingsXml) {        
        for (TiXmlElement* element = pluginSettingsXml->FirstChildElement(); element; element = element->NextSiblingElement()) {
          wxString elementName = wxString::FromUTF8(element->Value());
          TiXmlHandle handle(element);

          if (elementName == _T("Plugin")) {
            wxString uuid = XmlUtil::ReadElementText(handle, "UUID");
            if (uuid == p->GetUUID()) {
              pluginIsEnabled = XmlUtil::ReadElementTextAsBool(handle, "Enabled", true);
              break;          
            }
          } else {
            WLOG(wxString::Format(_T("PluginManager::Initialize: Unknown element: %s"), elementName));
          }
        } // for
      } // if




      p->Enable(pluginIsEnabled);
      p->SetInitiallyEnabled(pluginIsEnabled);

      if (pluginIsEnabled) {
        ILOG(_T("Loading plugin: ") + folderName);
        p->Load(folderPath);
      } else {
        ILOG(_T("Skipping disabled plugin: ") + folderName);
      }

      success = pluginFolder.GetNext(&folderName);
    }
  }
}


void PluginManager::Save() {
  if (!initialized_) return;

  TiXmlDocument doc;
  doc.LinkEndChild(new TiXmlDeclaration("1.0", "UTF-8", ""));

  TiXmlElement* xmlRoot = new TiXmlElement("Plugins");
  xmlRoot->SetAttribute("version", "1.0");
  doc.LinkEndChild(xmlRoot);

  for (int i = 0; i < plugins_.size(); i++) {
    Plugin* plugin = plugins_.at(i);

    TiXmlElement* xmlPlugin = new TiXmlElement("Plugin");
    xmlRoot->LinkEndChild(xmlPlugin);

    XmlUtil::AppendTextElement(xmlPlugin, "UUID", plugin->GetUUID());
    XmlUtil::AppendTextElement(xmlPlugin, "Enabled", plugin->IsEnabled());
  }

  FilePaths::CreateSettingsDirectory();
  bool saved = doc.SaveFile(FilePaths::GetPluginSettingsFile().mb_str());
  if (!saved) ELOG(_T("Could not save plugin.xml file"));
}


PluginVector PluginManager::GetPlugins() {
  return plugins_;
}


int PluginManager::GetEventIdByName(const wxString& eventName) {
  return eventNames_.Index(eventName);
}


Plugin* PluginManager::GetPluginByLuaState(lua_State* L) {
  for (int i = 0; i < plugins_.size(); i++) {
    if (plugins_.at(i)->GetLuaState() == L) return plugins_.at(i);
  }

  return NULL;
}


void PluginManager::DispatchEvent(wxObject* sender, int eventId, LuaHostTable& arguments) {
  if (!initialized_) return;

  for (int i = 0; i < plugins_.size(); i++) {
    Plugin* plugin = plugins_.at(i);
    if (!plugin->WasInitiallyEnabled()) continue;

    plugins_.at(i)->DispatchEvent(sender, eventId, arguments);
  }
}


void PluginManager::DispatchEvent(wxObject* sender, const wxString& eventName, LuaHostTable& arguments) {
  if (!initialized_) return;

  int eventId = GetEventIdByName(eventName);
  DispatchEvent(sender, eventId, arguments);
}


bool PluginManager::HandleMenuItemClick(ExtendedMenuItem* menuItem) {
  if (!initialized_) return false;

  lua_State* luaState = (lua_State*)menuItem->GetMetadataPointer(_T("plugin_luaState"));
  if (!luaState) return false;

  Plugin* p = GetPluginByLuaState(luaState);
  if (!p) return false;

  return p->HandleMenuItemClick(menuItem);
}