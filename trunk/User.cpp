/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "User.h"
#include <wx/dir.h>
#include <wx/filename.h>
#include "MiniLaunchBar.h"
#include "Constants.h"
#include "Log.h"
#include "FolderItem.h"
#include "utilities/StringUtil.h"
#include "FilePaths.h"
#include "third_party/tinyxml/tinyxml.h"


BEGIN_EVENT_TABLE(User, wxEvtHandler)
  EVT_TIMER(ID_TIMER_User_ScheduleSave, User::OnTimer)
END_EVENT_TABLE()


User::User() {
  scheduledSaveTimer_ = NULL;
  shortcutEditorDialog_ = NULL;
  groupEditorDialog_ = NULL;
  rootFolderItem_.reset(new FolderItem(true));
  settings_.reset(new UserSettings());  
}


User::~User() {
  if (groupEditorDialog_) groupEditorDialog_->Destroy();
  if (shortcutEditorDialog_) shortcutEditorDialog_->Destroy();
}


FolderItemSP User::GetRootFolderItem() {
  return rootFolderItem_;
}


void User::OnTimer(wxTimerEvent& evt) {
  Save(true);
}


void User::ScheduleSave() {
  if (!scheduledSaveTimer_) {
    scheduledSaveTimer_ = new wxTimer(this, ID_TIMER_User_ScheduleSave);
  }

  scheduledSaveTimer_->Start(2000, true);
}


void User::Save(bool force) {
  if (!force) {
    // If no save operation is scheduled, exit now
    if (!scheduledSaveTimer_) return;
    if (!scheduledSaveTimer_->IsRunning()) return;
  }

  settings_->Save();

  TiXmlDocument doc;
  doc.LinkEndChild(new TiXmlDeclaration("1.0", "", ""));

  TiXmlElement* xmlRoot = new TiXmlElement("FolderItems");
  xmlRoot->SetAttribute("version", "1.0");
  doc.LinkEndChild(xmlRoot);

  for (int i = 0; i < autoAddExclusions_.size(); i++) {
    XmlUtil::AppendTextElement(xmlRoot, "ExcludedPath", autoAddExclusions_[i]);
  }

  FolderItemVector folderItems = rootFolderItem_->GetChildren();
  for (int i = 0; i < folderItems.size(); i++) {
    FolderItemSP folderItem = folderItems.at(i);
    xmlRoot->LinkEndChild(folderItem->ToXml());   
  }

  FilePaths::CreateSettingsDirectory();
  doc.SaveFile(FilePaths::GetFolderItemsFile().mb_str());
}


void User::Load() {
  settings_->Load();

  autoAddExclusions_.Clear();

  TiXmlDocument doc(FilePaths::GetFolderItemsFile().mb_str());
  doc.LoadFile();

  TiXmlElement* root = doc.FirstChildElement("FolderItems");
  if (!root) {
    wlog("User::Load: Could not load XML. No FolderItems element found.");
    return;
  }

  rootFolderItem_.reset(new FolderItem(true));
  
  for (TiXmlElement* element = root->FirstChildElement(); element; element = element->NextSiblingElement()) {
    wxString elementName = wxString(element->Value(), wxConvUTF8);

    if (elementName == _T("FolderItem")) {
      FolderItemSP folderItem(new FolderItem());
      folderItem->FromXml(element);

      rootFolderItem_->AddChild(folderItem);
    } else if (elementName == _T("ExcludedPath")) {
      wxString path = wxString(element->GetText(), wxConvUTF8);
      path.Trim(true).Trim(false);
      if (path == wxEmptyString) continue;
      AddAutoAddExclusion(path);
    } else {
      wlog(wxString::Format(_T("User::Load: Unknown element: %s"), elementName));
    }
  }
}


FolderItemSP User::AddNewFolderItemFromPath(FolderItemSP parent, wxString folderItemPath) {
  FolderItemSP folderItem(new FolderItem());
  folderItem->SetFilePath(FolderItem::ConvertToRelativePath(folderItemPath));
  folderItem->AutoSetName();

  parent->AddChild(folderItem);
  wxGetApp().FolderItems_CollectionChange();

  return folderItem;
}


FolderItemSP User::EditNewFolderItem(FolderItemSP parent, bool isGroup) {
  FolderItemSP folderItem(new FolderItem(isGroup));

  int result = EditFolderItem(folderItem);

  if (result == wxID_OK) {
    parent->AddChild(folderItem);
    wxGetApp().FolderItems_CollectionChange();
    return folderItem;
  }

  FolderItemSP nullFolderItem;
  return nullFolderItem;
}


int User::EditFolderItem(FolderItemSP folderItem) {
  int result;

  if (folderItem->IsGroup()) {
    if (!groupEditorDialog_) groupEditorDialog_ = new GroupEditorDialog();
    groupEditorDialog_->LoadFolderItem(folderItem);
    result = groupEditorDialog_->ShowModal();
  } else {
    if (!shortcutEditorDialog_) shortcutEditorDialog_ = new ShortcutEditorDialog();
    shortcutEditorDialog_->LoadFolderItem(folderItem);
    result = shortcutEditorDialog_->ShowModal();
  }

  if (result == wxID_OK) wxGetApp().FolderItems_FolderItemChange(folderItem);

  return result;   
}


void User::AddAutoAddExclusion(const wxString& filePath) {
  if (IsAutoAddExclusion(filePath)) return;
  autoAddExclusions_.Add(filePath);
}


bool User::IsAutoAddExclusion(const wxString& filePath) {
  for (int i = 0; i < autoAddExclusions_.size(); i++) {
    if (autoAddExclusions_[i] == filePath) return true;
  }
  return false;
}


UserSettingsSP User::GetSettings() {
  return settings_;
}


void User::AutomaticallyAddNewApps() {
  wxString portableAppsFolderPath = FolderItem::ResolvePath(GetSettings()->PortableAppsPath);

  wxArrayString foundFilePaths;
  wxDir portableAppsFolder;

  bool folderItemsChanged = false;

  //***************************************************************************
  // Look for all the executable files two levels down the PortableApps folder
  // i.e. it will find PortableApps/7-Zip/7-ZipPortable.exe
  //      but not PortableApps/7-Zip/App/7-Zip/7zG.exe
  //***************************************************************************
  if (wxFileName::DirExists(portableAppsFolderPath) && portableAppsFolder.Open(portableAppsFolderPath)) {
    wxString folderName;
    bool success = portableAppsFolder.GetFirst(&folderName, wxALL_FILES_PATTERN, wxDIR_DIRS);
    
    while (success) {
      wxArrayString executables;
      wxDir::GetAllFiles(portableAppsFolder.GetName() + _T("/") + folderName, &executables, _T("*.exe"), wxDIR_FILES);

      for (int i = 0; i < executables.GetCount(); i++) foundFilePaths.Add(executables[i]);

      success = portableAppsFolder.GetNext(&folderName);
    }
  } 

  //***************************************************************************
  // Add special folders to the list of files to process
  //***************************************************************************
  wxString documentsPath = FolderItem::ResolvePath(GetSettings()->DocumentsPath);
  wxString musicPath = FolderItem::ResolvePath(GetSettings()->MusicPath);
  wxString picturesPath = FolderItem::ResolvePath(GetSettings()->PicturesPath);
  wxString videosPath = FolderItem::ResolvePath(GetSettings()->VideosPath);

  if (wxFileName::DirExists(documentsPath)) foundFilePaths.Add(documentsPath);
  if (wxFileName::DirExists(musicPath)) foundFilePaths.Add(musicPath);
  if (wxFileName::DirExists(picturesPath)) foundFilePaths.Add(picturesPath);
  if (wxFileName::DirExists(videosPath)) foundFilePaths.Add(videosPath);

  //***************************************************************************
  // Loop through the files we've just found and create folder items
  // when needed.
  //***************************************************************************
  for (int i = 0; i < foundFilePaths.GetCount(); i++) {
    wxString filePath = foundFilePaths[i];
    wxString resolvedPath = FolderItem::ResolvePath(filePath);

    // This path has previously been deleted by the user
    // so don't automatically add it again.
    if (IsAutoAddExclusion(filePath)) continue;

    // Check if there is already a folder item
    // for this file path. If so: skip it.
    FolderItemSP foundFolderItem = rootFolderItem_->GetChildByResolvedPath(resolvedPath);
    if (foundFolderItem.get()) continue;

    //if (alreadyExists) continue;

    FolderItemSP folderItem(new FolderItem());
    folderItem->SetFilePath(FolderItem::ConvertToRelativePath(filePath));
    folderItem->AutoSetName();
    folderItem->SetAutomaticallyAdded(true);
    
    rootFolderItem_->AddChild(folderItem);

    folderItemsChanged = true;
  }

  // Notify the controller that we've updated the folder items
  if (folderItemsChanged) wxGetApp().FolderItems_CollectionChange();
}


