/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#include "User.h"
#include "MiniLaunchBar.h"
#include "Constants.h"
#include "FolderItem.h"
#include "utilities/StringUtil.h"
#include "FilePaths.h"


BEGIN_EVENT_TABLE(User, wxEvtHandler)
  EVT_TIMER(ID_TIMER_User_ScheduleSave, User::OnTimer)
END_EVENT_TABLE()


User::User() {
  scheduledSaveTimer_ = NULL;
  settings_ = NULL;
  rootFolderItem_ = appFolderItem::CreateFolderItem(true);
  rootFolderItem_->SetName(_T("<root>"));
  settings_ = new UserSettings();
}


wxArrayString User::GetAutoAddExclusions() {
  return autoAddExclusions_;
}


void User::SetAutoAddExclusions(wxArrayString& arrayString) {
  autoAddExclusions_ = arrayString;
}


User::~User() {
  wxDELETE(scheduledSaveTimer_);
  wxDELETE(settings_);
}


appFolderItem* User::GetRootFolderItem() {
  return rootFolderItem_;
}


void User::OnTimer(wxTimerEvent& evt) {
  ILOG(_T("Doing scheduled save"));
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
  wxGetApp().GetPluginManager()->Save();

  TiXmlDocument doc;
  doc.LinkEndChild(new TiXmlDeclaration("1.0", "UTF-8", ""));

  TiXmlElement* xmlRoot = new TiXmlElement("FolderItems");
  xmlRoot->SetAttribute("version", "1.0");
  doc.LinkEndChild(xmlRoot);

  for (int i = 0; i < autoAddExclusions_.size(); i++) {
    XmlUtil::AppendTextElement(xmlRoot, "ExcludedPath", autoAddExclusions_[i]);
  }

  FolderItemVector folderItems = rootFolderItem_->GetChildren();
  for (int i = 0; i < folderItems.size(); i++) {
    appFolderItem* folderItem = folderItems.at(i);
    xmlRoot->LinkEndChild(folderItem->ToXml());   
  }

  FilePaths::CreateSettingsDirectory();
  bool saved = doc.SaveFile(FilePaths::GetFolderItemsFile().mb_str());
  if (!saved) ELOG(_T("Could not save file"));
}


void User::Load() {
  settings_->Load();

  autoAddExclusions_.Clear();

  TiXmlDocument doc(FilePaths::GetFolderItemsFile().mb_str());
  doc.LoadFile(TIXML_ENCODING_UTF8);

  TiXmlElement* root = doc.FirstChildElement("FolderItems");
  if (!root) {
    WLOG(_T("User::Load: Could not load XML. No FolderItems element found."));
    return;
  }

  rootFolderItem_ = appFolderItem::CreateFolderItem(true);
  
  for (TiXmlElement* element = root->FirstChildElement(); element; element = element->NextSiblingElement()) {
    wxString elementName = wxString(element->Value(), wxConvUTF8);

    if (elementName == _T("FolderItem") || elementName == _T("appFolderItem")) {
      appFolderItem* folderItem = appFolderItem::CreateFolderItem();
      folderItem->FromXml(element);

      rootFolderItem_->AddChild(folderItem);
    } else if (elementName == _T("ExcludedPath")) {
      const char* cString = element->GetText();
      if (!cString) continue;
      wxString path = wxString::FromUTF8(cString);
      path.Trim(true).Trim(false);
      if (path == wxEmptyString) continue;
      autoAddExclusions_.Add(appFolderItem::ConvertToRelativePath(path));
    } else {
      WLOG(wxString::Format(_T("User::Load: Unknown element: %s"), elementName));
    }
  }
}


appFolderItem* User::CreateFolderItemFromShortcut(wxString shortcutPath) {
  ShortcutInfo info(shortcutPath, (HWND)(wxGetApp().GetMainFrame()->GetHandle()));
  if (!info.IsOk()) return NULL;

  appFolderItem* folderItem = appFolderItem::CreateFolderItem();
  folderItem->SetFilePath(appFolderItem::ConvertToRelativePath(info.GetPath()));
  folderItem->AutoSetName();
  folderItem->SetCustomIcon(info.GetIconLocation(), info.GetIconIndex());
  folderItem->SetParameters(info.GetArguments());

  return folderItem;
}


appFolderItem* User::AddNewFolderItemFromPath(appFolderItem* parent, wxString folderItemPath) {
  appFolderItem* folderItem = appFolderItem::CreateFolderItem();
  folderItem->SetFilePath(appFolderItem::ConvertToRelativePath(folderItemPath));
  folderItem->AutoSetName();

  parent->AddChild(folderItem);
  wxGetApp().FolderItems_CollectionChange();

  return folderItem;
}


appFolderItem* User::AddNewFolderItemFromShortcut(appFolderItem* parent, wxString shortcutPath) {
  appFolderItem* folderItem = CreateFolderItemFromShortcut(shortcutPath);
  if (!folderItem) return NULL;

  parent->AddChild(folderItem);
  wxGetApp().FolderItems_CollectionChange();

  return folderItem;
}


appFolderItem* User::EditNewFolderItem(appFolderItem* parent, bool isGroup) {
  appFolderItem* folderItem = appFolderItem::CreateFolderItem(isGroup);

  int result = EditFolderItem(folderItem);

  if (result == wxID_OK) {
    parent->AddChild(folderItem);
    wxGetApp().FolderItems_CollectionChange();
    return folderItem;
  }

  folderItem->Dispose();

  return NULL;
}


int User::EditFolderItem(appFolderItem* folderItem) {
  int result;

  ShortcutEditorDialog* dialog = new ShortcutEditorDialog(wxGetApp().GetMainFrame());
  dialog->LoadFolderItem(folderItem);
  result = dialog->ShowModal();
  dialog->Destroy();

  if (result == wxID_OK) wxGetApp().FolderItems_FolderItemChange(folderItem);

  return result;   
}


void User::AddAutoAddExclusion(const wxString& filePath) {
  if (IsAutoAddExclusion(filePath)) return;
  autoAddExclusions_.Add(filePath);
}


bool User::IsAutoAddExclusion(const wxString& filePath) {
  wxString resolvedPath = appFolderItem::ResolvePath(filePath);

  for (int i = 0; i < autoAddExclusions_.size(); i++) {
    wxString exResolvedPath = appFolderItem::ResolvePath(autoAddExclusions_[i], false);    
    if (exResolvedPath == resolvedPath) return true;
    if (StringUtil::FileMatchesPattern(exResolvedPath, resolvedPath)) return true;
  }

  return false;
}


UserSettings* User::GetSettings() {
  return settings_;
}


void User::PortableAppsFormatSynchronization() {
  wxString portableAppsFolderPath = appFolderItem::ResolvePath(GetSettings()->GetString(_T("PortableAppsPath")));

  wxArrayString foundFilePaths;
  wxDir portableAppsFolder;  

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
  wxString documentsPath = appFolderItem::ResolvePath(GetSettings()->GetString(_T("DocumentsPath")));
  wxString musicPath = appFolderItem::ResolvePath(GetSettings()->GetString(_T("MusicPath")));
  wxString picturesPath = appFolderItem::ResolvePath(GetSettings()->GetString(_T("PicturesPath")));
  wxString videosPath = appFolderItem::ResolvePath(GetSettings()->GetString(_T("VideosPath")));

  if (wxFileName::DirExists(documentsPath)) foundFilePaths.Add(documentsPath);
  if (wxFileName::DirExists(musicPath)) foundFilePaths.Add(musicPath);
  if (wxFileName::DirExists(picturesPath)) foundFilePaths.Add(picturesPath);
  if (wxFileName::DirExists(videosPath)) foundFilePaths.Add(videosPath);

  BatchAddFolderItems(foundFilePaths, true);
}


void User::GetShortcutsFromFolder(const wxString& folderPath, wxArrayString* result, bool recurse) {
  wxArrayString shortcutPaths;
  int flags = wxDIR_FILES;
  if (recurse) flags |= wxDIR_DIRS;
  wxDir::GetAllFiles(folderPath, &shortcutPaths, _T("*.lnk"), flags);

  for (int i = 0; i < shortcutPaths.GetCount(); i++) {
    wxString shortcutPath = shortcutPaths[i];

    wxFileName shortcutFN(shortcutPath);
    shortcutFN.Normalize();
    if (!shortcutFN.FileExists() && !wxFileName::DirExists(shortcutFN.GetFullPath())) continue;
    if (shortcutFN.GetExt().Lower() != _T("exe")) continue;

    result->Add(shortcutFN.GetFullPath());
  }
}


void User::StartMenuSynchronization() {

  wxArrayString foundFilePaths;
  wxString startMenuPath;

  for (int folderIndex = 0; folderIndex < 2; folderIndex++) {

    if (folderIndex == 0) {
      startMenuPath = FilePaths::GetAllUsersShellDirectory(_T("Common Programs"));
    } else {
      startMenuPath = FilePaths::GetUserShellDirectory(_T("Programs"));
    }
   
    wxDir startMenuFolder;  

    GetShortcutsFromFolder(startMenuPath, &foundFilePaths, true);

    if (wxFileName::DirExists(startMenuPath) && startMenuFolder.Open(startMenuPath)) {
      wxString folderName;
      bool success = startMenuFolder.GetFirst(&folderName, wxALL_FILES_PATTERN, wxDIR_DIRS | wxDIR_HIDDEN);
      
      while (success) {
        GetShortcutsFromFolder(startMenuFolder.GetName() + _T("/") + folderName, &foundFilePaths, true);

        success = startMenuFolder.GetNext(&folderName);
      }
    }
  } 

  BatchAddFolderItems(foundFilePaths, true);
}


void User::QuickLaunchSynchronization() {
  wxArrayString foundFilePaths; 
  GetShortcutsFromFolder(FilePaths::GetQuickLaunchDirectory(), &foundFilePaths);
  BatchAddFolderItems(foundFilePaths, true);
}


void User::CustomFolderSynchronization(const wxString& folderPath) {
  wxArrayString foundFilePaths; 
  wxDir::GetAllFiles(folderPath, &foundFilePaths, _T("*.exe"), wxDIR_FILES | wxDIR);
  wxArrayString temp; 
  wxDir::GetAllFiles(folderPath, &temp, _T("*.lnk"), wxDIR_FILES | wxDIR);

  for (int i = 0; i < temp.GetCount(); i++) {
    foundFilePaths.Add(temp[i]);
  }

  BatchAddFolderItems(foundFilePaths, true);
}


void User::BatchAddFolderItems(const wxArrayString& filePaths, bool useAutoAddExclusions) {
  bool folderItemsChanged = false;

  //***************************************************************************
  // Loop through the files and create folder items when needed.
  //***************************************************************************

  for (int i = 0; i < filePaths.GetCount(); i++) {   
    wxString filePath = filePaths[i];
    wxString extension = wxFileName(filePath).GetExt().Lower();
    wxString resolvedPath = appFolderItem::ResolvePath(filePath);
    wxString relativePath = appFolderItem::ConvertToRelativePath(filePath);

    // This path has previously been deleted by the user
    // so don't automatically add it again.
    if (IsAutoAddExclusion(relativePath)) continue;

    // Check if there is already a folder item
    // for this file path. If so: skip it.
    appFolderItem* foundFolderItem = rootFolderItem_->GetChildByResolvedPath(resolvedPath);
    if (foundFolderItem) continue;

    appFolderItem* folderItem = NULL;

    if (extension == _T("lnk")) folderItem = CreateFolderItemFromShortcut(filePath);

    if (!folderItem) {  // Either the file is not a shortcut (.lnk), or is a shortcut that could not be resolved (in which case CreateFolderItemFromShortcut() returns NULL)
      folderItem = appFolderItem::CreateFolderItem();
      folderItem->SetFilePath(relativePath);
      folderItem->AutoSetName();
    }

    folderItem->SetAutomaticallyAdded(true);
    rootFolderItem_->AddChild(folderItem);

    folderItemsChanged = true;
  }

  // Notify the controller that we've updated the folder items
  if (folderItemsChanged) wxGetApp().FolderItems_CollectionChange();
}


