/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "User.h"
#include <wx/dir.h>
#include <wx/filename.h>
#include "Controller.h"
#include "Constants.h"
#include "FolderItem.h"
#include "utilities/StringUtil.h"
#include "FilePaths.h"
#include "third_party/tinyxml/tinyxml.h"

extern Controller gController;


BEGIN_EVENT_TABLE(User, wxEvtHandler)
  EVT_TIMER(ID_TIMER_User_ScheduleSave, User::OnTimer)
END_EVENT_TABLE()


User::User() {
  scheduledSaveTimer_ = NULL;
  shortcutEditorDialog_ = NULL;
  settings_.reset(new UserSettings());  
}


void User::OnTimer(wxTimerEvent& evt) {
  Save(true);
}


void User::DoMultiLaunch() {
  for (int i = 0; i < folderItems_.size(); i++) {
    FolderItemSP folderItem = folderItems_.at(i);
    if (folderItem->BelongsToMultiLaunchGroup()) folderItem->Launch();
  }
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

  for (int i = 0; i < folderItems_.size(); i++) {
    FolderItemSP folderItem = folderItems_.at(i);
    xmlRoot->LinkEndChild(folderItem->ToXML());   
  }

  FilePaths::CreateSettingsDirectory();
  doc.SaveFile(FilePaths::FolderItemsFile.mb_str());
}


void User::Load() {
  settings_->Load();

  autoAddExclusions_.Clear();

  TiXmlDocument doc(FilePaths::FolderItemsFile.mb_str());
  doc.LoadFile();

  TiXmlElement* root = doc.FirstChildElement("FolderItems");
  if (!root) {
    wxLogDebug(_T("User::Load: Could not load XML. No FolderItems element found."));
    return;
  }
  
  for (TiXmlElement* element = root->FirstChildElement(); element; element = element->NextSiblingElement()) {
    wxString elementName = wxString(element->Value(), wxConvUTF8);

    if (elementName == _T("FolderItem")) {
      FolderItemSP folderItem(new FolderItem());
      folderItem->FromXML(element);
      folderItems_.push_back(folderItem);
    } else if (elementName == _T("ExcludedPath")) {
      wxString path = wxString(element->GetText(), wxConvUTF8);
      path.Trim(true).Trim(false);
      if (path == wxEmptyString) continue;
      AddAutoAddExclusion(path);
    } else {
      wxLogDebug(_T("User::Load: Unknown element: %s"), elementName);
    }
  }
}


std::vector<FolderItemSP> User::GetFolderItems() {
  return folderItems_;
}


FolderItemSP User::GetFolderItemById(int folderItemId) {
  for (int i = 0; i < folderItems_.size(); i++) {
    if (folderItems_.at(i)->GetId() == folderItemId) return folderItems_.at(i);
  }
  FolderItemSP nullPointer;
  return nullPointer;
}


FolderItemSP User::EditNewFolderItem() {
  FolderItemSP folderItem(new FolderItem());
  int result = EditFolderItem(folderItem);

  if (result == wxID_OK) {
    folderItems_.push_back(folderItem);
    gController.User_FolderItemCollectionChange();
    return folderItem;
  }

  FolderItemSP nullFolderItem;
  return nullFolderItem;
}


int User::EditFolderItem(FolderItemSP folderItem) {
  shortcutEditorDialog_ = new ShortcutEditorDialog();
  shortcutEditorDialog_->LoadFolderItem(folderItem);
  int result = shortcutEditorDialog_->ShowModal();
  shortcutEditorDialog_->Destroy();
  shortcutEditorDialog_ = NULL;

  if (result == wxID_OK) gController.User_FolderItemChange(folderItem);

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


void User::DeleteFolderItem(int folderItemId) {
  bool wasDeleted = false;

  for (int i = 0; i < folderItems_.size(); i++) {
    FolderItemSP folderItem = folderItems_.at(i);
    if (folderItem->GetId() == folderItemId) {
      if (folderItem->GetAutomaticallyAdded()) {
        AddAutoAddExclusion(folderItem->GetResolvedPath());
      }
      folderItems_.erase(folderItems_.begin() + i);
      wasDeleted = true;
      break;
    }
  }

  if (wasDeleted) gController.User_FolderItemCollectionChange();
}


void User::MoveFolderItem(int folderItemId, int insertionIndex) {

  // Get the folder item that we need to move
  FolderItemSP folderItemToMove = GetFolderItemById(folderItemId);
  if (!folderItemToMove.get()) {
    wxLogDebug(_T("Could not find folder item #%d"), folderItemId);
    return;
  }

  // Create the new vector of folder items that
  // is going to replace the old one
  std::vector<FolderItemSP> newFolderItems;

  bool isPushed = false;

  if (insertionIndex < 0) {
    newFolderItems.push_back(folderItemToMove);
    isPushed = true;
  }

  for (int i = 0; i < folderItems_.size(); i++) {
    FolderItemSP folderItem = folderItems_.at(i);

    // If the current folder item is the one
    // we want to move, skip it
    if (folderItem->GetId() == folderItemToMove->GetId()) continue;

    if (i == insertionIndex && !isPushed) {
      // If we are at the insertion index, insert the
      // the folder item
      newFolderItems.push_back(folderItemToMove);
      isPushed = true;
    }

    // Keep copying the other folder items
    newFolderItems.push_back(folderItem);
  }

  // If we didn't insert the folder item, do it now
  if (!isPushed) newFolderItems.push_back(folderItemToMove);

  // Swap the vectors
  folderItems_ = newFolderItems;

  // Notify everybody that we've changed the item collection
  gController.User_FolderItemCollectionChange();
}


void User::DumpFolderItems() {
  for (int i = 0; i < folderItems_.size(); i++) {
    FolderItemSP folderItem = folderItems_.at(i);
    wxLogDebug(_T("%d - %s"), folderItem->GetId(), folderItem->GetFilePath());
  }
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
    bool alreadyExists = false;
    for (int j = 0; j < folderItems_.size(); j++) {
      if (folderItems_.at(j)->GetResolvedPath() == resolvedPath) {
        alreadyExists = true;
        break;
      }
    }

    if (alreadyExists) continue;

    FolderItemSP folderItem(new FolderItem());
    folderItem->SetFilePath(FolderItem::ConvertToRelativePath(filePath));
    folderItem->AutoSetName();
    folderItem->SetAutomaticallyAdded(true);
    folderItems_.push_back(folderItem);

    folderItemsChanged = true;
  }

  // Notify the controller that we've updated the folder items
  if (folderItemsChanged) gController.User_FolderItemCollectionChange();
}


