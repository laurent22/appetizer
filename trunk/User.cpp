#include "User.h"
#include <wx/dir.h>
#include <wx/filename.h>
#include "Controller.h"
#include "FolderItem.h"
#include "utilities/StringUtil.h"

extern ControllerSP gController;


User::User() {
  shortcutEditorDialog_ = NULL;
  settings_.reset(new UserSettings());  
}


void User::ScheduleSave() {
  // @todo: Implement scheduled save
  Save();
}


void User::Save() {
  settings_->Save();

  wxFileConfig config(_T(""), _T(""), gController->GetFilePaths().FolderItemsFile, _T(""), wxCONFIG_USE_RELATIVE_PATH);

  config.DeleteAll();

  for (int i = 0; i < folderItems_.size(); i++) {
    FolderItemSP folderItem = folderItems_.at(i);
    config.SetPath(_T("/FolderItem") + StringUtil::ZeroPadding(i, 4));
    config.Write(_T("FilePath"), folderItem->GetFilePath());
  }

  config.Flush();
}


void User::Load() {
  wxFileConfig config(_T(""), _T(""), gController->GetFilePaths().FolderItemsFile, _T(""), wxCONFIG_USE_RELATIVE_PATH);

  wxString folderItemGroup;
  long index;
  bool ok = config.GetFirstGroup(folderItemGroup, index);

  while (ok) {
    wxString filePath;
    bool success = config.Read(_T("/") + folderItemGroup + _T("/FilePath"), &filePath);
    if (!success) continue;

    FolderItemSP folderItem(new FolderItem());
    folderItem->SetFilePath(filePath);
    folderItems_.push_back(folderItem);

    ok = config.GetNextGroup(folderItemGroup, index);
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


void User::EditFolderItem(int folderItemId) {
  shortcutEditorDialog_ = new ShortcutEditorDialog();
  shortcutEditorDialog_->ShowModal();
  shortcutEditorDialog_->Destroy();
  shortcutEditorDialog_ = NULL;
}


void User::DeleteFolderItem(int folderItemId) {
  for (int i = 0; i < folderItems_.size(); i++) {
    FolderItemSP folderItem = folderItems_.at(i);
    if (folderItem->GetId() == folderItemId) {
      folderItems_.erase(folderItems_.begin() + i);
      ScheduleSave();
      break;
    }
  }

  gController->User_FolderItemCollectionChange();
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
  wxString portableAppsFolderPath = gController->GetApplicationDrive() + _T("/PortableApps");

  wxArrayString foundFilePaths;
  wxDir portableAppsFolder;

  bool folderItemsChanged = false;

  //***************************************************************************
  // Look for all the executable files two levels down the PortableApps folder
  // i.e. it will find PortableApps/7-Zip/7-ZipPortable.exe
  //      but not PortableApps/7-Zip/App/7-Zip/7zG.exe
  //***************************************************************************
  if (portableAppsFolder.Open(portableAppsFolderPath)) {
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
  // Loop through the files we've just found and create folder items
  // when needed.
  //***************************************************************************
  for (int i = 0; i < foundFilePaths.GetCount(); i++) {
    wxString filePath = foundFilePaths[i];
    wxString resolvedPath = FolderItem::ResolvePath(filePath);

    // Check if there is already a folder item
    // for this file path. If so: skip it.
    bool alreadyExists = false;
    for (int j = 0; j < folderItems_.size(); j++) {
      if (folderItems_.at(j)->GetResolvedFilePath() == resolvedPath) {
        alreadyExists = true;
        break;
      }
    }

    if (alreadyExists) continue;

    FolderItemSP folderItem(new FolderItem());
    folderItem->SetFilePath(filePath);
    folderItems_.push_back(folderItem);

    folderItemsChanged = true;
  }

  // Notify the controller that we've updated the folder items
  if (folderItemsChanged) {
    gController->User_FolderItemCollectionChange();
    ScheduleSave();
  }
}


