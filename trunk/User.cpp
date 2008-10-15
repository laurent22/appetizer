#include "User.h"
#include <wx/dir.h>
#include <wx/filename.h>
#include "Controller.h"
#include "FolderItem.h"

extern Controller gController;


User::User() {
  settings_ = new UserSettings();
}


void User::LoadSettings() {

}


std::vector<FolderItem*> User::GetFolderItems() {
  return folderItems_;
}


FolderItem* User::GetFolderItemById(int folderItemId) {
  for (int i = 0; i < folderItems_.size(); i++) {
    if (folderItems_.at(i)->GetId() == folderItemId) return folderItems_.at(i);
  }
  return NULL;
}


UserSettings* User::GetSettings() {
  return settings_;
}


void User::AutomaticallyAddNewApps() {
  wxString portableAppsFolderPath = gController.GetApplicationDrive() + _T("/PortableApps");

  wxArrayString foundFilePaths;
  wxDir portableAppsFolder;

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

  for (int i = 0; i < foundFilePaths.GetCount(); i++) {
    wxString filePath = foundFilePaths[i];

    FolderItem* folderItem = new FolderItem();
    folderItem->SetFilePath(filePath);
    folderItems_.push_back(folderItem);
  }

  gController.User_FolderItemCollectionChange();

}


