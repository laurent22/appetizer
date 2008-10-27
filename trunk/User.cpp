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

  TiXmlDocument doc;
  doc.LinkEndChild(new TiXmlDeclaration("1.0", "", ""));

  TiXmlElement* xmlRoot = new TiXmlElement("FolderItems");
  xmlRoot->SetAttribute("version", "1.0");
  doc.LinkEndChild(xmlRoot);

  for (int i = 0; i < folderItems_.size(); i++) {
    FolderItemSP folderItem = folderItems_.at(i);
    xmlRoot->LinkEndChild(folderItem->ToXML());   
  }

  wxString filePath = FilePaths::FolderItemsFile;
  doc.SaveFile(filePath.mb_str());
}


void User::Load() {



  return;

  wxFileConfig config(_T(""), _T(""), FilePaths::FolderItemsFile, _T(""), wxCONFIG_USE_RELATIVE_PATH);

  wxString folderItemGroup;
  long index;
  bool ok = config.GetFirstGroup(folderItemGroup, index);

  while (ok) {
    wxString filePath;
    wxString name;
    bool success = config.Read(_T("/") + folderItemGroup + _T("/FilePath"), &filePath);
    if (!success) continue;

    config.Read(_T("/") + folderItemGroup + _T("/Name"), &name);
    if (!success) name = _T("");

    FolderItemSP folderItem(new FolderItem());
    folderItem->SetFilePath(filePath);
    folderItem->SetName(name);
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


FolderItemSP User::EditNewFolderItem() {
  FolderItemSP folderItem(new FolderItem());
  int result = EditFolderItem(folderItem);

  if (result == wxID_OK) {
    folderItems_.push_back(folderItem);
    ScheduleSave();
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

  if (result == wxID_OK) {
    gController.User_FolderItemChange(folderItem);
    ScheduleSave();
  }

  return result;
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
  wxString portableAppsFolderPath = FilePaths::ApplicationDrive + _T("/PortableApps");

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
      if (folderItems_.at(j)->GetResolvedPath() == resolvedPath) {
        alreadyExists = true;
        break;
      }
    }

    if (alreadyExists) continue;

    FolderItemSP folderItem(new FolderItem());
    folderItem->SetFilePath(FolderItem::ConvertToRelativePath(filePath));
    folderItem->AutoSetName();
    folderItems_.push_back(folderItem);

    folderItemsChanged = true;
  }

  // Notify the controller that we've updated the folder items
  if (folderItemsChanged) {
    gController.User_FolderItemCollectionChange();
    ScheduleSave();
  }
}


