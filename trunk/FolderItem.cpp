/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#include "FolderItem.h"
#include "FolderItemProcess.h"
#include "utilities/IconGetter.h"
#include "utilities/VersionInfo.h"
#include "MessageBoxes.h"
#include "Enumerations.h"
#include "FilePaths.h"
#include "Styles.h"
#include "Log.h"
#include "MiniLaunchBar.h"


int FolderItem::uniqueID_ = 1000;

FolderItemProcessVector FolderItem::processVector_;


FolderItem::FolderItem(bool isGroup) {
  FolderItem::uniqueID_++;
  id_ = FolderItem::uniqueID_;

  parent_ = NULL;
  isGroup_ = isGroup;
  uuid_ = wxEmptyString;
  customIconIndex_ = 0;

  automaticallyAdded_ = false;
  belongsToMultiLaunchGroup_ = false;
}


FolderItem::~FolderItem() {

}


void FolderItem::SetCustomIcon(const wxString& filePath, int index) {
  if (filePath == customIconPath_ && index == customIconIndex_) return;

  customIconPath_ = filePath;
  customIconIndex_ = index;
  ClearCachedIcons();
}


wxString FolderItem::GetCustomIconPath() {
  return customIconPath_;
}


int FolderItem::GetCustomIconIndex() {
  return customIconIndex_;
}


void FolderItem::SetParameters(const wxString& parameters) {
  if (parameters_ == parameters) return;
  parameters_ = wxString(parameters);
  parameters_.Trim(true).Trim(false);
}


wxString FolderItem::GetParameters() {
  return parameters_;
}


wxString FolderItem::GetUUID() {
  if (uuid_ == wxEmptyString) uuid_ = wxGetApp().GetUtilities().CreateUUID();
  return uuid_;
}


void FolderItem::InsertChildBefore(FolderItemSP toAdd, FolderItemSP previousFolderItem) {
  for (int i = 0; i < children_.size(); i++) {
    FolderItemSP child = children_.at(i);
    if (child->GetId() == previousFolderItem->GetId()) {
      MoveChild(toAdd, i);
      break;
    }
  }
}


void FolderItem::InsertChildAfter(FolderItemSP toAdd, FolderItemSP previousFolderItem) {  
  for (int i = 0; i < children_.size(); i++) {
    FolderItemSP child = children_.at(i);
    if (child->GetId() == previousFolderItem->GetId()) {
      MoveChild(toAdd, i + 1);
      break;
    }
  }
}


void FolderItem::PrependChild(FolderItemSP toAdd) {
  MoveChild(toAdd, 0);
}


bool FolderItem::IsGroup() {
  return isGroup_;
}


FolderItemSP FolderItem::GetChildByResolvedPath(const wxString& filePath) {
  for (int i = 0; i < children_.size(); i++) {
    FolderItemSP child = children_.at(i);
    if (child->GetResolvedPath() == filePath) {
      return child;
    } else {
      FolderItemSP found = child->GetChildByResolvedPath(filePath);
      if (found.get()) return found;
    }
  }
  FolderItemSP nullOutput;
  return nullOutput;
}


void FolderItem::DoMultiLaunch() {
  if (!IsGroup()) {
    if (BelongsToMultiLaunchGroup()) Launch();
  } else {
    for (int i = 0; i < children_.size(); i++) {
      FolderItemSP folderItem = children_.at(i);
      folderItem->DoMultiLaunch();
    }
  }
}


void FolderItem::MoveChild(FolderItemSP folderItemToMove, int insertionIndex) {
  // Create the new vector of folder items that
  // is going to replace the old one
  FolderItemVector newFolderItems;

  bool isPushed = false;

  if (insertionIndex < 0) {
    newFolderItems.push_back(folderItemToMove);
    isPushed = true;
  }

  for (int i = 0; i < children_.size(); i++) {
    FolderItemSP folderItem = children_.at(i);

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
  children_ = newFolderItems;

  folderItemToMove->SetParent(this);

  // Notify everybody that we've changed the item collection
  wxGetApp().FolderItems_CollectionChange();
}


FolderItemVector FolderItem::GetChildren() {
  return children_;
}


void FolderItem::AddChild(FolderItemSP folderItem) {
  folderItem->SetParent(this);
  children_.push_back(folderItem);
}


void FolderItem::RemoveChild(FolderItemSP folderItem) {
  for (int i = 0; i < children_.size(); i++) {
    FolderItemSP child = children_.at(i);
    if (child->GetId() == folderItem->GetId()) {

      if (folderItem->GetAutomaticallyAdded()) {
        wxGetApp().GetUser()->AddAutoAddExclusion(folderItem->GetResolvedPath());
      }

      child->SetParent(NULL);
      children_.erase(children_.begin() + i);

      wxGetApp().FolderItems_CollectionChange();
      return;
    }
  }
}


FolderItemSP FolderItem::GetChildAt(int index) {
  return children_.at(index);
}


FolderItemSP FolderItem::GetChildByUUID(const wxString& uuid, bool recurse) {
  for (int i = 0; i < children_.size(); i++) {
    FolderItemSP folderItem = children_.at(i);

    if (folderItem->GetUUID() == uuid) return folderItem;

    if (recurse && folderItem->IsGroup()) {
      FolderItemSP temp = folderItem->GetChildByUUID(uuid, recurse);
      if (temp.get()) return temp;
    }
  }

  FolderItemSP nullOutput;
  return nullOutput;
}


FolderItemSP FolderItem::GetChildById(int folderItemId, bool recurse) {
  for (int i = 0; i < children_.size(); i++) {
    FolderItemSP folderItem = children_.at(i);

    if (folderItem->GetId() == folderItemId) return folderItem;

    if (recurse && folderItem->IsGroup()) {
      FolderItemSP temp = folderItem->GetChildById(folderItemId, recurse);
      if (temp.get()) return temp;
    }
  }

  FolderItemSP nullOutput;
  return nullOutput;
}


int FolderItem::ChildrenCount() {
  return children_.size();
}


void FolderItem::SetParent(FolderItem* folderItem) {
  parent_ = folderItem;
}


FolderItem* FolderItem::GetParent() {
  return parent_;
}


int FolderItem::GetId() const {
  return id_;
}


void FolderItem::AddToMultiLaunchGroup() {
  belongsToMultiLaunchGroup_ = true;
}


void FolderItem::RemoveFromMultiLaunchGroup() {
  belongsToMultiLaunchGroup_ = false;
}


bool FolderItem::BelongsToMultiLaunchGroup() {
  return belongsToMultiLaunchGroup_;
}


void FolderItem::SetGroupIcon(FolderItemSP folderItem) {
  if (!folderItem.get()) {
    groupIconUUID_ = wxEmptyString;
  } else {
    if (groupIconUUID_ == folderItem->GetUUID()) return;
    groupIconUUID_ = folderItem->GetUUID();    
  }
  ClearCachedIcons();
}


void FolderItem::OnMenuItemClick(wxCommandEvent& evt) {
  FolderItemSP folderItem = wxGetApp().GetUser()->GetRootFolderItem()->GetChildById(evt.GetId());
  if (!folderItem.get()) {
    evt.Skip();
  } else {
    if (folderItem->IsGroup()) {
      wxGetApp().GetUtilities().ShowTreeViewDialog(evt.GetId());
    } else {
      folderItem->Launch();
      wxGetApp().GetMainFrame()->DoAutoHide();
    }
  }
}


void FolderItem::AppendAsMenuItem(wxMenu* parentMenu, int iconSize) {
  if (IsGroup()) {
    wxMenu* menu = ToMenu(iconSize);
    wxMenuItem* menuItem = parentMenu->AppendSubMenu(menu, GetName(true));
  } else {
    wxMenuItem* menuItem = ToMenuItem(parentMenu, iconSize);
    parentMenu->Append(menuItem);
  }
}


wxMenu* FolderItem::ToMenu(int iconSize) {
  if (!IsGroup()) {
    elog(_T("A non-group folder item cannot be converted to a menu. Call ToMenuItem() instead"));
    return NULL;
  }

  wxMenu* menu = new wxMenu();

  for (int i = 0; i < children_.size(); i++) {
    FolderItemSP child = children_.at(i);
    if (!child.get()) continue;
    child->AppendAsMenuItem(menu, iconSize);
  } 

  if (menu->GetMenuItemCount() > 0) menu->AppendSeparator();

  menu->Append(GetId(), _("Organize shortcuts"));  

  menu->Connect(
    wxID_ANY,
    wxEVT_COMMAND_MENU_SELECTED,
    wxCommandEventHandler(FolderItem::OnMenuItemClick),
    NULL,
    this);

  return menu;
}


wxMenuItem* FolderItem::ToMenuItem(wxMenu* parentMenu, int iconSize) {
  if (IsGroup()) {
    elog(_T("A group cannot be converted to a menu item. Call ToMenu() instead"));
    return NULL;
  }

  wxMenuItem* menuItem = new wxMenuItem(parentMenu, GetId(), GetName(true));

  wxIconSP icon = GetIcon(iconSize);
  if (!icon->IsOk()) {
    elog(wxString::Format(_T("Icon is not ok for: %s"), GetName()));
  } else {
    wxBitmap iconBitmap(*icon);
    menuItem->SetBitmap(iconBitmap);
  }

  return menuItem;
}


/**
 * Search for the first folder item having a filename that matches the given string. The match mode
 * can take any of these values:
 * 1: A folder item's name must start with the specified string to be a match.
 * 2: A folder item's name can contain the string anywhere inside it to be a match. 
 * 3: A folder item's name must exactly match the string to be a match.
 * @param filename The filename to search for.
 * @param matchMode The matching behavior
 */
FolderItemSP FolderItem::SearchChildByFilename(const wxString& filename, int matchMode) { 
  for (int i = 0; i < children_.size(); i++) {
    FolderItemSP folderItem = children_.at(i);
    if (!folderItem.get()) continue;
    
    if (folderItem->IsGroup()) {
      FolderItemSP foundFolderItem = folderItem->SearchChildByFilename(filename, matchMode);
      if (foundFolderItem.get()) return foundFolderItem;
    } else {
      wxString folderItemFilename = folderItem->GetFileName().Lower();
      if (folderItemFilename.Find(filename.Lower()) != wxNOT_FOUND) return folderItem;
    }
  }    

  FolderItemSP nullOutput;
  return nullOutput;
}


void FolderItem::Launch(const wxString& filePath, const wxString& arguments) {
  wxFileName filename(filePath);

  if (filename.FileExists()) {
    //***************************************************************************
    // Launch executable
    //***************************************************************************   

    if (filename.GetExt().Upper() == _T("EXE")) {
      // Set the current directory to the app directory
      wxString saveCurrentDirectory = wxGetCwd();
      wxSetWorkingDirectory(filename.GetPath());
      if (arguments == wxEmptyString) {
        FolderItemProcess* process = new FolderItemProcess();
        FolderItem::processVector_.push_back(process);
        wxExecute(filePath, wxEXEC_ASYNC, process);
      } else {
        wxString tArguments(arguments); 
        // If the argument is a file path, then check that it has double quotes        
        if (wxFileName::FileExists(tArguments)) {
          if (tArguments[0] != _T('"') && tArguments[arguments.Len() - 1] != _T('"')) {
            tArguments = _T('"') + tArguments + _T('"');
          }
          tArguments = FolderItem::ResolvePath(tArguments);
        }
        wxExecute(wxString::Format(_T("%s %s"), filePath, tArguments));
      }
      // Restore the current directory
      wxSetWorkingDirectory(saveCurrentDirectory);
      return;
    }

    //***************************************************************************
    // Launch document
    //***************************************************************************

    wxFileType* fileType = wxTheMimeTypesManager->GetFileTypeFromExtension(filename.GetExt());
    if (!fileType) {
      MessageBoxes::ShowError(wxString::Format(_("This file doesn't exist or has been deleted (Error %s)"), _T("UnknownMimeType")));
      return;
    }

    wxString command; 
    bool ok = fileType->GetOpenCommand(&command, wxFileType::MessageParameters(filePath, wxEmptyString)); 
    if (!ok) {
      MessageBoxes::ShowError(wxString::Format(_("This file doesn't exist or has been deleted (Error %s)"), _T("CannotBuildCommand")));
    } else {
      wxExecute(command);
    }

    wxDELETE(fileType);    

  } else if (wxFileName::DirExists(filePath)) {
    // Strangely enough filename.DirExists() returns true
    // even if the directory doesn't exist, so we need to
    // use the static method wxFileName::DirExists() instead

    //***************************************************************************
    // Open folder
    //***************************************************************************

    #ifdef __WINDOWS__
    wxString command = _T("explorer ") + filename.GetFullPath();
    wxExecute(command);
    #else
    elog("TO BE IMPLEMENTED");
    #endif
  } else {
    MessageBoxes::ShowError(wxString::Format(_("This file doesn't exist or has been deleted (Error %s)"), _T("DoesNotExist")));
  }
}


void FolderItem::Launch() {

  wxString parameters;
  wxString filePath;

  wxString uFilePath = filePath_.Upper();

  if (uFilePath.Index(_T("%AZ_")) != wxNOT_FOUND) {
    if (uFilePath == _T("%AZ_CONTROL_PANEL%")) {


      filePath = FilePaths::GetSystem32Directory() + _T("\\control.exe");


    } else if (uFilePath == _T("%AZ_MY_COMPUTER%")) {

      filePath = FilePaths::GetWindowsDirectory() + _T("\\explorer.exe");
      parameters = _T(",::{20D04FE0-3AEA-1069-A2D8-08002B30309D}");


    } else if (uFilePath == _T("%AZ_MY_NETWORK%")) {

      filePath = FilePaths::GetWindowsDirectory() + _T("\\explorer.exe");
      parameters = _T(",::{208D2C60-3AEA-1069-A2D7-08002B30309D}");  


    } else if (uFilePath == _T("%AZ_RECYCLE_BIN%")) {

      filePath = FilePaths::GetWindowsDirectory() + _T("\\explorer.exe");
      parameters = _T(",::{645FF040-5081-101B-9F08-00AA002F954E}");  


    } else if (uFilePath == _T("%AZ_SHOW_DESKTOP%")) {

      wxString showDesktopFilePath = wxString::Format(_T("%s/%s"), FilePaths::GetSettingsDirectory(), _T("ShowDesktop.scf"));
      if (!wxFileName::FileExists(showDesktopFilePath)) {
        wxString fileContent;
        fileContent += _T("[Shell]\n");
        fileContent += _T("Command=2\n");
        fileContent += _T("IconFile=explorer.exe,3\n");
        fileContent += _T("[Taskbar]\n");
        fileContent += _T("Command=ToggleDesktop\n");

        wxFile file;
        bool success = file.Create(showDesktopFilePath);
        if (!success) {
          elog(_T("Couldn't create ShowDesktop.scf"));
          return;
        }
        file.Open(showDesktopFilePath, wxFile::write);
        file.Write(fileContent);
        file.Close();        
      }
      filePath = showDesktopFilePath;


    } else if (uFilePath == _T("%AZ_EXPLORER%")) {

      filePath = FilePaths::GetWindowsDirectory() + _T("\\explorer.exe");


    } else if (uFilePath == _T("%AZ_SEARCH%")) {
      
      wxString findFilePath = wxString::Format(_T("%s/%s"), FilePaths::GetSettingsDirectory(), _T("FindFile.vbs"));
      if (!wxFileName::FileExists(findFilePath)) {
        wxString fileContent;
        fileContent += _T("CreateObject(\"Shell.Application\").FindFiles");

        wxFile file;
        bool success = file.Create(findFilePath);
        if (!success) {
          elog(_T("Couldn't create FindFile.vbs"));
          return;
        }
        file.Open(findFilePath, wxFile::write);
        file.Write(fileContent);
        file.Close();
      }

      filePath = wxString::Format(_T("%s\\FindFile.vbs"), FilePaths::GetSettingsDirectory());

    } else if (uFilePath == _T("%AZ_MY_DOCUMENTS%")) {

      wxLogNull logNo;

      wxRegKey regKey(_T("HKEY_CURRENT_USER\\Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\User Shell Folders"));
      if (regKey.Exists()) {
        regKey.QueryValue(_T("Personal"), filePath);
      } else {
        elog(_T("Couldn't get My Documents path"));
      }

    } else if (uFilePath == _T("%AZ_MY_PICTURES%")) {

      wxLogNull logNo;

      wxRegKey regKey(_T("HKEY_CURRENT_USER\\Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\User Shell Folders"));
      if (regKey.Exists()) {
        regKey.QueryValue(_T("My Pictures"), filePath);
      } else {
        elog(_T("Couldn't get My Pictures path"));
      }


    } else if (uFilePath == _T("%AZ_MY_MUSIC%")) {

      wxLogNull logNo;                          
      wxRegKey regKey(_T("HKEY_CURRENT_USER\\Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\Shell Folders"));
      if (regKey.Exists()) {
        regKey.QueryValue(_T("My Music"), filePath);
      } else {
        elog(_T("Couldn't get My Music path"));
      }


    } else if (uFilePath == _T("%AZ_MY_VIDEO%")) {

      wxLogNull logNo;

      wxRegKey regKey(_T("HKEY_CURRENT_USER\\Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\Shell Folders"));
      if (regKey.Exists()) {
        regKey.QueryValue(_T("My Video"), filePath);
      } else {
        elog(_T("Couldn't get My Video path"));
      }


    }
  } else {
    parameters = parameters_;
    filePath = GetResolvedPath();
  }
    
  if (parameters != wxEmptyString) {
    FolderItem::Launch(filePath, parameters);
  } else {
    FolderItem::Launch(filePath);
  }
}


void FolderItem::LaunchWithArguments(const wxString& arguments) {
  wxString resolvedFilePath = GetResolvedPath();
  FolderItem::Launch(resolvedFilePath, arguments);
}


TiXmlElement* FolderItem::ToXml() {
  TiXmlElement* xml = new TiXmlElement("FolderItem");

  XmlUtil::AppendTextElement(xml, "FilePath", GetFilePath().mb_str());
  XmlUtil::AppendTextElement(xml, "Name", GetName().mb_str());
  XmlUtil::AppendTextElement(xml, "AutomaticallyAdded", GetAutomaticallyAdded());
  XmlUtil::AppendTextElement(xml, "MultiLaunchGroup", BelongsToMultiLaunchGroup());
  XmlUtil::AppendTextElement(xml, "IsGroup", IsGroup());
  XmlUtil::AppendTextElement(xml, "UUID", uuid_);
  XmlUtil::AppendTextElement(xml, "GroupIconUUID", groupIconUUID_);
  XmlUtil::AppendTextElement(xml, "Parameters", parameters_);
  
  if (customIconPath_ != wxEmptyString) {
    XmlUtil::AppendTextElement(xml, "CustomIcon", wxString::Format(_T("%s,%d"), customIconPath_, customIconIndex_));
  }

  if (IsGroup()) {
    TiXmlElement* childrenXml = new TiXmlElement("Children");
    xml->LinkEndChild(childrenXml);

    for (int i = 0; i < children_.size(); i++) {
      FolderItemSP folderItem = children_.at(i);
      if (!folderItem.get()) continue;
      childrenXml->LinkEndChild(folderItem->ToXml());
    }    
  }

  return xml;
}


void FolderItem::FromXml(TiXmlElement* xml) {
  TiXmlHandle handle(xml);

  SetName(XmlUtil::ReadElementText(handle, "Name"));
  SetFilePath(XmlUtil::ReadElementText(handle, "FilePath"));
  SetAutomaticallyAdded(XmlUtil::ReadElementTextAsBool(handle, "AutomaticallyAdded"));
  belongsToMultiLaunchGroup_ = XmlUtil::ReadElementTextAsBool(handle, "MultiLaunchGroup");
  isGroup_ = XmlUtil::ReadElementTextAsBool(handle, "IsGroup");
  uuid_ = XmlUtil::ReadElementText(handle, "UUID");
  groupIconUUID_ = XmlUtil::ReadElementText(handle, "GroupIconUUID");
  parameters_ = XmlUtil::ReadElementText(handle, "Parameters");

  wxArrayString customIconData;
  XmlUtil::ReadElementTextAsArrayString(handle, "CustomIcon", customIconData);
  if (customIconData.Count() >= 2) {
    customIconPath_ = customIconData[0];
    customIconIndex_ = 0;
    long t;
    if (customIconData[1].ToLong(&t)) customIconIndex_ = (int)t;
  }

  children_.clear();
  TiXmlElement* childrenXml = handle.Child("Children", 0).ToElement();
  if (childrenXml) {
    for (TiXmlElement* element = childrenXml->FirstChildElement(); element; element = element->NextSiblingElement()) {
      wxString elementName = wxString(element->Value(), wxConvUTF8);
      if (elementName != _T("FolderItem")) continue;
      
      FolderItemSP folderItem(new FolderItem());
      folderItem->FromXml(element);
      AddChild(folderItem);
    }
  }

}


bool FolderItem::GetAutomaticallyAdded() {
  return automaticallyAdded_;
}


void FolderItem::SetAutomaticallyAdded(bool automaticallyAdded) {
  automaticallyAdded_ = automaticallyAdded;
}




wxString FolderItem::GetName(bool returnUnnamedIfEmpty) {
  if (returnUnnamedIfEmpty) {
    if (name_ == wxEmptyString) return _("Group");
  }
  return name_;
}


void FolderItem::SetName(const wxString& name) {
  name_ = wxString(name);
  name_.Trim(true).Trim(false);
}


void FolderItem::ClearCachedIcons() {
  icons_.clear();

  //icon16_.reset();
  //icon32_.reset();
}


void FolderItem::CacheIconToDisk(const wxString& hash, wxIconSP icon, int iconSize) {
  wxASSERT_MSG(icon.get(), _T("Couldn't cache icon: null pointer"));
  wxASSERT_MSG(icon->IsOk(), _T("Couldn't cache icon: is not ok"));
  wxASSERT_MSG(hash != wxEmptyString, _T("Couldn't cache icon: empty hash"));

  wxBitmap bitmap(*icon);
  bitmap.SaveFile(wxString::Format(_T("%s/%s-%d.png"), FilePaths::GetDataDirectory() + _T("/IconCache"), hash, iconSize), wxBITMAP_TYPE_PNG);
}


wxString FolderItem::GetIconDiskCacheHash() {
  if (IsGroup()) return wxEmptyString;
  if (iconCacheHash_ != wxEmptyString) return iconCacheHash_;

  wxFileName filename(GetResolvedPath());
  if (!filename.IsOk()) return _T("");

  wxString toHash;

  if (!wxFileName::DirExists(filename.GetFullPath())) {
    wxString modString(_T(""));
    wxString createString(_T(""));
    wxDateTime dtMod;
    wxDateTime dtCreate;
    filename.GetTimes(NULL, &dtMod, &dtCreate);  
    if (dtMod.IsValid()) modString = dtMod.Format();
    if (dtCreate.IsValid()) createString = dtCreate.Format();

    toHash = wxString::Format(_T("%s:%d:%s:%s"), filename.GetName(), filename.GetSize(), modString, createString);
  } else {
    toHash = wxString::Format(_T("%s"), GetFilePath());
  }
  
  wxMD5 md5(toHash);
  iconCacheHash_ = md5.GetDigest();

  return iconCacheHash_;
}


wxIconSP FolderItem::GetIconFromDiskCache(const wxString& hash, int iconSize) {
  wxString cacheDirectory = FilePaths::GetDataDirectory() + _T("/IconCache");

  wxIconSP output;

  if (hash != wxEmptyString) {
    // Look for it in the cache first
    wxString filePath = wxString::Format(_T("%s/%s-%d.png"), cacheDirectory, hash, iconSize);
    if (wxFileName::FileExists(filePath)) {
      output.reset(new wxIcon(filePath, wxBITMAP_TYPE_PNG));
    }
  }

  return output;
}


wxIconSP FolderItem::GetIcon(int iconSize) {

  iconSize = wxGetApp().GetOSValidIconSize(iconSize);

  // If the icon has already been generated, return it
  if (icons_.find(iconSize) != icons_.end()) return icons_[iconSize];

  wxIconSP output;

  bool cacheEnabled = !true;
  wxString cacheHash;

  if (cacheEnabled) {
    cacheHash = GetIconDiskCacheHash();
    wxIconSP cachedIcon = FolderItem::GetIconFromDiskCache(cacheHash, iconSize);
    if (cachedIcon.get()) {
      icons_[iconSize] = cachedIcon;
      return cachedIcon;
    }
  }

  if (IsGroup()) {
    if (groupIconUUID_ != wxEmptyString) {
      FolderItemSP associatedFolderItem = GetChildByUUID(groupIconUUID_);
      if (associatedFolderItem.get()) {
        wxIconSP t = associatedFolderItem->GetIcon(iconSize);
        output.reset(new wxIcon(*t));
      }
    }

    if (!output.get()) {
      output.reset(new wxIcon(FilePaths::GetSkinFile(wxString::Format(_T("FolderIcon%d.png"), iconSize)), wxBITMAP_TYPE_PNG));
    }
  } else {

    wxString uFilePath = filePath_.Upper();

    if (uFilePath.Index(_T("%AZ_")) != wxNOT_FOUND) {
      wxString dllPath = FilePaths::GetSystem32Directory() + _T("\\shell32.dll");
      int inc = iconSize >= 48 ? 1 : 0;

      if (uFilePath == _T("%AZ_CONTROL_PANEL%")) {
        output.reset(IconGetter::GetExecutableIcon(dllPath, iconSize, SHELL32_ICON_INDEX_CONTROL_PANEL + inc));
      } else if (uFilePath == _T("%AZ_MY_COMPUTER%")) {
        output.reset(IconGetter::GetExecutableIcon(dllPath, iconSize, SHELL32_ICON_INDEX_MY_COMPUTER + inc));
      } else if (uFilePath == _T("%AZ_MY_NETWORK%")) {
        output.reset(IconGetter::GetExecutableIcon(dllPath, iconSize, SHELL32_ICON_INDEX_MY_NETWORK + inc));
      } else if (uFilePath == _T("%AZ_RECYCLE_BIN%")) {
        output.reset(IconGetter::GetExecutableIcon(dllPath, iconSize, SHELL32_ICON_INDEX_RECYCLE_BIN + inc));
      } else if (uFilePath == _T("%AZ_SHOW_DESKTOP%")) {
        output.reset(IconGetter::GetExecutableIcon(dllPath, iconSize, SHELL32_ICON_INDEX_DESKTOP + inc));
      } else if (uFilePath == _T("%AZ_EXPLORER%")) {
        output.reset(IconGetter::GetExecutableIcon(dllPath, iconSize, SHELL32_ICON_INDEX_EXPLORER + inc));
      } else if (uFilePath == _T("%AZ_SEARCH%")) {
        output.reset(IconGetter::GetExecutableIcon(dllPath, iconSize, SHELL32_ICON_INDEX_SEARCH + inc));
      } else if (uFilePath == _T("%AZ_MY_DOCUMENTS%")) {
        output.reset(IconGetter::GetExecutableIcon(dllPath, iconSize, SHELL32_ICON_INDEX_MY_DOCUMENTS + inc));
      } else if (uFilePath == _T("%AZ_MY_PICTURES%")) {
        output.reset(IconGetter::GetExecutableIcon(dllPath, iconSize, SHELL32_ICON_INDEX_MY_PICTURES + inc));
      } else if (uFilePath == _T("%AZ_MY_MUSIC%")) {
        output.reset(IconGetter::GetExecutableIcon(dllPath, iconSize, SHELL32_ICON_INDEX_MY_MUSIC + inc));
      } else if (uFilePath == _T("%AZ_MY_VIDEO%")) {
        output.reset(IconGetter::GetExecutableIcon(dllPath, iconSize, SHELL32_ICON_INDEX_MY_VIDEOS + inc));
      }
    }
  }

  if (!output.get()) {
    output.reset(IconGetter::GetFolderItemIcon(GetResolvedPath(), iconSize));
    if (output.get()) {
      if (cacheEnabled) FolderItem::CacheIconToDisk(cacheHash, output, iconSize);
    }
  }

  if (!output.get()) {
    output.reset(new wxIcon(FilePaths::GetSkinFile(wxString::Format(_T("DefaultIcon%d.png"), iconSize)), wxBITMAP_TYPE_PNG));
  }

  // Cache the icon
  icons_[iconSize] = output;

  return output;
}


void FolderItem::AutoSetName() {
  wxString uFilePath = filePath_.Upper();
  wxString theName;

  if (uFilePath.Index(_T("%AZ_")) != wxNOT_FOUND) {
    if (uFilePath == _T("%AZ_CONTROL_PANEL%")) {
      theName = _("Control Panel");
    } else if (uFilePath == _T("%AZ_MY_COMPUTER%")) {
      theName = _("My Computer");
    } else if (uFilePath == _T("%AZ_MY_NETWORK%")) {
      theName = _("My Network Places");
    } else if (uFilePath == _T("%AZ_RECYCLE_BIN%")) {
      theName = _("Recycle Bin");
    } else if (uFilePath == _T("%AZ_SHOW_DESKTOP%")) {
      theName = _("Show Desktop");
    } else if (uFilePath == _T("%AZ_EXPLORER%")) {
      theName = _("Windows Explorer");
    } else if (uFilePath == _T("%AZ_SEARCH%")) {
      theName = _("Search");
    } 
    //else if (uFilePath == _T("%AZ_MY_DOCUMENTS%")) {
    //  output.reset(IconGetter::GetExecutableIcon(dllPath, iconSize, SHELL32_ICON_INDEX_MY_DOCUMENTS));
    //} else if (uFilePath == _T("%AZ_MY_PICTURES%")) {
    //  output.reset(IconGetter::GetExecutableIcon(dllPath, iconSize, SHELL32_ICON_INDEX_MY_PICTURES));
    //} else if (uFilePath == _T("%AZ_MY_MUSIC%")) {
    //  output.reset(IconGetter::GetExecutableIcon(dllPath, iconSize, SHELL32_ICON_INDEX_MY_MUSIC));
    //} else if (uFilePath == _T("%AZ_MY_VIDEO%")) {
    //  output.reset(IconGetter::GetExecutableIcon(dllPath, iconSize, SHELL32_ICON_INDEX_MY_VIDEOS));
    //}
  }

  if (theName != wxEmptyString) {
    SetName(theName);    
  } else {
    SetName(VersionInfo::GetFileDescription(GetResolvedPath()));
  }
}



void FolderItem::SetFilePath(const wxString& filePath) {
  if (filePath == filePath_) return;

  ClearCachedIcons();
  filePath_ = wxString(filePath);
  filePath_.Trim();
}


wxString FolderItem::GetFileName(bool includeExtension) {
  wxFileName f(GetResolvedPath());
  if (includeExtension) return f.GetFullName();
  return f.GetName();
}


wxString FolderItem::GetFilePath() {
  return filePath_;
}


wxString FolderItem::GetResolvedPath() {
  return FolderItem::ResolvePath(filePath_);
}


wxString FolderItem::ResolvePath(const wxString& filePath) {
  wxString result(filePath);
  result.Trim();
  result.Replace(_T("%DRIVE%"), FilePaths::GetApplicationDrive());
  result.Replace(_T("%AZ_DRIVE%"), FilePaths::GetApplicationDrive());
  result.Replace(_T("%AZ_SYSTEM32%"), FilePaths::GetSystem32Directory());
  result.Replace(_T("%AZ_WINDOWS%"), FilePaths::GetWindowsDirectory());
  result.Replace(_T("%AZ_CONTROL_PANEL%"), FilePaths::GetSystem32Directory() + _T("\\control.exe"));
  result.Replace(_T("%AZ_MY_COMPUTER%"), FilePaths::GetWindowsDirectory() + _T("\\explorer.exe"));
  return result;
}


wxString FolderItem::ConvertToRelativePath(const wxString& filePath) {
  wxString result(filePath);

  wxString test1 = result.Mid(0, 2).Upper();
  wxString test2 = FilePaths::GetApplicationDrive().Upper();
  
  if (result.Mid(0, 2).Upper() == FilePaths::GetApplicationDrive().Upper()) {
    result = _T("%DRIVE%") + result.Mid(2, result.Len());
  }

  return result;
}