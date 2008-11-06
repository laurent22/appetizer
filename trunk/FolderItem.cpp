/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "FolderItem.h"
#include "utilities/IconGetter.h"
#include "utilities/VersionInfo.h"
#include <wx/filename.h>
#include <wx/mimetype.h>
#include "MessageBoxes.h"
#include "FilePaths.h"
#include "Log.h"
#include "Localization.h"
#include "Controller.h"


extern Controller gController;


int FolderItem::uniqueID_ = 0;


FolderItem::FolderItem(bool isGroup) {

  // @todo: Should use wxNewId()
  FolderItem::uniqueID_++;
  id_ = FolderItem::uniqueID_;

  parent_ = NULL;
  isGroup_ = isGroup;

  automaticallyAdded_ = false;
  belongsToMultiLaunchGroup_ = false;
}


FolderItem::~FolderItem() {

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

  // Notify everybody that we've changed the item collection
  gController.FolderItems_CollectionChange();
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
        gController.GetUser()->AddAutoAddExclusion(folderItem->GetResolvedPath());
      }

      children_.erase(children_.begin() + i);

      gController.FolderItems_CollectionChange();
      return;
    }
  }
}


FolderItemSP FolderItem::GetChildAt(int index) {
  return children_.at(index);
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


wxMenuItem* FolderItem::ToMenuItem(wxMenu* parentMenu) {
  wxMenuItem* menuItem = new wxMenuItem(parentMenu, GetId(), GetName());

  const wxIcon* icon = GetIcon(16).get();
  if (!icon->IsOk()) {
    elog(wxString::Format(_T("Icon is not ok for: %s"), GetName()));
  } else {
    wxBitmap* iconBitmap = new wxBitmap(*icon);
    menuItem->SetBitmap(*iconBitmap);
  }

  return menuItem;
}


void FolderItem::Launch(const wxString& filePath, const wxString& arguments) {
  wxFileName filename(filePath);

  if (filename.FileExists()) {

    //***************************************************************************
    // Launch executable
    //***************************************************************************

    if (filename.GetExt().Upper() == _T("EXE")) {
      if (arguments == wxEmptyString) {
        wxExecute(filePath);
      } else {
        wxExecute(wxString::Format(_T("%s %s"), filePath, arguments));
      }
      return;
    }

    //***************************************************************************
    // Launch document
    //***************************************************************************

    wxFileType* fileType = wxTheMimeTypesManager->GetFileTypeFromExtension(filename.GetExt());
    if (!fileType) {
      MessageBoxes::ShowError(LOC1(_T("FolderItem.LaunchFileError"), _T("UnknownMimeType")));
      return;
    }

    wxString command; 
    bool ok = fileType->GetOpenCommand(&command, wxFileType::MessageParameters(filePath, wxEmptyString)); 
    if (!ok) {
      MessageBoxes::ShowError(LOC1(_T("FolderItem.LaunchFileError"), _T("CannotBuildCommand")));
      return;
    }

    wxExecute(command);

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
    MessageBoxes::ShowError(LOC1(_T("FolderItem.LaunchFileError"), _T("DoesNotExist")));
  }
}


void FolderItem::Launch() {
  wxString resolvedFilePath = GetResolvedPath();
  FolderItem::Launch(resolvedFilePath);
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

  children_.clear();
  TiXmlElement* childrenXml = handle.Child("Children", 0).ToElement();
  if (childrenXml) {
    for (TiXmlElement* element = childrenXml->FirstChildElement(); element; element = element->NextSiblingElement()) {
      wxString elementName = wxString(element->Value(), wxConvUTF8);
      if (elementName != _T("FolderItem")) continue;
      
      FolderItemSP folderItem(new FolderItem());
      folderItem->FromXml(element);
      children_.push_back(folderItem);
    }
  }

}


bool FolderItem::GetAutomaticallyAdded() {
  return automaticallyAdded_;
}


void FolderItem::SetAutomaticallyAdded(bool automaticallyAdded) {
  automaticallyAdded_ = automaticallyAdded;
}


void FolderItem::AutoSetName() {
  name_ = VersionInfo::GetFileDescription(GetResolvedPath());
  //DelphiToolsInterface::GetFileDescription(GetResolvedPath(), name_);
}


wxString FolderItem::GetName() {
  return name_;
}


void FolderItem::SetName(const wxString& name) {
  name_ = name;
}


void FolderItem::ClearCachedIcons() {
  icon16_.reset();
  icon32_.reset();
}


wxIconSP FolderItem::GetIcon(int iconSize) {
  if (iconSize == 16) {
    if (IsGroup() && !icon16_.get()) icon16_.reset(new wxIcon(FilePaths::GetIconsDirectory() + _T("/FolderIcon16.png"), wxBITMAP_TYPE_PNG));

    if (!icon16_.get()) icon16_.reset(IconGetter::GetFolderItemIcon(GetResolvedPath(), iconSize));
    if (!icon16_.get()) {
      icon16_.reset(new wxIcon(FilePaths::GetIconsDirectory() + _T("/DefaultIcon16.png"), wxBITMAP_TYPE_PNG));
    }
    return icon16_;
  } else {
    if (IsGroup() && !icon32_.get()) icon32_.reset(new wxIcon(FilePaths::GetIconsDirectory() + _T("/FolderIcon32.png"), wxBITMAP_TYPE_PNG));

    if (!icon32_.get()) icon32_.reset(IconGetter::GetFolderItemIcon(GetResolvedPath(), iconSize));
    if (!icon32_.get()) {
      icon32_.reset(new wxIcon(FilePaths::GetIconsDirectory() + _T("/DefaultIcon32.png"), wxBITMAP_TYPE_PNG));
    }
    return icon32_;
  }
}


void FolderItem::SetFilePath(const wxString& filePath) {
  if (filePath == filePath_) return;

  ClearCachedIcons();
  filePath_ = wxString(filePath);
  filePath_.Trim();
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