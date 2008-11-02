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


int FolderItem::uniqueID_ = 0;


FolderItem::FolderItem() {
  FolderItem::uniqueID_++;
  id_ = FolderItem::uniqueID_;
  automaticallyAdded_ = false;
  belongsToMultiLaunchGroup_ = false;
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
    wxLogDebug(_T("Icon is not ok for: %s"), GetName());
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

    #ifdef __WIN32__
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


TiXmlElement* FolderItem::ToXML() {
  TiXmlElement* xml = new TiXmlElement("FolderItem");

  XmlUtil::AppendTextElement(xml, "FilePath", GetFilePath().mb_str());
  XmlUtil::AppendTextElement(xml, "Name", GetName().mb_str());
  XmlUtil::AppendTextElement(xml, "AutomaticallyAdded", GetAutomaticallyAdded());
  XmlUtil::AppendTextElement(xml, "MultiLaunchGroup", BelongsToMultiLaunchGroup());

  return xml;
}


void FolderItem::FromXML(TiXmlElement* xml) {
  TiXmlHandle handle(xml);

  SetName(XmlUtil::ReadElementText(handle, "Name"));
  SetFilePath(XmlUtil::ReadElementText(handle, "FilePath"));
  SetAutomaticallyAdded(XmlUtil::ReadElementTextAsBool(handle, "AutomaticallyAdded"));
  belongsToMultiLaunchGroup_ = XmlUtil::ReadElementTextAsBool(handle, "MultiLaunchGroup");
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
    if (!icon16_.get()) icon16_.reset(IconGetter::GetFolderItemIcon(GetResolvedPath(), iconSize));
    if (!icon16_.get()) {
      // Note: IconGetter::GetFolderItemIcon() should normally already return a
      // default icon if there is any problem. 
      icon16_.reset(new wxIcon(FilePaths::IconsDirectory + _T("/DefaultIcon16.png"), wxBITMAP_TYPE_PNG));
    }
    return icon16_;
  } else {
    if (!icon32_.get()) icon32_.reset(IconGetter::GetFolderItemIcon(GetResolvedPath(), iconSize));
    if (!icon32_.get()) {
      icon32_.reset(new wxIcon(FilePaths::IconsDirectory + _T("/DefaultIcon32.png"), wxBITMAP_TYPE_PNG));
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
  result.Replace(_T("%DRIVE%"), FilePaths::ApplicationDrive);
  return result;
}


wxString FolderItem::ConvertToRelativePath(const wxString& filePath) {
  wxString result(filePath);

  wxString test1 = result.Mid(0, 2).Upper();
  wxString test2 = FilePaths::ApplicationDrive.Upper();
  
  if (result.Mid(0, 2).Upper() == FilePaths::ApplicationDrive.Upper()) {
    result = _T("%DRIVE%") + result.Mid(2, result.Len());
  }

  return result;
}