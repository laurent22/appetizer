#include "FolderItem.h"
#include "utilities/IconGetter.h"
#include "utilities/DelphiToolsInterface.h"
#include <wx/filename.h>
#include "MessageBoxes.h"
#include "FilePaths.h"


int FolderItem::uniqueID_ = 0;


FolderItem::FolderItem() {
  FolderItem::uniqueID_++;
  id_ = FolderItem::uniqueID_;
}


int FolderItem::GetId() const {
  return id_;
}


void FolderItem::Launch() {
  wxString resolvedFilePath = GetResolvedPath();
  wxFileName filename(resolvedFilePath);

  if (filename.FileExists()) {
    wxExecute(resolvedFilePath, true);
  } else if (wxFileName::DirExists(resolvedFilePath)) {
    // Strangely enough filename.DirExists() returns true
    // even if the directory doesn't exist, so we need to
    // use the static method wxFileName::DirExists() instead
    #ifdef __WIN32__
    wxString commandLine = _T("explorer ") + resolvedFilePath;
    wxExecute(commandLine, true);
    #else
    wxLogDebug(_T("TO BE IMPLEMENTED"));
    #endif
  } else {
    MessageBoxes::ShowError(filename.GetName() + _T(' could not be found.'));
  }
}


TiXmlElement* FolderItem::ToXML() {
  TiXmlElement* xml = new TiXmlElement("FolderItem");

  XmlUtil::AppendTextElement(xml, "FilePath", GetFilePath().mb_str());
  XmlUtil::AppendTextElement(xml, "Name", GetName().mb_str());

  return xml;
}


void FolderItem::FromXML(TiXmlElement* xml) {
  TiXmlHandle handle(xml);

  SetName(XmlUtil::ReadElementText(handle, "Name"));
  SetFilePath(XmlUtil::ReadElementText(handle, "FilePath"));
}


void FolderItem::AutoSetName() {
  DelphiToolsInterface::GetFileDescription(GetResolvedPath(), name_);
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