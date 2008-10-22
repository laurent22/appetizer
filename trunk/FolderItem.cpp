#include "FolderItem.h"
#include "utilities/IconGetter.h"
#include "utilities/DelphiToolsInterface.h"


int FolderItem::uniqueID_ = 0;


FolderItem::FolderItem() {
  FolderItem::uniqueID_++;
  id_ = FolderItem::uniqueID_;
}


int FolderItem::GetId() const {
  return id_;
}


void FolderItem::AutoSetName() {
  DelphiToolsInterface::GetFileDescription(GetResolvedFilePath(), name_);
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
    if (!icon16_.get()) icon16_.reset(IconGetter::GetFolderItemIcon(filePath_, iconSize));
    return icon16_;
  } else {
    if (!icon32_.get()) icon32_.reset(IconGetter::GetFolderItemIcon(filePath_, iconSize));
    return icon32_;
  }
}


void FolderItem::SetFilePath(const wxString& filePath) {
  if (filePath == filePath_) return;

  ClearCachedIcons();
  filePath_ = filePath;
}


wxString FolderItem::GetFilePath() {
  return filePath_;
}


wxString FolderItem::GetResolvedFilePath() {
  return filePath_;
}


wxString FolderItem::ResolvePath(const wxString& filePath) {
  return filePath;
}