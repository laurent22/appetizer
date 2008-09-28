#include "FolderItem.h"
#include "utilities/IconGetter.h"


FolderItem::FolderItem() {
  filePath_ = _T("");
  icon16_ = NULL;
  icon32_ = NULL;
  
}


void FolderItem::ClearCachedIcons() {
  wxDELETE(icon16_);
  wxDELETE(icon32_);
}


wxIcon* FolderItem::GetIcon(int iconSize) {
  if (iconSize == 16) {
    if (icon16_ == NULL) icon16_ = IconGetter::GetFolderItemIcon(filePath_, iconSize);
    return icon16_;
  } else {
    if (icon32_ == NULL) icon32_ = IconGetter::GetFolderItemIcon(filePath_, iconSize);
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