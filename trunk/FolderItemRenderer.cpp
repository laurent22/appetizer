#include "FolderItemRenderer.h"
#include "FolderItem.h"
#include "utilities/IconGetter.h"
#include <wx/log.h>



FolderItemRenderer::FolderItemRenderer(wxWindow *owner, int id, wxPoint point, wxSize size):
BitmapControl(owner, id, point, size) {
  folderItem_ = NULL;
}


void FolderItemRenderer::FitToContent() {
  SetSize(32, 32);
}


void FolderItemRenderer::UpdateControlBitmap() {
  BitmapControl::UpdateControlBitmap();

  if (!folderItem_) return;

  wxIcon* icon = folderItem_->GetIcon(32);

  if (icon->IsOk()) {
    wxMemoryDC destDC;
    destDC.SelectObject(*controlBitmap_);
    destDC.DrawIcon(*icon, 0, 0);
    destDC.SelectObject(wxNullBitmap);
  }
}


void FolderItemRenderer::LoadData(FolderItem* folderItem) {
  wxDELETE(folderItem_);
  
  folderItem_ = folderItem;  

  InvalidateControlBitmap();
}