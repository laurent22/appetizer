#include "FolderItemRenderer.h"
#include "FolderItem.h"
#include "utilities/IconGetter.h"
#include <wx/log.h>

#include "Controller.h"
extern Controller gController;


FolderItemRenderer::FolderItemRenderer(wxWindow *owner, int id, wxPoint point, wxSize size):
BitmapControl(owner, id, point, size) {
  folderItem_ = NULL;
}


void FolderItemRenderer::FitToContent() {
  SetSize(gController.GetUser()->GetSettings()->IconSize + gController.GetStyles().Icon.PaddingWidth,
          gController.GetUser()->GetSettings()->IconSize + gController.GetStyles().Icon.PaddingHeight);
}


void FolderItemRenderer::UpdateControlBitmap() {
  BitmapControl::UpdateControlBitmap();

  if (!folderItem_) return;

  wxIcon* icon = folderItem_->GetIcon(gController.GetUser()->GetSettings()->IconSize);

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