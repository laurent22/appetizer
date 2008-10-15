#include "FolderItemRenderer.h"
#include "FolderItem.h"
#include "utilities/IconGetter.h"
#include <wx/log.h>

#include "Controller.h"
extern Controller gController;


int FolderItemRenderer::uniqueID_ = 0;


BEGIN_EVENT_TABLE(FolderItemRenderer, BitmapControl)
  EVT_MOTION(FolderItemRenderer::OnMotion)
  EVT_ENTER_WINDOW(FolderItemRenderer::OnEnterWindow)
  EVT_LEAVE_WINDOW(FolderItemRenderer::OnLeaveWindow)
  EVT_LEFT_DOWN(FolderItemRenderer::OnLeftDown)
  EVT_LEFT_UP(FolderItemRenderer::OnLeftUp)
END_EVENT_TABLE()


FolderItemRenderer::FolderItemRenderer(wxWindow *owner, int id, wxPoint point, wxSize size):
BitmapControl(owner, id, point, size) {

  // Make sure that each renderer has a unique ID
  FolderItemRenderer::uniqueID_++;
  SetId(FolderItemRenderer::uniqueID_);

  folderItem_ = NULL;
  iconOverlayPainterUp_ = NULL;
  iconOverlayPainterDown_ = NULL;
  pressPosition_ = NULL;
  mouseInside_ = false;
  mousePressed_ = false;
  draggingStarted_ = false;
}


FolderItem* FolderItemRenderer::GetFolderItem() {
  return folderItem_;
}


void FolderItemRenderer::OnEnterWindow(wxMouseEvent& evt) {
  mouseInside_ = true;
  InvalidateControlBitmap();
}


void FolderItemRenderer::OnLeaveWindow(wxMouseEvent& evt) {
  mouseInside_ = false;
  InvalidateControlBitmap();
}


void FolderItemRenderer::OnLeftDown(wxMouseEvent& evt) {
  if (!HasCapture()) CaptureMouse();
  mousePressed_ = true;
  draggingStarted_ = false;

  if (!pressPosition_) pressPosition_ = new wxPoint();

  pressPosition_->x = evt.m_x;
  pressPosition_->y = evt.m_y;

  InvalidateControlBitmap();
}


void FolderItemRenderer::OnLeftUp(wxMouseEvent& evt) {
  if (HasCapture()) ReleaseMouse();  

  if (mouseInside_ && mousePressed_) {
    wxDELETE(folderItem_);
    std::vector<FolderItem*> folderItems = gController.GetUser()->GetFolderItems();
    wxLogDebug(_T("CLICK"));
  }

  mousePressed_ = false;

  InvalidateControlBitmap();
}


void FolderItemRenderer::OnMotion(wxMouseEvent& evt) {
  if (!mousePressed_) return;

  if (!draggingStarted_) {
    wxASSERT(pressPosition_ != NULL);

    wxPoint p(evt.m_x, evt.m_y);
    double distance = MathUtil::GetPointDistance(p, *pressPosition_);

    if (distance > 5.0) {
      if (HasCapture()) ReleaseMouse(); 
      draggingStarted_ = true;

      gController.SetDraggedFolderItem(folderItem_);

      wxFileDataObject fileData;
      fileData.AddFile(folderItem_->GetResolvedFilePath());

      wxDropSource dragSource(this);
      dragSource.SetData(fileData);
      wxDragResult result = dragSource.DoDragDrop(true);

      gController.SetDraggedFolderItem(NULL);
      mousePressed_ = false;
      draggingStarted_ = false;

      InvalidateControlBitmap();
    }

  } else { // Folder item is being dragged

  }

}


void FolderItemRenderer::FitToContent() {
  SetSize(gController.GetUser()->GetSettings()->IconSize + gController.GetStyles().Icon.PaddingWidth,
          gController.GetUser()->GetSettings()->IconSize + gController.GetStyles().Icon.PaddingHeight);
}


void FolderItemRenderer::UpdateControlBitmap() {
  BitmapControl::UpdateControlBitmap();

  if (!folderItem_) return;

  wxMemoryDC destDC;
  destDC.SelectObject(*controlBitmap_);

  if (mouseInside_) {

    if (mousePressed_) {
      if (!iconOverlayPainterDown_) {
        iconOverlayPainterDown_ = new NineSlicesPainter();
        iconOverlayPainterDown_->LoadImage(gController.GetFilePaths().SkinDirectory + _T("/IconOverlayDown.png"));
      }
      iconOverlayPainterDown_->Draw(&destDC, 0, 0, GetClientRect().GetWidth(), GetClientRect().GetHeight());
    } else {
      if (!iconOverlayPainterUp_) {
        iconOverlayPainterUp_ = new NineSlicesPainter();
        iconOverlayPainterUp_->LoadImage(gController.GetFilePaths().SkinDirectory + _T("/IconOverlayUp.png"));
      }
      iconOverlayPainterUp_->Draw(&destDC, 0, 0, GetClientRect().GetWidth(), GetClientRect().GetHeight());
    }

  }

  wxIcon* icon = folderItem_->GetIcon(gController.GetUser()->GetSettings()->IconSize);

  if (icon->IsOk()) {
    destDC.DrawIcon(*icon, gController.GetStyles().Icon.PaddingLeft, gController.GetStyles().Icon.PaddingTop);
  }

  destDC.SelectObject(wxNullBitmap);
}


void FolderItemRenderer::LoadData(FolderItem* folderItem) {
  wxDELETE(folderItem_);
  
  folderItem_ = folderItem;  

  InvalidateControlBitmap();
}