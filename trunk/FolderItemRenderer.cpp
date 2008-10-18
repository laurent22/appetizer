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

  folderItemId_ = -1;
  iconOverlayPainterUp_ = NULL;
  iconOverlayPainterDown_ = NULL;
  pressPosition_ = NULL;
  mouseInside_ = false;
  mousePressed_ = false;
  draggingStarted_ = false;
}


FolderItemSP FolderItemRenderer::GetFolderItem() {
  return gController.GetUser()->GetFolderItemById(folderItemId_);
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

      FolderItemSP folderItem = GetFolderItem();
      if (!folderItem.get()) return;

      // Tell the main controller that we've started dragging
      // a folder item. Other objects can then do GetDraggedFolderItem()
      // to know if a folder item is being dragged.
      gController.SetDraggedFolderItem(folderItem->GetId());

      wxFileDataObject fileData;
      fileData.AddFile(folderItem->GetResolvedFilePath());

      wxDropSource dragSource(this);
      dragSource.SetData(fileData);
      wxDragResult result = dragSource.DoDragDrop(true);

      // Tell the main controller that we've finished dragging
      // the folder item
      gController.SetDraggedFolderItem(-1);
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

  FolderItemSP folderItem = GetFolderItem();

  if (!folderItem.get()) return;

  wxMemoryDC destDC;
  destDC.SelectObject(*controlBitmap_);

  if (mouseInside_) {
    // If the mouse is inside the control,
    // draw the icon overlay

    if (mousePressed_) { // DOWN state      
      if (!iconOverlayPainterDown_) {
        iconOverlayPainterDown_ = new NineSlicesPainter();
        iconOverlayPainterDown_->LoadImage(gController.GetFilePaths().SkinDirectory + _T("/IconOverlayDown.png"));
      }
      iconOverlayPainterDown_->Draw(&destDC, 0, 0, GetClientRect().GetWidth(), GetClientRect().GetHeight());
    } else { // UP state      
      if (!iconOverlayPainterUp_) {
        iconOverlayPainterUp_ = new NineSlicesPainter();
        iconOverlayPainterUp_->LoadImage(gController.GetFilePaths().SkinDirectory + _T("/IconOverlayUp.png"));
      }
      iconOverlayPainterUp_->Draw(&destDC, 0, 0, GetClientRect().GetWidth(), GetClientRect().GetHeight());
    }

  }

  // Get the icon from the folder item
  wxIcon* icon = folderItem->GetIcon(gController.GetUser()->GetSettings()->IconSize);
  wxASSERT_MSG(icon, _T("Folder item icon cannot be NULL"));

  if (icon->IsOk()) {
    // If the icon is valid, draw it
    destDC.DrawIcon(*icon, gController.GetStyles().Icon.PaddingLeft, gController.GetStyles().Icon.PaddingTop);
  }

  destDC.SelectObject(wxNullBitmap);
}


void FolderItemRenderer::LoadData(int folderItemId) {  
  folderItemId_ = folderItemId;  

  InvalidateControlBitmap();
}