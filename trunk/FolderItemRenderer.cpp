#include "FolderItemRenderer.h"
#include "FolderItem.h"
#include "utilities/IconGetter.h"
#include <wx/log.h>

#include "Controller.h"
extern Controller gController;


BEGIN_EVENT_TABLE(FolderItemRenderer, BitmapControl)
  EVT_ENTER_WINDOW(FolderItemRenderer::OnEnterWindow)
  EVT_LEAVE_WINDOW(FolderItemRenderer::OnLeaveWindow)
  EVT_LEFT_DOWN(FolderItemRenderer::OnLeftDown)
  EVT_LEFT_UP(FolderItemRenderer::OnLeftUp)
  EVT_TIMER(PRESS_TIMER_ID, FolderItemRenderer::OnTimer)
END_EVENT_TABLE()


FolderItemRenderer::FolderItemRenderer(wxWindow *owner, int id, wxPoint point, wxSize size):
BitmapControl(owner, id, point, size) {
  folderItem_ = NULL;
  iconOverlayPainterUp_ = NULL;
  iconOverlayPainterDown_ = NULL;
  pressTimer_ = NULL;
  pressPosition_ = NULL;
  mouseInside_ = false;
  mousePressed_ = false;
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
   // wxTextDataObject my_data(_T("This text will be dragged."));
   // wxDropSource dragSource( this );
	  //dragSource.SetData( my_data );
	  //wxDragResult result = dragSource.DoDragDrop( TRUE );

  if (!HasCapture()) CaptureMouse();
  mousePressed_ = true;

  if (!pressTimer_) 
    pressTimer_ = new wxTimer(this, PRESS_TIMER_ID);
  if (!pressPosition_) pressPosition_ = new wxPoint();

  pressPosition_->x = evt.m_x;
  pressPosition_->y = evt.m_y;
  pressTimer_->Start(50);

  InvalidateControlBitmap();
}


void FolderItemRenderer::OnLeftUp(wxMouseEvent& evt) {
  if (HasCapture()) ReleaseMouse();  

  pressTimer_->Stop();

  if (mouseInside_ && mousePressed_) {
    wxLogDebug(_T("CLICK"));
  }

  mousePressed_ = false;

  InvalidateControlBitmap();
}


void FolderItemRenderer::OnTimer(wxTimerEvent& evt) {
  // @todo How can we get the position of the mouse cursor?
  // Once distance > 10, start dragging
  
  //wxMouseEvent t;
  //wxLogDebug(_T("%d"), t.m_x);
  //wxLogDebug(_T("%d"), MathUtil::GetPointDistance();
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