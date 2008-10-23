#include "MainFrame.h"
#include "Constants.h"
#include "wx/dcbuffer.h"
#include "Controller.h"
#include "OptionFrame.h"

extern ControllerSP gController;
extern OptionFrame* gOptionFrame;


BEGIN_EVENT_TABLE(MainFrame, wxFrame)
  EVT_SIZE(MainFrame::OnSize)
  EVT_MOVE(MainFrame::OnMove)
  EVT_ERASE_BACKGROUND(MainFrame::OnEraseBackground)
  EVT_PAINT(MainFrame::OnPaint)
  EVT_CLOSE(MainFrame::OnClose)
END_EVENT_TABLE()


MainFrame::MainFrame()
: wxFrame(
  (wxFrame *)NULL,
  wxID_ANY,
  wxEmptyString,
  wxDefaultPosition,
  wxDefaultSize,
  0 | wxFRAME_SHAPED | wxNO_BORDER
  )
{  
  logWindow_ = NULL;

  #ifdef __WXDEBUG__
    logWindow_ = new wxLogWindow(this, wxEmptyString, true);
  #endif // __WXDEBUG__

  needLayoutUpdate_ = true;
  needMaskUpdate_ = true;

  // Load the mask and background images
  maskNineSlices_.LoadImage(gController->GetFilePaths().SkinDirectory + _T("/BarBackgroundRegion.png"));

  windowDragData_.DraggingStarted = false;

  backgroundPanel_ = new NineSlicesPanel(this, wxID_ANY, wxPoint(0,0), wxSize(50,50));
  backgroundPanel_->LoadImage(gController->GetFilePaths().SkinDirectory + _T("/BarBackground.png"));

  backgroundPanel_->Connect(wxID_ANY, wxEVT_LEFT_DOWN, wxMouseEventHandler(MainFrame::OnMouseDown), NULL, this);
  backgroundPanel_->Connect(wxID_ANY, wxEVT_LEFT_UP, wxMouseEventHandler(MainFrame::OnMouseUp), NULL, this);
  backgroundPanel_->Connect(wxID_ANY, wxEVT_MOTION, wxMouseEventHandler(MainFrame::OnMouseMove), NULL, this);

  resizerPanel_ = new ImagePanel(backgroundPanel_, wxID_ANY, wxPoint(0, 0), wxSize(50, 50));
  resizerPanel_->LoadImage(gController->GetFilePaths().SkinDirectory + _T("/Resizer.png"));
  resizerPanel_->FitToContent();

  resizerPanel_->Connect(wxID_ANY, wxEVT_LEFT_DOWN, wxMouseEventHandler(MainFrame::OnResizerMouseDown), NULL, this);
  resizerPanel_->Connect(wxID_ANY, wxEVT_LEFT_UP, wxMouseEventHandler(MainFrame::OnResizerMouseUp), NULL, this);
  resizerPanel_->Connect(wxID_ANY, wxEVT_MOTION, wxMouseEventHandler(MainFrame::OnResizerMouseMove), NULL, this);

  iconPanel_ = new IconPanel(backgroundPanel_, wxID_ANY, wxPoint(0, 0), wxSize(200, 200));

  iconPanel_->Connect(wxID_ANY, wxEVT_LEFT_DOWN, wxMouseEventHandler(MainFrame::OnMouseDown), NULL, this);
  iconPanel_->Connect(wxID_ANY, wxEVT_LEFT_UP, wxMouseEventHandler(MainFrame::OnMouseUp), NULL, this);
  iconPanel_->Connect(wxID_ANY, wxEVT_MOTION, wxMouseEventHandler(MainFrame::OnMouseMove), NULL, this);
} 


IconPanel* MainFrame::GetIconPanel() { return iconPanel_; }


void MainFrame::UpdateMask() {
  // Create the bitmap on which the 9-slices scaled mask is going to be drawn
  wxBitmap maskBitmap = wxBitmap(GetRect().GetWidth(), GetRect().GetHeight());
  
  // Create a temporary DC to do the actual drawing and assign it the bitmap
  wxMemoryDC maskDC;
  maskDC.SelectObject(maskBitmap);
  
  // Draw the nine slices on the DC
  maskNineSlices_.Draw(&maskDC, 0, 0, maskBitmap.GetWidth(), maskBitmap.GetHeight());

  // Select NULL to release the bitmap
  maskDC.SelectObject(wxNullBitmap);

  // Create the region from the bitmap and assign it to the window
  wxRegion region(maskBitmap, MASK_COLOR);
  SetShape(region);

  needMaskUpdate_ = false;
}


void MainFrame::UpdateLayout(int width, int height) {
  ControllerStyles styles = gController->GetStyles();

  resizerPanel_->Move(width - resizerPanel_->GetRect().GetWidth(), height - resizerPanel_->GetRect().GetHeight());
  backgroundPanel_->SetSize(0, 0, width, height);
  iconPanel_->SetSize(styles.MainPanel.PaddingLeft, styles.MainPanel.PaddingRight, width - styles.MainPanel.PaddingWidth, height - styles.MainPanel.PaddingHeight);

  needLayoutUpdate_ = false;
}


void MainFrame::UpdateLayout() {
  UpdateLayout(GetClientRect().GetWidth(), GetClientRect().GetHeight());
}


void MainFrame::OnEraseBackground(wxEraseEvent &evt) {

}


void MainFrame::OnPaint(wxPaintEvent& evt) {
  wxPaintDC dc(this);

  if (needLayoutUpdate_) UpdateLayout();
  if (needMaskUpdate_) UpdateMask();
}


void MainFrame::AttachOptionPanel() {
  gOptionFrame->SetSize(GetRect().GetLeft() - gOptionFrame->GetRect().GetWidth(), GetRect().GetTop(), gOptionFrame->GetRect().GetWidth(), GetRect().GetHeight());
}


void MainFrame::OnMove(wxMoveEvent& evt) {
  AttachOptionPanel();
}


void MainFrame::OnSize(wxSizeEvent& evt) {
  AttachOptionPanel();
  gOptionFrame->Update();

  needLayoutUpdate_ = true;
  needMaskUpdate_ = true;
	Refresh();
}


void MainFrame::OnMouseDown(wxMouseEvent& evt) {
  static_cast<wxWindow*>(evt.GetEventObject())->CaptureMouse();

  windowDragData_.Resizing = false;
  windowDragData_.DraggingStarted = true;
  windowDragData_.InitMousePos = ClientToScreen(evt.GetPosition());
  windowDragData_.InitWindowPos = GetPosition();
}


void MainFrame::OnMouseUp(wxMouseEvent& evt) {
  wxWindow* w = static_cast<wxWindow*>(evt.GetEventObject());
  if (w->HasCapture()) w->ReleaseMouse();
  windowDragData_.DraggingStarted = false;
}


void MainFrame::OnMouseMove(wxMouseEvent& evt) {
  if (windowDragData_.DraggingStarted && evt.Dragging() && evt.LeftIsDown() && !windowDragData_.Resizing) {
    wxPoint mousePos = ClientToScreen(evt.GetPosition());
    wxPoint mouseOffset = mousePos - windowDragData_.InitMousePos;
    Move(mouseOffset.x + windowDragData_.InitWindowPos.x, mouseOffset.y + windowDragData_.InitWindowPos.y);
  }
}


void MainFrame::OnResizerMouseDown(wxMouseEvent& evt) {
  wxWindow* w = static_cast<wxWindow*>(evt.GetEventObject());
  w->CaptureMouse();

  windowDragData_.Resizing = true;
  windowDragData_.DraggingStarted = true;
  windowDragData_.InitMousePos = w->ClientToScreen(evt.GetPosition());
  windowDragData_.InitWindowSize.SetWidth(GetSize().GetWidth());
  windowDragData_.InitWindowSize.SetHeight(GetSize().GetHeight());
}


void MainFrame::OnResizerMouseUp(wxMouseEvent& evt) {
  wxWindow* w = static_cast<wxWindow*>(evt.GetEventObject());
  if (w->HasCapture()) w->ReleaseMouse();
  windowDragData_.DraggingStarted = false;
}


void MainFrame::OnResizerMouseMove(wxMouseEvent& evt) {
  if (windowDragData_.DraggingStarted && evt.Dragging() && evt.LeftIsDown() && windowDragData_.Resizing) {
    wxWindow* w = static_cast<wxWindow*>(evt.GetEventObject());
    
    wxPoint mousePos = w->ClientToScreen(evt.GetPosition());
    wxPoint mouseOffset = mousePos - windowDragData_.InitMousePos;

    SetSize(windowDragData_.InitWindowSize.GetWidth() + mouseOffset.x,
            windowDragData_.InitWindowSize.GetHeight() + mouseOffset.y);
    Update();
  }
}


void MainFrame::OnClose(wxCloseEvent& evt) {
  gOptionFrame->Close(true);
  gOptionFrame = NULL;
  Destroy();
}