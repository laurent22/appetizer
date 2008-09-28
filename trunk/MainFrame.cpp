#include "MainFrame.h"
#include "Constants.h"
#include "wx/dcbuffer.h"

#include "Controller.h"
extern Controller gController;


BEGIN_EVENT_TABLE(MainFrame, wxFrame)
  EVT_SIZE(MainFrame::OnSize)
  EVT_ERASE_BACKGROUND(MainFrame::OnEraseBackground)
  EVT_PAINT(MainFrame::OnPaint)
END_EVENT_TABLE()


MainFrame::MainFrame()
: wxFrame(
  (wxFrame *)NULL,
  wxID_ANY,
  wxEmptyString,
  wxDefaultPosition,
  wxDefaultSize /*,
  0 | wxFRAME_SHAPED | wxSIMPLE_BORDER*/
  ) // 0 | wxFRAME_SHAPED | wxSIMPLE_BORDER | wxFRAME_NO_TASKBAR | wxSTAY_ON_TOP
{  
  gController.SetMainFrame(this);

  needLayoutUpdate_ = true;
  needMaskUpdate_ = true;

  // Load the mask and background images
  maskNineSlices_.LoadImage(wxT("Data/Skin/Default/BarBackgroundRegion.png"));

  windowDragData_.DraggingStarted = false;

  backgroundPanel_ = new NineSlicesPanel(this, wxID_ANY, wxPoint(0,0), wxSize(50,50));
  backgroundPanel_->LoadImage(wxT("Data/Skin/Default/BarBackground.png"));

  backgroundPanel_->Connect(wxID_ANY, wxEVT_LEFT_DOWN, wxMouseEventHandler(MainFrame::OnMouseDown), NULL, this);
  backgroundPanel_->Connect(wxID_ANY, wxEVT_LEFT_UP, wxMouseEventHandler(MainFrame::OnMouseUp), NULL, this);
  backgroundPanel_->Connect(wxID_ANY, wxEVT_MOTION, wxMouseEventHandler(MainFrame::OnMouseMove), NULL, this);

  resizerPanel_ = new ImagePanel(backgroundPanel_, wxID_ANY, wxPoint(0, 0), wxSize(50, 50));
  resizerPanel_->LoadImage(wxT("Data/Skin/Default/Resizer.png"));
  resizerPanel_->FitToContent();

  iconPanel_ = new IconPanel(backgroundPanel_, wxID_ANY, wxPoint(0, 0), wxSize(200, 200));

  gController.GetUser()->AutomaticallyAddNewApps();
} 


IconPanel* MainFrame::GetIconPanel() { return iconPanel_; }


void MainFrame::UpdateMask() {
  return;

  // Create the bitmap on which the 9-slices scaled mask is going to be drawn
  wxBitmap maskBitmap = wxBitmap(GetClientSize().GetWidth(), GetClientSize().GetHeight());
  
  // Create a temporary DC to do the actual drawing and assign it the bitmap
  wxMemoryDC maskDC;
  maskDC.SelectObject(maskBitmap);
  
  // Draw the nine slices on the DC
  maskNineSlices_.Draw(&maskDC, 0, 0, maskBitmap.GetWidth(), maskBitmap.GetHeight());

  // Select NULL to release the bitmap
  // TODO: Should the bitmap be manually deleted at this point or is it handled by wxWidgets?
  maskDC.SelectObject(wxNullBitmap);

  // Create the region from the bitmap and assign it to the window
  wxRegion region(maskBitmap, MASK_COLOR);
  SetShape(region);

  needMaskUpdate_ = false;
}


void MainFrame::UpdateLayout(int width, int height) {
  ControllerStyles styles = gController.GetStyles();

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


void MainFrame::OnSize(wxSizeEvent& evt) {
  needLayoutUpdate_ = true;
  needMaskUpdate_ = true;
	Refresh();
}


void MainFrame::OnMouseDown(wxMouseEvent& evt) {
  static_cast<wxWindow*>(evt.GetEventObject())->CaptureMouse();

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
  if (windowDragData_.DraggingStarted && evt.Dragging() && evt.LeftIsDown()) {
    wxPoint mousePos = ClientToScreen(evt.GetPosition());
    wxPoint mouseOffset = mousePos - windowDragData_.InitMousePos;
    Move(mouseOffset.x + windowDragData_.InitWindowPos.x, mouseOffset.y + windowDragData_.InitWindowPos.y);
  }
}