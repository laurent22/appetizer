#include "MainFrame.h"
#include "Constants.h"
#include "wx/dcbuffer.h"


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
  // Necessary because we are going to do the drawing ourselves
  //SetBackgroundStyle(wxBG_STYLE_CUSTOM);
  
  pNeedLayoutUpdate = true;
  pNeedMaskUpdate = true;

  // Load the mask and background images
  pMaskNineSlices.LoadImage(wxT("Data/Skin/Default/BarBackgroundRegion.png"));

  pWindowDragData.DraggingStarted = false;

  pBackgroundPanel = new NineSlicesPanel(this, wxID_ANY, wxPoint(0,0), wxSize(50,50));
  pBackgroundPanel->LoadImage(wxT("Data/Skin/Default/BarBackground.png"));

  pBackgroundPanel->Connect(wxID_ANY, wxEVT_LEFT_DOWN, wxMouseEventHandler(MainFrame::OnMouseDown), NULL, this);
  pBackgroundPanel->Connect(wxID_ANY, wxEVT_LEFT_UP, wxMouseEventHandler(MainFrame::OnMouseUp), NULL, this);
  pBackgroundPanel->Connect(wxID_ANY, wxEVT_MOTION, wxMouseEventHandler(MainFrame::OnMouseMove), NULL, this);

  pResizerPanel = new ImagePanel(pBackgroundPanel, wxID_ANY, wxPoint(0, 0), wxSize(50, 50));
  pResizerPanel->LoadImage(wxT("Data/Skin/Default/Resizer.png"));
  pResizerPanel->FitToContent();

  pInnerPanel = new NineSlicesPanel(pBackgroundPanel, wxID_ANY, wxPoint(0, 0), wxSize(200, 200));
  pInnerPanel->LoadImage(wxT("Data/Skin/Default/BarInnerPanel.png"));
} 


void MainFrame::UpdateMask() {
  return;

  // Create the bitmap on which the 9-slices scaled mask is going to be drawn
  wxBitmap maskBitmap = wxBitmap(GetClientSize().GetWidth(), GetClientSize().GetHeight());
  
  // Create a temporary DC to do the actual drawing and assign it the bitmap
  wxMemoryDC maskDC;
  maskDC.SelectObject(maskBitmap);
  
  // Draw the nine slices on the DC
  pMaskNineSlices.Draw(&maskDC, 0, 0, maskBitmap.GetWidth(), maskBitmap.GetHeight());

  // Select NULL to release the bitmap
  // TODO: Should the bitmap be manually deleted at this point or is it handled by wxWidgets?
  maskDC.SelectObject(wxNullBitmap);

  // Create the region from the bitmap and assign it to the window
  wxRegion region(maskBitmap, lcMASK_COLOR);
  SetShape(region);

  pNeedMaskUpdate = false;
}


void MainFrame::UpdateLayout(int width, int height) {
  pResizerPanel->Move(width - pResizerPanel->GetRect().GetWidth(), height - pResizerPanel->GetRect().GetHeight());
  pBackgroundPanel->SetSize(0, 0, width, height);
  pInnerPanel->SetSize(10, 10, width - 20, height - 20);

  pNeedLayoutUpdate = false;
}


void MainFrame::UpdateLayout() {
  UpdateLayout(GetClientRect().GetWidth(), GetClientRect().GetHeight());
}


void MainFrame::OnEraseBackground(wxEraseEvent &evt) {

}


void MainFrame::OnPaint(wxPaintEvent& evt) {
  wxPaintDC dc(this);

  if (pNeedLayoutUpdate) UpdateLayout();
  if (pNeedMaskUpdate) UpdateMask();
}


void MainFrame::OnSize(wxSizeEvent& evt) {
  pNeedLayoutUpdate = true;
  pNeedMaskUpdate = true;
	Refresh();
}


void MainFrame::OnMouseDown(wxMouseEvent& evt) {
  static_cast<wxWindow*>(evt.GetEventObject())->CaptureMouse();

  pWindowDragData.DraggingStarted = true;
  pWindowDragData.InitMousePos = ClientToScreen(evt.GetPosition());
  pWindowDragData.InitWindowPos = GetPosition();
}


void MainFrame::OnMouseUp(wxMouseEvent& evt) {
  wxWindow* w = static_cast<wxWindow*>(evt.GetEventObject());
  if (w->HasCapture()) w->ReleaseMouse();
  pWindowDragData.DraggingStarted = false;
}


void MainFrame::OnMouseMove(wxMouseEvent& evt) {
  if (pWindowDragData.DraggingStarted && evt.Dragging() && evt.LeftIsDown()) {
    wxPoint mousePos = ClientToScreen(evt.GetPosition());
    wxPoint mouseOffset = mousePos - pWindowDragData.InitMousePos;
    Move(mouseOffset.x + pWindowDragData.InitWindowPos.x, mouseOffset.y + pWindowDragData.InitWindowPos.y);
  }
}