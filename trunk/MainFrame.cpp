#include "MainFrame.h"
#include "Constants.h"
#include "wx/dcbuffer.h"
#include "Controller.h"
#include "FilePaths.h"
#include "Styles.h"
#include "bitmap_controls/ImageButton.h"

extern ControllerSP gController;


BEGIN_EVENT_TABLE(MainFrame, wxFrame)
  EVT_SIZE(MainFrame::OnSize)
  EVT_MOVE(MainFrame::OnMove)
  EVT_ERASE_BACKGROUND(MainFrame::OnEraseBackground)
  EVT_PAINT(MainFrame::OnPaint)
  EVT_CLOSE(MainFrame::OnClose)
  EVT_COMMAND(wxID_ANY, wxeEVT_CLICK, MainFrame::OnImageButtonClick)
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
  optionPanelOpen_ = false;
  openCloseAnimationDockLeft_ = true;
  optionPanelOpenWidth_ = 0;
  optionPanelMaxOpenWidth_ = 50;
  openCloseAnimationDuration_ = 50;

  // Load the mask and background images
  maskNineSlices_.LoadImage(FilePaths::SkinDirectory + _T("/BarBackgroundRegion.png"), false);

  windowDragData_.DraggingStarted = false;

  arrowButton_ = new ImageButton(this, ID_BUTTON_Arrow, wxPoint(0, 0), wxSize(10, 10));
  arrowButton_->LoadImage(FilePaths::SkinDirectory + _T("/ArrowButton"));
  // @todo: Grid should not be hardcoded but set in styles
  arrowButton_->SetGrid(6, 30, 1, 1);

  backgroundPanel_ = new NineSlicesPanel(this, wxID_ANY, wxPoint(0,0), wxSize(50,50));
  backgroundPanel_->LoadImage(FilePaths::SkinDirectory + _T("/BarBackground.png"));

  backgroundPanel_->Connect(wxID_ANY, wxEVT_LEFT_DOWN, wxMouseEventHandler(MainFrame::OnMouseDown), NULL, this);
  backgroundPanel_->Connect(wxID_ANY, wxEVT_LEFT_UP, wxMouseEventHandler(MainFrame::OnMouseUp), NULL, this);
  backgroundPanel_->Connect(wxID_ANY, wxEVT_MOTION, wxMouseEventHandler(MainFrame::OnMouseMove), NULL, this);

  optionPanel_ = new OptionPanel(this);

  resizerPanel_ = new ImagePanel(backgroundPanel_, wxID_ANY, wxPoint(0, 0), wxSize(50, 50));
  resizerPanel_->LoadImage(FilePaths::SkinDirectory + _T("/Resizer.png"));
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
  arrowButton_->SetSize(
    0,
    0,
    Styles::OptionPanel.ArrowButtonWidth,
    height);
  
  int bgPanelX = Styles::OptionPanel.ArrowButtonWidth + optionPanelOpenWidth_;
  int bgPanelWidth = width - bgPanelX;

  backgroundPanel_->SetSize(bgPanelX, 0, bgPanelWidth, height);
  
  iconPanel_->SetSize(
    Styles::MainPanel.PaddingLeft,
    Styles::MainPanel.PaddingRight,
    bgPanelWidth - Styles::MainPanel.PaddingWidth,
    height - Styles::MainPanel.PaddingHeight);
  
  resizerPanel_->Move(
    bgPanelWidth - resizerPanel_->GetRect().GetWidth(),
    height - resizerPanel_->GetRect().GetHeight());

  optionPanel_->SetSize(
    Styles::OptionPanel.ArrowButtonWidth,
    0,
    optionPanelOpenWidth_,
    height);
  
  needLayoutUpdate_ = false;
}


void MainFrame::UpdateLayout() {
  UpdateLayout(GetClientRect().GetWidth(), GetClientRect().GetHeight());
}


int MainFrame::GetMinHeight() {
  return iconPanel_->GetMinHeight() + Styles::MainPanel.PaddingHeight;
}


int MainFrame::GetMinWidth() {
  return GetMinHeight();
}


void MainFrame::OnEraseBackground(wxEraseEvent &evt) {

}


void MainFrame::OnPaint(wxPaintEvent& evt) {
  wxPaintDC dc(this);

  if (needLayoutUpdate_) UpdateLayout();
  if (needMaskUpdate_) UpdateMask();
}


void MainFrame::OnMove(wxMoveEvent& evt) {

}


void MainFrame::OnSize(wxSizeEvent& evt) {
  InvalidateLayout();
  InvalidateMask();
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

    int newHeight = windowDragData_.InitWindowSize.GetHeight() + mouseOffset.y;
    if (newHeight < GetMinHeight()) newHeight = GetMinHeight();

    int newWidth = windowDragData_.InitWindowSize.GetWidth() + mouseOffset.x;
    if (newWidth < GetMinWidth()) newWidth = GetMinWidth();

    SetSize(newWidth,
            newHeight);
    Update();
  }
}


void MainFrame::InvalidateLayout() {
  needLayoutUpdate_ = true;
  Refresh();
}


void MainFrame::InvalidateMask() {
  needMaskUpdate_ = true;
  Refresh();
}


void MainFrame::OnClose(wxCloseEvent& evt) {
  Destroy();
}


void MainFrame::OnImageButtonClick(wxCommandEvent& evt) {
  wxWindow* w = static_cast<wxWindow*>(evt.GetEventObject());
  wxWindowID id = w->GetId();

  if (id == ID_BUTTON_Arrow) ToggleOptionPanel();
}


void MainFrame::OpenOptionPanel(bool open) {
  if (open == optionPanelOpen_) return;

  optionPanelOpen_ = open;
  openCloseAnimationStartTime_ = gController->GetTimer();

  openCloseAnimationWindowRight_ = GetRect().GetRight();
  openCloseAnimationDockLeft_ = !false;

  if (optionPanelOpen_) {
    optionPanelOpenWidth_ = optionPanelMaxOpenWidth_;
  } else {
    optionPanelOpenWidth_ = 0;
  }

  int newWindowWidth = arrowButton_->GetSize().GetWidth() + optionPanelOpenWidth_ + backgroundPanel_->GetSize().GetWidth();

  if (openCloseAnimationDockLeft_) {
    SetSize(
      newWindowWidth, 
      GetSize().GetHeight());
  } else {
    SetSize(
      openCloseAnimationWindowRight_ - newWindowWidth + 1,
      GetRect().GetTop(),
      newWindowWidth, 
      GetSize().GetHeight());    
  }  

  InvalidateLayout();
  InvalidateMask();
  Update();
}


void MainFrame::CloseOptionPanel() {
  OpenOptionPanel(false);
}


void MainFrame::ToggleOptionPanel() {
  OpenOptionPanel(!optionPanelOpen_);
}