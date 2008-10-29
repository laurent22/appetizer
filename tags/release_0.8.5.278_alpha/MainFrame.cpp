#include <wx/dcbuffer.h>
#include "MainFrame.h"
#include "Constants.h"
#include "Controller.h"
#include "FilePaths.h"
#include "Styles.h"
#include "utilities/DelphiToolsInterface.h"
#include "utilities/XmlUtil.h"
#include "bitmap_controls/ImageButton.h"




extern Controller gController;


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
  0 | wxFRAME_SHAPED | wxNO_BORDER | wxFRAME_NO_TASKBAR 
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
  arrowButtonOpenIcon_ = new wxBitmap(FilePaths::IconsDirectory + _T("/ArrowButtonIconRight.png"), wxBITMAP_TYPE_PNG);
  arrowButtonCloseIcon_ = new wxBitmap(FilePaths::IconsDirectory + _T("/ArrowButtonIconLeft.png"), wxBITMAP_TYPE_PNG);
  arrowButton_->SetIcon(arrowButtonCloseIcon_, false);

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

  frameIcon_.LoadFile(FilePaths::IconsDirectory + _T("/Application.ico"), wxBITMAP_TYPE_ICO);
  taskBarIcon_.SetIcon(frameIcon_);

  SetIcon(frameIcon_);
  SetTitle(APPLICATION_NAME);



  TiXmlDocument doc(FilePaths::WindowFile.mb_str());
  doc.LoadFile();

  TiXmlElement* root = doc.FirstChildElement("Window");
  if (!root) {
    wxLogDebug(_T("MainFrame: Could not load XML. No Window element found."));

    SetSize(0, 0, MAIN_FRAME_DEFAULT_WIDTH, MAIN_FRAME_DEFAULT_HEIGHT);
  } else {
    TiXmlHandle handle(root);
    int displayIndex = XmlUtil::ReadElementTextAsInt(handle, "DisplayIndex", 0);
    bool isLeftOfDisplay = XmlUtil::ReadElementTextAsBool(handle, "IsLeftOfDisplay", true);
    bool isTopOfDisplay = XmlUtil::ReadElementTextAsBool(handle, "IsTopOfDisplay", true);
    int horizontalGap = XmlUtil::ReadElementTextAsInt(handle, "HorizontalGap", 0);
    int verticalGap = XmlUtil::ReadElementTextAsInt(handle, "VerticalGap", 0);
    int width = XmlUtil::ReadElementTextAsInt(handle, "Width", MAIN_FRAME_DEFAULT_WIDTH);
    int height = XmlUtil::ReadElementTextAsInt(handle, "Height", MAIN_FRAME_DEFAULT_HEIGHT);

    if (displayIndex >= wxDisplay::GetCount()) displayIndex = wxDisplay::GetCount() - 1;
    
    wxDisplay display(displayIndex);

    int x = 0;
    int y = 0;

    if (isLeftOfDisplay) {
      x = display.GetGeometry().GetLeft() + horizontalGap;
    } else {
      x = display.GetGeometry().GetRight() - horizontalGap - width;
    }

    if (isTopOfDisplay) {
      y = display.GetGeometry().GetTop() + verticalGap;
    } else {
      y = display.GetGeometry().GetBottom() - verticalGap - height;
    }

    ConvertToWindowValidCoordinates(&display, x, y, width, height);

    SetSize(x, y, width, height);
  }
} 


OptionPanel* MainFrame::GetOptionPanel() {
  return optionPanel_;
}


void MainFrame::Localize() {
  if (optionPanel_) optionPanel_->Localize();
}


void MainFrame::ConvertToWindowValidCoordinates(const wxDisplay* display, int& x, int& y, int& width, int& height) {  
  int displayX = x - display->GetGeometry().GetLeft();
  int displayY = y - display->GetGeometry().GetTop();

  if (displayX + width < WINDOW_VISIBILITY_BORDER) {
    displayX = WINDOW_VISIBILITY_BORDER - width;
  } else if (displayX > display->GetGeometry().GetWidth() - WINDOW_VISIBILITY_BORDER) {
    displayX = display->GetGeometry().GetWidth() - WINDOW_VISIBILITY_BORDER;
  }

  if (displayY + height < WINDOW_VISIBILITY_BORDER) {
    displayY = WINDOW_VISIBILITY_BORDER - height;
  } else if (displayY > display->GetGeometry().GetHeight() - WINDOW_VISIBILITY_BORDER) {
    displayY = display->GetGeometry().GetHeight() - WINDOW_VISIBILITY_BORDER;
  }

  x = displayX + display->GetGeometry().GetLeft();
  y = displayY + display->GetGeometry().GetTop();
}


IconPanel* MainFrame::GetIconPanel() { return iconPanel_; }


int MainFrame::GetDisplayIndex() {
  int displayCount = wxDisplay::GetCount();
  if (displayCount <= 1) return 0;

  wxPoint centerPoint(
    GetRect().GetLeft() + floor((double)GetRect().GetWidth() / 2),
    GetRect().GetTop() + floor((double)GetRect().GetHeight() / 2));

  int index = wxDisplay::GetFromPoint(centerPoint);
  if (index < 0) return 0;
  return index;
}


bool MainFrame::IsLeftOfDisplay() {
  wxDisplay display(GetDisplayIndex());

  wxRect geometry = display.GetGeometry();
  wxPoint centerPoint(
    GetRect().GetLeft() + floor((double)GetRect().GetWidth() / 2),
    GetRect().GetTop() + floor((double)GetRect().GetHeight() / 2));

  wxRect leftRect(geometry.GetLeft(), geometry.GetTop(), geometry.GetWidth() / 2, geometry.GetHeight());

  return leftRect.Contains(centerPoint);
}


bool MainFrame::IsTopOfDisplay() {
  wxDisplay display(GetDisplayIndex());

  wxRect geometry = display.GetGeometry();
  wxPoint centerPoint(
    GetRect().GetLeft() + floor((double)GetRect().GetWidth() / 2),
    GetRect().GetTop() + floor((double)GetRect().GetHeight() / 2));

  wxRect topRect(geometry.GetLeft(), geometry.GetTop(), geometry.GetWidth(), geometry.GetHeight() / 2);

  return topRect.Contains(centerPoint);
}


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


int MainFrame::GetOptionPanelTotalWidth() {
  return arrowButton_->GetSize().GetWidth() + optionPanelOpenWidth_;
}


int MainFrame::GetMinHeight() {
  return iconPanel_->GetMinHeight() + Styles::MainPanel.PaddingHeight;
}


int MainFrame::GetMinWidth() {
  return iconPanel_->GetMinWidth() + GetOptionPanelTotalWidth() + Styles::MainPanel.PaddingWidth;
}


int MainFrame::GetMaxWidth() {
  return iconPanel_->GetMaxWidth() + Styles::MainPanel.PaddingWidth + GetOptionPanelTotalWidth();
}


int MainFrame::GetMaxHeight() {
  return iconPanel_->GetMaxHeight() + Styles::MainPanel.PaddingHeight;
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
    if (newHeight < GetMinHeight()) {
      newHeight = GetMinHeight();
    } else if (newHeight > GetMaxHeight()) {
      newHeight = GetMaxHeight();
    }

    int newWidth = windowDragData_.InitWindowSize.GetWidth() + mouseOffset.x;
    if (newWidth < GetMinWidth()) {
      newWidth = GetMinWidth();
    } else if (newWidth > GetMaxWidth()) {
      // no restriction on maximum width
    }

    if (newWidth == GetSize().GetWidth() && newHeight == GetSize().GetHeight()) return;

    SetSize(newWidth, newHeight);

    int previousPanelWidth = optionPanel_->GetRequiredWidth();

    Update();
    iconPanel_->Update();

    if (optionPanelOpen_) {
      if (previousPanelWidth != optionPanel_->GetRequiredWidth()) {
        optionPanelOpenWidth_ = optionPanel_->GetRequiredWidth();
        InvalidateLayout();
        InvalidateMask();
        Update();
      }
    }

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
  gController.GetUser()->Save();

  TiXmlDocument doc;
  doc.LinkEndChild(new TiXmlDeclaration("1.0", "", ""));
  TiXmlElement* xmlRoot = new TiXmlElement("Window");;
  xmlRoot->SetAttribute("version", "1.0");
  doc.LinkEndChild(xmlRoot);

  XmlUtil::AppendTextElement(xmlRoot, "DisplayIndex", GetDisplayIndex());
  XmlUtil::AppendTextElement(xmlRoot, "IsLeftOfDisplay", IsLeftOfDisplay());
  XmlUtil::AppendTextElement(xmlRoot, "IsTopOfDisplay", IsTopOfDisplay());
  XmlUtil::AppendTextElement(xmlRoot, "Width", GetRect().GetWidth() - optionPanelOpenWidth_);
  XmlUtil::AppendTextElement(xmlRoot, "Height", GetRect().GetHeight());
  
  int hGap;
  int vGap;

  wxDisplay display(GetDisplayIndex());

  if (IsLeftOfDisplay()) {
    hGap = GetRect().GetLeft() - display.GetGeometry().GetLeft();
  } else {    
    hGap = display.GetGeometry().GetRight() - GetRect().GetRight();
  }

  if (IsTopOfDisplay()) {
    vGap = GetRect().GetTop() - display.GetGeometry().GetTop();
  } else {    
    vGap = display.GetGeometry().GetBottom() - GetRect().GetBottom();
  }

  XmlUtil::AppendTextElement(xmlRoot, "HorizontalGap", hGap);
  XmlUtil::AppendTextElement(xmlRoot, "VerticalGap", vGap);

  doc.SaveFile(FilePaths::WindowFile.mb_str());

  DelphiToolsInterface::UnloadDLL();

  Destroy();
}


void MainFrame::OnImageButtonClick(wxCommandEvent& evt) {
  wxWindow* w = static_cast<wxWindow*>(evt.GetEventObject());

  switch(w->GetId()) {
   
    case ID_BUTTON_Arrow:
      
      ToggleOptionPanel();
      break;

    default:

      evt.Skip();
      break;

  } 
}


void MainFrame::OpenOptionPanel(bool open) {
  if (open == optionPanelOpen_) return;

  optionPanelOpen_ = open;
  openCloseAnimationStartTime_ = gController.GetTimer();

  openCloseAnimationWindowRight_ = GetRect().GetRight();
  openCloseAnimationDockLeft_ = !false;

  if (optionPanelOpen_) {
    optionPanel_->UpdateLayout();
    optionPanelOpenWidth_ = optionPanel_->GetRequiredWidth();
  } else {
    optionPanelOpenWidth_ = 0;
  }

  int newWindowWidth = arrowButton_->GetSize().GetWidth() + optionPanelOpenWidth_ + backgroundPanel_->GetSize().GetWidth();

  if (IsLeftOfDisplay()) {
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

  if (optionPanelOpen_) {
    arrowButton_->SetIcon(arrowButtonOpenIcon_, false);
  } else {
    arrowButton_->SetIcon(arrowButtonCloseIcon_, false);
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


MainFrame::~MainFrame() {
  arrowButton_->SetIcon(NULL);
  wxDELETE(arrowButtonCloseIcon_);
  wxDELETE(arrowButtonOpenIcon_);
}