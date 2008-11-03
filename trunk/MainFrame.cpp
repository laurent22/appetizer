/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <wx/dcbuffer.h>
#include <wx/cursor.h>
#include <wx/datetime.h>
#include <wx/filename.h>
#include <wx/menu.h>
#include "MainFrame.h"
#include "Log.h"
#include "Constants.h"
#include "Controller.h"
#include "MessageBoxes.h"
#include "FilePaths.h"
#include "Localization.h"
#include "Styles.h"
#include "utilities/XmlUtil.h"
#include "utilities/Updater.h"
#include "utilities/VersionInfo.h"
#include "gui/AboutDialog.h"
#include "bitmap_controls/ImageButton.h"




extern Controller gController;


BEGIN_EVENT_TABLE(MainFrame, wxFrame)
  EVT_SIZE(MainFrame::OnSize)
  EVT_MOVE(MainFrame::OnMove)
  EVT_ERASE_BACKGROUND(MainFrame::OnEraseBackground)
  EVT_PAINT(MainFrame::OnPaint)
  EVT_CLOSE(MainFrame::OnClose)
  EVT_COMMAND(wxID_ANY, wxeEVT_CLICK, MainFrame::OnImageButtonClick)
  EVT_IDLE(MainFrame::OnIdle)
  EVT_MOUSE_CAPTURE_LOST(MainFrame::OnMouseCaptureLost)
  EVT_MENU(ID_MENU_OptionPanel_Help, MainFrame::OnMenuHelp)
  EVT_MENU(ID_MENU_OptionPanel_About, MainFrame::OnMenuAbout)
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
  rotated_ = false;

  #ifdef __WXDEBUG__
    logWindow_ = new wxLogWindow(this, wxEmptyString, true);
  #endif // __WXDEBUG__

  firstIdleEventSent_ = false;
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
  arrowButton_->SetGrid(Styles::OptionPanel.ArrowButtonScaleGrid);

  arrowButton_->SetCursor(wxCursor(wxCURSOR_HAND));
  arrowButtonOpenIcon_ = new wxBitmap(FilePaths::IconsDirectory + _T("/ArrowButtonIconRight.png"), wxBITMAP_TYPE_PNG);
  arrowButtonCloseIcon_ = new wxBitmap(FilePaths::IconsDirectory + _T("/ArrowButtonIconLeft.png"), wxBITMAP_TYPE_PNG);
  arrowButton_->SetIcon(arrowButtonCloseIcon_, false);

  backgroundPanel_ = new NineSlicesPanel(this, wxID_ANY, wxPoint(0,0), wxSize(50,50));
  backgroundPanel_->LoadImage(FilePaths::SkinDirectory + _T("/BarBackground.png"));
  backgroundPanel_->SetGrid(Styles::MainPanel.ScaleGrid);

  backgroundPanel_->Connect(wxID_ANY, wxEVT_LEFT_DOWN, wxMouseEventHandler(MainFrame::OnMouseDown), NULL, this);
  backgroundPanel_->Connect(wxID_ANY, wxEVT_LEFT_UP, wxMouseEventHandler(MainFrame::OnMouseUp), NULL, this);
  backgroundPanel_->Connect(wxID_ANY, wxEVT_MOTION, wxMouseEventHandler(MainFrame::OnMouseMove), NULL, this);

  optionPanel_ = new OptionPanel(this);

  resizerPanel_ = new ImagePanel(backgroundPanel_, wxID_ANY, wxPoint(0, 0), wxSize(50, 50));
  resizerPanel_->LoadImage(FilePaths::SkinDirectory + _T("/Resizer.png"));
  resizerPanel_->FitToContent();
  resizerPanel_->SetCursor(wxCursor(wxCURSOR_HAND));

  resizerPanel_->Connect(wxID_ANY, wxEVT_LEFT_DOWN, wxMouseEventHandler(MainFrame::OnResizerMouseDown), NULL, this);
  resizerPanel_->Connect(wxID_ANY, wxEVT_LEFT_UP, wxMouseEventHandler(MainFrame::OnResizerMouseUp), NULL, this);
  resizerPanel_->Connect(wxID_ANY, wxEVT_MOTION, wxMouseEventHandler(MainFrame::OnResizerMouseMove), NULL, this);

  iconPanel_ = new IconPanel(backgroundPanel_, wxID_ANY, wxPoint(0, 0), wxSize(200, 200));

  iconPanel_->Connect(wxID_ANY, wxEVT_LEFT_DOWN, wxMouseEventHandler(MainFrame::OnMouseDown), NULL, this);
  iconPanel_->Connect(wxID_ANY, wxEVT_LEFT_UP, wxMouseEventHandler(MainFrame::OnMouseUp), NULL, this);
  iconPanel_->Connect(wxID_ANY, wxEVT_MOTION, wxMouseEventHandler(MainFrame::OnMouseMove), NULL, this);

  closeSideButton_ = new ImageButton(backgroundPanel_, ID_BUTTON_MainFrame_CloseButton);
  closeSideButton_->LoadImage(FilePaths::SkinDirectory + _T("/CloseButton"));
  closeSideButton_->FitToImage();
  closeSideButton_->SetCursor(wxCursor(wxCURSOR_HAND));

  helpSideButton_ = new ImageButton(backgroundPanel_, ID_BUTTON_MainFrame_HelpButton);
  helpSideButton_->LoadImage(FilePaths::SkinDirectory + _T("/HelpButton"));
  helpSideButton_->FitToImage();
  helpSideButton_->SetCursor(wxCursor(wxCURSOR_HAND));

  frameIcon_.LoadFile(FilePaths::IconsDirectory + _T("/Application.ico"), wxBITMAP_TYPE_ICO);
  taskBarIcon_.SetIcon(frameIcon_);

  SetIcon(frameIcon_);
  SetTitle(APPLICATION_NAME);



  TiXmlDocument doc(FilePaths::WindowFile.mb_str());
  doc.LoadFile();

  TiXmlElement* root = doc.FirstChildElement("Window");
  if (!root) {
    wlog("MainFrame: Could not load XML. No Window element found.");

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


void MainFrame::OnIdle(wxIdleEvent& evt) {
  if (!firstIdleEventSent_) {
    firstIdleEventSent_ = true;

    ilog("Update check...");

    wxDateTime now = wxDateTime::Now();
    wxDateTime nextUpdateTime = gController.GetUser()->GetSettings()->NextUpdateCheckTime;
    ilog(wxString::Format(_T("Now is %s"), now.Format()));
    ilog(wxString::Format(_T("Next update check on %s"), nextUpdateTime.Format()));

    if (nextUpdateTime.IsLaterThan(now)) return;

    gController.CheckForNewVersion(true);

    gController.GetUser()->GetSettings()->NextUpdateCheckTime = now;
    gController.GetUser()->GetSettings()->NextUpdateCheckTime.Add(wxTimeSpan(24 * CHECK_VERSION_DAY_INTERVAL));
  }  
}


void MainFrame::ApplySkin(const wxString& skinName) {
  FilePaths::SkinDirectory = FilePaths::BaseSkinDirectory + _T("/") + gController.GetUser()->GetSettings()->Skin;
  Styles::LoadSkinFile(FilePaths::SkinDirectory + _T("/") + SKIN_FILE_NAME);
  FilePaths::IconsDirectory = FilePaths::SkinDirectory + _T("/") + ICONS_FOLDER_NAME;

  wxDELETE(arrowButtonOpenIcon_);
  wxDELETE(arrowButtonCloseIcon_);

  maskNineSlices_.LoadImage(FilePaths::SkinDirectory + _T("/BarBackgroundRegion.png"), false);

  arrowButton_->LoadImage(FilePaths::SkinDirectory + _T("/ArrowButton"));
  arrowButtonOpenIcon_ = new wxBitmap(FilePaths::IconsDirectory + _T("/ArrowButtonIconRight.png"), wxBITMAP_TYPE_PNG);
  arrowButtonCloseIcon_ = new wxBitmap(FilePaths::IconsDirectory + _T("/ArrowButtonIconLeft.png"), wxBITMAP_TYPE_PNG);
  arrowButton_->SetIcon(arrowButtonCloseIcon_, false);

  backgroundPanel_->LoadImage(FilePaths::SkinDirectory + _T("/BarBackground.png"));
  resizerPanel_->LoadImage(FilePaths::SkinDirectory + _T("/Resizer.png"));
  closeSideButton_->LoadImage(FilePaths::SkinDirectory + _T("/CloseButton"));
  helpSideButton_->LoadImage(FilePaths::SkinDirectory + _T("/HelpButton"));
  iconPanel_->ApplySkin(skinName);
  optionPanel_->ApplySkin(skinName);

  frameIcon_.LoadFile(FilePaths::IconsDirectory + _T("/Application.ico"), wxBITMAP_TYPE_ICO);
  taskBarIcon_.SetIcon(frameIcon_);

  InvalidateMask();
  InvalidateLayout();
}


void MainFrame::SetRotated(bool rotated) {
  if (rotated == rotated_) return;
  rotated_ = rotated;

  optionPanel_->SetRotated(rotated);
  optionPanel_->UpdateLayout(); // We need a layout update to get a valid "required width" property
  arrowButton_->SetBitmapRotation(rotated ? -90 : 0);
  backgroundPanel_->SetBitmapRotation(rotated ? -90 : 0);
  resizerPanel_->SetBitmapRotation(rotated ? -90 : 0);

  UpdateLayout();
  UpdateMask();

  ToggleOptionPanel();
  ToggleOptionPanel();

  int newWidth = GetSize().GetWidth();
  int newHeight = GetSize().GetHeight();
  if (GetSize().GetWidth() < GetMinWidth()) newWidth = GetMinWidth(); 
  if (GetSize().GetHeight() < GetMinHeight()) newHeight = GetMinHeight();

  SetSize(newWidth, newHeight);
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
  int sideButtonGap = 0;

  if (rotated_) {
    int bgPanelHeight = height - Styles::OptionPanel.ArrowButtonWidth - optionPanelOpenWidth_;

    arrowButton_->SetSize(
      0,
      bgPanelHeight + optionPanelOpenWidth_,
      width,
      Styles::OptionPanel.ArrowButtonWidth);

    backgroundPanel_->SetSize(0, 0, width, bgPanelHeight);

    iconPanel_->SetSize(
      Styles::MainPanel.Padding.Bottom,
      Styles::MainPanel.Padding.Right,
      width - Styles::MainPanel.Padding.Height,
      bgPanelHeight - Styles::MainPanel.Padding.Width);
    
    resizerPanel_->Move(
      width - resizerPanel_->GetRect().GetWidth(),
      bgPanelHeight - resizerPanel_->GetRect().GetHeight());

    optionPanel_->SetSize(
      0,
      bgPanelHeight,
      width,
      optionPanelOpenWidth_);

    closeSideButton_->Move(
      Styles::MainPanel.Padding.Bottom,
      0);

    helpSideButton_->Move(
      closeSideButton_->GetRect().GetRight() + sideButtonGap,
      closeSideButton_->GetRect().GetTop());

  } else {
    arrowButton_->SetSize(
      0,
      0,
      Styles::OptionPanel.ArrowButtonWidth,
      height);
    
    int bgPanelX = Styles::OptionPanel.ArrowButtonWidth + optionPanelOpenWidth_;
    int bgPanelWidth = width - bgPanelX;

    backgroundPanel_->SetSize(bgPanelX, 0, bgPanelWidth, height);
    
    iconPanel_->SetSize(
      Styles::MainPanel.Padding.Left,
      Styles::MainPanel.Padding.Top,
      bgPanelWidth - Styles::MainPanel.Padding.Width,
      height - Styles::MainPanel.Padding.Height);
    
    resizerPanel_->Move(
      bgPanelWidth - resizerPanel_->GetRect().GetWidth(),
      height - resizerPanel_->GetRect().GetHeight());

    optionPanel_->SetSize(
      Styles::OptionPanel.ArrowButtonWidth,
      0,
      optionPanelOpenWidth_,
      height);

    closeSideButton_->Move(
      bgPanelWidth - closeSideButton_->GetSize().GetWidth(),
      Styles::MainPanel.Padding.Top);

    helpSideButton_->Move(
      closeSideButton_->GetRect().GetLeft(),
      closeSideButton_->GetRect().GetBottom() + sideButtonGap);
  }
  
  needLayoutUpdate_ = false;
}


void MainFrame::UpdateLayout() {
  UpdateLayout(GetClientRect().GetWidth(), GetClientRect().GetHeight());
}


int MainFrame::GetOptionPanelTotalWidth() {
  if (rotated_) {
    return arrowButton_->GetSize().GetHeight() + optionPanelOpenWidth_;
  } else {
    return arrowButton_->GetSize().GetWidth() + optionPanelOpenWidth_;
  }
}


int MainFrame::GetMinHeight() {
  if (rotated_) {
    return iconPanel_->GetMinHeight() + GetOptionPanelTotalWidth() + Styles::MainPanel.Padding.Width;
  } else {
    return iconPanel_->GetMinHeight() + Styles::MainPanel.Padding.Height;
  }
}


int MainFrame::GetMinWidth() {
  if (rotated_) {
    return iconPanel_->GetMinWidth() + Styles::MainPanel.Padding.Height;
  } else {
    return iconPanel_->GetMinWidth() + GetOptionPanelTotalWidth() + Styles::MainPanel.Padding.Width;
  }
}


int MainFrame::GetMaxWidth() {
  if (rotated_) {
    return iconPanel_->GetMaxWidth() + Styles::MainPanel.Padding.Height;
  } else {
    return iconPanel_->GetMaxWidth() + Styles::MainPanel.Padding.Width + GetOptionPanelTotalWidth();
  }
}


int MainFrame::GetMaxHeight() {
  if (rotated_) {
    return iconPanel_->GetMaxHeight() + Styles::MainPanel.Padding.Width + GetOptionPanelTotalWidth();
  } else {
    return iconPanel_->GetMaxHeight() + Styles::MainPanel.Padding.Height;
  }
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


void MainFrame::OnMouseCaptureLost(wxMouseCaptureLostEvent& evt) {
  // Any MSW application that uses wxWindow::CaptureMouse() must implement an 
  // wxEVT_MOUSE_CAPTURE_LOST event handler as of wxWidgets 2.8.0.
  wxWindow* w = static_cast<wxWindow*>(evt.GetEventObject());
  if (w->HasCapture()) w->ReleaseMouse();
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
    }
    
    // Note: There are no restrictions on maximum width

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

  if (rotated_) {
    XmlUtil::AppendTextElement(xmlRoot, "Width", GetRect().GetWidth());
    XmlUtil::AppendTextElement(xmlRoot, "Height", GetRect().GetHeight() - optionPanelOpenWidth_);
  } else {
    XmlUtil::AppendTextElement(xmlRoot, "Width", GetRect().GetWidth() - optionPanelOpenWidth_);
    XmlUtil::AppendTextElement(xmlRoot, "Height", GetRect().GetHeight());
  }
  
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

  FilePaths::CreateSettingsDirectory();
  doc.SaveFile(FilePaths::WindowFile.mb_str());

  wxDELETE(Localization::Instance);

  Destroy();
}


void MainFrame::OnMenuHelp(wxCommandEvent& evt) {  
  wxString helpFile = FilePaths::HelpDirectory + _T("/") + gController.GetUser()->GetSettings()->Locale + _T("/") + HELP_FILE_NAME;
  if (!wxFileName::FileExists(helpFile)) {
    // Default to english
    helpFile = FilePaths::HelpDirectory + _T("/en/") + HELP_FILE_NAME;
  }

  FolderItem::Launch(helpFile);
}


void MainFrame::OnMenuAbout(wxCommandEvent& evt) {
  AboutDialog aboutDialog;
  aboutDialog.LoadContent();
  aboutDialog.ShowModal();
}


void MainFrame::OnImageButtonClick(wxCommandEvent& evt) {
  wxWindow* w = static_cast<wxWindow*>(evt.GetEventObject());

  switch(w->GetId()) {
   
    case ID_BUTTON_Arrow:
      
      ToggleOptionPanel();
      break;

    case ID_BUTTON_MainFrame_CloseButton:
      
      Hide();
      break;

    case ID_BUTTON_MainFrame_HelpButton: {

      wxMenu menu;

      menu.Append(ID_MENU_OptionPanel_Help, LOC(_T("HelpPopup.Help")));
      menu.AppendSeparator();
      menu.Append(ID_MENU_OptionPanel_About, LOC1(_T("HelpPopup.About"), APPLICATION_NAME));
      
      wxPoint pos(w->GetRect().GetLeft(), w->GetRect().GetBottom());
      PopupMenu(&menu, pos);

    } break;

    default:

      evt.Skip();
      break;

  } 
}


void MainFrame::OpenOptionPanel(bool open) {
  if (open == optionPanelOpen_) return;

  optionPanelOpen_ = open;

  if (rotated_) {
    openCloseAnimationWindowRight_ = GetRect().GetBottom();
  } else {
    openCloseAnimationWindowRight_ = GetRect().GetRight();
  }

  if (optionPanelOpen_) {
    optionPanel_->UpdateLayout();
    optionPanelOpenWidth_ = optionPanel_->GetRequiredWidth();
  } else {
    optionPanelOpenWidth_ = 0;
  }

  int newWindowWidth;
  
  if (rotated_) {
    newWindowWidth = arrowButton_->GetSize().GetHeight() + optionPanelOpenWidth_ + backgroundPanel_->GetSize().GetHeight();
  } else {
    newWindowWidth = arrowButton_->GetSize().GetWidth() + optionPanelOpenWidth_ + backgroundPanel_->GetSize().GetWidth();
  }

  if (!rotated_) {
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
  } else {
    if (IsTopOfDisplay()) {
      SetSize(
        GetSize().GetWidth(),
        newWindowWidth);
    } else {
      SetSize(
        GetRect().GetLeft(),
        openCloseAnimationWindowRight_ - newWindowWidth + 1,
        GetSize().GetWidth(), 
        newWindowWidth);    
    } 
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