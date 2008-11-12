/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "precompiled.h"

#include "IconPanelFrame.h"
#include "FilePaths.h"
#include "Constants.h"


BEGIN_EVENT_TABLE(IconPanelFrame, wxFrame)
  EVT_ERASE_BACKGROUND(IconPanelFrame::OnEraseBackground)
  EVT_PAINT(IconPanelFrame::OnPaint)
END_EVENT_TABLE()



IconPanelFrame::IconPanelFrame(wxWindow* parent, wxWindowID id, const wxString& title, const wxPoint& pos, const wxSize& size)
: wxFrame(
  parent,
  id,
  title,
  pos,
  size,
  0 | wxFRAME_SHAPED | wxNO_BORDER | wxFRAME_NO_TASKBAR 
  )
{ 
  layoutInvalidated_ = true;
  maskInvalidated_ = true;

  maskNineSlices_.LoadImage(FilePaths::GetSkinDirectory() + _T("/BarBackgroundRegion.png"), false);

  backgroundPanel_ = new NineSlicesPanel(this, wxID_ANY, wxPoint(0,0), wxSize(10,10));
  backgroundPanel_->LoadImage(FilePaths::GetSkinFile(_T("IconPanelFrameBackground.png")));
  
  iconPanel_ = new IconPanel(backgroundPanel_, wxID_ANY, wxPoint(0,0), wxSize(10,10));  

  SetSize(200,200);
  iconPanel_->SetSize(10,10,180,180);
}


void IconPanelFrame::InvalidateLayout() {
  layoutInvalidated_ = true;
  Refresh();
}


void IconPanelFrame::InvalidateMask() {
  maskInvalidated_ = true;
  Refresh();
}


void IconPanelFrame::OnPaint(wxPaintEvent& evt) {
  wxPaintDC dc(this);

  if (layoutInvalidated_) UpdateLayout();
  if (maskInvalidated_) UpdateMask();
}


void IconPanelFrame::FitToIconPanel() {
  SetSize(
    iconPanel_->GetSize().GetWidth() + 16,
    iconPanel_->GetSize().GetHeight() + 16);
  InvalidateLayout();
  InvalidateMask();
}


void IconPanelFrame::OnEraseBackground(wxEraseEvent &evt) {

}


IconPanel* IconPanelFrame::GetIconPanel() {
  return iconPanel_;
}


void IconPanelFrame::UpdateMask() {
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

  maskInvalidated_ = false;
}


void IconPanelFrame::UpdateLayout() {  
  layoutInvalidated_ = false;

  iconPanel_->SetSize(
    8,8,GetSize().GetWidth() - 16, GetSize().GetHeight() - 16);
}