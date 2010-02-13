/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#include "Launchapp.h"
#include "../Styles.h"
#include "../Constants.h"
#include "../FilePaths.h"
#include "../Enumerations.h"
#include "../User.h"
#include "../FolderItem.h"
#include "../MiniLaunchBar.h"


BEGIN_EVENT_TABLE(Launchapp, wxFrame)
  EVT_PAINT(Launchapp::OnPaint)
  EVT_TEXT(ID_TEXTCTRL_Launchapp_TextInput, Launchapp::OnTextChange)  
END_EVENT_TABLE()


Launchapp::Launchapp(wxWindow* parent)
: wxFrame(
  parent,
  wxID_ANY,
  wxEmptyString,
  wxDefaultPosition,
  wxDefaultSize,
  wxFRAME_SHAPED | wxNO_BORDER
  )
{  
  textInput_ = NULL;

  backgroundPanel_ = new NineSlicesPanel(this, wxID_ANY, wxPoint(0,0), wxSize(50,50));
  backgroundPanel_->SetName(_T("BackgroundPanel"));

  backgroundLeftImage_ = new NineSlicesPanel(this, wxID_ANY, wxPoint(0,0), wxSize(50,50));
  backgroundLeftImage_->SetName(_T("BackgroundPanelLeft"));

  textInput_ = new wxTextCtrl(backgroundPanel_, ID_TEXTCTRL_Launchapp_TextInput, _T("test"));
  textInput_->SetEditable(true);

  iconPanel_ = new IconPanel(backgroundPanel_, wxID_ANY, wxPoint(0, 0), wxSize(200, 200));
  iconPanel_->SetFolderItemSource(ICON_PANEL_SOURCE_CUSTOM);
}



Launchapp::~Launchapp() {

}


void Launchapp::OnPaint(wxPaintEvent& evt) {
  wxPaintDC dc(this);

  if (needLayoutUpdate_) UpdateLayout();
  if (needMaskUpdate_) UpdateMask();
}


void Launchapp::InvalidateLayout() {
  needLayoutUpdate_ = true;
  Refresh();
}


void Launchapp::InvalidateMask() {
  needMaskUpdate_ = true;
  Refresh();
}


void Launchapp::UpdateMask() {
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


void Launchapp::UpdateLayout(int width, int height) {
  backgroundLeftImage_->SetSize(
    0,
    0,
    Styles::ArrowButton.SourceRectangle.GetWidth(),
    height);
  
  int bgPanelX = Styles::ArrowButton.SourceRectangle.GetWidth();
  int bgPanelWidth = width - bgPanelX;

  backgroundPanel_->SetSize(bgPanelX, 0, bgPanelWidth, height);

  int rectX = Styles::InnerPanel.SourceRectangle.GetLeft() - Styles::MainPanel.SourceRectangle.GetLeft() + Styles::InnerPanel.Padding.Left;
  int rectY = Styles::InnerPanel.SourceRectangle.GetTop() - Styles::MainPanel.SourceRectangle.GetTop() + Styles::InnerPanel.Padding.Top;
  int rectWidth = bgPanelWidth - rectX - (Styles::MainPanel.SourceRectangle.GetRight() - Styles::InnerPanel.SourceRectangle.GetRight()) - Styles::InnerPanel.Padding.Right;
  int rectHeight = 24;
  textInput_->SetSize(
    rectX,
    rectY,
    rectWidth,
    rectHeight);

  iconPanel_->SetSize(rectX, rectY + rectHeight + 6, rectWidth, 100);

  needLayoutUpdate_ = false;
}


void Launchapp::UpdateLayout() {
  UpdateLayout(GetClientRect().GetWidth(), GetClientRect().GetHeight());
}


void Launchapp::ApplySkin(wxBitmap* mainBackgroundBitmap) {
  wxMemoryDC targetDC;
  wxMemoryDC mainBackgroundDC;
  mainBackgroundDC.SelectObject(*mainBackgroundBitmap);  
  wxBitmap* barBackgroundBitmap = new wxBitmap(Styles::MainPanel.SourceRectangle.GetWidth(), Styles::MainPanel.SourceRectangle.GetHeight());  
  targetDC.SelectObject(*barBackgroundBitmap);
  targetDC.Blit(0, 0, barBackgroundBitmap->GetWidth(), barBackgroundBitmap->GetHeight(), &mainBackgroundDC, Styles::MainPanel.SourceRectangle.GetX(), Styles::MainPanel.SourceRectangle.GetY());  
  targetDC.SelectObject(wxNullBitmap);
  backgroundPanel_->LoadImage(barBackgroundBitmap);


  wxBitmap* arrowBitmapUp = new wxBitmap(Styles::ArrowButton.SourceRectangle.GetWidth(), Styles::ArrowButton.SourceRectangle.GetHeight());  
  targetDC.SelectObject(*arrowBitmapUp);
  targetDC.Blit(0, 0, arrowBitmapUp->GetWidth(), arrowBitmapUp->GetHeight(), &mainBackgroundDC, Styles::ArrowButton.SourceRectangle.GetX(), Styles::ArrowButton.SourceRectangle.GetY());  
  targetDC.SelectObject(wxNullBitmap);

  backgroundLeftImage_->LoadImage(arrowBitmapUp);

  maskNineSlices_.LoadImage(FilePaths::GetSkinDirectory() + _T("/MainBackground.png"), false);
}


void Launchapp::UpdateFolderItemsFromText() {
  if (!textInput_) return;

  wxString text = textInput_->GetValue().Lower();

  appFolderItem* rootFolderItem = wxGetApp().GetUser()->GetRootFolderItem();
  FolderItemVector folderItemVector = rootFolderItem->GetChildren(true);
  
  iconPanel_->ClearFolderItems();

  for (int i = 0; i < folderItemVector.size(); i++) {
    appFolderItem* folderItem = folderItemVector.at(i);
    if (folderItem->GetName().Lower().Find(text) != wxNOT_FOUND) {
      iconPanel_->AddFolderItem(folderItem->GetId());
    }
  }

  iconPanel_->RefreshIcons();
}


void Launchapp::OnTextChange(wxCommandEvent& evt) {
  UpdateFolderItemsFromText();
}