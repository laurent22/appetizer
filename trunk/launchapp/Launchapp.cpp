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
  EVT_KEY_DOWN(Launchapp::textInput_keyDown)
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
  numberOfVisibleIcons_ = LAUNCHPAD_VISIBLE_ICONS;

  backgroundPanel_ = new NineSlicesPanel(this, wxID_ANY, wxPoint(0,0), wxSize(50,50));
  backgroundPanel_->SetName(_T("BackgroundPanel"));

  backgroundLeftImage_ = new NineSlicesPanel(this, wxID_ANY, wxPoint(0,0), wxSize(50,50));
  backgroundLeftImage_->SetName(_T("BackgroundPanelLeft"));

  textInput_ = new wxTextCtrl(backgroundPanel_, ID_TEXTCTRL_Launchapp_TextInput, _T(""), wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER);
  textInput_->Connect(wxID_ANY, wxEVT_KEY_DOWN, wxKeyEventHandler(Launchapp::textInput_keyDown), NULL, this);
  textInput_->SetEditable(true);

  int iconSize = wxGetApp().GetOSValidIconSize(EXTRA_LARGE_ICON_SIZE);
  wxString labelPosition = _T("bottom");

  iconPanel_ = new IconPanel(backgroundPanel_, wxID_ANY, wxPoint(0, 0), wxSize(200, 200));
  iconPanel_->SetFolderItemSource(ICON_PANEL_SOURCE_CUSTOM);
  iconPanel_->OverrideIconSize(iconSize);
  iconPanel_->OverrideLabelPosition(labelPosition);
  iconPanel_->ShowBrowseButton(false);
  iconPanel_->CenterIcons(true);
}


Launchapp::~Launchapp() {

}


void Launchapp::textInput_keyDown(wxKeyEvent& event) {
  int keyCode = event.GetKeyCode();
  if (keyCode == VK_LEFT || keyCode == 314) {
    iconPanel_->SelectPrevious();
  } else if (keyCode == VK_RIGHT || keyCode == 316) {
    iconPanel_->SelectNext();
  } else if (keyCode == VK_RETURN) {
    iconPanel_->LaunchSelected();
  } else {
    event.Skip();
  }
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
  int rectHeight = LAUNCHPAD_TEXTBOX_HEIGHT;
  textInput_->SetSize(rectX, rectY, rectWidth, rectHeight);

  iconPanel_->SetSize(Styles::MainPanel.Padding.Left, Styles::MainPanel.Padding.Top + rectHeight + LAUNCHPAD_TEXTBOX_GAP, iconPanelWidth_, 100);

  needLayoutUpdate_ = false;
}


void Launchapp::UpdateLayout() {
  UpdateLayout(GetClientRect().GetWidth(), GetClientRect().GetHeight());
}


void Launchapp::ApplySkin(wxBitmap* mainBackgroundBitmap) {
  //***************************************************************************
  // Update the size of the window according to the skin and
  // the number of icons to be displayed (LAUNCHPAD_VISIBLE_ICONS)
  //***************************************************************************
  int iconSize = wxGetApp().GetOSValidIconSize(EXTRA_LARGE_ICON_SIZE);
  wxString labelPosition = _T("bottom");

  int iconCount = numberOfVisibleIcons_;
  wxSize iconAreaSize = wxGetApp().GetIconAreaSize(iconSize, labelPosition);

  int width = iconAreaSize.GetWidth() * iconCount + Styles::InnerPanel.Padding.Width;
  iconPanelWidth_ = width;
  width += (Styles::MainPanel.SourceRectangle.GetRight() - Styles::InnerPanel.SourceRectangle.GetRight());
  width += (Styles::InnerPanel.SourceRectangle.GetLeft() - Styles::OptionPanel.SourceRectangle.GetWidth());

  int height = iconAreaSize.GetHeight() + Styles::InnerPanel.Padding.Height;
  height += (Styles::MainPanel.SourceRectangle.GetHeight() - Styles::InnerPanel.SourceRectangle.GetHeight());
  height += LAUNCHPAD_TEXTBOX_HEIGHT + LAUNCHPAD_TEXTBOX_GAP;

  SetSize(width, height);

  //***************************************************************************
  // Assign the skin files to the various window components
  //***************************************************************************
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

  if (text == wxEmptyString) {
    iconPanel_->ClearFolderItems();
    return;
  }

  std::vector<int> currentFolderItemIds = iconPanel_->GetFolderItemIds();
  FolderItemVector newFolderItems;

  appFolderItem* rootFolderItem = wxGetApp().GetUser()->GetRootFolderItem();
  FolderItemVector folderItemVector = rootFolderItem->GetChildren(true);

  int addedCount = 0;
  for (int i = 0; i < folderItemVector.size(); i++) {
    appFolderItem* folderItem = folderItemVector.at(i);
    if (folderItem->GetName().Lower().Find(text) != wxNOT_FOUND) {
      newFolderItems.push_back(folderItem);
      addedCount++;
      if (addedCount >= numberOfVisibleIcons_) break;
    }
  }

  bool areEqual = false;
  if (currentFolderItemIds.size() == newFolderItems.size()) {
    areEqual = true;
    for (int i = 0; i < newFolderItems.size(); i++) {
      appFolderItem* folderItem = newFolderItems.at(i);
      if (folderItem->GetId() != currentFolderItemIds.at(i)) {
        areEqual = false;
        break;
      }
    }
  }
  
  if (areEqual) return;

  iconPanel_->ClearFolderItems();
  for (int i = 0; i < newFolderItems.size(); i++) iconPanel_->AddFolderItem(newFolderItems.at(i)->GetId());
  iconPanel_->RefreshIcons();

  if (folderItemVector.size() > 0) iconPanel_->SetSelectedIndex(0);
}


void Launchapp::OnTextChange(wxCommandEvent& evt) {
  UpdateFolderItemsFromText();
}