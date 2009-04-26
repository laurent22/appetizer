/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#include "ToolTipFrame.h"
#include "FilePaths.h"
#include "MiniLaunchBar.h"



ToolTipFrame::ToolTipFrame(wxWindow* parent, wxWindowID id):
wxFrame(parent, id, _T("tooltip"), wxDefaultPosition, wxDefaultSize, wxBORDER_NONE) {

  backgroundPanel_ = new NineSlicesPanel(this, wxID_ANY, wxPoint(0,0), wxSize(50,50));
  backgroundPanel_->SetName(_T("ToolTip_BackgroundPanel"));

  // TODO: Make a static text bitmap controls, which manually draw the text using DrawText()

  label_ = new wxStaticText(backgroundPanel_, wxID_ANY, _T(""));

  ApplySkin();
}


ToolTipFrame::~ToolTipFrame() {

}


void ToolTipFrame::ApplySkin(const wxString& skinName) {
  wxString tSkinName;

  if (skinName == wxEmptyString) {
    tSkinName = wxGetApp().GetUser()->GetSettings()->GetString(_T("Skin"));
  } else {
    tSkinName = skinName;
  }

  backgroundPanel_->LoadImage(FilePaths::GetSkinFile(_T("ToolTipBackground.png")));
}


void ToolTipFrame::ShowToolTip(const wxString& text, const wxRect& objectScreenRect) {
  int labelMaxWidth = 150;
  int gap = 10;

  label_->SetLabel(text);
  
  wxSize labelSize = label_->GetBestSize();

  if (labelSize.GetWidth() > labelMaxWidth) {
    label_->Wrap(labelMaxWidth);
    labelSize = label_->GetBestSize();
  }

  SetSize(labelSize.GetWidth() + gap * 2, labelSize.GetHeight() + gap * 2);
  Move(objectScreenRect.GetLeft(), objectScreenRect.GetTop());
  backgroundPanel_->SetSize(GetSize().GetWidth(), GetSize().GetHeight());

  label_->SetSize(labelSize.GetWidth(), labelSize.GetHeight());
  label_->SetPosition(wxPoint(gap, gap));

  Show();
}


void ToolTipFrame::HideToolTip() {
  Hide();
}