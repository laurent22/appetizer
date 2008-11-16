/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#include "BetterMessageDialog.h"
#include "../utilities/IconGetter.h"
#include "../FilePaths.h"


BEGIN_EVENT_TABLE(BetterMessageDialog, wxDialog)
  EVT_BUTTON(wxID_ANY, BetterMessageDialog::OnButton)
END_EVENT_TABLE()


BetterMessageDialog* BetterMessageDialog::messageDialog_ = NULL;
bool BetterMessageDialog::lastCheckBoxState_ = false;


BetterMessageDialog::BetterMessageDialog(
    const wxString& message, 
    const wxString& title,
    int style,
    bool showCheckBox,
    bool checkBoxState,
    wxString checkBoxLabel,
    wxWindow* parent,
    int id,
    const wxPoint& pos,
    const wxSize& size,
    const wxString& name):
wxDialog(parent, id, title, pos, size, wxDEFAULT_DIALOG_STYLE, name) {

  int gap = 10;

  wxBoxSizer* topSizer = new wxBoxSizer(wxVERTICAL);

  wxStaticText* messageLabel = new wxStaticText(this, wxID_ANY, message, wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE);

  wxIcon* icon = NULL;
  wxStaticBitmap* iconBitmap = NULL;

  #ifdef __WINDOWS__
  // 1: "Exclamation" icon
  // 2: "Question" icon
  // 3: "Error" icon
  // 4: "Information" icon

  wxString user32DllPath = FilePaths::GetSystem32Directory() + _T("\\user32.dll");
  int iconSize = 32;

  if (style & wxICON_EXCLAMATION) {
    icon = IconGetter::GetExecutableIcon(user32DllPath, iconSize, 1);
  } else if (style & wxICON_ERROR) {
    icon = IconGetter::GetExecutableIcon(user32DllPath, iconSize, 3);
  } else if (style & wxICON_INFORMATION) {
    icon = IconGetter::GetExecutableIcon(user32DllPath, iconSize, 4);
  } else if (style & wxICON_QUESTION) {
    icon = IconGetter::GetExecutableIcon(user32DllPath, iconSize, 2);
  }
  #endif // __WINDOWS  

  if (icon) {
    iconBitmap = new wxStaticBitmap(this, -1, wxNullBitmap);
    iconBitmap->SetBitmap(*icon);     
  }

  wxFlexGridSizer* iconMessageSizer = new wxFlexGridSizer(1, icon ? 2 : 1, gap, gap);
  
  if (iconBitmap) iconMessageSizer->Add(iconBitmap, 1, 0, 0);
  iconMessageSizer->Add(messageLabel, 1, 0, 0);

  if (iconBitmap) {
    messageLabel->Wrap(size.GetWidth() - gap * 3 - icon->GetWidth());
  } else {
    messageLabel->Wrap(size.GetWidth() - gap * 2);
  } 

  wxSize messageSize = messageLabel->GetBestSize();

  topSizer->Add(iconMessageSizer, 1, wxEXPAND | wxLEFT | wxTOP | wxRIGHT, gap);

  checkbox_ = NULL;
  if (showCheckBox) {
    checkbox_ = new wxCheckBox(this, wxID_ANY, checkBoxLabel);
    checkbox_->SetValue(checkBoxState);
    topSizer->Add(checkbox_, 0, wxLEFT | wxTOP | wxRIGHT | wxALIGN_CENTER_HORIZONTAL, gap);
  }
  
  wxBoxSizer* buttonSizer = new wxBoxSizer(wxHORIZONTAL);
  
  std::vector<int> buttonFlags;
  buttonFlags.push_back(wxOK);
  buttonFlags.push_back(wxYES);
  buttonFlags.push_back(wxNO);
  buttonFlags.push_back(wxCANCEL);

  std::vector<int> buttonIds;
  buttonIds.push_back(wxID_OK);
  buttonIds.push_back(wxID_YES);
  buttonIds.push_back(wxID_NO);
  buttonIds.push_back(wxID_CANCEL);

  wxArrayString buttonLabels;
  buttonLabels.Add(_("OK"));
  buttonLabels.Add(_("Yes"));
  buttonLabels.Add(_("No"));
  buttonLabels.Add(_("Cancel"));

  bool isFirstButton = true;

  int buttonHeight = 0;
  for (int i = 0; i < buttonFlags.size(); i++) {
    int buttonFlag = buttonFlags.at(i);
    if (!(style & buttonFlag)) continue;

    int buttonId = buttonIds.at(i);
    wxString buttonLabel = buttonLabels[i];

    wxButton* button = new wxButton(this, buttonId, buttonLabel);
    buttonHeight = button->GetSize().GetHeight();
    buttonSizer->Add(button, 0, wxTOP | wxBOTTOM | (isFirstButton ? 0 : wxLEFT), gap);
    isFirstButton = false;
  }

  topSizer->Add(buttonSizer, 0, wxALIGN_CENTER);

  SetSizer(topSizer);

  topSizer->SetSizeHints(this);

  int h = gap + messageSize.GetHeight();
  if (icon) {
    if (icon->GetHeight() > messageSize.GetHeight()) h = gap + icon->GetHeight();
  }
  h += gap;
  if (checkbox_) h += checkbox_->GetSize().GetHeight();
  h += gap;
  h += buttonHeight;
  h += gap;

  wxDELETE(icon);

  SetClientSize(size.GetWidth(), h);
}


bool BetterMessageDialog::GetCheckBoxState() {
  return BetterMessageDialog::lastCheckBoxState_;
}


void BetterMessageDialog::OnButton(wxCommandEvent& evt) {
  int buttonId = evt.GetId();

  if (checkbox_) BetterMessageDialog::lastCheckBoxState_ = checkbox_->GetValue();

  if (buttonId == wxID_OK) {
    EndDialog(wxID_OK);
  } else if (buttonId == wxID_YES) {
    EndDialog(wxID_YES);
  } else if (buttonId == wxID_NO) {
    EndDialog(wxID_NO);
  } else if (buttonId == wxID_CANCEL) {
    EndDialog(wxID_CANCEL);
  } else {
    evt.Skip();
  }
}


int BetterMessageDialog::ShowMessage(
  const wxString& message, 
  const wxString& title,
  int style,
  bool showCheckBox,
  bool checkBoxState,
  wxString checkBoxLabel,
  wxWindow* parent) {

  if (BetterMessageDialog::messageDialog_) {
    BetterMessageDialog::messageDialog_->Destroy();
    BetterMessageDialog::messageDialog_ = NULL;
  }

  BetterMessageDialog::messageDialog_ = new BetterMessageDialog(message, title, style, showCheckBox, checkBoxState, checkBoxLabel, parent);
  int result = BetterMessageDialog::messageDialog_->ShowModal();
  if (BetterMessageDialog::messageDialog_) {
    BetterMessageDialog::messageDialog_->Destroy();
    BetterMessageDialog::messageDialog_ = NULL;
  }

  return result;
}


void BetterMessageDialog::DestroyInstance() {
  if (BetterMessageDialog::messageDialog_) {
    BetterMessageDialog::messageDialog_->Destroy();
    BetterMessageDialog::messageDialog_ = NULL;
  }
}