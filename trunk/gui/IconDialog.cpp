/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#include "IconDialog.h"
#include "../utilities/IconGetter.h"


BEGIN_EVENT_TABLE(IconDialog, wxDialog)
  EVT_BUTTON(ID_ICONDLG_BrowseButton, IconDialog::OnBrowseButtonClick)
  EVT_TEXT_ENTER(ID_ICONDLG_SourceTextBox, IconDialog::OnSourceTextBoxEnter)
  EVT_SIZE(IconDialog::OnSize)
  EVT_LIST_ITEM_ACTIVATED(wxID_ANY, IconDialog::OnIconActivated)
  EVT_LIST_ITEM_SELECTED(wxID_ANY, IconDialog::OnSelectionChanged)
  EVT_LIST_ITEM_DESELECTED(wxID_ANY, IconDialog::OnSelectionChanged)
  EVT_IDLE(IconDialog::OnIdle)
END_EVENT_TABLE()


IconDialog::IconDialog(wxWindow* parent, wxWindowID id, const wxString& title, const wxPoint& pos, const wxSize& size, long style, const wxString& name):
wxDialog(parent, id, title, pos, size, style | wxRESIZE_BORDER, name) {

  listViewImageList = NULL;

  int gap = 8;

  topSizer = new wxBoxSizer(wxHORIZONTAL);

  verticalSizer = new wxFlexGridSizer(3, 1, gap, gap);
  iconSourceSizer = new wxFlexGridSizer(1, 3, 0, gap);
  buttonSizer = new wxFlexGridSizer(2, 3, 0, gap);

  iconSourceLabel = new wxStaticText(this, wxID_ANY, wxEmptyString);
  iconSourceTextBox = new wxTextCtrl(this, ID_ICONDLG_SourceTextBox, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER);
  iconSourceButton = new wxButton(this, ID_ICONDLG_BrowseButton, _T("..."), wxDefaultPosition, wxSize(30, -1));

  listView = new wxListView(this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxLC_ALIGN_TOP |wxLC_ICON|wxSUNKEN_BORDER | wxLC_EDIT_LABELS);


  okButton = new wxButton(this, wxID_OK, wxEmptyString);
  cancelButton = new wxButton(this, wxID_CANCEL, wxEmptyString);


  iconSourceSizer->AddGrowableCol(1);
  iconSourceSizer->Add(iconSourceLabel, 0, wxALIGN_CENTER_VERTICAL);
  iconSourceSizer->Add(iconSourceTextBox, 0, wxEXPAND);
  iconSourceSizer->Add(iconSourceButton);



  buttonSizer->AddGrowableCol(0);

  buttonSizer->Add(new wxStaticText(this, wxID_ANY, _T(" ")), 0, wxEXPAND);
  buttonSizer->Add(okButton);
  buttonSizer->Add(cancelButton);




  verticalSizer->AddGrowableCol(0);
  verticalSizer->AddGrowableRow(1);
  
  verticalSizer->Add(iconSourceSizer, 1, wxEXPAND);
  verticalSizer->Add(listView, 1, wxEXPAND);
  verticalSizer->Add(buttonSizer, 1, wxEXPAND);



  topSizer->Add(verticalSizer, 1, wxEXPAND | wxALL, gap);
  SetSizer(topSizer);

  Localize();

  idleEventCount_ = 0;

  RefreshListView();
}


void IconDialog::OnIdle(wxIdleEvent& evt) {
  idleEventCount_++;

  if (idleEventCount_ == 2) {
    // Need to wait for at least two idle events before showing the window
    // otherwise it sometime fails in Release builds. I don't know why.
    OnBrowseButtonClick(wxCommandEvent());
    if (iconSourceTextBox->GetValue() == wxEmptyString) EndModal(wxID_CANCEL);
  }
}


IconDialog::~IconDialog() {
  wxDELETE(listViewImageList);
}


void IconDialog::Localize() {
  SetTitle(_("Select an icon"));
  iconSourceLabel->SetLabel(_("Icon file or library:"));
  okButton->SetLabel(_("OK"));
  cancelButton->SetLabel(_("Cancel"));
}


void IconDialog::OnSelectionChanged(wxListEvent& evt) {
  okButton->Enable(listView->GetSelectedItemCount() > 0);
}


wxString IconDialog::GetIconSource() {
  return iconSourceTextBox->GetValue();
}


int IconDialog::GetIconIndex() {
  return listView->GetFirstSelected();
}


void IconDialog::LoadIconSource(const wxString& filePath) {
  if (iconSourcePath_ == filePath) return;
  iconSourcePath_ = filePath;

  wxFileName filename(iconSourcePath_);
  filename.Normalize();
  wxString fileExtension = filename.GetExt().Lower();

  iconSourceTextBox->SetValue(iconSourcePath_);

  wxDELETE(listViewImageList);
  listViewImageList = new wxImageList(32, 32);

  if (fileExtension == _T("exe") || fileExtension == _T("dll") || fileExtension == _T("ico") || fileExtension == _T("icl")) {
    for (int i = 0; true; i++) {
      wxIcon* icon = IconGetter::GetExecutableIcon(iconSourcePath_, 32, i);
      if (!icon) break;
      listViewImageList->Add(*icon);
      wxDELETE(icon);
    }
  }

  listView->SetImageList(listViewImageList, wxIMAGE_LIST_NORMAL);

  okButton->Enable(listView->GetSelectedItemCount() > 0);

  RefreshListView();
}


void IconDialog::OnBrowseButtonClick(wxCommandEvent& evt) {
  wxString wildCard;
  wildCard = wxString::Format(_T("%s%s"),   _("All supported files"), _T(" (*.ico, *.exe, *.dll, *.icl)|*.ico;*.exe;*.dll;*.icl"));
  wildCard += wxString::Format(_T("|%s%s"), _("Icons"),               _T(" (*.ico)|*.ico"));
  wildCard += wxString::Format(_T("|%s%s"), _("Executables"),         _T(" (*.exe)|*.exe"));
  wildCard += wxString::Format(_T("|%s%s"), _("Dynamic libraries"),   _T(" (*.dll)|*.dll"));
  wildCard += wxString::Format(_T("|%s%s"), _("Icon libraries"),      _T(" (*.icl)|*.icl"));
  wildCard += wxString::Format(_T("|%s%s"), _("All files"),           _T(" (*.*)|*.*"));

  wxFileDialog fileDialog(this, _("Select an icon file or library"), wxEmptyString, wxEmptyString, wildCard);
  int result = fileDialog.ShowModal();
  if (result != wxID_OK) return;

  LoadIconSource(fileDialog.GetPath());
}


void IconDialog::OnSourceTextBoxEnter(wxCommandEvent& evt) {
  LoadIconSource(iconSourceTextBox->GetValue());
}


void IconDialog::OnSize(wxSizeEvent& evt) {
  wxDialog::OnSize(evt);
  RefreshListView();
}


void IconDialog::OnIconActivated(wxListEvent& evt) {
  EndDialog(wxID_OK);
}


void IconDialog::RefreshListView() {
  if (!listView || !listViewImageList) return;

  listView->ClearAll();
  for (int i = 0; i < listViewImageList->GetImageCount(); i++) {
    listView->InsertItem(i, i);
  }

  okButton->Enable(listView->GetSelectedItemCount() > 0);
}