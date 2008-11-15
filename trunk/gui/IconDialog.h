/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#ifndef __IconDialog_H
#define __IconDialog_H


enum {
  ID_ICONDLG_BrowseButton,
  ID_ICONDLG_SourceTextBox
};


class IconDialog: public wxDialog {

public:

  IconDialog(wxWindow* parent, wxWindowID id = wxID_ANY, const wxString& title = _("Select an icon"), const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxSize(450,400), long style = wxDEFAULT_DIALOG_STYLE, const wxString& name = _T("dialogBox"));
  ~IconDialog();
  void Localize();
  void LoadIconSource(const wxString& filePath);
  wxString GetIconSource();
  int GetIconIndex();

protected:

  wxBoxSizer* topSizer;
  wxFlexGridSizer* verticalSizer;
  wxFlexGridSizer* iconSourceSizer;
  wxFlexGridSizer* buttonSizer;
  wxStaticText* iconSourceLabel;
  wxTextCtrl* iconSourceTextBox;
  wxButton* iconSourceButton;
  wxListView* listView;
  wxButton* okButton;
  wxButton* cancelButton;
  wxImageList* listViewImageList;

  wxString iconSourcePath_;

  void RefreshListView();

  void OnBrowseButtonClick(wxCommandEvent& evt);
  void OnSourceTextBoxEnter(wxCommandEvent& evt);
  void OnSize(wxSizeEvent& evt);
  void OnIconActivated(wxListEvent& evt);

  DECLARE_EVENT_TABLE();

};


#endif // __IconDialog_H