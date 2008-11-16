/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#ifndef __FileOrFolderDialog_H
#define __FileOrFolderDialog_H


#include "FileExplorerControl.h"


enum {
  ID_FILEEXPLODLG_ExplorerControl
};


class FileOrFolderDialog: public wxDialog {

public:

  FileOrFolderDialog(wxWindow* parent, wxWindowID id = wxID_ANY, const wxString& title = wxEmptyString, const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxSize(350, 400), long style = wxDEFAULT_DIALOG_STYLE, const wxString& name = _T("fileOrFolderDialog"));
  void Localize();
  wxString GetPath();
  void ExpandDirectory(const wxString& directory);

  void OnTreeSelectionChanged(wxTreeEvent& evt);
  void OnButtonClicked(wxCommandEvent& evt);
  void OnIdle(wxIdleEvent& evt);

protected:

  wxBoxSizer* topSizer;
  wxFlexGridSizer* verticalSizer;
  wxFlexGridSizer* buttonSizer;
  FileExplorerControl* explorerControl;
  wxButton* okButton;
  wxButton* cancelButton;    
  wxStaticText* statusLabel;
  bool firstIdleEvent_;

  static wxString lastSelectedDirectory_;

  DECLARE_EVENT_TABLE()

};


#endif // __AboutDialog_H