/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../precompiled.h"

#ifndef __GroupEditorDialog_H
#define __GroupEditorDialog_H


#include "GroupEditorDialogBase.h"
#include "../FolderItem.h"


class GroupEditorDialog: public GroupEditorDialogBase {

public:

  GroupEditorDialog(wxWindow* parent = NULL, wxWindowID id = wxID_ANY, const wxString& title = wxEmptyString, const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize, long style = wxDEFAULT_DIALOG_STYLE, const wxString& name = _T("dialogBox"));
  void LoadFolderItem(FolderItemSP folderItem);
  void Localize();

private:

  FolderItemSP folderItem_;
  FolderItemSP iconFolderItem_;
  bool hasSelectedDefaultIcon_;

  void UpdateFromFolderItem();

  void OnShow(wxShowEvent& evt);
  void OnMenuItemClick(wxCommandEvent& evt);
  void OnChangeIconButtonClick(wxCommandEvent& evt);
  void OnCancelButtonClick(wxCommandEvent& evt);
  void OnSaveButtonClick(wxCommandEvent& evt);

  DECLARE_EVENT_TABLE();

};


#endif // __GroupEditorDialog_H