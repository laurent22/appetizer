/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "GroupEditorDialog.h"
#include <wx/filename.h>
#include <wx/dirdlg.h>
#include <wx/filedlg.h>
#include "../FilePaths.h"
#include "../Controller.h"
#include "../MessageBoxes.h"
#include "../utilities/VersionInfo.h"


extern Controller gController;


BEGIN_EVENT_TABLE(GroupEditorDialog, wxDialog)
  EVT_BUTTON(ID_GROUPEDIT_DLG_BUTTON_Cancel, GroupEditorDialog::OnCancelButtonClick)
  EVT_BUTTON(ID_GROUPEDIT_DLG_BUTTON_Save, GroupEditorDialog::OnSaveButtonClick)
  EVT_BUTTON(ID_GROUPEDIT_DLG_BUTTON_ChangeIcon, GroupEditorDialog::OnChangeIconButtonClick)  
  EVT_SHOW(GroupEditorDialog::OnShow)
END_EVENT_TABLE()


GroupEditorDialog::GroupEditorDialog(wxWindow* parent, wxWindowID id, const wxString& title, const wxPoint& pos, const wxSize& size, long style, const wxString& name)
: GroupEditorDialogBase(NULL, wxID_ANY, wxEmptyString) {
  Localize();
}


void GroupEditorDialog::Localize() {
  SetTitle(_("Shorcut Properties"));
  nameLabel->SetLabel(_("Name:"));
  saveButton->SetLabel(_("Save"));
  cancelButton->SetLabel(_("Cancel"));
}


void GroupEditorDialog::LoadFolderItem(FolderItemSP folderItem) {
  folderItem_ = folderItem;
  UpdateFromFolderItem();
}


void GroupEditorDialog::OnMenuItemClick(wxCommandEvent& evt) {
  FolderItemSP folderItem = gController.GetUser()->GetRootFolderItem()->GetChildById(evt.GetId());
  if (!folderItem.get()) {
    evt.Skip();
  } else {
    // @todo: Shouldn't change the group icon right now.
    // We should wait till Save()
    folderItem_->SetGroupIcon(folderItem);
    UpdateFromFolderItem();
  }
}


void GroupEditorDialog::OnChangeIconButtonClick(wxCommandEvent& evt) {
  wxMenu menu;
  
  for (int i = 0; i < folderItem_->ChildrenCount(); i++) {
    FolderItemSP child = folderItem_->GetChildAt(i);
    child->AppendAsMenuItem(&menu);
  }

  menu.Connect(
    wxID_ANY,
    wxEVT_COMMAND_MENU_SELECTED,
    wxCommandEventHandler(GroupEditorDialog::OnMenuItemClick),
    NULL,
    this);

  PopupMenu(&menu, wxPoint(changeIconButton->GetRect().GetX(), changeIconButton->GetRect().GetBottom()));
}


void GroupEditorDialog::OnCancelButtonClick(wxCommandEvent& evt) {
  EndDialog(wxID_CANCEL);
}


void GroupEditorDialog::OnSaveButtonClick(wxCommandEvent& evt) {
  //wxString folderItemFilePath = locationTextBox->GetValue();
  //wxString folderItemName = nameTextBox->GetValue();

  //wxFileName filename(FolderItem::ResolvePath(folderItemFilePath));
  //if (!filename.FileExists() && !wxFileName::DirExists(filename.GetFullPath())) {
  //  // If the shortcut location doesn't exist, just show a warning but allow
  //  // the user to continue. Invalid shortcuts are allowed since they
  //  // might be referencing files from a different computer.
  //  int result = MessageBoxes::ShowWarning(_("The shorcut location doesn't exist. Do you wish to continue?"), wxYES | wxNO);
  //  if (result == wxID_NO) return;
  //}

  //folderItem_->SetFilePath(folderItemFilePath);
  //folderItem_->SetName(folderItemName);

  EndDialog(wxID_OK);
}


void GroupEditorDialog::UpdateFromFolderItem() {  
  nameTextBox->SetValue(folderItem_->GetName());
  wxIconSP icon = folderItem_->GetIcon(LARGE_ICON_SIZE);
  wxBitmap bitmap(*icon);
  iconBitmap->SetBitmap(bitmap);
}


void GroupEditorDialog::OnShow(wxShowEvent& evt) {
  
}