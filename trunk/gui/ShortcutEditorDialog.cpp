/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../precompiled.h"

#include "ShortcutEditorDialog.h"
#include "../FilePaths.h"
#include "../MiniLaunchBar.h"
#include "../MessageBoxes.h"
#include "../utilities/VersionInfo.h"


BEGIN_EVENT_TABLE(ShortcutEditorDialog, wxDialog)
  EVT_BUTTON(ID_BUTTON_Cancel, ShortcutEditorDialog::OnCancelButtonClick)
  EVT_BUTTON(ID_BUTTON_Save, ShortcutEditorDialog::OnSaveButtonClick)
  EVT_BUTTON(ID_BUTTON_SelectFile, ShortcutEditorDialog::OnSelectFileButtonClick)
  EVT_BUTTON(ID_BUTTON_SelectFolder, ShortcutEditorDialog::OnSelectFolderButtonClick)
END_EVENT_TABLE()


ShortcutEditorDialog::ShortcutEditorDialog()
: ShortcutEditorDialogBase(NULL, wxID_ANY, wxEmptyString) {
  selectFolderButton->SetBitmapLabel(wxBitmap(FilePaths::GetSkinFile(_T("FolderIcon16.png")), wxBITMAP_TYPE_PNG));
  selectFileButton->SetBitmapLabel(wxBitmap(FilePaths::GetSkinFile(_T("FileIcon16.png")), wxBITMAP_TYPE_PNG));
  Localize();
}


void ShortcutEditorDialog::Localize() {
  SetTitle(_("Shorcut Properties"));
  nameLabel->SetLabel(_("Name:"));
  locationLabel->SetLabel(_("Location:"));
  parametersLabel->SetLabel(_("Parameters:"));
  saveButton->SetLabel(_("Save"));
  cancelButton->SetLabel(_("Cancel"));
  selectFileButton->SetToolTip(_("Select a file"));
  selectFolderButton->SetToolTip(_("Select a folder"));
}


void ShortcutEditorDialog::LoadFolderItem(FolderItemSP folderItem) {
  folderItem_ = folderItem;
  Localize();
  UpdateFromFolderItem();
}


void ShortcutEditorDialog::OnCancelButtonClick(wxCommandEvent& evt) {
  EndDialog(wxID_CANCEL);
}


void ShortcutEditorDialog::OnSaveButtonClick(wxCommandEvent& evt) {
  wxString folderItemFilePath = locationTextBox->GetValue();
  wxString folderItemName = nameTextBox->GetValue();

  wxFileName filename(FolderItem::ResolvePath(folderItemFilePath));
  if (!filename.FileExists() && !wxFileName::DirExists(filename.GetFullPath())) {
    // If the shortcut location doesn't exist, just show a warning but allow
    // the user to continue. Invalid shortcuts are allowed since they
    // might be referencing files from a different computer.
    int result = MessageBoxes::ShowWarning(_("The shorcut location doesn't exist. Do you wish to continue?"), wxYES | wxNO);
    if (result == wxID_NO) return;
  }

  folderItem_->SetFilePath(folderItemFilePath);
  folderItem_->SetName(folderItemName);
  folderItem_->SetParameters(parametersTextBox->GetValue());

  EndDialog(wxID_OK);
}


void ShortcutEditorDialog::UpdateFromFolderItem() {  
  nameTextBox->SetValue(folderItem_->GetName());
  locationTextBox->SetValue(folderItem_->GetFilePath());
  parametersTextBox->SetValue(folderItem_->GetParameters());
}


void ShortcutEditorDialog::OnSelectFileButtonClick(wxCommandEvent& evt) {
  wxFileDialog fileDialog(this, _("Select file"));
  int result = fileDialog.ShowModal();
  if (result != wxID_OK) return;

  wxString newValue = FolderItem::ConvertToRelativePath(fileDialog.GetPath());
  if (locationTextBox->GetValue() == newValue) return;

  locationTextBox->SetValue(newValue); 
  
  nameTextBox->SetValue(VersionInfo::GetFileDescription(FolderItem::ResolvePath(newValue)));
}


void ShortcutEditorDialog::OnSelectFolderButtonClick(wxCommandEvent& evt) {
  wxDirDialog dirDialog(this, _("Select folder"));
  int result = dirDialog.ShowModal();
  if (result != wxID_OK) return;

  wxString newValue = FolderItem::ConvertToRelativePath(dirDialog.GetPath());
  if (locationTextBox->GetValue() == newValue) return;

  locationTextBox->SetValue(newValue);

  nameTextBox->SetValue(VersionInfo::GetFileDescription(FolderItem::ResolvePath(newValue)));
}


