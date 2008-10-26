#include "ShortcutEditorDialog.h"
#include "../Controller.h"
#include <wx/filename.h>
#include "../FilePaths.h"
#include <wx/dirdlg.h>
#include <wx/filedlg.h>



extern Controller gController;


BEGIN_EVENT_TABLE(ShortcutEditorDialog, wxDialog)
  EVT_BUTTON(ID_BUTTON_Cancel, ShortcutEditorDialog::OnCancelButtonClick)
  EVT_BUTTON(ID_BUTTON_Save, ShortcutEditorDialog::OnSaveButtonClick)
  EVT_BUTTON(ID_BUTTON_SelectFile, ShortcutEditorDialog::OnSelectFileButtonClick)
  EVT_BUTTON(ID_BUTTON_SelectFolder, ShortcutEditorDialog::OnSelectFolderButtonClick)
END_EVENT_TABLE()


ShortcutEditorDialog::ShortcutEditorDialog()
: ShortcutEditorDialogBase(NULL, wxID_ANY, wxEmptyString) {
  selectFolderButton->SetBitmapLabel(wxBitmap(FilePaths::IconsDirectory + _T("/FolderIcon.png"), wxBITMAP_TYPE_PNG));
  selectFileButton->SetBitmapLabel(wxBitmap(FilePaths::IconsDirectory + _T("/FileIcon.png"), wxBITMAP_TYPE_PNG));
}


void ShortcutEditorDialog::LoadFolderItem(FolderItemSP folderItem) {
  folderItem_ = folderItem;
  UpdateFromFolderItem();
}


void ShortcutEditorDialog::OnCancelButtonClick(wxCommandEvent& evt) {
  Close();
}


void ShortcutEditorDialog::OnSaveButtonClick(wxCommandEvent& evt) {
  wxString folderItemFilePath = locationTextBox->GetValue();
  wxString folderItemName = nameTextBox->GetValue();

  wxFileName filename(FolderItem::ResolvePath(folderItemFilePath));
  if (!filename.FileExists() && !wxFileName::DirExists(folderItemFilePath)) {
    // If the shortcut location doesn't exist, just show a warning but allow
    // the user to continue. Invalid shortcuts are allowed since they
    // might be referencing files from a different computer.
    int result = gController.ShowWarningMessage(_T("The shorcut location doesn't exist. Do you wish to continue?"), wxOK | wxCANCEL);
    if (result == wxID_CANCEL) return;
  }

  folderItem_->SetFilePath(folderItemFilePath);
  folderItem_->SetName(folderItemName);

  gController.User_FolderItemChange(folderItem_);
  gController.GetUser()->ScheduleSave();

  Close();
}


void ShortcutEditorDialog::UpdateFromFolderItem() {  
  nameTextBox->SetValue(folderItem_->GetName());
  locationTextBox->SetValue(folderItem_->GetFilePath());
}


void ShortcutEditorDialog::OnSelectFileButtonClick(wxCommandEvent& evt) {
  wxFileDialog fileDialog(this, _T("Pick a file"));
  int result = fileDialog.ShowModal();
  if (result != wxID_OK) return;

  locationTextBox->SetValue(fileDialog.GetPath());
}


void ShortcutEditorDialog::OnSelectFolderButtonClick(wxCommandEvent& evt) {
  wxDirDialog dirDialog(this, _T("Pick a folder"));
  int result = dirDialog.ShowModal();
  if (result != wxID_OK) return;

  locationTextBox->SetValue(dirDialog.GetPath());
}


