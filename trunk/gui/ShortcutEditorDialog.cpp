/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#include "ShortcutEditorDialog.h"
#include "IconDialog.h"
#include "FileOrFolderDialog.h"
#include "../Constants.h"
#include "../FilePaths.h"
#include "../MiniLaunchBar.h"
#include "../MessageBoxes.h"
#include "../utilities/IconGetter.h"
#include "../utilities/VersionInfo.h"
#include "../utilities/StringUtil.h"


BEGIN_EVENT_TABLE(ShortcutEditorDialog, wxDialog)
  EVT_BUTTON(ID_BUTTON_Save, ShortcutEditorDialog::OnSaveButtonClick)
  EVT_BUTTON(ID_SHORTCUTDLG_BUTTON_UseDefaultIcon, ShortcutEditorDialog::OnUseDefaultIconButtonClick)
  EVT_BUTTON(ID_SHORTCUTDLG_BUTTON_ChangeIcon, ShortcutEditorDialog::OnChangeIconButtonClick)
  EVT_BUTTON(ID_SHORTCUTDLG_BUTTON_Browse, ShortcutEditorDialog::OnBrowseButtonClick)
END_EVENT_TABLE()


ShortcutEditorDialog::ShortcutEditorDialog(wxWindow* parent)
: ShortcutEditorDialogBase(parent, wxID_ANY, wxEmptyString) {

  folderItem_ = NULL;

  Localize();
}


void ShortcutEditorDialog::Localize() {
  if (folderItem_) SetTitle(folderItem_->IsGroup() ? _("Group Properties") : _("Shorcut Properties"));
  nameLabel->SetLabel(_("Name:"));
  locationLabel->SetLabel(_("Location:"));
  parametersLabel->SetLabel(_("Parameters:"));
  saveButton->SetLabel(_("Save"));
  cancelButton->SetLabel(_("Cancel"));
  iconLabel->SetLabel(_("Icon:"));
  changeIconButton->SetLabel(_("Change icon..."));
  useDefaultIconButton->SetLabel(_("Use default"));
  locationTextBoxDesc->SetLabel(_("The location may be a file, a folder or a URL."));
}


void ShortcutEditorDialog::EnableDisableFields() {
  if (!folderItem_) return;
  bool isGroup = folderItem_->IsGroup();

  locationLabel->Enable(!isGroup);
  locationTextBox->Enable(!isGroup);
  parametersLabel->Enable(!isGroup);
  parametersTextBox->Enable(!isGroup);
  browseButton->Enable(!isGroup);
  locationTextBoxDesc->Enable(!isGroup);
}


void ShortcutEditorDialog::LoadFolderItem(appFolderItem* folderItem) {
  folderItem_ = folderItem;
  selectedIconPath_ = folderItem_->GetCustomIconPath();
  selectedIconIndex_ = folderItem_->GetCustomIconIndex();
  
  EnableDisableFields();

  Localize();
  UpdateFromFolderItem();

  nameTextBox->SetFocus();
  nameTextBox->SetSelection(-1, -1);
}


void ShortcutEditorDialog::UpdateFolderItemIconFields() {    

  wxIcon* icon = NULL;

  if (selectedIconPath_ != wxEmptyString) {
    
    wxFileName fn(selectedIconPath_);

    if (fn.GetExt().Lower() == _T("png")) {

      icon = Imaging::CreateIconFromPng(appFolderItem::ResolvePath(selectedIconPath_), LARGE_ICON_SIZE);

    } else {

      if (selectedIconIndex_ > 0) {
        icon = IconGetter::GetExecutableIcon(selectedIconPath_, LARGE_ICON_SIZE, selectedIconIndex_);
      } else {
        icon = IconGetter::GetFolderItemIcon(selectedIconPath_, LARGE_ICON_SIZE);
      }

    }

    if (icon) iconStaticBitmap->SetBitmap(*icon);
    wxDELETE(icon);

  } else {

    if (folderItem_->IsWebLink()) {

      icon = IconGetter::GetDefaultWebLinkIcon(LARGE_ICON_SIZE);
      if (icon) iconStaticBitmap->SetBitmap(*icon);
      wxDELETE(icon);

    } else {

      if (folderItem_->IsGroup()) {
        icon = appFolderItem::CreateDefaultGroupIcon(LARGE_ICON_SIZE);
        if (icon) {
          iconStaticBitmap->SetBitmap(*icon);
          wxDELETE(icon);
        }
      } else {

        icon = appFolderItem::CreateSpecialItemIcon(locationTextBox->GetValue(), LARGE_ICON_SIZE);
        if (icon) {
          iconStaticBitmap->SetBitmap(*icon);
        } else {
          icon = IconGetter::GetFolderItemIcon(appFolderItem::ResolvePath(locationTextBox->GetValue()), LARGE_ICON_SIZE, true);
          if (icon) iconStaticBitmap->SetBitmap(*icon);
        }

        wxDELETE(icon);
        
      }

    }

  }

  useDefaultIconButton->Enable(selectedIconPath_ != wxEmptyString);
}


void ShortcutEditorDialog::OnChangeIconButtonClick(wxCommandEvent& evt) {
  IconDialog* d = new IconDialog(this);
  int result = d->ShowModal();
  d->Destroy();
  if (result != wxID_OK) return;

  selectedIconPath_ = d->GetIconSource();
  selectedIconIndex_ = d->GetIconIndex();

  UpdateFolderItemIconFields();
}


void ShortcutEditorDialog::OnUseDefaultIconButtonClick(wxCommandEvent& evt) {
  selectedIconPath_ = wxEmptyString;
  selectedIconIndex_ = 0;
  UpdateFolderItemIconFields();
}


void ShortcutEditorDialog::OnSaveButtonClick(wxCommandEvent& evt) {
  wxString folderItemFilePath = locationTextBox->GetValue();
  wxString folderItemName = nameTextBox->GetValue();

  folderItemFilePath.Trim(true).Trim(false);

  if (!StringUtil::IsWebLink(folderItemFilePath)) {

    wxFileName filename(appFolderItem::ResolvePath(folderItemFilePath));
    filename.Normalize();

    wxString f = filename.GetFullPath();

    if (!wxFileName::FileExists(f) && !wxFileName::DirExists(filename.GetFullPath())) {
      // If the shortcut location doesn't exist, just show a warning but allow
      // the user to continue. Invalid shortcuts are allowed since they
      // might be referencing files from a different computer.
      int result = MessageBoxes::ShowWarning(_("The shortcut location doesn't exist. Do you wish to continue?"), wxYES | wxNO);
      if (result == wxID_NO) return;
    }

  }

  folderItem_->SetCustomIcon(selectedIconPath_, selectedIconIndex_);
  folderItem_->SetFilePath(folderItemFilePath);
  folderItem_->SetName(folderItemName);
  folderItem_->SetParameters(parametersTextBox->GetValue());

  EndDialog(wxID_OK);
}


void ShortcutEditorDialog::UpdateFromFolderItem() {  
  nameTextBox->SetValue(folderItem_->GetName());
  locationTextBox->SetValue(folderItem_->GetFilePath());
  parametersTextBox->SetValue(folderItem_->GetParameters());

  UpdateFolderItemIconFields();
}


void ShortcutEditorDialog::OnBrowseButtonClick(wxCommandEvent& evt) {
  FileOrFolderDialog* d = new FileOrFolderDialog(this);
  int result = d->ShowModal();

  if (result == wxID_OK) {
    wxString newValue = appFolderItem::ConvertToRelativePath(d->GetPath());
    if (locationTextBox->GetValue() == newValue) return;

    locationTextBox->SetValue(newValue); 
    
    nameTextBox->SetValue(appFolderItem::GetDisplayName(newValue));

    UpdateFolderItemIconFields();
  }  

  d->Destroy();
}