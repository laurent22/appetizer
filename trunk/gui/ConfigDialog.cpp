/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "ConfigDialog.h"
#include "../FilePaths.h"
#include "../UserSettings.h"
#include "../MiniLaunchBar.h"
#include "../Localization.h"
#include "../Constants.h"
#include "../MessageBoxes.h"
#include "../Log.h"
#include "../Styles.h"
#include <wx/arrstr.h>
#include <wx/dir.h>
#include <wx/clntdata.h>
#include <wx/filename.h>


BEGIN_EVENT_TABLE(ConfigDialog, wxDialog)
  EVT_BUTTON(ID_CDLG_BUTTON_Cancel, ConfigDialog::OnCancelButtonClick)
  EVT_BUTTON(ID_CDLG_BUTTON_Save, ConfigDialog::OnSaveButtonClick)
  EVT_BUTTON(ID_CDLG_BUTTON_CheckForUpdate, ConfigDialog::OnCheckForUpdateButtonClick)  
  EVT_SHOW(ConfigDialog::OnShow)
END_EVENT_TABLE()


ConfigDialog::ConfigDialog()
: ConfigDialogBase(NULL, wxID_ANY, wxEmptyString) {
  languageComboBox->SetMinSize(wxSize(0, languageComboBox->GetMinSize().GetHeight()));
  iconSizeComboBox->SetMinSize(wxSize(0, iconSizeComboBox->GetMinSize().GetHeight()));
  
  Localize();
}


void ConfigDialog::OnShow(wxShowEvent& evt) {
  // Update "General" panel
  languageLabel->GetParent()->Layout();
  // Update "Appearance" panel
  skinLabel->GetParent()->Layout();
}


void ConfigDialog::Localize() {
  SetTitle(_("Configuration"));  
  notebook->SetPageText(0, _("General"));
  notebook->SetPageText(1, _("Appearance"));
  languageLabel->SetLabel(_("Language:"));
  iconSizeLabel->SetLabel(_("Icon size:"));
  skinLabel->SetLabel(_("Skin:"));
  saveButton->SetLabel(_("Save"));
  cancelButton->SetLabel(_("Cancel"));
  orientationLabel->SetLabel(_("Orientation:"));
  checkForUpdateButton->SetLabel(_("Check for update"));
  autohideCheckBox->SetLabel(_("Auto-hide after launching an application"));
  alwaysOnTopCheckBox->SetLabel(_("Always on top"));
  oneInstanceCheckBox->SetLabel(wxString::Format(_("Allow only one instance of %s at a time"), APPLICATION_NAME));
}


void ConfigDialog::LoadSettings() {
  UserSettingsSP userSettings = wxGetApp().GetUser()->GetSettings();
  wxArrayString foundFilePaths;

  //***************************************************************************
  // Populate language dropdown list
  //***************************************************************************

  wxString localeFolderPath = FilePaths::GetLocalesDirectory();

  languageComboBox->Clear();
  wxString currentLocaleCode = userSettings->Locale;

  foundFilePaths.Clear();
  wxDir localeFolder;

  int selectedIndex = 0;
  if (wxFileName::DirExists(localeFolderPath) && localeFolder.Open(localeFolderPath)) {
    wxString folderName;
    bool success = localeFolder.GetFirst(&folderName, wxALL_FILES_PATTERN, wxDIR_DIRS);
    int i = 0;

    while (success) {
      // Note: The folder name is the locale code

      // Get the language name from the file
      wxString languageName = Localization::Instance()->GetLanguageName(folderName);
      wxStringClientData* clientData = new wxStringClientData(folderName);  

      languageComboBox->Append(languageName, clientData);

      if (folderName == currentLocaleCode) selectedIndex = i;

      success = localeFolder.GetNext(&folderName);      
      i++;
    }
  } 

  languageComboBox->Select(selectedIndex);

  //***************************************************************************
  // Populate "icon size" dropdown list
  //***************************************************************************

  iconSizeComboBox->Clear();
  iconSizeComboBox->Append(_("Small"), new wxStringClientData(_T("16")));
  iconSizeComboBox->Append(_("Large"), new wxStringClientData(_T("32")));

  if (userSettings->IconSize == 16) {
    iconSizeComboBox->Select(0);
  } else {
    iconSizeComboBox->Select(1);
  }

  //***************************************************************************
  // Populate "Orientation" dropdown list
  //***************************************************************************

  orientationComboBox->Clear();
  orientationComboBox->Append(_("Horizontal"), new wxStringClientData(_T("h")));
  orientationComboBox->Append(_("Vertical"), new wxStringClientData(_T("v")));
  orientationComboBox->Select(userSettings->Rotated ? 1 : 0);

  //***************************************************************************
  // Populate "Skin" dropdown list
  //***************************************************************************

  skinComboBox->Clear();
  wxString skinFolderPath = FilePaths::GetBaseSkinDirectory();

  foundFilePaths.Clear();
  wxDir skinFolder;

  if (wxFileName::DirExists(skinFolderPath) && skinFolder.Open(skinFolderPath)) {
    wxString folderName;
    bool success = skinFolder.GetFirst(&folderName, wxALL_FILES_PATTERN, wxDIR_DIRS);
    int i = 0;

    while (success) {
      SkinMetadata skinMetadata;
      wxString skinFile = skinFolderPath + _T("/") + folderName + _T("/") + SKIN_FILE_NAME;
      Styles::GetSkinMetadata(skinFile, skinMetadata);

      skinComboBox->Append(skinMetadata.Name, new wxStringClientData(folderName));
      if (folderName == userSettings->Skin) selectedIndex = i;

      success = skinFolder.GetNext(&folderName);      
      i++;
    }
  } 

  //***************************************************************************
  // "Auto-hide" and "Always on top"
  //***************************************************************************
  autohideCheckBox->SetValue(userSettings->AutoHideApplication);
  alwaysOnTopCheckBox->SetValue(userSettings->AlwaysOnTop);
  oneInstanceCheckBox->SetValue(userSettings->UniqueApplicationInstance);

  skinComboBox->Select(selectedIndex);
}


void ConfigDialog::OnCheckForUpdateButtonClick(wxCommandEvent& evt) {
  checkForUpdateButton->SetLabel(_("Please wait..."));
  checkForUpdateButton->Disable();
  checkForUpdateButton->Update();
  wxGetApp().CheckForNewVersion(false);
  checkForUpdateButton->Enable();
  checkForUpdateButton->SetLabel(_("Check for update"));
}


void ConfigDialog::OnCancelButtonClick(wxCommandEvent& evt) {
  EndDialog(wxID_CANCEL);
}


void ConfigDialog::OnSaveButtonClick(wxCommandEvent& evt) {
  UserSettingsSP userSettings = wxGetApp().GetUser()->GetSettings();
  wxStringClientData* clientData;

  bool mustRestart = false;
  
  //***************************************************************************
  // Apply changes to locale code
  //***************************************************************************

  clientData = (wxStringClientData*)(languageComboBox->GetClientObject(languageComboBox->GetSelection()));
  wxString localeCode = clientData->GetData();
  
  if (localeCode != userSettings->Locale) {
    if (wxGetApp().ChangeLocale(localeCode)) {
      userSettings->Locale = localeCode;
      wxGetApp().User_LocaleChange();
    }
  }

  //***************************************************************************
  // Apply changes to icon size
  //***************************************************************************
  clientData = (wxStringClientData*)(iconSizeComboBox->GetClientObject(iconSizeComboBox->GetSelection()));
  wxString newIconSizeS = clientData->GetData();
  long newIconSize; 
  newIconSizeS.ToLong(&newIconSize);

  if (newIconSize != userSettings->IconSize) {
    userSettings->IconSize = newIconSize;
    wxGetApp().User_IconSizeChange();
  }

  //***************************************************************************
  // Apply changes to orientation
  //***************************************************************************
  clientData = (wxStringClientData*)(orientationComboBox->GetClientObject(orientationComboBox->GetSelection()));
  bool rotated = clientData->GetData() == _T("v");

  if (rotated != userSettings->Rotated) {
    userSettings->Rotated = rotated;
    wxGetApp().GetMainFrame()->SetRotated(rotated, true);
  }

  //***************************************************************************
  // Apply changes to skin
  //***************************************************************************
  clientData = (wxStringClientData*)(skinComboBox->GetClientObject(skinComboBox->GetSelection()));
  wxString skinName = clientData->GetData();

  if (skinName != userSettings->Skin) {
    userSettings->Skin = skinName;
    wxGetApp().GetMainFrame()->ApplySkin();
  }

  if (userSettings->AutoHideApplication != autohideCheckBox->GetValue()) {
    userSettings->AutoHideApplication = autohideCheckBox->GetValue();
  }

  if (userSettings->AlwaysOnTop != alwaysOnTopCheckBox->GetValue()) {
    userSettings->AlwaysOnTop = alwaysOnTopCheckBox->GetValue();
    mustRestart = true;
  }

  if (userSettings->UniqueApplicationInstance != oneInstanceCheckBox->GetValue()) {
    userSettings->UniqueApplicationInstance = oneInstanceCheckBox->GetValue();
  }

  wxGetApp().GetUser()->Save(true);

  if (mustRestart) {
    MessageBoxes::ShowInformation(_("Some of these changes will only be applied until after you restart the application."));
  }

  EndDialog(wxID_OK);
}