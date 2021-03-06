﻿/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#include "ConfigDialog.h"
#include "../FilePaths.h"
#include "../UserSettings.h"
#include "../MiniLaunchBar.h"
#include "../Localization.h"
#include "../Constants.h"
#include "../MessageBoxes.h"
#include "../utilities/StringUtil.h"
#include "../Log.h"
#include "../Styles.h"


BEGIN_EVENT_TABLE(ConfigDialog, wxDialog)
  EVT_BUTTON(ID_CDLG_BUTTON_Save, ConfigDialog::OnSaveButtonClick)
  EVT_BUTTON(ID_CDLG_BUTTON_CheckForUpdate, ConfigDialog::OnCheckForUpdateButtonClick)  
  EVT_BUTTON(wxID_ANY, ConfigDialog::OnButtonClick)  
  EVT_SHOW(ConfigDialog::OnShow)
  EVT_NOTEBOOK_PAGE_CHANGED(ID_CDLG_MainNotebook, ConfigDialog::OnNoteBookPageChanged)
END_EVENT_TABLE()


ConfigDialog::ConfigDialog()
: ConfigDialogBase(NULL, wxID_ANY, wxEmptyString) {
  languageComboBox->SetMinSize(wxSize(0, languageComboBox->GetMinSize().GetHeight()));
  iconSizeComboBox->SetMinSize(wxSize(0, iconSizeComboBox->GetMinSize().GetHeight()));
  
  Localize();
}


void ConfigDialog::OnShow(wxShowEvent& evt) {

  
}


void ConfigDialog::Localize() {
  SetTitle(_("Configuration"));  
  notebook->SetPageText(0, _("General"));
  notebook->SetPageText(1, _("Appearance"));
  notebook->SetPageText(2, _("Operations"));
  notebook->SetPageText(3, _("Import"));
  saveButton->SetLabel(_("Save"));
  cancelButton->SetLabel(_("Cancel"));  
}


void ConfigDialog::OnNoteBookPageChanged(wxNotebookEvent& evt) {
  if (evt.GetSelection() < 0) return;
  UpdatePage(evt.GetSelection());
}


void ConfigDialog::UpdatePage(int pageIndex) {
  if (updatedPages_[pageIndex]) return;

  UserSettingsSP userSettings = wxGetApp().GetUser()->GetSettings();  
  User* user = wxGetApp().GetUser();

  wxArrayString foundFilePaths;
  int selectedIndex = 0;



  switch (pageIndex) {







    // *********************************************************************************************
    //
    // GENERAL TAB
    //
    // *********************************************************************************************

    case 0: {

      languageLabel->SetLabel(_("Language:"));
      alwaysOnTopCheckBox->SetLabel(_("Always on top"));
      oneInstanceCheckBox->SetLabel(wxString::Format(_("Allow only one instance of %s at a time"), APPLICATION_NAME));
      checkForUpdateButton->SetLabel(_("Check for update"));
      installAutorunButton->SetLabel(_("Install autorun file"));

      //---------------------------------------------------------------------------
      // Populate language dropdown list
      //---------------------------------------------------------------------------

      wxString localeFolderPath = FilePaths::GetLocalesDirectory();

      languageComboBox->Clear();
      wxString currentLocaleCode = userSettings->Locale;

      foundFilePaths.Clear();
      wxDir localeFolder;
      
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

      alwaysOnTopCheckBox->SetValue(userSettings->AlwaysOnTop);
      oneInstanceCheckBox->SetValue(userSettings->UniqueApplicationInstance);

      #ifndef __WXDEBUG__
      installAutorunButton->Enable(wxGetApp().GetUtilities().IsApplicationOnRemoteDrive());
      #endif

      languageLabel->GetParent()->Layout();

      } break;











    // *********************************************************************************************
    //
    // APPEARANCE TAB
    //
    // *********************************************************************************************

    case 1: {

      iconSizeLabel->SetLabel(_("Icon size:"));
      skinLabel->SetLabel(_("Skin:"));
      orientationLabel->SetLabel(_("Orientation:"));
          
      //---------------------------------------------------------------------------
      // Populate "icon size" dropdown list
      //---------------------------------------------------------------------------

      iconSizeComboBox->Clear();

      IntVector allowedIconSizes = wxGetApp().GetAllowedIconSizes();
      selectedIndex = 0;

      for (int i = 0; i < allowedIconSizes.size(); i++) {
        int iconSize = allowedIconSizes.at(i);
        wxString iconName = wxGetApp().GetIconSizeName(iconSize);
        wxString iconSizeString = wxString::Format(_T("%d"), iconSize);
        iconSizeComboBox->Append(iconName, new wxStringClientData(iconSizeString));

        if (iconSize == userSettings->IconSize) selectedIndex = i;
      }

      iconSizeComboBox->Select(selectedIndex);

      //---------------------------------------------------------------------------
      // Populate "Orientation" dropdown list
      //---------------------------------------------------------------------------

      orientationComboBox->Clear();
      orientationComboBox->Append(_("Horizontal"), new wxStringClientData(_T("h")));
      orientationComboBox->Append(_("Vertical"), new wxStringClientData(_T("v")));
      orientationComboBox->Select(userSettings->Rotated ? 1 : 0);

      //---------------------------------------------------------------------------
      // Populate "Skin" dropdown list
      //---------------------------------------------------------------------------

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

          wxString skinName = skinMetadata.Name;
          if (folderName == _T("Default")) skinName += wxString::Format(_T("%s%s%s"), _T(" ("), _("Default"), _T(")"));

          skinComboBox->Append(skinName, new wxStringClientData(folderName));
          if (folderName == userSettings->Skin) selectedIndex = i;

          success = skinFolder.GetNext(&folderName);      
          i++;
        }
      } 

      skinComboBox->Select(selectedIndex);

      skinLabel->GetParent()->Layout();
      
      } break;













    // *********************************************************************************************
    //
    // OPERATIONS TAB
    //
    // *********************************************************************************************

    case 2: {

      autohideCheckBox->SetLabel(_("Auto-hide after launching an application"));      
      multiLaunchAutoRunCheckBox->SetLabel(_("Run multi-launch group on startup"));
      hotKeyGroupSizer_staticbox->SetLabel(_("Hot key to hide / show the bar"));
      hotKeyCtrlCheckBox->SetLabel(_("Control +"));
      hotKeyAltCheckBox->SetLabel(_("Alt +"));
      hotKeyShiftCheckBox->SetLabel(_("Shift +"));

      multiLaunchAutoRunCheckBox->SetValue(userSettings->RunMultiLaunchOnStartUp);

      //---------------------------------------------------------------------------
      // Populate "Hot key" dropdown list
      //---------------------------------------------------------------------------
      wxArrayString keyStrings;
      std::vector<int> keyCodes;

      // NOTE: all the WXK_<code> constants are completely inaccurate, at least on Windows,
      // so we have to use VK_<code>, which may not be cross-platform.

      keyStrings.Add(_("<None>")); keyCodes.push_back(0);

      keyStrings.Add(_T("A")); keyCodes.push_back(0x41);
      keyStrings.Add(_T("B")); keyCodes.push_back(0x42);
      keyStrings.Add(_T("C")); keyCodes.push_back(0x43);
      keyStrings.Add(_T("D")); keyCodes.push_back(0x44);
      keyStrings.Add(_T("E")); keyCodes.push_back(0x45);
      keyStrings.Add(_T("F")); keyCodes.push_back(0x46);
      keyStrings.Add(_T("G")); keyCodes.push_back(0x47);
      keyStrings.Add(_T("H")); keyCodes.push_back(0x48);
      keyStrings.Add(_T("I")); keyCodes.push_back(0x49);
      keyStrings.Add(_T("J")); keyCodes.push_back(0x4A);
      keyStrings.Add(_T("K")); keyCodes.push_back(0x4B);
      keyStrings.Add(_T("L")); keyCodes.push_back(0x4C);
      keyStrings.Add(_T("M")); keyCodes.push_back(0x4D);
      keyStrings.Add(_T("N")); keyCodes.push_back(0x4E);
      keyStrings.Add(_T("O")); keyCodes.push_back(0x4F);
      keyStrings.Add(_T("P")); keyCodes.push_back(0x50);
      keyStrings.Add(_T("Q")); keyCodes.push_back(0x51);
      keyStrings.Add(_T("R")); keyCodes.push_back(0x52);
      keyStrings.Add(_T("S")); keyCodes.push_back(0x53);
      keyStrings.Add(_T("T")); keyCodes.push_back(0x54);
      keyStrings.Add(_T("U")); keyCodes.push_back(0x55);
      keyStrings.Add(_T("V")); keyCodes.push_back(0x56);
      keyStrings.Add(_T("W")); keyCodes.push_back(0x57);
      keyStrings.Add(_T("X")); keyCodes.push_back(0x58);
      keyStrings.Add(_T("Y")); keyCodes.push_back(0x59);
      keyStrings.Add(_T("Z")); keyCodes.push_back(0x5A);

      keyStrings.Add(_T("F1")); keyCodes.push_back(VK_F1);
      keyStrings.Add(_T("F2")); keyCodes.push_back(VK_F2);
      keyStrings.Add(_T("F3")); keyCodes.push_back(VK_F3);
      keyStrings.Add(_T("F4")); keyCodes.push_back(VK_F4);
      keyStrings.Add(_T("F5")); keyCodes.push_back(VK_F5);
      keyStrings.Add(_T("F6")); keyCodes.push_back(VK_F6);
      keyStrings.Add(_T("F7")); keyCodes.push_back(VK_F7);
      keyStrings.Add(_T("F8")); keyCodes.push_back(VK_F8);
      keyStrings.Add(_T("F9")); keyCodes.push_back(VK_F9);
      keyStrings.Add(_T("F10")); keyCodes.push_back(VK_F10);
      keyStrings.Add(_T("F11")); keyCodes.push_back(VK_F11);
      keyStrings.Add(_T("F12")); keyCodes.push_back(VK_F12);

      keyStrings.Add(_("Numpad 0")); keyCodes.push_back(VK_NUMPAD0);
      keyStrings.Add(_("Numpad 1")); keyCodes.push_back(VK_NUMPAD1);
      keyStrings.Add(_("Numpad 2")); keyCodes.push_back(VK_NUMPAD2);
      keyStrings.Add(_("Numpad 3")); keyCodes.push_back(VK_NUMPAD3);
      keyStrings.Add(_("Numpad 4")); keyCodes.push_back(VK_NUMPAD4);
      keyStrings.Add(_("Numpad 5")); keyCodes.push_back(VK_NUMPAD5);
      keyStrings.Add(_("Numpad 6")); keyCodes.push_back(VK_NUMPAD6);
      keyStrings.Add(_("Numpad 7")); keyCodes.push_back(VK_NUMPAD7);
      keyStrings.Add(_("Numpad 8")); keyCodes.push_back(VK_NUMPAD8);
      keyStrings.Add(_("Numpad 9")); keyCodes.push_back(VK_NUMPAD9);

      keyStrings.Add(_("Backspace")); keyCodes.push_back(VK_BACK);
      keyStrings.Add(_("Tab")); keyCodes.push_back(VK_TAB);
      keyStrings.Add(_("Return")); keyCodes.push_back(VK_RETURN);
      keyStrings.Add(_("Escape")); keyCodes.push_back(VK_ESCAPE);
      keyStrings.Add(_("Space")); keyCodes.push_back(VK_SPACE);
      keyStrings.Add(_("Delete")); keyCodes.push_back(VK_DELETE);

      keyStrings.Add(_("End")); keyCodes.push_back(VK_END);
      keyStrings.Add(_("Home")); keyCodes.push_back(VK_HOME);
      keyStrings.Add(_("Left")); keyCodes.push_back(VK_LEFT);
      keyStrings.Add(_("Up")); keyCodes.push_back(VK_UP);
      keyStrings.Add(_("Right")); keyCodes.push_back(VK_RIGHT);
      keyStrings.Add(_("Down")); keyCodes.push_back(VK_DOWN);

      hotKeyComboBox->Clear();
      int selectedHotKey = userSettings->HotKeyKey;
      int selectedIndex = 0;

      for (int i = 0; i < keyStrings.Count(); i++) {
        wxString keyString = keyStrings[i];
        int keyCode = keyCodes.at(i);
        hotKeyComboBox->Append(keyString, new wxStringClientData(wxString::Format(_T("%d"), keyCode)));

        if (keyCode == selectedHotKey) selectedIndex = i;
      }

      hotKeyComboBox->Select(selectedIndex);
      hotKeyCtrlCheckBox->SetValue(userSettings->HotKeyControl);
      hotKeyAltCheckBox->SetValue(userSettings->HotKeyAlt);
      hotKeyShiftCheckBox->SetValue(userSettings->HotKeyShift);
      
      //---------------------------------------------------------------------------
      // "Auto-hide" and "Always on top"
      //---------------------------------------------------------------------------
      autohideCheckBox->SetValue(userSettings->AutoHideApplication);

      hotKeyCtrlCheckBox->GetParent()->Layout();
      
      } break;











    // *********************************************************************************************
    //
    // IMPORT TAB
    //
    // *********************************************************************************************

    case 3: {

      importExclusionSizer_staticbox->SetLabel(_("Exclude these items from import operations"));

      wxString exclusionString;
      wxArrayString exclusions = user->GetAutoAddExclusions();      
      for (int i = 0; i < exclusions.Count(); i++) exclusionString += exclusions[i] + _T("\n");
      importExclusionTextBox->SetValue(exclusionString);

      } break;



  }




  updatedPages_[pageIndex] = true;  
}


void ConfigDialog::OnButtonClick(wxCommandEvent& evt) {
  switch (evt.GetId()) {

    case ID_CDLG_BUTTON_InstallAutorunButton: {

      int result = MessageBoxes::ShowConfirmation(wxString::Format(_("By installing the autorun file on your removable drive, %s will start automatically when you insert your drive.\n\nDo you wish to continue?"), APPLICATION_NAME));
      if (result == wxID_YES) {
        if (!wxGetApp().GetUtilities().InstallAutorunFile()) {
          MessageBoxes::ShowError(_("Could not install autorun file. Please try again."));
        } else {
          MessageBoxes::ShowInformation(_("The autorun file has been installed successfully"));
        }
      }

      } break;

    default:

      evt.Skip();
      break;

  }
}


void ConfigDialog::LoadSettings() {
  updatedPages_.clear();  
  for (int i = 0; i < notebook->GetPageCount(); i++) updatedPages_.push_back(false);

  notebook->ChangeSelection(0);
  UpdatePage(0);
}


void ConfigDialog::OnCheckForUpdateButtonClick(wxCommandEvent& evt) {
  checkForUpdateButton->SetLabel(_("Please wait..."));
  checkForUpdateButton->Disable();
  checkForUpdateButton->Update();
  wxGetApp().CheckForNewVersion(false);
  checkForUpdateButton->Enable();
  checkForUpdateButton->SetLabel(_("Check for update"));
}


void ConfigDialog::OnSaveButtonClick(wxCommandEvent& evt) {
  UserSettingsSP userSettings = wxGetApp().GetUser()->GetSettings();
  User* user = wxGetApp().GetUser();
  
  wxStringClientData* clientData;

  bool mustRestart = false;















  // *********************************************************************************************
  //
  // GENERAL TAB
  //
  // *********************************************************************************************

  if (updatedPages_[0]) {
  
    //---------------------------------------------------------------------------
    // Apply changes to locale code
    //---------------------------------------------------------------------------

    clientData = (wxStringClientData*)(languageComboBox->GetClientObject(languageComboBox->GetSelection()));
    wxString localeCode = clientData->GetData();
    
    if (localeCode != userSettings->Locale) {
      if (wxGetApp().ChangeLocale(localeCode)) {
        userSettings->Locale = localeCode;
        wxGetApp().User_LocaleChange();
      }
    }

    if (userSettings->AlwaysOnTop != alwaysOnTopCheckBox->GetValue()) {
      userSettings->AlwaysOnTop = alwaysOnTopCheckBox->GetValue();
      mustRestart = true;
    }

    userSettings->UniqueApplicationInstance = oneInstanceCheckBox->GetValue();

  }














  // *********************************************************************************************
  //
  // APPEARANCE TAB
  //
  // *********************************************************************************************

  if (updatedPages_[1]) {

    //---------------------------------------------------------------------------
    // Apply changes to icon size
    //---------------------------------------------------------------------------
    
    clientData = (wxStringClientData*)(iconSizeComboBox->GetClientObject(iconSizeComboBox->GetSelection()));
    wxString newIconSizeS = clientData->GetData();
    long newIconSize; 
    newIconSizeS.ToLong(&newIconSize);

    if (newIconSize != userSettings->IconSize) {
      userSettings->IconSize = newIconSize;
      wxGetApp().User_IconSizeChange();
    }

    //---------------------------------------------------------------------------
    // Apply changes to orientation
    //---------------------------------------------------------------------------
    clientData = (wxStringClientData*)(orientationComboBox->GetClientObject(orientationComboBox->GetSelection()));
    bool rotated = clientData->GetData() == _T("v");

    if (rotated != userSettings->Rotated) {
      userSettings->Rotated = rotated;
      wxGetApp().GetMainFrame()->SetRotated(rotated, true);
    }

    //---------------------------------------------------------------------------
    // Apply changes to skin
    //---------------------------------------------------------------------------
    clientData = (wxStringClientData*)(skinComboBox->GetClientObject(skinComboBox->GetSelection()));
    wxString skinName = clientData->GetData();

    if (skinName != userSettings->Skin) {
      userSettings->Skin = skinName;
      wxGetApp().GetMainFrame()->ApplySkin();
    }

  }









  // *********************************************************************************************
  //
  // OPERATIONS TAB
  //
  // *********************************************************************************************

  if (updatedPages_[2]) {

    userSettings->AutoHideApplication = autohideCheckBox->GetValue();
    userSettings->RunMultiLaunchOnStartUp = multiLaunchAutoRunCheckBox->GetValue();
    
    clientData = (wxStringClientData*)(hotKeyComboBox->GetClientObject(hotKeyComboBox->GetSelection()));
    wxString hotKeyCodeS = clientData->GetData();

    long hotKeyCode;
    hotKeyCodeS.ToLong(&hotKeyCode);

    if (userSettings->HotKeyAlt != hotKeyAltCheckBox->GetValue() ||
        userSettings->HotKeyShift != hotKeyShiftCheckBox->GetValue() ||
        userSettings->HotKeyControl != hotKeyCtrlCheckBox->GetValue() ||
        userSettings->HotKeyKey != (int)hotKeyCode) {

        userSettings->HotKeyAlt = hotKeyAltCheckBox->GetValue();
        userSettings->HotKeyShift = hotKeyShiftCheckBox->GetValue();
        userSettings->HotKeyControl = hotKeyCtrlCheckBox->GetValue();
        userSettings->HotKeyKey = (int)hotKeyCode;

        wxGetApp().GetMainFrame()->RegisterHideShowHotKey();
    }

  }






  // *********************************************************************************************
  //
  // IMPORT TAB
  //
  // *********************************************************************************************

  if (updatedPages_[3]) {
    wxString str = importExclusionTextBox->GetValue();
    str.Replace(_T("\r"), _T("\n"));
    wxArrayString splitted;
    StringUtil::Split(str, splitted, _T("\n"));

    wxArrayString finalStrings;
    
    for (int i = 0; i < splitted.Count(); i++) {
      wxString s = splitted[i];
      s.Trim(true).Trim(false);
      if (s == wxEmptyString) continue;
      finalStrings.Add(s);
    }

    user->SetAutoAddExclusions(finalStrings);
  }
  

  user->Save(true);

  if (mustRestart) {
    MessageBoxes::ShowInformation(_("Some of these changes will only be applied until after you restart the application."));
  }

  EndDialog(wxID_OK);
}