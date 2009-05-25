/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/


/**
 * This class contains a number of utility functions to display various dialog boxes.
 * You do not need to construct an instance of this class - instead use the global
 * <code>dialogs</code> object.
 * @see Global#dialogs
 *
 */	


#include "../stdafx.h"

#include "azDialogs.h"
#include "azPreferences.h"
#include "LuaUtil.h"
#include "../MiniLaunchBar.h"
#include "../MessageBoxes.h"
#include "../gui/PluginPreferencesDialog.h"




//*****************************************************************
//
// LUNAR DATA
//
//*****************************************************************

const char azDialogs::className[] = "Dialogs";

#define method(class, name) {#name, &class::name}

Lunar<azDialogs>::RegType azDialogs::methods[] = {
  method(azDialogs, showMessage),
  method(azDialogs, showConfigDialog),
  method(azDialogs, showNewShortcutDialog),
  method(azDialogs, showImportDialog),
  method(azDialogs, showEjectDriveDialog),
  method(azDialogs, showPreferences),  
  method(azDialogs, showForm),
  method(azDialogs, showSplashForm),  
  method(azDialogs, closeSplashForm), 
  {0,0}
};


//*****************************************************************
//
// NON-EXPORTED MEMBERS
//
//*****************************************************************


azDialogs::azDialogs() {}


//*****************************************************************
//
// EXPORTED MEMBERS
//
//*****************************************************************


azDialogs::azDialogs(lua_State *L) {
  luaL_error(L, "This object cannot be directly created. Use the 'dialogs' global object instead.");
}


/**
 * Shows a message box to the user. There are four kinds of message boxes: <code>information</code>,
 * <code>warning</code>, <code>error</code> and <code>confirmation</code>. Currently, the only difference
 * between them is that they display a different icon. Additionally, each message box can have different
 * combinations of buttons among <code>yes</code>, <code>no</code>, <code>ok</code> and <code>cancel</code>.
 * When you call this function the script is stopped until the user clicks on a button. You can then
 * check the return value of the function to know which button has been clicked. To display a simple alert
 * box with just an "OK" button, simply use <code>dialogs.showMessage("my message")</code>.
 * @param String message The message to display.
 * @param String buttons The buttons to display. Possible values are "ok", "yesNo" and "yesNoCancel". (default "ok")
 * @param String type The type of message box. Possible values are "information", "warning", "error" and "confirmation". (default "information")
 * @return String The button that was clicked. Possible values are "ok", "yes", "no" or "cancel".
 * @example This script shows different kinds of message boxes and their results:
 * <listing version="3.0">
 * result = dialogs:showMessage("This is an error message with an 'ok' button", "ok", "error")
 * dialogs:showMessage("'"..result.."' was clicked")
 * 
 * result = dialogs:showMessage("This is a warning message with yes / no buttons", "yesNo", "warning")
 * dialogs:showMessage("'"..result.."' was clicked")	
 * 
 * result = dialogs:showMessage("This is a confirmation message with yes / no / cancel buttons", "yesNoCancel", "confirmation")
 * dialogs:showMessage("'"..result.."' was clicked")	
 * </listing>
 * 
 */	
int azDialogs::showMessage(lua_State *L) {
  wxString message = LuaUtil::ToString(L, 1);
  wxString buttons = LuaUtil::ToString(L, 2, true);
  if (buttons == wxEmptyString) buttons = _T("ok");
  wxString type = LuaUtil::ToString(L, 3, true);
  if (type == wxEmptyString) type = _T("information");

  int buttonFlags = 0;

  if (buttons == _T("yesNo")) {
    buttonFlags = wxYES | wxNO;
  } else if (buttons == _T("yesNoCancel")) {
    buttonFlags = wxYES | wxNO | wxCANCEL;
  } else {
    buttonFlags = wxOK;
  }

  int result = 0;

  if (type == _T("error")) {
    result = MessageBoxes::ShowError(message, buttonFlags);
  } else if (type == _T("warning")) {
    result = MessageBoxes::ShowWarning(message, buttonFlags);
  } else if (type == _T("confirmation")) {
    result = MessageBoxes::ShowConfirmation(message, buttonFlags);
  } else {
    result = MessageBoxes::ShowInformation(message, buttonFlags);
  }

  wxString output;

  if (result == wxID_YES) {
    output = _T("yes");
  } else if (result == wxID_OK) {
    output = _T("ok");
  } else if (result == wxID_NO) {
    output = _T("no");
  } else if (result == wxID_CANCEL) {
    output = _T("cancel");
  } else {
    output = _T("close");
  }

  LuaUtil::PushString(L, output);

  return 1;
}


/**
 * Shows the application configuration dialog.
 * 
 */	
int azDialogs::showConfigDialog(lua_State *L) { wxGetApp().GetUtilities().ShowConfigDialog(); return 0; }

/**
 * Shows the application "New shortcut" dialog. If the user clicks "Save" on it, a new shortcut will
 * be created.
 * 
 */	
int azDialogs::showNewShortcutDialog(lua_State *L) { wxGetApp().GetUtilities().CreateNewShortcut(); return 0; }

/**
 * Shows the application "Import" dialog, which allows importing shortcuts.
 * 
 */	
int azDialogs::showImportDialog(lua_State *L) { wxGetApp().GetUtilities().ShowImportDialog(); return 0; }

/**
 * Shows the system "Eject drive" dialog.
 * 
 */	
int azDialogs::showEjectDriveDialog(lua_State *L) { wxGetApp().GetUtilities().ShowEjectDriveDialog(); return 0; }


/**
 * Shows the Preferences dialog box of the plugin.
 * @see Preferences
 * 
 */	
int azDialogs::showPreferences(lua_State *L) {
  Plugin* plugin = wxGetApp().GetPluginManager()->GetPluginByLuaState(L);
  if (!plugin) return 0;

  plugin->ShowPreferencesDialog();

  return 0;
}


/**
 * Builds and displays a form dialog, which can be used to get user's input.
 * The function takes, as a first parameter, a table of objects, each representing a
 * control (such as a check box or text field). The properties of these controls
 * are the same as the properties of a preference.
 * @example This script creates three controls and displays them.
 * <listing version="3.0">
 * -- Build the table of controls
 * controls = {}
 *
 * -- Add a text field
 * table.insert(controls, {
 *   type = "Text",
 *   name = "textExample",
 *   title = "Type some text:",
 *   description = "This is a text field"
 * })
 *
 * -- Add a check box
 * table.insert(controls, {
 *   type = "CheckBox",
 *   name = "checkboxExample",
 *   defaultValue = "true",
 *   title = "Are you sure?"
 * })
 *
 * -- Add a popup
 * table.insert(controls, {
 *   type = "Popup",
 *   name = "popupExample",
 *   defaultValue = "two",
 *   title = "Popup",
 *   options = {
 *     one = "First option",
 *     two = "Second option",
 *     three = "Third option"
 *   }
 * })
 *
 * -- Display the form
 * result = dialogs:showForm(controls, "Please select some values", "Save")
 *
 * if result == null then
 *   trace("User cancelled")
 * else
 *   for key, value in pairs(result) do
 *     trace(key, " = ", value)
 *   end
 * end
 * 
 * appetizer:addEventListener("trayIconMenuOpening", "appetizer_trayIconMenuOpening")
 * 
 * trace("Listening to the "trayIconMenuOpening" event...")
 * </listing>
 * @param Array controls The table of controls
 * @param String title The form title (default "Appetizer")
 * @param String buttonLabel The form button label (default "OK")
 * @result Array An associative array containing the key / value pairs or <code>null</code> if the user cancels
 * @see Preferences
 * 
 */	
int azDialogs::showForm(lua_State *L) {
  Plugin* plugin = wxGetApp().GetPluginManager()->GetPluginByLuaState(L);
  if (!plugin) return 0;

  if (!lua_istable(L, 1)) {
    luaL_error(L, "The first parameter must be a table");
    return 0;
  }

  wxString inputTitle = LuaUtil::ToString(L, 2, true, _T("Appetizer"));
  wxString inputOkButtonLabel = LuaUtil::ToString(L, 3, true, _("OK"));

  PluginPreferences* preferences = new PluginPreferences();

  lua_pushnil(L);

  while (lua_next(L, 1) != 0) {
    PluginPreference* p = LuaUtil::ToPluginPreference(L, preferences, -2);
    preferences->RegisterPreference(p);

    lua_pop(L, 1); // pop the value but keep the key for the next iteration
  }


  PluginPreferencesDialog* dialog = new PluginPreferencesDialog(wxGetApp().GetMainFrame(), wxID_ANY);
  dialog->LoadPreferences(preferences, true, inputOkButtonLabel);
  dialog->SetTitle(inputTitle);
  int result = dialog->ShowModal();  
  dialog->Destroy();

  if (result != wxSAVE) {
    wxDELETE(preferences);
    return 0;
  }

  lua_createtable(L, preferences->Count(), 0);
  int tableIndex = lua_gettop(L);

  for (int i = 0; i < preferences->Count(); i++) {
    PluginPreference* p = preferences->GetPreferenceAt(i);
    
    LuaUtil::PushString(L, p->GetName());

    if (p->IsBoolean()) {
      lua_pushboolean(L, p->GetBoolValue());
    } else if (p->IsInteger()) {
      lua_pushinteger(L, p->GetIntValue());
    } else {
      LuaUtil::PushString(L, p->GetValue());
    }
    
    lua_settable(L, tableIndex);
  }

  wxDELETE(preferences);

  return 1;
}


/**
 * Displays some text on a modal splash screen. It can be used for example as a "waiting" screen during
 * a time consuming operation. The function returns an ID that you will need to close the form
 * using <code>closeSplashScreen</code>.
 * @example This script opens a splash form, does some processing and close the form.
 * <listing version="3.0">
 * local formId = dialogs:showSplashForm("Operation in progress...", "Please wait...")
 * doSomeTimeConsumingOperationHere()
 * dialogs:closeSplashForm(formId)
 * </listing>
 * @param String message The message to display
 * @param String title The form title (default "")
 * @return Number The form identifier
 * @see #closeSplashScreen
 * 
 */	
int azDialogs::showSplashForm(lua_State *L) {
  wxString inputMessage = LuaUtil::ToString(L, 1);
  wxString inputTitle = LuaUtil::ToString(L, 2, true);

  int margin = 20;

  wxDialog* dialog = new wxDialog(wxGetApp().GetMainFrame(), wxID_ANY, inputTitle, wxDefaultPosition, wxDefaultSize, wxCAPTION | wxSTAY_ON_TOP);

  wxBoxSizer* sizer = new wxBoxSizer(wxVERTICAL);

  wxStaticText* messageLabel = new wxStaticText(dialog, wxID_ANY, inputMessage);
  wxSize bestSize = messageLabel->GetBestSize();
  int messageWidth = bestSize.GetWidth();

  if (messageWidth > 200) {
    messageWidth = 200;
    messageLabel->Wrap(messageWidth);
    bestSize = messageLabel->GetBestSize();    
  }

  messageLabel->SetSize(bestSize);

  sizer->Add(messageLabel, 0, wxALL, margin);

  dialog->SetSizer(sizer);
  sizer->SetSizeHints(dialog);

  dialog->Show();

  splashDialogs_.push_back(dialog);

  lua_pushinteger(L, splashDialogs_.size() - 1);

  return 1;
}


/**
 * Closes a splash form opened with <code>showSplashForm</code>.
 * @param Number formId The form identifier
 * @see #showSplashForm
 * 
 */	
int azDialogs::closeSplashForm(lua_State *L) {
  int dialogIndex = luaL_checkinteger(L, 1);

  if (splashDialogs_.size() <= dialogIndex) {
    luaL_error(L, "Invalid splash form ID");
    return 0;
  }

  wxDialog* dialog = splashDialogs_.at(dialogIndex);
  if (!dialog) return 0;

  dialog->Destroy();
  splashDialogs_[dialogIndex] = NULL;

  return 0;
}