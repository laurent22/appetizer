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


int azDialogs::showPreferences(lua_State *L) {
  Plugin* plugin = wxGetApp().GetPluginManager()->GetPluginByLuaState(L);
  if (!plugin) return 0;

  plugin->ShowPreferencesDialog();

  return 0;
}


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

  if (result != wxSAVE) return 0;

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

