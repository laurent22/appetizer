/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#include "azDialogs.h"
#include "LuaUtil.h"
#include "../MiniLaunchBar.h"
#include "../MessageBoxes.h"



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

int azDialogs::showConfigDialog(lua_State *L) { wxGetApp().GetUtilities().ShowConfigDialog(); return 0; }
int azDialogs::showNewShortcutDialog(lua_State *L) { wxGetApp().GetUtilities().CreateNewShortcut(); return 0; }
int azDialogs::showImportDialog(lua_State *L) { wxGetApp().GetUtilities().ShowImportDialog(); return 0; }
int azDialogs::showEjectDriveDialog(lua_State *L) { wxGetApp().GetUtilities().ShowEjectDriveDialog(); return 0; }