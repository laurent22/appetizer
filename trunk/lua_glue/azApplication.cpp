/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/



/**
 * This class represents an application. You do not need to directly create an instance
 * of it. Instead, use the <code>appetizer</code> global object to call the 
 * application methods.
 *     
 * @beginEventTable
 * @event MenuOpening trayIconMenuOpening When the tray icon menu is about to be displayed
 * @event IconMenuOpening iconMenuOpening When a dock icon menu is about to be displayed
 * @endEventTable
 *
 */	



#include "../stdafx.h"

#include "azApplication.h"
#include "azShortcut.h"
#include "azGlobal.h"
#include "LuaUtil.h"
#include "../FilePaths.h"
#include "../MiniLaunchBar.h"



//*****************************************************************
//
// LUNAR DATA
//
//*****************************************************************

const char azApplication::className[] = "Application";

#define method(class, name) {#name, &class::name}

Lunar<azApplication>::RegType azApplication::methods[] = {
  method(azApplication, addEventListener),
  method(azApplication, getShortcutRoot),
  method(azApplication, getShortcutById),
  method(azApplication, hide),
  method(azApplication, show),
  method(azApplication, close),
  method(azApplication, isVisible),
  method(azApplication, openOptionPanel),
  method(azApplication, closeOptionPanel),
  method(azApplication, isOptionPanelOpen),
  method(azApplication, setOrientation),
  method(azApplication, getOrientation),
  method(azApplication, doMultiLaunch),
  method(azApplication, installAutoRunFile),
  method(azApplication, getDrive),
  method(azApplication, showHelpFile),
  {0,0}
};


//*****************************************************************
//
// NON-EXPORTED MEMBERS
//
//*****************************************************************


MiniLaunchBar* azApplication::Get() const { return &(wxGetApp()); }


//*****************************************************************
//
// EXPORTED MEMBERS
//
//*****************************************************************


azApplication::azApplication(lua_State *L) {
  luaL_error(L, "This object cannot be directly created. Instead use the global 'appetizer' object.");
}


/**
 * This method lets you register event listener functions with the specified control or object.
 * The event listener functions always receive an <code>event</code> object as a parameter. This
 * event has, among other, a <code>sender</code> property which is a reference to the object that has
 * dispatched the event. This object may also have additional properties which depend on the
 * the event.
 * @param String eventName The event you wish to listen to
 * @param String callback The Lua function to call when the event is triggered
 * @example The following code register an event listener with the "trayIconMenuOpening",
 * which is triggered when the user right-click on the tray icon. A menu item is then added
 * to the menu.
 * <listing version="3.0">
 * function appetizer_trayIconMenuOpening(event)
 *   trace("Sender: "..event.sender) -- the application is the sender
 *  
 *   -- The tray icon menu is about to be shown, but before it is
 *   -- we add a menu item to it.
 *   menu = event.menu -- get the tray icon menu
 *  	
 *   menu:appendSeparator()
 *  
 *   menuItem = {}
 *   menuItem.text = "Tray Icon Test"
 *  	
 *   menu:append(menuItem)
 * end
 * 
 * appetizer:addEventListener("trayIconMenuOpening", "appetizer_trayIconMenuOpening")
 * 
 * trace("Listening to the "trayIconMenuOpening" event...")
 * </listing>
 * 
 */	
int azApplication::addEventListener(lua_State *L) {
  wxString eventName = LuaUtil::ToString(L, 1);
  wxString functionName = LuaUtil::ToString(L, 2);
  Plugin* p = wxGetApp().GetPluginManager()->GetPluginByLuaState(L);
  p->AddEventListener(this->Get(), eventName, functionName);
  return 0;
}


int azApplication::getShortcutRoot(lua_State *L) {
  FolderItem* rootFolderItem = wxGetApp().GetUser()->GetRootFolderItem();
  Lunar<azShortcut>::push(L, new azShortcut(rootFolderItem), true);

  return 1;
}


int azApplication::getShortcutById(lua_State *L) {
  int folderItemId = luaL_checkinteger(L, 1);
  FolderItem* folderItem = FolderItem::GetFolderItemById(folderItemId);
  Lunar<azShortcut>::push(L, new azShortcut(folderItem), true);
  
  return 1;
}


int azApplication::hide(lua_State *L) { wxGetApp().GetMainFrame()->Hide(); return 0; }
int azApplication::show(lua_State *L) { wxGetApp().GetMainFrame()->Show(); return 0; };

int azApplication::close(lua_State *L) {
  // Don't directly close the app, but schedule a close operation
  // This allows the script and plugin to finish properly
  wxGetApp().GetMainFrame()->ScheduleCloseOperation();
  luaL_error(L, "Closing application (This is NOT an error)"); // This is just to force the script to exit  
  return 0;
};

int azApplication::isVisible(lua_State *L) { lua_pushboolean(L, wxGetApp().GetMainFrame()->IsVisible()); return 1; }
int azApplication::openOptionPanel(lua_State *L) { wxGetApp().GetMainFrame()->OpenOptionPanel(); return 0; }
int azApplication::closeOptionPanel(lua_State *L) { wxGetApp().GetMainFrame()->CloseOptionPanel(); return 0; }
int azApplication::isOptionPanelOpen(lua_State *L) { lua_pushboolean(L, wxGetApp().GetMainFrame()->IsOptionPanelOpen()); return 1; }

int azApplication::setOrientation(lua_State *L) {
  wxString orientation = LuaUtil::ToString(L, 1);

  if (orientation == _T("vertical")) {
     if (wxGetApp().GetMainFrame()->GetRotated()) return 0;
     wxGetApp().GetMainFrame()->SetRotated(true, true);
  } else { // default to "horizontal"
     if (!wxGetApp().GetMainFrame()->GetRotated()) return 0;
     wxGetApp().GetMainFrame()->SetRotated(false, true);
  }

  return 0;
}

int azApplication::getOrientation(lua_State *L) { LuaUtil::PushString(L, wxGetApp().GetMainFrame()->GetRotated() ? _T("vertical") : _T("horizontal")); return 1; }
int azApplication::doMultiLaunch(lua_State *L) { wxGetApp().GetUtilities().DoMultiLaunch(); return 0; }
int azApplication::installAutoRunFile(lua_State *L) { wxGetApp().GetUtilities().InstallAutorunFile(); return 0; }
int azApplication::getDrive(lua_State *L) { LuaUtil::PushString(L, FilePaths::GetApplicationDrive()); return 1; }

int azApplication::showHelpFile(lua_State *L) {
  wxString anchor = LuaUtil::ToString(L, 1, true);
  wxGetApp().GetUtilities().ShowHelpFile(anchor);
  return 0;
}
