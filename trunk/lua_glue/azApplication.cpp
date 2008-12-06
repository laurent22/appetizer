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
 * @example The following code registers a listener for the "trayIconMenuOpening" event,
 * which is triggered when the user right-click on the tray icon. A menu item is then added
 * to the menu.
 * <listing version="3.0">
 * function appetizer_trayIconMenuOpening(event)
 *     trace("Sender: "..event.sender) -- the application is the sender
 *  
 *     -- The tray icon menu is about to be shown, but before it is
 *     -- we add a menu item to it.
 *     menu = event.menu -- get the tray icon menu
 *  	
 *     menu:appendSeparator()
 *  
 *     menuItem = MenuItem:new("Tray Icon Test")
 *  	
 *     menu:append(menuItem)
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


/**
 * Returns the root of all the shortcut objects. Shortcut objects are organized in a tree-like structure
 * with groups being the branches and everything else the leafs. This function allows getting the
 * root of this structure. You can then call <code>childrenCount</code>, <code>getChildAt</code>, etc. to
 * access the children.
 * @return Shortcut The root of all the shortcuts
 * @example The following code gets the shortcut root and prints the name of all its children
 * <listing version="3.0">
 * root = appetizer:getShortcutRoot()
 * childrenCount = root:childrenCount()
 * 
 * for i = 0, (childrenCount - 1) do
 *     subShortcut = root:getChildAt(i)
 *     trace(subShortcut:getName())
 * end
 * </listing>
 * 
 */	
int azApplication::getShortcutRoot(lua_State *L) {
  FolderItem* rootFolderItem = wxGetApp().GetUser()->GetRootFolderItem();
  Lunar<azShortcut>::push(L, new azShortcut(rootFolderItem), true);

  return 1;
}


/**
 * Gets the shortcut with the given id or <code>nil</code> if it does not exist or has been deleted. Note
 * that this function will also look for the shortcut in all the groups and subgroups.
 * @return Shortcut The shortcut or <code>nil</code> if does not exists.
 * 
 */	
int azApplication::getShortcutById(lua_State *L) {
  int folderItemId = luaL_checkinteger(L, 1);
  FolderItem* folderItem = FolderItem::GetFolderItemById(folderItemId);
  Lunar<azShortcut>::push(L, new azShortcut(folderItem), true);
  
  return 1;
}


/**
 * Hides the application
 * 
 */	
int azApplication::hide(lua_State *L) { wxGetApp().GetMainFrame()->Hide(); return 0; }

/**
 * Shows the application
 * 
 */	
int azApplication::show(lua_State *L) { wxGetApp().GetMainFrame()->Show(); return 0; };

/**
 * Closes the application
 * 
 */	
int azApplication::close(lua_State *L) {
  // Don't directly close the app, but schedule a close operation
  // This allows the script and plugin to finish properly
  wxGetApp().GetMainFrame()->ScheduleCloseOperation();
  luaL_error(L, "Closing application (This is NOT an error)"); // This is just to force the script to exit  
  return 0;
};

/**
 * Tells whether the application is visible or not
 * @return Boolean <code>true</code> if the application is visible
 * @example The following code toggles the application visibility
 * <listing version="3.0">
 * function hideShowToggle()
 *     if appetizer:isVisible() then
 *         appetizer:hide()
 *     else
 *         appetizer:show()
 *     end
 * end
 * 
 * hideShowToggle()
 * </listing>
 * 
 */	
int azApplication::isVisible(lua_State *L) { lua_pushboolean(L, wxGetApp().GetMainFrame()->IsVisible()); return 1; }


/**
 * Sets the application orientation.
 * @param String orientation Can be "horizontal" or "vertical"
 * @example The following code rotates the application
 * <listing version="3.0">
 * function rotateApplication()
 *     if appetizer:getOrientation() == "horizontal" then
 *         appetizer:setOrientation("vertical")
 *     else
 *         appetizer:setOrientation("horizontal")
 *     end
 * end
 * 
 * rotateApplication()
 * </listing>
 * 
 */	
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


/**
 * Gets the application orientation.
 * @return String Can be "horizontal" or "vertical"
 * @see #setOrientation()
 * 
 */	
int azApplication::getOrientation(lua_State *L) { LuaUtil::PushString(L, wxGetApp().GetMainFrame()->GetRotated() ? _T("vertical") : _T("horizontal")); return 1; }

/**
 * Triggers multi-launch functionality. See Appetizer's help file for some information about this feature.
 * 
 */	
int azApplication::doMultiLaunch(lua_State *L) { wxGetApp().GetUtilities().DoMultiLaunch(); return 0; }


/**
 * Installs Appetizer's autorun.inf file at the root of the current drive.
 * 
 */	
int azApplication::installAutoRunFile(lua_State *L) { wxGetApp().GetUtilities().InstallAutorunFile(); return 0; }


/**
 * Returns the drive on which the application is running
 * 
 */	
int azApplication::getDrive(lua_State *L) { LuaUtil::PushString(L, FilePaths::GetApplicationDrive()); return 1; }


/**
 * Opens the help file
 * @param String sectionName The section to open the help file at (default "")
 * 
 */	
int azApplication::showHelpFile(lua_State *L) {
  wxString anchor = LuaUtil::ToString(L, 1, true);
  wxGetApp().GetUtilities().ShowHelpFile(anchor);
  return 0;
}
