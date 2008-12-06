/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/


/**
 * The dock item is the main Appetizer object. It can represent a shortcut or a group. If
 * it is a shortcut, it is normally linked to a file, which can be launched when the user
 * click on the associated icon. If it is a group, it can contain other groups or shortcuts
 * and it is possible to add or remove dock items to/from it. Use the <code>isGroup()</code>
 * to differentiate between a shortcut and a group.
 * @see Icon
 *
 */	


#include "../stdafx.h"

#include "azGlobal.h"
#include "azDockItem.h"

#include "../MiniLaunchBar.h"
#include "LuaUtil.h"


//*****************************************************************
//
// LUNAR DATA
//
//*****************************************************************

const char azDockItem::className[] = "DockItem";

#define method(class, name) {#name, &class::name}

Lunar<azDockItem>::RegType azDockItem::methods[] = {
  method(azDockItem, getAllGroups),
  method(azDockItem, getName),
  method(azDockItem, getId),
  method(azDockItem, addChild),
  method(azDockItem, setName),
  method(azDockItem, setPath),
  method(azDockItem, autoSetName),
  method(azDockItem, getPath),
  method(azDockItem, getResolvedPath),
  method(azDockItem, launch),
  method(azDockItem, setParameters),
  method(azDockItem, getParameters),
  method(azDockItem, addToMultiLaunchGroup),
  method(azDockItem, belongsToMultiLaunchGroup),
  method(azDockItem, removeFromMultiLaunchGroup),
  method(azDockItem, childrenCount),
  method(azDockItem, getChildAt),
  method(azDockItem, getParent),
  method(azDockItem, removeFromParent),
  method(azDockItem, insertChildAt),
  method(azDockItem, isGroup),
  {0,0}
};


//*****************************************************************
//
// NON-EXPORTED MEMBERS
//
//*****************************************************************


azDockItem::azDockItem(FolderItem* folderItem) {
  folderItemId_ = folderItem->GetId();
}


FolderItem* azDockItem::Get() const {
  // This is very fast (50 milliseconds for 1000000 iterations) so no need to optimize
  return FolderItem::GetFolderItemById(folderItemId_);
}


//*****************************************************************
//
// EXPORTED MEMBERS
//
//*****************************************************************


/**
 * Constructor. When creating a new dock item, you must specify whether it is a group or 
 * a shortcut by passing a boolean to the constructor.
 * @param Boolean isGroup Set this to <code>true</code> if the new dock item is a group; <code>false</code> if it is a shortcut (default false)
 * 
 */	
azDockItem::azDockItem(lua_State *L) {
  FolderItem* folderItem = FolderItem::CreateFolderItem(LuaUtil::ToBoolean(L, 1, true, false));
  folderItemId_ = folderItem->GetId();
}


/**
 * <span class="groupsOnly">Only applies to groups.</span> Recursively gets all the groups within this group.
 * @param Boolean isGroup Set this to <code>true</code> if the new dock item is a group; <code>false</code> if it is a shortcut (default false)
 * @return Array The groups contained within this group.
 *
 */	
int azDockItem::getAllGroups(lua_State *L) {
  CheckWrappedObject(L, Get()); 

  if (!Get()->IsGroup()) luaL_error(L, "This dock item is not a group");

  bool recursively = LuaUtil::ToBoolean(L, 1, true, false);

  FolderItemVector allGroups = Get()->GetAllGroups(recursively);

  lua_createtable(L, allGroups.size(), 0);
  int tableIndex = lua_gettop(L);

  for (int i = 0; i < allGroups.size(); i++) {
    FolderItem* f = allGroups.at(i);

    lua_pushinteger(L, i + 1);
    Lunar<azDockItem>::push(L, new azDockItem(f), true);
    lua_settable(L, tableIndex);
  }

  return 1;
}


/**
 * Gets the name of the dock item
 * @return String Name of the dock item.
 *
 */	
int azDockItem::getName(lua_State *L) {
  CheckWrappedObject(L, Get()); 

  lua_pushstring(L, Get()->GetName(true).ToUTF8());
  
  return 1;
}


/**
 * Gets the unique id of the dock item. You cannot set the id of a dock item as it is automatically assigned
 * on creation. However you can use this id to refer or look for a particular dock item.
 * @return Number ID of the dock item
 *
 */	
int azDockItem::getId(lua_State *L) {
  CheckWrappedObject(L, Get()); 

  lua_pushinteger(L, Get()->GetId());
  
  return 1;
}


/**
 * <span class="groupsOnly">Only applies to groups.</span> Adds a child dock item to this group.
 * @param DockItem The dock item to add to the group
 * @example This script create a new shortcut and add it to a group
 * <listing version="3.0">
 * -- Create the shortcut
 * shortcut = DockItem:new()
 * shortcut:setPath("c:\\window\\system32\\calc.exe")
 * 
 * -- Add it to a group
 * someGroup:addChild(shortcut)
 * </listing>
 * @see Application#getDockItemsRoot()
 *
 */	
int azDockItem::addChild(lua_State *L) {
  CheckWrappedObject(L, Get()); 

  if (!Get()->IsGroup()) luaL_error(L, "No child can be added to a non-group object");

  const azDockItem* shortcut = Lunar<azDockItem>::check(L, 1); 

  Get()->AddChild(shortcut->Get());

  wxGetApp().FolderItems_CollectionChange();

  return 0;
}

/**
 * Tells whether the dock item is a group or not
 * @return Boolean <code>true</code> if it is a group; <code>false</code> if it is a shortcut
 *
 */
int azDockItem::isGroup(lua_State *L) { CheckWrappedObject(L, Get()); lua_pushboolean(L, Get()->IsGroup()); return 1; }

/**
 * Sets the dock item name
 * @param String name The name
 * @see #autoSetName()
 *
 */
int azDockItem::setName(lua_State *L) { CheckWrappedObject(L, Get()); wxString n = LuaUtil::ToString(L, 1); Get()->SetName(n); return 0; }

/**
 * Auto-sets the name of the shortcut, based on the shortcut target or any other information that Appetizer
 * can retrieves about the target (such as the file description). This is usually the best way to assign
 * a name to a shortcut.
 * @param String name The name
 * @example This script create a new shortcut and auto-assign a name to it
 * <listing version="3.0">
 * -- Create the shortcut
 * shortcut = DockItem:new()
 * shortcut:setPath("c:\\window\\system32\\calc.exe")
 * shortcut:autoSetName()
 *
 * trace(shortcut:getName()) -- should be "Windows Calculator application file"
 * </listing>
 *
 */
int azDockItem::autoSetName(lua_State *L) { CheckWrappedObject(L, Get()); Get()->AutoSetName(); return 0; }

/**
 * Sets the shortcut path. You may use the special variable <code>$(Drive)</code> to refer to the drive the
 * application is on. For example, <code>$(Drive)\Total Commander\Total Commander Portable.exe</code> would
 * be internally translated to <code>f:\Total Commander\Total Commander Portable.exe</code> if the application
 * is on the <code>f:</code> drive
 * @param String path The file path
 *
 */
int azDockItem::setPath(lua_State *L) { CheckWrappedObject(L, Get()); wxString n = LuaUtil::ToString(L, 1); Get()->SetFilePath(n); return 0; }

/**
 * Gets the shortcut file path, which may include special variables.
 * @return String The file path
 * @see #getResolvedPath()
 *
 */
int azDockItem::getPath(lua_State *L) { CheckWrappedObject(L, Get()); LuaUtil::PushString(L, Get()->GetFilePath()); return 1; }

/**
 * Gets the resolved / normalized shortcut file path. It means that every variables is translated and every relative paths
 * are converted to absolute paths. This is useful if you need to pass the shortcut path to an external tool for example. If the 
 * file path is not a relative path and does not contain any variables, it will be equal to the resolved path.
 * @return String The resolved file path
 *
 */
int azDockItem::getResolvedPath(lua_State *L) { CheckWrappedObject(L, Get()); LuaUtil::PushString(L, Get()->GetResolvedPath()); return 1; }

/**
 * Sets the shortcut parameters (i.e. command line arguments)
 * @param String parameters The parameters
 *
 */
int azDockItem::setParameters(lua_State *L) { CheckWrappedObject(L, Get()); wxString n = LuaUtil::ToString(L, 1); Get()->SetParameters(n); return 0; }

/**
 * Gets the shortcut parameters (i.e. command line arguments)
 * @return String The parameters
 *
 */
int azDockItem::getParameters(lua_State *L) { CheckWrappedObject(L, Get()); LuaUtil::PushString(L, Get()->GetParameters()); return 1; }

/**
 * Adds the shortcut to the multi-launch group
 *
 */
int azDockItem::addToMultiLaunchGroup(lua_State *L) { CheckWrappedObject(L, Get()); Get()->AddToMultiLaunchGroup(); return 0; }

/**
 * Tells whether the shortcut belongs to the multi-launch group or not
 * @return Boolean <code>true</code> if the shortcut belongs to the multi-launch group
 *
 */
int azDockItem::belongsToMultiLaunchGroup(lua_State *L) { CheckWrappedObject(L, Get()); lua_pushboolean(L, Get()->BelongsToMultiLaunchGroup()); return 1; }

/**
 * Removes the shortcut from the multi-launch group
 *
 */
int azDockItem::removeFromMultiLaunchGroup(lua_State *L) { CheckWrappedObject(L, Get()); Get()->RemoveFromMultiLaunchGroup(); return 0; }

/**
 * <span class="groupsOnly">Only applies to groups.</span> Gets the number of children within this group
 * @return Number The number of children
 *
 */	
int azDockItem::childrenCount(lua_State *L) { CheckWrappedObject(L, Get()); lua_pushinteger(L, Get()->ChildrenCount()); return 1; }


/**
 * Launches the shortcut. You may also specify additional command line arguments.
 * @param String arguments Additional arguments (default "")
 * @example This script create a new shortcut and launches it with parameters
 * <listing version="3.0">
 * shortcut = DockItem:new()
 * shortcut:setPath("explorer.exe")
 * shortcut:launch(" /select,c:\\windows")
 * </listing>
 *
 */	
int azDockItem::launch(lua_State *L) {
  CheckWrappedObject(L, Get()); 

  int argc = lua_gettop(L);
  if (argc == 0) {
    Get()->Launch();
  } else {
    Get()->LaunchWithArguments(LuaUtil::ToString(L, 1));
  }
  return 0;
}


/**
 * <span class="groupsOnly">Only applies to groups.</span> Gets the sub-dock item at the given index.
 * @param Number index Index of the child to retrieve
 *
 */	
int azDockItem::getChildAt(lua_State *L) {
  CheckWrappedObject(L, Get()); 

  if (!Get()->IsGroup()) luaL_error(L, "This dock item is not a group and therefore does not have any children");
  int index = luaL_checkinteger(L, 1);
  if (index >= Get()->ChildrenCount()) luaL_error(L, "Index out of bounds");

  Lunar<azDockItem>::push(L, new azDockItem(Get()->GetChildAt(index)), true);
  return 1;
}


/**
 * Gets the dock item's parent or <code>nil</code> if it does not have one.
 * @return DockItem The parent
 *
 */	
int azDockItem::getParent(lua_State *L) {
  CheckWrappedObject(L, Get()); 

  FolderItem* p = Get()->GetParent();
  if (!p) return 0; // No error - just return nil

  FolderItem* sp = FolderItem::GetFolderItemById(p->GetId());
  Lunar<azDockItem>::push(L, new azDockItem(sp), true);
  return 1;
}


/**
 * Removes the dock item from its parent.
 *
 */
int azDockItem::removeFromParent(lua_State *L) {
  CheckWrappedObject(L, Get()); 

  FolderItem* p = Get()->GetParent();
  if (!p) return 0; // No error - just exit
  
  p->RemoveChild(Get());
  return 0;
}


/**
 * <span class="groupsOnly">Only applies to groups.</span> Inserts the given dock item at the given index. If necessary, the dock item is removed from its
 * current parent before being inserted.
 * @param DockItem dockItem The dock item to insert
 * @param Number index The insertion index
 *
 */
int azDockItem::insertChildAt(lua_State *L) { 
  CheckWrappedObject(L, Get()); 

  if (!Get()->IsGroup()) luaL_error(L, "No child can be added to a non-group object");
  const azDockItem* shortcut = Lunar<azDockItem>::check(L, 1);
  int index = luaL_checkinteger(L, 2);  
  Get()->MoveChild(shortcut->Get(), index);
  return 0;
}