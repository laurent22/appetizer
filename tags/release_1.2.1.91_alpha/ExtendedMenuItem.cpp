/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#include "ExtendedMenuItem.h"


ExtendedMenuItem::ExtendedMenuItem(wxMenu* parentMenu, int id, const wxString& text , const wxString& helpString, wxItemKind kind, wxMenu* subMenu):
  wxMenuItem(parentMenu, id, text, helpString, kind, subMenu) {

}


const wxString& ExtendedMenuItem::GetMetadata(const wxString& name) {
  return metadata_[name];
}


int ExtendedMenuItem::GetMetadataInt(const wxString& name) {
  return metadataInt_[name];
}


void* ExtendedMenuItem::GetMetadataPointer(const wxString& name) {
  return metadataPointer_[name];
}


void ExtendedMenuItem::SetMetadata(const wxString& name, const wxString& value) {
  metadata_[name] = value;
}


void ExtendedMenuItem::SetMetadata(const wxString& name, int value) {
  metadataInt_[name] = value;
}


void ExtendedMenuItem::SetMetadataPointer(const wxString& name, void* value) {
  metadataPointer_[name] = value;
}


ExtendedMenuItem* GetMenuItemById(wxMenu* menu, int menuItemId) {
  wxMenu* subMenu = NULL;
  wxMenuItem* menuItem = menu->FindItem(menuItemId, &subMenu);
  if (!menuItem) return NULL;

  ExtendedMenuItem* output = dynamic_cast<ExtendedMenuItem*>(menuItem);

  return output;
}


ExtendedMenuItem* GetClickedMenuItem(wxCommandEvent& evt) {
  wxMenu* menu = dynamic_cast<wxMenu*>(evt.GetEventObject());
  if (!menu) return NULL;

  return GetMenuItemById(menu, evt.GetId());
}