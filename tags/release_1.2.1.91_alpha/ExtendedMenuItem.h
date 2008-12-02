/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#ifndef __ExtendedMenuItem_H
#define __ExtendedMenuItem_H


class ExtendedMenuItem : public wxMenuItem {

public:

  ExtendedMenuItem(wxMenu* parentMenu = NULL, int id = wxID_SEPARATOR, const wxString& text = wxEmptyString, const wxString& helpString = wxEmptyString, wxItemKind kind = wxITEM_NORMAL, wxMenu* subMenu = NULL);
  const wxString& GetMetadata(const wxString& name);
  int GetMetadataInt(const wxString& name);
  void* GetMetadataPointer(const wxString& name);
  void SetMetadata(const wxString& name, const wxString& value);
  void SetMetadata(const wxString& name, int value);
  void SetMetadataPointer(const wxString& name, void* value);

private:

  std::map<wxString, wxString> metadata_;
  std::map<wxString, int> metadataInt_;
  std::map<wxString, void*> metadataPointer_;

};


ExtendedMenuItem* GetMenuItemById(wxMenu* menu, int menuItemId);
ExtendedMenuItem* GetClickedMenuItem(wxCommandEvent& evt);



#endif // __ExtendedMenuItem_H