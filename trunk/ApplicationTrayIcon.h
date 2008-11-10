/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#ifndef __ApplicationTrayIcon_H
#define __ApplicationTrayIcon_H

#include <wx/wx.h>
#include <wx/taskbar.h>
#include <wx/menu.h>
#include "Enumerations.h"


class ApplicationTrayIcon : public wxTaskBarIcon {

public:

  ApplicationTrayIcon();
  wxMenu* CreatePopupMenu();

  void OnLeftUp(wxTaskBarIconEvent& evt);
  void OnMenuItemClick(wxCommandEvent& evt);

  DECLARE_EVENT_TABLE()

};


#endif // __ApplicationTrayIcon_H