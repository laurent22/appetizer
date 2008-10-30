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


enum {
  ID_MENU_Exit,
  ID_MENU_HideShow
};


class ApplicationTrayIcon : public wxTaskBarIcon {

public:

  ApplicationTrayIcon();
  wxMenu* CreatePopupMenu();

  void OnMenuHideShow(wxCommandEvent& evt);
  void OnMenuExit(wxCommandEvent& evt);
  void OnLeftUp(wxTaskBarIconEvent& evt);

  DECLARE_EVENT_TABLE()

};


#endif // __ApplicationTrayIcon_H