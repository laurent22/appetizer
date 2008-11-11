/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "precompiled.h"

#include "ApplicationTrayIcon.h"
#include "MiniLaunchBar.h"
#include "utilities/Utilities.h"


BEGIN_EVENT_TABLE(ApplicationTrayIcon, wxTaskBarIcon)
  EVT_MENU(wxID_ANY, ApplicationTrayIcon::OnMenuItemClick)
  EVT_TASKBAR_LEFT_UP(ApplicationTrayIcon::OnLeftUp)
END_EVENT_TABLE()


ApplicationTrayIcon::ApplicationTrayIcon() {}


wxMenu* ApplicationTrayIcon::CreatePopupMenu() {
  wxMenu* menu = new wxMenu();

  wxMenuItem* menuItem = new wxMenuItem(menu, ID_MENU_HideShow, wxGetApp().GetMainFrame()->IsVisible() ? _("Hide") : _("Show"));
  #ifdef __WINDOWS__
  wxFont font(menuItem->GetFont());
  font.SetWeight(wxFONTWEIGHT_BOLD);
  menuItem->SetFont(font);
  #endif
  menu->Append(menuItem);
  menu->AppendSeparator();
  if (wxGetApp().GetUtilities().IsApplicationOnRemoteDrive()) menu->Append(ID_MENU_Eject, _("Eject drive"));
  menu->Append(ID_MENU_Config, _("Configuration"));
  menu->AppendSeparator();
  menu->Append(ID_MENU_Exit, _("Close"));

  return menu;
}


void ApplicationTrayIcon::OnMenuItemClick(wxCommandEvent& evt) {
  int itemId = evt.GetId();

  switch (itemId) {

    case ID_MENU_Eject:

      wxGetApp().GetUtilities().EjectDriveAndExit();
      break;

    case ID_MENU_HideShow:

      wxGetApp().GetMainFrame()->Show(!wxGetApp().GetMainFrame()->IsVisible());
      break;

    case ID_MENU_Config:

      wxGetApp().GetUtilities().ShowConfigDialog();
      break;

    case ID_MENU_Exit:

      wxGetApp().GetMainFrame()->Close();
      break;

  }
}


void ApplicationTrayIcon::OnLeftUp(wxTaskBarIconEvent& evt) {
  if (!wxGetApp().GetMainFrame()->IsVisible()) {
    wxGetApp().GetMainFrame()->Show();
    wxGetApp().GetMainFrame()->Raise();
  } else {
    wxGetApp().GetMainFrame()->Raise();
  }
}