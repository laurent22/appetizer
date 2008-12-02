/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#include "ApplicationTrayIcon.h"
#include "MiniLaunchBar.h"
#include "ExtendedMenuItem.h"
#include "utilities/Utilities.h"


BEGIN_EVENT_TABLE(ApplicationTrayIcon, wxTaskBarIcon)
  EVT_MENU(wxID_ANY, ApplicationTrayIcon::OnMenuItemClick)
  EVT_TASKBAR_LEFT_UP(ApplicationTrayIcon::OnLeftUp)
END_EVENT_TABLE()


ApplicationTrayIcon::ApplicationTrayIcon() {}


wxMenu* ApplicationTrayIcon::CreatePopupMenu() {
  wxMenu* menu = new wxMenu();
  ExtendedMenuItem* menuItem = NULL;

  menuItem = new ExtendedMenuItem(menu, wxGetApp().GetUniqueInt(), wxGetApp().GetMainFrame()->IsVisible() ? _("Hide") : _("Show"));
  #ifdef __WINDOWS__
  wxFont font(menuItem->GetFont());
  font.SetWeight(wxFONTWEIGHT_BOLD);
  menuItem->SetFont(font);
  #endif
  menuItem->SetMetadata(_T("name"), _T("hideShow"));
  menu->Append(menuItem);
  
  menu->AppendSeparator();
  
  if (wxGetApp().GetUtilities().IsApplicationOnPortableDrive()) {
    menuItem = new ExtendedMenuItem(menu, wxGetApp().GetUniqueInt(), _("Eject drive"));
    menuItem->SetMetadata(_T("name"), _T("ejectDrive"));
    menu->Append(menuItem);
  }

  menuItem = new ExtendedMenuItem(menu, wxGetApp().GetUniqueInt(), _("Configuration"));
  menuItem->SetMetadata(_T("name"), _T("configuration"));
  menu->Append(menuItem);

  menu->AppendSeparator();

  menuItem = new ExtendedMenuItem(menu, wxGetApp().GetUniqueInt(), _("Close"));
  menuItem->SetMetadata(_T("name"), _T("close"));
  menu->Append(menuItem);

  return menu;
}


void ApplicationTrayIcon::OnMenuItemClick(wxCommandEvent& evt) {
  ExtendedMenuItem* menuItem = GetClickedMenuItem(evt);
  wxString name = menuItem->GetMetadata(_T("name"));

  if (name == _T("hideShow")) {

    wxGetApp().GetMainFrame()->Show(!wxGetApp().GetMainFrame()->IsVisible());

  } else if (name == _T("ejectDrive")) {

    wxGetApp().GetUtilities().EjectDriveAndExit();

  } else if (name == _T("configuration")) {

    wxGetApp().GetUtilities().ShowConfigDialog();

  } else if (name == _T("close")) {

    wxGetApp().GetMainFrame()->Close();

  } else {
    
    evt.Skip();

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