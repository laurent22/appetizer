/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "ApplicationTrayIcon.h"
#include "MainFrame.h"
#include "utilities/Utilities.h"


extern MainFrame* gMainFrame;
extern Utilities gUtilities;


BEGIN_EVENT_TABLE(ApplicationTrayIcon, wxTaskBarIcon)
  EVT_MENU(ID_MENU_HideShow, ApplicationTrayIcon::OnMenuHideShow)
  EVT_MENU(ID_MENU_Exit, ApplicationTrayIcon::OnMenuExit)
  EVT_MENU(ID_MENU_Config, ApplicationTrayIcon::OnMenuConfig)
  EVT_TASKBAR_LEFT_UP(ApplicationTrayIcon::OnLeftUp)
END_EVENT_TABLE()


ApplicationTrayIcon::ApplicationTrayIcon() {}


wxMenu* ApplicationTrayIcon::CreatePopupMenu() {
  wxMenu* menu = new wxMenu();

  menu->Append(ID_MENU_HideShow, gMainFrame->IsVisible() ? _("Hide") : _("Show"));
  menu->AppendSeparator();
  menu->Append(ID_MENU_Config, _("Configuration"));
  menu->AppendSeparator();
  menu->Append(ID_MENU_Exit, _("Close"));

  return menu;
}


void ApplicationTrayIcon::OnMenuConfig(wxCommandEvent& evt) {
  gUtilities.ShowConfigDialog();
}


void ApplicationTrayIcon::OnMenuHideShow(wxCommandEvent& evt) {
  gMainFrame->Show(!gMainFrame->IsVisible());
}


void ApplicationTrayIcon::OnMenuExit(wxCommandEvent& evt) {
  gMainFrame->Close();
}


void ApplicationTrayIcon::OnLeftUp(wxTaskBarIconEvent& evt) {
  if (!gMainFrame->IsVisible()) {
    gMainFrame->Show();
    gMainFrame->Raise();
  } else {
    gMainFrame->Raise();
  }
}