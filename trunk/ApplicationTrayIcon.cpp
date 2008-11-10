/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <wx/font.h>
#include "ApplicationTrayIcon.h"
#include "MainFrame.h"
#include "utilities/Utilities.h"


extern MainFrame* gMainFrame;
extern Utilities gUtilities;


BEGIN_EVENT_TABLE(ApplicationTrayIcon, wxTaskBarIcon)
  EVT_MENU(wxID_ANY, ApplicationTrayIcon::OnMenuItemClick)
  EVT_TASKBAR_LEFT_UP(ApplicationTrayIcon::OnLeftUp)
END_EVENT_TABLE()


ApplicationTrayIcon::ApplicationTrayIcon() {}


wxMenu* ApplicationTrayIcon::CreatePopupMenu() {
  wxMenu* menu = new wxMenu();

  wxMenuItem* menuItem = new wxMenuItem(menu, ID_MENU_HideShow, gMainFrame->IsVisible() ? _("Hide") : _("Show"));
  #ifdef __WINDOWS__
  wxFont font(menuItem->GetFont());
  font.SetWeight(wxFONTWEIGHT_BOLD);
  menuItem->SetFont(font);
  #endif
  menu->Append(menuItem);
  menu->AppendSeparator();
  if (gUtilities.IsApplicationOnRemoteDrive()) menu->Append(ID_MENU_Eject, _("Eject drive"));
  menu->Append(ID_MENU_Config, _("Configuration"));
  menu->AppendSeparator();
  menu->Append(ID_MENU_Exit, _("Close"));

  return menu;
}


void ApplicationTrayIcon::OnMenuItemClick(wxCommandEvent& evt) {
  int itemId = evt.GetId();

  switch (itemId) {

    case ID_MENU_Eject:

      gUtilities.EjectDriveAndExit();
      break;

    case ID_MENU_HideShow:

      gMainFrame->Show(!gMainFrame->IsVisible());
      break;

    case ID_MENU_Config:

      gUtilities.ShowConfigDialog();
      break;

    case ID_MENU_Exit:

      gMainFrame->Close();
      break;

  }
}


void ApplicationTrayIcon::OnLeftUp(wxTaskBarIconEvent& evt) {
  if (!gMainFrame->IsVisible()) {
    gMainFrame->Show();
    gMainFrame->Raise();
  } else {
    gMainFrame->Raise();
  }
}