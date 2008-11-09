/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <wx/wx.h>
#include <wx/sysopt.h>
#include <wx/stdpaths.h>
#include <wx/filename.h>
#include <wx/cmdline.h>

#ifdef __WXDEBUG__
#ifdef __WINDOWS__
// To find memory leaks, add _CrtSetBreakAlloc(int memoryBlock)
// just at the beginning of MiniLaunchBar::OnInit
#define _CRTDBG_MAP_ALLOC
#include <crtdbg.h>
#endif // __WINDOWS__
#endif // __WXDEBUG__

#include "MainFrame.h"
#include "Controller.h"
#include "Constants.h"
#include "FilePaths.h"
#include "Styles.h"
#include "utilities/Utilities.h"


// The application class. An instance is created and initialized
// below in IMPLEMENT_APP()
class MiniLaunchBar: public wxApp {
  virtual bool OnInit();
};


IMPLEMENT_APP(MiniLaunchBar) 


// Initialize the global controller
Controller gController;

Utilities gUtilities;

// We can't use a smart pointer for the main frame
// since it's going to be owned by the wxApp
MainFrame* gMainFrame;

wxCmdLineParser gCommandLine;


bool MiniLaunchBar::OnInit() {
  //_CrtSetBreakAlloc(7471);

  wxCmdLineEntryDesc cmdLineDesc[] = {
    { wxCMD_LINE_SWITCH, _T("u"), _T("useuserdatadir"), _("Use user data directory to save settings.") },
    { wxCMD_LINE_OPTION, _T("d"), _T("datapath"),  _("Set user data path (-u will be ignored)") },
    { wxCMD_LINE_NONE }
  };

  gCommandLine.SetDesc(cmdLineDesc);
  gCommandLine.SetCmdLine(argc, argv);
  gCommandLine.Parse(); 

  // Required to enabled PNG support
  wxInitAllImageHandlers();

  // Setting this option to "0" removed the flickering.
  wxSystemOptions::SetOption(_T("msw.window.no-clip-children"), _T("0"));

  FilePaths::InitializePaths();

  gController.InitializeLocalization();
  gController.SetIsFirstLaunch(!wxFileName::FileExists(FilePaths::GetSettingsFile()));
  gController.GetUser()->Load();

  Styles::LoadSkinFile(FilePaths::GetSkinDirectory() + _T("/") + SKIN_FILE_NAME);

  gMainFrame = new MainFrame();
  gMainFrame->Show();
  gMainFrame->SetRotated(gController.GetUser()->GetSettings()->Rotated);  

  SetTopWindow(gMainFrame);

  gController.GetUser()->AutomaticallyAddNewApps();

  if (gController.IsFirstLaunch()) {
    gMainFrame->InvalidateLayout();
    gMainFrame->InvalidateMask();
    gMainFrame->Update();
    gMainFrame->OpenOptionPanel();
  } 

  gMainFrame->Localize();

  wxString skinName = _T("BlueGlass");
  gController.GetUser()->GetSettings()->Skin = skinName;
  gMainFrame->ApplySkin();

  return true;
} 