/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <wx/wx.h>
#include <wx/sysopt.h>
#include <wx/stdpaths.h>
#include <wx/filename.h>

#ifdef __WXDEBUG__
#ifdef __WINDOWS__
// To find memory leaks, add _CrtSetBreakAlloc(int memoryBlock)
// just at the beginning of MiniLaunchBar::OnInit
#define _CRTDBG_MAP_ALLOC
#include <crtdbg.h>
#endif // __WINDOWS__
#endif // __WXDEBUG__

#include "MiniLaunchBar.h"
#include "MainFrame.h"
#include "MessageBoxes.h"
#include "Controller.h"
#include "Constants.h"
#include "FilePaths.h"
#include "Styles.h"
#include "Log.h"
#include "utilities/Utilities.h"


// We can't use a smart pointer for the main frame
// since it's going to be owned by the wxApp
MainFrame* gMainFrame = NULL;

// Implement the application
IMPLEMENT_APP(MiniLaunchBar) 

// Initialize the global controller
Controller gController;

// Some utility functions
Utilities gUtilities;


bool MiniLaunchBar::OnInit() {
  //_CrtSetBreakAlloc(1654);

  singleInstanceChecker_ = NULL;

  // ***********************************************************************************
  // Initialize the command line object
  // ***********************************************************************************

  wxCmdLineEntryDesc cmdLineDesc[] = {
    { wxCMD_LINE_SWITCH, _T("u"), _T("useuserdatadir"), _("Use user data directory to save settings.") },
    { wxCMD_LINE_OPTION, _T("d"), _T("datapath"),  _("Set user data path (-u will be ignored)") },
    { wxCMD_LINE_NONE }
  };

  commandLine_.SetDesc(cmdLineDesc);
  commandLine_.SetCmdLine(argc, argv);
  commandLine_.Parse(); 

  // Required to enabled PNG support
  wxInitAllImageHandlers();

  // Setting this option to "0" removed the flickering.
  wxSystemOptions::SetOption(_T("msw.window.no-clip-children"), _T("0"));

  // ***********************************************************************************
  // Initialize the file paths
  // ***********************************************************************************

  FilePaths::InitializePaths();

  gController.InitializeLocalization();
  gController.SetIsFirstLaunch(!wxFileName::FileExists(FilePaths::GetSettingsFile()));
  gController.GetUser()->Load();

  // ***********************************************************************************
  // At this point, user settings are loaded
  // ***********************************************************************************

  if (gController.GetUser()->GetSettings()->UniqueApplicationInstance) {
    const wxString name = wxString::Format(_T("%s-%s"), APPLICATION_NAME, wxGetUserId());
    singleInstanceChecker_ = new wxSingleInstanceChecker(name);

    if (singleInstanceChecker_->IsAnotherRunning()) {
      ilog(_T("Another instance of the application is already running."));
      wxDELETE(singleInstanceChecker_);
      return false;
    }
  }

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


const wxCmdLineParser& MiniLaunchBar::GetCommandLine() {
  return commandLine_;
}


int MiniLaunchBar::OnExit() {
  wxDELETE(singleInstanceChecker_);
  return 0;
}