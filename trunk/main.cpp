#include <wx/wx.h>
#include <wx/sysopt.h>
#include <wx/stdpaths.h>
#include <wx/filename.h>
#include "MainFrame.h"
#include "Controller.h"
#include "Constants.h"
#include "FilePaths.h"
#include "Styles.h"
#include "Localization.h"
#include "utilities/DelphiToolsInterface.h"




// The application class. An instance is created and initialized
// below in IMPLEMENT_APP()
class MiniLaunchBar: public wxApp {
  virtual bool OnInit();
};


IMPLEMENT_APP(MiniLaunchBar) 


// Initialize the global controller
Controller gController;

// We can't use a smart pointer for the main frame
// since it's going to be owned by the wxApp
MainFrame* gMainFrame;


bool MiniLaunchBar::OnInit() {
  // Required to enabled PNG support
  wxInitAllImageHandlers();

  // Setting this option to "0" removed the flickering.
  wxSystemOptions::SetOption(_T("msw.window.no-clip-children"), _T("0"));

  wxFileName executablePath = wxFileName(wxStandardPaths().GetExecutablePath());
  wxString applicationDirectory = executablePath.GetPath();
  wxString applicationDrive;
  wxFileName::SplitPath(executablePath.GetPath(), &applicationDrive, NULL, NULL, NULL, false, wxPATH_NATIVE);

  FilePaths::ApplicationDrive = applicationDrive;
  #ifdef __WIN32__
  FilePaths::ApplicationDrive += _T(":");
  #endif // __WIN32__

  FilePaths::ApplicationDirectory = applicationDirectory;
  FilePaths::DataDirectory = applicationDirectory + _T("\\") + DATA_FOLDER_NAME;
  FilePaths::SettingsDirectory = FilePaths::DataDirectory + _T("\\") + SETTING_FOLDER_NAME;
  FilePaths::SkinDirectory = FilePaths::DataDirectory + _T("\\") + SKIN_FOLDER_NAME + _T("\\Default");
  FilePaths::LocalesDirectory = FilePaths::DataDirectory + _T("\\") + LOCALES_FOLDER_NAME;
  FilePaths::SettingsFile = FilePaths::SettingsDirectory + _T("\\") + SETTING_FILE_NAME;
  FilePaths::IconsDirectory = FilePaths::SkinDirectory + _T("\\") + ICONS_FOLDER_NAME;
  FilePaths::FolderItemsFile = FilePaths::SettingsDirectory + _T("\\") + FOLDER_ITEMS_FILE_NAME;
  FilePaths::WindowFile = FilePaths::SettingsDirectory + _T("\\") + WINDOW_FILE_NAME;  

  Styles::MainPanel.PaddingLeft = 8;
  Styles::MainPanel.PaddingRight = 8;
  Styles::MainPanel.PaddingTop = 8;
  Styles::MainPanel.PaddingBottom = 8;
  Styles::MainPanel.PaddingWidth = Styles::MainPanel.PaddingLeft + Styles::MainPanel.PaddingRight;
  Styles::MainPanel.PaddingHeight = Styles::MainPanel.PaddingTop + Styles::MainPanel.PaddingBottom;

  Styles::InnerPanel.PaddingLeft = 8;
  Styles::InnerPanel.PaddingRight = 8;
  Styles::InnerPanel.PaddingTop = 4;
  Styles::InnerPanel.PaddingBottom = 4;
  Styles::InnerPanel.PaddingWidth = Styles::InnerPanel.PaddingLeft + Styles::InnerPanel.PaddingRight;
  Styles::InnerPanel.PaddingHeight = Styles::InnerPanel.PaddingTop + Styles::InnerPanel.PaddingBottom;

  Styles::Icon.PaddingLeft = 4;
  Styles::Icon.PaddingRight = 4;
  Styles::Icon.PaddingTop = 4;
  Styles::Icon.PaddingBottom = 4;
  Styles::Icon.PaddingWidth = Styles::Icon.PaddingLeft + Styles::Icon.PaddingRight;
  Styles::Icon.PaddingHeight = Styles::Icon.PaddingTop + Styles::Icon.PaddingBottom;

  Styles::OptionPanel.PaddingLeft = 6;
  Styles::OptionPanel.PaddingRight = 6;
  Styles::OptionPanel.PaddingTop = 4;
  Styles::OptionPanel.PaddingBottom = 4;
  Styles::OptionPanel.PaddingWidth = Styles::OptionPanel.PaddingLeft + Styles::OptionPanel.PaddingRight;
  Styles::OptionPanel.PaddingHeight = Styles::OptionPanel.PaddingTop + Styles::OptionPanel.PaddingBottom;
  Styles::OptionPanel.ArrowButtonWidth = 16;
  Styles::OptionPanel.ButtonHGap = 4;
  Styles::OptionPanel.ButtonVGap = 3;

  DelphiToolsInterface::LoadDLL();
  gController.GetUser()->Load();

  Localization::Initialize();
  Localization::Instance->LoadLocale(gController.GetUser()->GetSettings()->Locale, FilePaths::LocalesDirectory);
  Localization::Instance->SetCurrentLocale(gController.GetUser()->GetSettings()->Locale);

  gMainFrame = new MainFrame();
  gMainFrame->Show(true);
  
  gController.GetUser()->AutomaticallyAddNewApps();

  SetTopWindow(gMainFrame);

  return true;
} 