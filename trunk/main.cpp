/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"


#include "MiniLaunchBar.h"
#include "MessageBoxes.h"
#include "Constants.h"
#include "FilePaths.h"
#include "Localization.h"
#include "gui/BetterMessageDialog.h"
#include "Styles.h"
#include "utilities/IconGetter.h"
#include "utilities/VersionInfo.h"
#include "utilities/Updater.h"


// Implement the application
IMPLEMENT_APP(MiniLaunchBar) 


int MiniLaunchBar::uniqueInt_ = 0;


/**
 * Initialize the application
 */
bool MiniLaunchBar::OnInit() {
  //_CrtSetBreakAlloc(18609);

  // Remove GUI log errors
  delete wxLog::SetActiveTarget(new wxLogStderr());

  #ifdef __WINDOWS__
  osInfo_.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
  BOOL gotInfo = GetVersionEx(&osInfo_);
  if (!gotInfo) {
    osInfo_.dwMajorVersion = 5; // Assume Windows 2000
    osInfo_.dwMinorVersion = 0;
  }  
  #endif // __WINDOWS__

  singleInstanceChecker_ = NULL;
  mainFrame_ = NULL;
  locale_ = NULL;
  pluginManager_ = NULL;
  ftLibrary_ = NULL;
  ftFace_ = NULL;
  stopWatch_.Start();
  user_ = new User();

  allowedIconSizes_.push_back(SMALL_ICON_SIZE);
  allowedIconSizes_.push_back(MEDIUM_ICON_SIZE);
  allowedIconSizes_.push_back(LARGE_ICON_SIZE);
  allowedIconSizes_.push_back(EXTRA_LARGE_ICON_SIZE);

  // ***********************************************************************************
  // Initialize the command line object
  // ***********************************************************************************

  wxCmdLineEntryDesc cmdLineDesc[] = {
    { wxCMD_LINE_SWITCH, _T("u"), _T("useuserdatadir"), _("Uses user data directory to save settings.") },
    { wxCMD_LINE_OPTION, _T("d"), _T("datapath"),  _("Sets user data path (-u will be ignored)") },
    { wxCMD_LINE_SWITCH, _T("l"), _T("logwindow"), _("Displays the log window (useful for debugging)") },
    { wxCMD_LINE_SWITCH, _T("?"), _T("help"), _("Displays this help message") },
    { wxCMD_LINE_NONE }
  };

  fileCommandLine_.SetDesc(cmdLineDesc);  

  wxFileName executablePath = wxFileName(wxStandardPaths().GetExecutablePath());
  wxString applicationDirectory = executablePath.GetPath();
  wxString argumentsFilePath = applicationDirectory + _T("/Arguments.txt");

  if (wxFileName::FileExists(argumentsFilePath)) {

    wxTextFile file;
    bool success = file.Open(argumentsFilePath);
    if (!success) {
      ELOG(_T("Couldn't open ") + argumentsFilePath);
    } else {
      wxString line;
      wxString fileArgv;

      for (line = file.GetFirstLine(); !file.Eof(); line = file.GetNextLine()) {
        line = line.Trim(true).Trim(false);
        if (line == wxEmptyString) continue;        
        fileArgv += _T(" ") + line;        
      }

      fileCommandLine_.SetCmdLine(fileArgv);
      fileCommandLine_.Parse();
    }

  }

  commandLine_.SetDesc(cmdLineDesc);
  commandLine_.SetCmdLine(argc, argv);
  commandLine_.Parse(); 

  if (commandLine_.Found(_T("?"))) commandLine_.Usage();

  // Required to enable PNG support
  wxInitAllImageHandlers();

  // Setting this option to "0" removed the flickering.
  wxSystemOptions::SetOption(_T("msw.window.no-clip-children"), _T("0"));

  // ***********************************************************************************
  // Initialize the file paths
  // ***********************************************************************************

  FilePaths::InitializePaths();

  // If the setting file doesn't exist, assume it's the first time the app is launched
  isFirstLaunch_ = !wxFileName::FileExists(FilePaths::GetSettingsFile());

  // ***********************************************************************************
  // Initialize user
  // ***********************************************************************************

  GetUser()->Load();
  UserSettings* userSettings = GetUser()->GetSettings();

  // ***********************************************************************************
  // Initialize locale
  // ***********************************************************************************

  bool localeSet = false;

  if (IsFirstLaunch()) {
    // If it is the first launch, try to detect the language

    int systemLanguage = wxLocale::GetSystemLanguage();
    
    if (systemLanguage != wxLANGUAGE_UNKNOWN) {
      const wxLanguageInfo* info = wxLocale::GetLanguageInfo(systemLanguage);
      if (info) {
        localeSet = ChangeLocale(info->CanonicalName);
        if (localeSet) {
          userSettings->SetString(_T("Locale"), Localization::Instance()->GetLanguageCodeOnly(info->CanonicalName));
        }
      }
    }
  } else {
    localeSet = ChangeLocale(userSettings->GetString(_T("Locale")));
  }
    
  if (!localeSet) {
    localeSet = ChangeLocale(_T("en"));
    if (!localeSet) {
      MessageBoxes::ShowError(_("Could not initialize locale."));
      ::wxExit();
    }
  }  

  // ***********************************************************************************
  // At this point, user settings are loaded
  // ***********************************************************************************

  if (userSettings->GetBool(_T("UniqueApplicationInstance"))) {
    const wxString name = wxString::Format(_T("%s-%s"), APPLICATION_NAME, wxGetUserId());
    singleInstanceChecker_ = new wxSingleInstanceChecker(name);

    if (singleInstanceChecker_->IsAnotherRunning()) {
      ILOG(_T("Another instance of the application is already running."));
      wxDELETE(singleInstanceChecker_);
      return false;
    }
  }

  // ***********************************************************************************
  // Load the skin file
  // ***********************************************************************************
  Styles::LoadSkinFile(FilePaths::GetSkinDirectory() + _T("/") + SKIN_FILE_NAME);

  // ***********************************************************************************
  // Create and initialize the main frame
  // ***********************************************************************************
  mainFrame_ = new MainFrame();
  mainFrame_->Show();
  mainFrame_->SetRotated(userSettings->GetBool(_T("Rotated")));  
  mainFrame_->UpdateTransparency();

  SetTopWindow(mainFrame_);

  if (IsFirstLaunch()) {
    user_->AddAutoAddExclusion(_T("*setup.exe"));
    user_->AddAutoAddExclusion(_T("*unins*.exe"));
    user_->AddAutoAddExclusion(_T("*installer.exe"));
    user_->AddAutoAddExclusion(_T("*unstall.exe"));
    user_->AddAutoAddExclusion(_T("*updater.exe"));
    user_->AddAutoAddExclusion(_T("*unwise.exe"));
    user_->AddAutoAddExclusion(_T("*uninst.exe"));
    user_->AddAutoAddExclusion(_T("*setup*.exe"));
  } 

  if (userSettings->GetBool(_T("OptionPanelOpen"))) {
    mainFrame_->InvalidateLayout();
    mainFrame_->InvalidateMask();
    mainFrame_->Update();
    mainFrame_->OpenOptionPanel();
  }

  // ***********************************************************************************
  // Localize the main frame (this is going to recursively call
  // all the Localize() handlers)
  // ***********************************************************************************
  mainFrame_->Localize();  

  FilePaths::GetFontFilePath(_T("Arial"));

  // Note: the rest of the initialization code is in MainFrame::OnIdle (on the first IDLE event)

  return true;
} 


FT_Library MiniLaunchBar::GetFreeTypeLibrary() {
  if (!ftLibrary_) {
    FT_Error error = FT_Init_FreeType(&ftLibrary_);
    if (error) ELOG(_T("Error initializing FT library"));
  }
  return ftLibrary_;
}


FT_Face MiniLaunchBar::GetFreeTypeFace() {
  if (!ftFace_) {
    wxString fontName = Styles::Font.Face;
    wxString fontPath;

    if (Styles::Font.Weight == wxBOLD) {
      fontPath = FilePaths::GetFontFilePath(fontName + _T(" Bold"));
    }

    if (fontPath == wxEmptyString) fontPath = FilePaths::GetFontFilePath(fontName);

    FT_Error error = FT_New_Face(ftLibrary_, fontPath.mb_str(), 0, &ftFace_); 
    if (error) ELOG(_T("Error initializing FT face"));
  }
  return ftFace_;
}


const bool MiniLaunchBar::GetCommandLineFound(const wxString& name) {
  if (commandLine_.Found(name)) return true;
  return fileCommandLine_.Found(name);
}


void MiniLaunchBar::InitializePluginManager() {
  GetPluginManager()->Initialize();
}


int MiniLaunchBar::GetUniqueInt() {
  uniqueInt_++;
  return uniqueInt_;
}


PluginManager* MiniLaunchBar::GetPluginManager() {
  if (!pluginManager_) pluginManager_ = new PluginManager();
  return pluginManager_;
}


IntVector MiniLaunchBar::GetAllowedIconSizes() {
  return allowedIconSizes_;
}


wxString MiniLaunchBar::GetIconSizeName(int iconSize) {
  switch (iconSize) {

    case LARGE_ICON_SIZE: return _("Large"); break;
    case MEDIUM_ICON_SIZE: return _("Medium"); break;
    case EXTRA_LARGE_ICON_SIZE: return _("Extra Large (XP and above)"); break;
    case JUMBO_ICON_SIZE: return _("Jumbo (Vista and above)"); break;

  }

  return _("Small");
}


wxString MiniLaunchBar::GetContactEmail() {
  wxString e(_T("tizer.o"));
  e += _T("rg");
  e.Prepend(_T("app@e"));
  return e;
}



/**
 * Returns a reference to the main frame
 */
MainFrame* MiniLaunchBar::GetMainFrame() {
  return mainFrame_;
}


/**
 * Returns a reference to the utilities object
 */
Utilities& MiniLaunchBar::GetUtilities() {
  return utilities_;
}


/**
 * Returns a reference to the application command line parser
 */
const wxCmdLineParser& MiniLaunchBar::GetCommandLine() {
  return commandLine_;
}


/**
 * Returns the number of milliseconds since the application started
 */
long MiniLaunchBar::GetTimer() {
  return stopWatch_.Time();
}


/**
 * Returns true if this is the first time the application was launched
 */
bool MiniLaunchBar::IsFirstLaunch() {
  return isFirstLaunch_;
}


/**
 * Sets the folder item that is currently being dragged
 * @param folderItemId The ID of the folder items
 */
void MiniLaunchBar::SetDraggedFolderItem(int folderItemId) {
  draggedFolderItemId_ = folderItemId;
}


/**
 * Gets the folder items that is currently being dragged
 */
FolderItem* MiniLaunchBar::GetDraggedFolderItem() {
  if (draggedFolderItemId_ < 0 || !user_) return NULL;

  return user_->GetRootFolderItem()->GetChildById(draggedFolderItemId_);
}


/**
 * Returns a reference to the user object
 */
User* MiniLaunchBar::GetUser() {
  return user_;
}


/**
 * Check if the new version of the app is available
 * @param silent If this is set to false, no error messages will be displayed to the user
 */
void MiniLaunchBar::CheckForNewVersion(bool silent) {

  ILOG(_T("Looking for an update..."));
  UpdaterVersionInfo versionInfo;
  bool success = Updater::CheckVersion(CHECK_VERSION_URL, versionInfo);
  
  if (!success) {
    ELOG(_T("Could not get update information"));
    if (!silent) MessageBoxes::ShowError(_("Could not get update information"));
    return;
  }

  wxString thisVersion = VersionInfo::GetVersionString();

  ILOG(wxString::Format(_T("This version: %s"), thisVersion));
  ILOG(wxString::Format(_T("Current version: %s"), versionInfo.Version));
  ILOG(wxString::Format(_T("Page URL: %s"), versionInfo.PageURL));
  ILOG(wxString::Format(_T("Download URL: %s"), versionInfo.DownloadURL));
  ILOG(wxString::Format(_T("Release notes: %s"), versionInfo.ReleaseNotes));

  if (Updater::CompareVersions(thisVersion, versionInfo.Version) >= 0) {
    ILOG(_T("=> No new version"));
    if (!silent) MessageBoxes::ShowInformation(_("You have the latest version."));
    return;
  } else {
    ILOG(_T("=> A new version is available"));
  }

  wxString message;
  message = wxString::Format(_("A new version of Appetizer is available!\n\nYour version: %s\nNew version: %s\nRelease notes: %s\nDo you wish to download it now?"), thisVersion, versionInfo.Version, versionInfo.ReleaseNotes);
      
  int result = MessageBoxes::ShowConfirmation(message, 2|8, wxEmptyString, false, _("New version of Appetizer"));

  if (result == wxID_YES) {
    bool wasLaunched = ::wxLaunchDefaultBrowser(versionInfo.PageURL, wxBROWSER_NEW_WINDOW);
    if (!wasLaunched) MessageBoxes::ShowError(_("Error launching web browser"));
  }
}


int MiniLaunchBar::GetOSValidIconSize(int requiredIconSize) {
  #ifdef __WINDOWS__

  int major = osInfo_.dwMajorVersion;
  int minor = osInfo_.dwMinorVersion;

  if (major < 5) {

    // Before Windows 2000
    if (requiredIconSize > 32) return 32;

  } else if (major == 5) {

    if (minor < 1) {
      // Windows 2000
      if (requiredIconSize > 32) return 32;
    } else {
      // Windows XP
      if (requiredIconSize > 48) return 48;
    }

  } else {
    
    // Vista and above
    if (requiredIconSize > 256) return 256;
  }

  #endif // __WINDOWS__

  return requiredIconSize;
}


/**
 * Change the application locale
 * @param localeCode The new locale code
 * @return true if the change was successful
 */
bool MiniLaunchBar::ChangeLocale(const wxString& localeCode) {
  const wxLanguageInfo* info = wxLocale::FindLanguageInfo(localeCode);
  if (!info) {
    ELOG(_T("Could not find language info for: ") + localeCode);
    return false;
  }

  wxDELETE(locale_);

  locale_ = new wxLocale();
  locale_->Init(info->Language);
  locale_->AddCatalogLookupPathPrefix(FilePaths::GetLocalesDirectory());
  locale_->AddCatalog(_T("appetizer"));

  return true;
}


wxLocale* MiniLaunchBar::GetLocale() {
  return locale_;
}


/**
 * "CollectionChange" event handler. Everything that happens when a folder item is moved
 * deleted or added should be in this method.
 */
void MiniLaunchBar::FolderItems_CollectionChange() {
  GetMainFrame()->GetIconPanel()->InvalidateIcons();
  GetUser()->ScheduleSave();
}


/**
 * "FolderItemChange" event handler. Everything that happens when the properties of a folder item 
 * are changed should be in this method.
 */
void MiniLaunchBar::FolderItems_FolderItemChange(FolderItem* folderItem) {
  FolderItemRenderer* renderer = GetMainFrame()->GetIconPanel()->GetRendererFromFolderItem(*folderItem);
  if (!renderer) {
    // The folder item is not on the icon panel. It
    // may happen if it has just been created.
    return;
  }

  renderer->InvalidateControlBitmap();
  GetUser()->ScheduleSave();
}


/**
 * "LocaleChange" event handler. Everything that happens when the locale is changed
 * should be in this method.
 */
void MiniLaunchBar::User_LocaleChange() {
  GetMainFrame()->Localize();
  GetUtilities().Localize();
}


/**
 * "IconSizeChange" event handler. Everything that happens when the size of the icons
 * is changed should be in this method.
 */
void MiniLaunchBar::User_IconSizeChange() {
  GetMainFrame()->GetIconPanel()->ClearIcons();
  GetMainFrame()->GetIconPanel()->InvalidateIcons();
  GetMainFrame()->GetIconPanel()->InvalidateLayout();
}


/**
 * Close the application and do some clean-up. Don't call this method directly,
 * it's going to be called from the MainFrame::OnClose event handler. The reason we
 * need to do the cleanup here and not in OnExit is that OnExit is not always going to
 * be called when closing the frame, for example if the user_ or utilities_ objects
 * have created some frames or dialogs that are still open. Destroying them here explicitely
 * will allow the application to properly close.
 */ 
void MiniLaunchBar::CloseApplication() {
  GetUser()->GetSettings()->SetBool(_T("OptionPanelOpen"), GetMainFrame()->IsOptionPanelOpen());  
  GetUser()->Save(true);

  wxDELETE(user_);
  wxDELETE(singleInstanceChecker_);
  wxDELETE(locale_);
  wxDELETE(pluginManager_);

  if (ftLibrary_) {
    FT_Done_FreeType(ftLibrary_);
    ftLibrary_ = NULL;
  }

  BetterMessageDialog::DestroyInstance();
  Localization::Destroy();
  utilities_.~Utilities();
  IconGetter::Destroy();
  FolderItem::DestroyStaticData();
}


/**
 * "Exit" event handler
 */
int MiniLaunchBar::OnExit() {
  return 0;
}