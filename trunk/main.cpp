/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "precompiled.h"


#include "MiniLaunchBar.h"
#include "MessageBoxes.h"
#include "Constants.h"
#include "FilePaths.h"
#include "Localization.h"
#include "Styles.h"
#include "Log.h"
#include "utilities/VersionInfo.h"
#include "utilities/Updater.h"


// Implement the application
IMPLEMENT_APP(MiniLaunchBar) 


/**
 * Initialize the application
 */
bool MiniLaunchBar::OnInit() {
  //_CrtSetBreakAlloc(7398);

  singleInstanceChecker_ = NULL;
  mainFrame_ = NULL;
  locale_ = NULL;
  stopWatch_.Start();
  user_ = new User();

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

  // If the setting file doesn't exist, assume it's the first time the app is launched
  isFirstLaunch_ = !wxFileName::FileExists(FilePaths::GetSettingsFile());

  // ***********************************************************************************
  // Initialize user
  // ***********************************************************************************

  GetUser()->Load();

  // ***********************************************************************************
  // Initialize locale
  // ***********************************************************************************

  bool success = ChangeLocale(GetUser()->GetSettings()->Locale);
  if (!success) {
    success = ChangeLocale(_T("en"));
    if (!success) {
      MessageBoxes::ShowError(_("Could not initialize locale."));
      ::wxExit();
    }
  }  

  // ***********************************************************************************
  // At this point, user settings are loaded
  // ***********************************************************************************

  if (GetUser()->GetSettings()->UniqueApplicationInstance) {
    const wxString name = wxString::Format(_T("%s-%s"), APPLICATION_NAME, wxGetUserId());
    singleInstanceChecker_ = new wxSingleInstanceChecker(name);

    if (singleInstanceChecker_->IsAnotherRunning()) {
      ilog(_T("Another instance of the application is already running."));
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
  mainFrame_->SetRotated(GetUser()->GetSettings()->Rotated);  

  SetTopWindow(mainFrame_);

  // ***********************************************************************************
  // Check if there are any new applications in the app folder
  // ***********************************************************************************
  GetUser()->AutomaticallyAddNewApps();

  if (IsFirstLaunch()) {
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

  return true;
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
 * Returns the number of milliseconds since the application start
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
FolderItemSP MiniLaunchBar::GetDraggedFolderItem() {
  if (draggedFolderItemId_ < 0 || !user_) {
    FolderItemSP nullOutput;
    return nullOutput;
  }
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
  ilog("Looking for an update...");
  UpdaterVersionInfo versionInfo;
  bool success = Updater::CheckVersion(CHECK_VERSION_URL, versionInfo);
  
  if (!success) {
    elog("Could not get update information");
    if (!silent) MessageBoxes::ShowError(_("Could not get update information"));
    return;
  }

  wxString thisVersion = VersionInfo::GetVersionString();

  ilog(wxString::Format(_T("This version: %s"), thisVersion));
  ilog(wxString::Format(_T("Current version: %s"), versionInfo.Version));
  ilog(wxString::Format(_T("Page URL: %s"), versionInfo.PageURL));
  ilog(wxString::Format(_T("Download URL: %s"), versionInfo.DownloadURL));
  ilog(wxString::Format(_T("Release notes: %s"), versionInfo.ReleaseNotes));

  if (Updater::CompareVersions(thisVersion, versionInfo.Version) >= 0) {
    ilog("=> No new version");
    if (!silent) MessageBoxes::ShowInformation(_("You have the latest version."));
    return;
  } else {
    ilog("=> A new version is available");
  }

  wxString message;
  message = wxString::Format(_("A new version is available!\n\nYour version: %s\nNew version: %s\nRelease notes: %s\n\nDo you wish to download it now?"), thisVersion, versionInfo.Version, versionInfo.ReleaseNotes);
      
  int result = MessageBoxes::ShowConfirmation(message);

  if (result == wxID_YES) {
    bool wasLaunched = ::wxLaunchDefaultBrowser(versionInfo.PageURL, wxBROWSER_NEW_WINDOW);
    if (!wasLaunched) MessageBoxes::ShowError(_("Error launching web browser"));
  }
}


/**
 * Change the application locale
 * @param localeCode The new locale code
 * @return true if the change was successful
 */
bool MiniLaunchBar::ChangeLocale(const wxString& localeCode) {
  const wxLanguageInfo* info = wxLocale::FindLanguageInfo(localeCode);
  if (!info) {
    elog(_T("Could not find language info for: ") + localeCode);
    return false;
  }

  wxDELETE(locale_);

	locale_ = new wxLocale();
  locale_->Init(info->Language);
  locale_->AddCatalogLookupPathPrefix(FilePaths::GetLocalesDirectory());
  locale_->AddCatalog(_T("appetizer"));

  return true;
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
void MiniLaunchBar::FolderItems_FolderItemChange(FolderItemSP folderItem) {
  FolderItemRendererSP renderer = GetMainFrame()->GetIconPanel()->GetRendererFromFolderItem(*folderItem);
  if (!renderer.get()) {
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
  user_->Save();

  wxDELETE(user_);
  wxDELETE(singleInstanceChecker_);
  wxDELETE(locale_);

  Localization::Destroy();
  utilities_.~Utilities();
}


/**
 * "Exit" event handler
 */
int MiniLaunchBar::OnExit() {
  return 0;
}