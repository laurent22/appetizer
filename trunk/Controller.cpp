/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <wx/wx.h>
#include <wx/intl.h>
#include "Controller.h"
#include "MainFrame.h"
#include "MessageBoxes.h"
#include "FilePaths.h"
#include "Log.h"
#include "utilities/VersionInfo.h"
#include "utilities/Updater.h"
#include "utilities/Utilities.h"


extern MainFrame* gMainFrame;
extern Utilities gUtilities;


Controller::Controller() {
  isFirstLaunch_ = false;
  locale_ = NULL;
  draggedFolderItemId_ = -1;  
  stopWatch_.Start();
  user_.reset(new User());
}


Controller::~Controller() {
  wxDELETE(locale_);
}


bool Controller::ChangeLocale(const wxString& localeCode) {
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


void Controller::InitializeLocalization() {
  bool success = ChangeLocale(GetUser()->GetSettings()->Locale);
  if (!success) {
    success = ChangeLocale(_T("en"));
    if (!success) {
      MessageBoxes::ShowError(_("Could not initialize locale."));
      ::wxExit();
    }
  }
}


void Controller::SetIsFirstLaunch(bool value) {
  isFirstLaunch_ = value;
}


bool Controller::IsFirstLaunch() {
  return isFirstLaunch_;
}


long Controller::GetTimer() {
  return stopWatch_.Time();
}


void Controller::CheckForNewVersion(bool silent) {
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


void Controller::SetDraggedFolderItem(int folderItemId) {
  draggedFolderItemId_ = folderItemId;
}


FolderItemSP Controller::GetDraggedFolderItem() {
  if (draggedFolderItemId_ < 0 || !user_) {
    FolderItemSP nullOutput;
    return nullOutput;
  }
  return user_->GetRootFolderItem()->GetChildById(draggedFolderItemId_);
}


UserSP Controller::GetUser() {
  return user_;
}


void Controller::FolderItems_CollectionChange() {
  gMainFrame->GetIconPanel()->InvalidateIcons();
  GetUser()->ScheduleSave();
}


void Controller::FolderItems_FolderItemChange(FolderItemSP folderItem) {
  FolderItemRendererSP renderer = gMainFrame->GetIconPanel()->GetRendererFromFolderItem(*folderItem);
  if (!renderer.get()) {
    // The folder item is not on the icon panel. It
    // may happen if it has just been created.
    return;
  }

  renderer->InvalidateControlBitmap();
  GetUser()->ScheduleSave();
}


void Controller::User_LocaleChange() {
  gMainFrame->Localize();
  gUtilities.Localize();
}


void Controller::User_IconSizeChange() {
  gMainFrame->GetIconPanel()->ClearIcons();
  gMainFrame->GetIconPanel()->InvalidateIcons();
  gMainFrame->GetIconPanel()->InvalidateLayout();
}