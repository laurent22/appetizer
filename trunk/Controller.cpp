/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <wx/wx.h> 
#include "Controller.h"
#include "MainFrame.h"
#include "MessageBoxes.h"
#include "Localization.h"
#include "Log.h"
#include "utilities/VersionInfo.h"
#include "utilities/Updater.h"


extern MainFrame* gMainFrame;


Controller::Controller() {
  draggedFolderItemId_ = -1;  
  stopWatch_.Start();
  user_.reset(new User());
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
    if (!silent) MessageBoxes::ShowError(LOC(_T("Updater.Error")));
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
    if (!silent) MessageBoxes::ShowInformation(LOC(_T("Updater.NoNewVersion")));
    return;
  } else {
    ilog("=> A new version is available");
  }

  wxString message;
  message = LOC3(_T("Updater.NewVersion"), thisVersion, versionInfo.Version, versionInfo.ReleaseNotes);
      
  int result = MessageBoxes::ShowConfirmation(message);

  if (result == wxID_YES) {
    bool wasLaunched = ::wxLaunchDefaultBrowser(versionInfo.PageURL, wxBROWSER_NEW_WINDOW);
    if (!wasLaunched) MessageBoxes::ShowError(LOC(_T("Global.BrowserError")));
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
}


void Controller::User_IconSizeChange() {
  gMainFrame->GetIconPanel()->ClearIcons();
  gMainFrame->GetIconPanel()->InvalidateIcons();
  gMainFrame->GetIconPanel()->InvalidateLayout();
}