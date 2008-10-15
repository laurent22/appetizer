#include "wx/wx.h" 
#include "wx/stdpaths.h"
#include "wx/filename.h"
#include "Controller.h"
#include "Constants.h"

Controller::Controller() {
  mainFrame_ = NULL;
  user_ = NULL;
  draggedFolderItem_ = NULL;  

  /****************************************************************************
   * Initialize paths
   ***************************************************************************/

  wxFileName executablePath = wxFileName(wxStandardPaths().GetExecutablePath());

  wxString applicationDirectory = executablePath.GetPath();

  wxFileName::SplitPath(executablePath.GetPath(), &applicationDrive_, NULL, NULL, NULL, false, wxPATH_NATIVE);

  #ifdef __WIN32__
  applicationDrive_ += _T(":");
  #endif // __WIN32__

  filePaths_.ApplicationDirectory = applicationDirectory;
  filePaths_.DataDirectory = applicationDirectory + wxT("\\") + DATA_FOLDER_NAME;
  filePaths_.SettingsDirectory = filePaths_.DataDirectory + wxT("\\") + SETTING_FOLDER_NAME;
  filePaths_.SkinDirectory = filePaths_.DataDirectory + wxT("\\") + SKIN_FOLDER_NAME + wxT("\\Default");
  filePaths_.LocalesDirectory = filePaths_.DataDirectory + wxT("\\") + LOCALES_FOLDER_NAME;
  filePaths_.ConfigFile = filePaths_.SettingsDirectory + wxT("\\") + CONFIG_FILE_NAME;
  filePaths_.IconsDirectory = filePaths_.SkinDirectory + wxT("\\") + ICONS_FOLDER_NAME;

  /****************************************************************************
   * Initialize styles
   ***************************************************************************/

  styles_ = ControllerStyles();

  styles_.MainPanel.PaddingLeft = 8;
  styles_.MainPanel.PaddingRight = 8;
  styles_.MainPanel.PaddingTop = 8;
  styles_.MainPanel.PaddingBottom = 8;
  styles_.MainPanel.PaddingWidth = styles_.MainPanel.PaddingLeft + styles_.MainPanel.PaddingRight;
  styles_.MainPanel.PaddingHeight = styles_.MainPanel.PaddingTop + styles_.MainPanel.PaddingBottom;

  styles_.InnerPanel.PaddingLeft = 8;
  styles_.InnerPanel.PaddingRight = 8;
  styles_.InnerPanel.PaddingTop = 8;
  styles_.InnerPanel.PaddingBottom = 8;
  styles_.InnerPanel.PaddingWidth = styles_.InnerPanel.PaddingLeft + styles_.InnerPanel.PaddingRight;
  styles_.InnerPanel.PaddingHeight = styles_.InnerPanel.PaddingTop + styles_.InnerPanel.PaddingBottom;

  styles_.Icon.PaddingLeft = 4;
  styles_.Icon.PaddingRight = 4;
  styles_.Icon.PaddingTop = 4;
  styles_.Icon.PaddingBottom = 4;
  styles_.Icon.PaddingWidth = styles_.Icon.PaddingLeft + styles_.Icon.PaddingRight;
  styles_.Icon.PaddingHeight = styles_.Icon.PaddingTop + styles_.Icon.PaddingBottom;

}


void Controller::Initialize() {
  user_ = new User();
}


void Controller::SetDraggedFolderItem(FolderItem* folderItem) {
  draggedFolderItem_ = folderItem;
}


FolderItem* Controller::GetDraggedFolderItem() {
  return draggedFolderItem_;
}


ControllerStyles Controller::GetStyles() {
  return styles_;
}


MainFrame* Controller::GetMainFrame() {
  return mainFrame_;
}


void Controller::SetMainFrame(MainFrame* mainFrame) {
  mainFrame_ = mainFrame;
}


wxString Controller::GetApplicationDrive() {
  return applicationDrive_;
}


User* Controller::GetUser() {
  return user_;
}


void Controller::User_FolderItemCollectionChange() {
  mainFrame_->GetIconPanel()->RefreshIcons();
}


FilePaths Controller::GetFilePaths() {
  return filePaths_;
}