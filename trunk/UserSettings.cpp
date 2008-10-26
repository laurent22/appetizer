#include "UserSettings.h"
#include <wx/fileconf.h>
#include "FilePaths.h"

#include "Controller.h"
extern ControllerSP gController;


UserSettings::UserSettings() {
  IconSize = 32;
}


void UserSettings::Load() {

}


void UserSettings::Save() {
  wxFileConfig config(_T(""), _T(""), FilePaths::ConfigFile, _T(""), wxCONFIG_USE_RELATIVE_PATH);
  config.SetPath(_T("/Config"));
  config.Write(_T("IconSize"), IconSize);
  config.Flush();
}