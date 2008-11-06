/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#ifndef __Updater_H
#define __Updater_H

#include <wx/wx.h>



struct UpdaterVersionInfo {
  wxString Version;
  wxString PageURL;
  wxString DownloadURL;
  wxString ReleaseNotes;
};


class Updater {

public:

  static bool CheckVersion(const wxString& url, UpdaterVersionInfo& versionInfoResult);
  static int CompareVersions(const wxString& v1, const wxString& v2);

};

#endif // __Updater_H