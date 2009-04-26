/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#include "Updater.h"
#include "XmlUtil.h"
#include "StringUtil.h"


bool Updater::CheckVersion(const wxString& url, UpdaterVersionInfo& versionInfoResult) {
  if (!wxApp::IsMainLoopRunning()) return false;

  bool success = false;
  
  wxHTTP get;
  get.SetHeader(_T("Content-type"), _T("text/xml; charset=utf-8"));
  get.SetTimeout(5);

  wxURL urlParser(url);

  wxStopWatch stopWatch;
  stopWatch.Start();
  wxString host = urlParser.GetServer();
  while (!get.Connect(host)) {
    wxSleep(1); 
    if (stopWatch.Time() > 1000 * 5) return false;
  }
      
  wxInputStream *httpStream = get.GetInputStream(urlParser.GetPath());

  if (get.GetError() == wxPROTO_NOERR) {
    wxString res;
    wxStringOutputStream out_stream(&res);
    httpStream->Read(out_stream);

    bool saveCondense = TiXmlDocument::IsWhiteSpaceCondensed();
    TiXmlDocument::SetCondenseWhiteSpace(false);
    TiXmlDocument doc;
    doc.Parse(res.mb_str());
    TiXmlDocument::SetCondenseWhiteSpace(saveCondense);

    TiXmlElement* root = doc.FirstChildElement("VersionInfo");
    if (root) {
      TiXmlHandle handle(root);

      versionInfoResult.Version = XmlUtil::ReadElementText(handle, "Number");
      versionInfoResult.PageURL = XmlUtil::ReadElementText(handle, "PageURL");
      versionInfoResult.DownloadURL = XmlUtil::ReadElementText(handle, "DownloadURL");
      versionInfoResult.ReleaseNotes = XmlUtil::ReadElementText(handle, "ReleaseNotes");

      success = versionInfoResult.Version != wxEmptyString;
    }    
  }

  wxDELETE(httpStream);
  get.Close();
  
  return success;
}


int Updater::CompareVersions(const wxString& v1, const wxString& v2) {
  // Equivalent to v1 - v2

  wxArrayString a1;
  wxArrayString a2;

  StringUtil::Split(v1, a1, _T("."));
  StringUtil::Split(v2, a2, _T("."));
  
  while (a1.Count() > a2.Count()) a1.RemoveAt(a1.Count() - 1, 1);
  while (a2.Count() > a1.Count()) a2.RemoveAt(a2.Count() - 1, 1);

  for (int i = 0; i < a1.Count(); i++) {
    long l1;
    if (!a1[i].ToLong(&l1)) break;
    long l2;
    if (!a2[i].ToLong(&l2)) break;

    if (l1 == l2) continue;
    if (l1 > l2) return +1;
    return -1;
  }

  return 0;
}

