/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "precompiled.h"

#include "Log.h"


void log__(wxString type, wxString s) {
  #ifdef __WINDOWS__
  // Doesn't work on Ubuntu:
  wxLogDebug(_T("[%s] %s"), type, s);
  #endif
}

void ilog(wxString s) {
  log__(_T("Info"), s);
}

void ilog(const char* s) { ilog(wxString(s, wxConvUTF8)); }

void elog(wxString s) {
  log__(_T("Error"), s);
}

void elog(const char* s) { elog(wxString(s, wxConvUTF8)); }

void wlog(wxString s) {
  log__(_T("Warning"), s);
}

void wlog(const char* s) { wlog(wxString(s, wxConvUTF8)); }