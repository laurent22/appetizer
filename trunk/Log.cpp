#include "Log.h"


void log__(wxString type, wxString s) {
  wxLogDebug(_T("[%s] %s"), type, s);
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