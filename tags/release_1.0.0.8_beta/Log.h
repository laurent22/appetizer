/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "precompiled.h"

#ifndef __Log_H
#define __Log_H


void log__(wxString type, wxString s);
void ilog(wxString s);
void ilog(const char* s);
void elog(wxString s);
void elog(const char* s);
void wlog(wxString s);
void wlog(const char* s);


#endif // __Log_H