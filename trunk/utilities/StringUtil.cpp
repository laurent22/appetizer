/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#include "StringUtil.h"

#define Slice(str, start, end) (str.Mid(start, end))


wxString StringUtil::ReadUTF8File(const wxString& filePath) {
  wxFile f;
  if (!f.Open(filePath)) return _T("");

  int size = f.Length();
  char* buffer = new char[size + 1];
  int sizeRead = f.Read(buffer, size);

  if (sizeRead >= 3) {    
    if ((buffer[0] == 'ï') && (buffer[1] == '»') && (buffer[2] == '¿')) { // ï»¿ == EF BB BG == BOM
      char* newBuffer = new char[size];
      int i;
      for (i = 3; i < strlen(buffer); i++) newBuffer[i - 3] = buffer[i];
      //delete[] buffer;
      buffer = newBuffer;
      size -= 3;
    }
  }

  if (strlen(buffer) >= size) buffer[size] = '\0';

  wxString conv(buffer, wxConvUTF8);

  //delete[] buffer;

  return conv;
}


wxString StringUtil::ZeroPadding(int number, int digits) {
  wxString output;
  output << number;
  
  while (output.Len() < digits) output = _T("0") + output;

  return output;
}


wxString StringUtil::RemoveDriveFromPath(const wxString& path) {
  wxFileName f(path);
  f.Normalize();

  wxString dataDirNoDrive = f.GetFullPath();
  int colonIndex = dataDirNoDrive.Index(_T(":"));

  if (colonIndex != wxNOT_FOUND) {
    dataDirNoDrive = dataDirNoDrive.Mid(colonIndex + 1, dataDirNoDrive.Len());

    for (int i = 0; i < dataDirNoDrive.Len(); i++) {
      wxChar c = dataDirNoDrive[i];
      if (c != wxFileName::GetPathSeparator()) break;
      dataDirNoDrive = dataDirNoDrive.Mid(1, dataDirNoDrive.Len());
      i--;
    }
  }

  return dataDirNoDrive;
}


void StringUtil::Split(const wxString& toSplit, wxArrayString& resultArray, const wxString& delimiter) {
  wxString currentString;
  int delimiterLength = delimiter.Len();
  bool lastTokenIsDelimiter = false;

  for (int i = 0; i < toSplit.Len(); i++) {
    wxString s = toSplit.Mid(i, delimiterLength);
    
    if (s == delimiter) {
      resultArray.Add(currentString);
      currentString = wxEmptyString;
      i += delimiterLength - 1;
      lastTokenIsDelimiter = true;
    } else {
      currentString = currentString + s;
      lastTokenIsDelimiter = false;
    }
  }

  if (lastTokenIsDelimiter) {
    resultArray.Add(wxEmptyString);
  } else {
    resultArray.Add(currentString);
  }

} 


bool StringUtil::FileMatchesPattern(const wxString& pattern, const wxString& filePath) {
  wxString p(pattern);
  wxString f(filePath);

  p = p.Lower();
  f = f.Lower();

  p.Replace(_T("\\"), _T("\\\\"), true);
  p.Replace(_T("/"), _T("\\"), true);
  p.Replace(_T("."), _T("\\."), true);
  p.Replace(_T("*"), _T(".*"), true);  
  p.Replace(_T("?"), _T("."), true);  

  f.Replace(_T("/"), _T("\\"));

  wxRegEx regex(p, wxRE_ADVANCED);
  return regex.Matches(f);
}