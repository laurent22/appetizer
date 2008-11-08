/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "StringUtil.h"

#define Slice(str, start, end) (str.Mid(start, end))


wxString StringUtil::ZeroPadding(int number, int digits) {
  wxString output;
  output << number;
  
  while (output.Len() < digits) output = _T("0") + output;

  return output;
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