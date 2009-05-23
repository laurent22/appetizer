/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#include "StringUtil.h"

#define Slice(str, start, end) (str.Mid(start, end))


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


wxString StringUtil::RemoveTrailingSlash(const wxString& path) {
  wxString output(path);
  
  while (output.Length() > 0) {
    wxChar c = output[output.Length() - 1];
    if (c != '\\' && c != '/') return output;
    output = output.Mid(0, output.Length() - 1);
  }

  return output;
}


int StringUtil::CompareVersions(const wxString& v1, const wxString& v2) {
  wxArrayString s1;
  wxArrayString s2;
  StringUtil::Split(v1, s1, _T("."));
	StringUtil::Split(v2, s2, _T("."));

  std::vector<int> nv1;
  std::vector<int> nv2;

  for (int i = 0; i < s1.Count(); i++) {
    long n = 0;
    s1[i].ToLong(&n);
    nv1.push_back((int)n);		
  }

  for (int i = 0; i < s2.Count(); i++) {
    long n = 0;
    s2[i].ToLong(&n);
    nv2.push_back((int)n);		
  }
	
  while (nv1.size() < nv2.size()) nv1.push_back(0);
	while (nv2.size() < nv1.size()) nv2.push_back(0);
			
	for (int i = 0; i < nv1.size(); i++) {
		int e1 = nv1[i];
		int e2 = nv2[i];
		
		if (e1 > e2) return +1;
		if (e1 < e2) return -1;
	}

  return 0;
}