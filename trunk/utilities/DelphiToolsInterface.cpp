#include "DelphiToolsInterface.h"
#include <wx/filename.h>


wxString DelphiToolsInterface::GetFileDescription(const wxString& filePath) {
  #ifdef __WIN32__

  typedef wxChar* (*GetVersionInfo_FileDescription)(const wxChar*);
  GetVersionInfo_FileDescription _GetVersionInfo_FileDescription;

  HINSTANCE hInstLibrary = LoadLibrary(_T("Data\\DelphiTools.dll"));

  if (hInstLibrary) {
    _GetVersionInfo_FileDescription = (GetVersionInfo_FileDescription)GetProcAddress(hInstLibrary, "GetVersionInfo_FileDescription");
    wxChar* outputChar = _GetVersionInfo_FileDescription(filePath.c_str());
    FreeLibrary(hInstLibrary);

    wxString output(outputChar);

    return output;
  } else {
    wxLogDebug(_T("DelphiTools.dll failed to load!"));
  }

  #endif // __WIN32__

  // If we couldn't get the file description field, return the
  // filename (without the extension)
  wxFileName filename(filePath);

  return filename.GetName();
}