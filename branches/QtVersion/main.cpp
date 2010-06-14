/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#include <Application.h>

using namespace appetizer;




#if defined(__WINDOWS__) && defined(__DEBUG__)

// Code to display the memory leak report
// We use a custom report hook to filter out Qt's own memory leaks
// Credit to Andreas Schmidts - http://www.schmidt-web-berlin.de/winfig/blog/?p=154

#define FALSE   0
#define TRUE    1

_CRT_REPORT_HOOK prevHook;

int customReportHook(int reportType, char *message, int *returnValue) {
  // This function is called several times for each memory leak.
  // Each time a part of the error message is supplied.
  // This holds number of subsequent detail messages after
  // a leak was reported
  const int numFollowupDebugMsgParts = 2;
  static bool ignoreMessage = false;
  static int debugMsgPartsCount = 0;
 
  // check if the memory leak reporting starts
  if ((strncmp(message,"Detected memory leaks!\n", 10) == 0)
    || ignoreMessage)
  {
    // check if the memory leak reporting ends
    if (strncmp(message,"Object dump complete.\n", 10) == 0)
    {
      _CrtSetReportHook(prevHook);
      ignoreMessage = false;
    } else
      ignoreMessage = true;
 
    // something from our own code?
    if(strstr(message, ".cpp") == NULL)
    {
      if(debugMsgPartsCount++ < numFollowupDebugMsgParts)
        // give it back to _CrtDbgReport() to be printed to the console
        return FALSE;
      else
        return TRUE;  // ignore it
    } else
    {
      debugMsgPartsCount = 0;
      // give it back to _CrtDbgReport() to be printed to the console
      return FALSE;
    }
  } else
    // give it back to _CrtDbgReport() to be printed to the console
    return FALSE;
}

#endif // __WINDOWS__ && __DEBUG__



int main(int argc, char *argv[]) {
  #if defined(__WINDOWS__) && defined(__DEBUG__)
  _CrtSetDbgFlag ( _CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF );
  prevHook = _CrtSetReportHook(customReportHook);
  #endif // __WINDOWS__ && __DEBUG__

  Application app(argc, argv);
  app.setOrganizationName("Appetizer Project");
  app.setApplicationName("Appetizer");
  return app.exec();
}