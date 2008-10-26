#ifndef __Controller_H
#define __Controller_H

#include "wx/wx.h" 
#include "MainFrame.h"
#include "Constants.h"
#include "User.h"
#include "boost/shared_ptr.hpp"
#include <wx/msgdlg.h>
#include <wx/stopwatch.h>


class Controller {

  public:

    Controller();      
    UserSP GetUser();
    void SetDraggedFolderItem(int folderItemId);
    FolderItemSP GetDraggedFolderItem();
    int ShowWarningMessage(const wxString& message, long style = wxOK);
    int ShowErrorMessage(const wxString& message, long style = wxOK);
    long GetTimer();

    /**
     * GLOBAL EVENTS
     */

    void User_FolderItemCollectionChange();
    void User_FolderItemChange(FolderItemSP folderItem);

  private:
      
    UserSP user_;
    wxString applicationDrive_;
    int draggedFolderItemId_;
    wxStopWatch stopWatch_;

};

typedef boost::shared_ptr<Controller> ControllerSP;

#endif
