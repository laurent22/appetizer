#ifndef __FolderItemRenderer_H
#define __FolderItemRenderer_H

#include "wx/wx.h"
#include "BitmapControl.h"
#include "FolderItem.h"


class FolderItemRenderer: public BitmapControl {

public:

  FolderItemRenderer(wxWindow *owner, int id, wxPoint point, wxSize size);
  void LoadData(FolderItem* folderItem);
  void UpdateControlBitmap();
  void FitToContent();

private:

  FolderItem* folderItem_;

};


#endif // __FolderItemRenderer_H