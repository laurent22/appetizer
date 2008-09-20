#include "NineSlicesPainter.h"


NineSlicesPainter::NineSlicesPainter() {
  pGridIsExplicitelySet = false;
}


void NineSlicesPainter::LoadImage(const wxString& filePath) {  
  pFilePath = filePath;
  pSourceBitmap = wxBitmap(pFilePath, wxBITMAP_TYPE_PNG);
  pSourceDC.SelectObject(wxNullBitmap);
  // TODO: Should the previously selected bitmap be explicitely deleted?
  pSourceDC.SelectObject(pSourceBitmap);
}


void NineSlicesPainter::Draw(wxDC *destDC, wxCoord x, wxCoord y, wxCoord width, wxCoord height) {

  int destX, destY, destWidth, destHeight;
  int sourceX, sourceY, sourceWidth, sourceHeight;
  
  wxRect grid;

  if (pGridIsExplicitelySet) {
    grid = pGrid;
  } else {
    grid.SetLeft(floor((double)(pSourceBitmap.GetWidth() / 2)));
    grid.SetTop(floor((double)(pSourceBitmap.GetHeight() / 2)));
    grid.SetWidth(10);
    grid.SetHeight(10);
    pGrid = grid;
  }

  int rightWidth = pSourceBitmap.GetWidth() - grid.GetRight();
  int bottomHeight = pSourceBitmap.GetHeight() - grid.GetBottom();

  for (int i = 0; i < 9; i++) {

    switch(i) {

      case 0: case 3: case 6:

        destX = 0;
        destWidth = grid.GetLeft();
        sourceX = 0;
        sourceWidth = grid.GetLeft();
        break;

      case 1: case 4: case 7:

        destX = grid.GetLeft();
        destWidth = width - destX - rightWidth;
        sourceX = grid.GetLeft();
        sourceWidth = grid.GetWidth();
        break;

      case 2: case 5: case 8:

        destX = width - rightWidth;
        destWidth = rightWidth;
        sourceX = grid.GetRight();
        sourceWidth = pSourceBitmap.GetWidth() - grid.GetRight();
        break;

    } // switch


    switch(i) {

      case 0:

        destY = 0;
        destHeight = grid.GetTop();
        sourceY = 0;
        sourceHeight = grid.GetTop();
        break;

      case 3:

        destY = grid.GetTop();
        destHeight = height - destY - bottomHeight;
        sourceY = grid.GetTop();
        sourceHeight = grid.GetHeight();
        break;

      case 6:

        destY = height - bottomHeight;
        destHeight = bottomHeight;
        sourceY = grid.GetBottom();
        sourceHeight = pSourceBitmap.GetHeight() - grid.GetBottom();
        break;

    } // switch

    Imaging::StretchBlit(destDC, &pSourceDC, destX, destY, destWidth, destHeight, sourceX, sourceY, sourceWidth, sourceHeight);

  } // for

}