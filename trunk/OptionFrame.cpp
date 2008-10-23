#include "OptionFrame.h"
#include "Controller.h"
#include "Constants.h"


extern ControllerSP gController;


BEGIN_EVENT_TABLE(OptionFrame, wxFrame)
  EVT_SIZE(OptionFrame::OnSize)
  EVT_MOVE(OptionFrame::OnMove)
  EVT_ERASE_BACKGROUND(OptionFrame::OnEraseBackground)
  EVT_PAINT(OptionFrame::OnPaint)
END_EVENT_TABLE()


OptionFrame::OptionFrame()
: wxFrame(
  (wxFrame *)NULL,
  wxID_ANY,
  wxEmptyString,
  wxDefaultPosition,
  wxDefaultSize,
  0 | wxFRAME_SHAPED | wxNO_BORDER
  )
{  
  maskInvalidated_ = true;
  layoutInvalidated_ = true;
  openWidth_ = 0;

  // @todo: Get path from gController
  maskNineSlices_.LoadImage(gController->GetFilePaths().SkinDirectory + _T("/OptionFrameRegion.png"));
  // @todo: Grid should not be hardcoded but set in styles
  maskNineSlices_.SetGrid(4, 5, 59, 54);

  arrowButton_ = new ImageButton(this, wxID_ANY, wxDefaultPosition, wxDefaultSize);
  // @todo: Get path from gController
  arrowButton_->LoadImage(gController->GetFilePaths().SkinDirectory + _T("/ArrowButton"));
  // @todo: Grid should not be hardcoded but set in styles
  arrowButton_->SetGrid(6, 30, 1, 1);

  SetSize(gController->GetStyles().OptionPanel.ArrowButtonWidth, 50);
}


void OptionFrame::InvalidateLayout() {
  layoutInvalidated_ = true;
  Refresh();
}


void OptionFrame::InvalidateMask() {
  maskInvalidated_ = true;
  Refresh();
}


void OptionFrame::UpdateLayout() {
  UpdateLayout(GetRect().GetWidth(), GetRect().GetHeight());
}


void OptionFrame::UpdateLayout(int width, int height) {
  layoutInvalidated_ = false;

  arrowButton_->SetSize(0, 0, width, height);
}


void OptionFrame::UpdateMask() {
  maskInvalidated_ = false;

  // Create the bitmap on which the 9-slices scaled mask is going to be drawn
  wxBitmap maskBitmap = wxBitmap(GetRect().GetWidth(), GetRect().GetHeight());
  
  // Create a temporary DC to do the actual drawing and assign it the bitmap
  wxMemoryDC maskDC;
  maskDC.SelectObject(maskBitmap);
  
  // Draw the nine slices on the DC
  maskNineSlices_.Draw(&maskDC, 0, 0, maskBitmap.GetWidth(), maskBitmap.GetHeight());

  // Select NULL to release the bitmap
  maskDC.SelectObject(wxNullBitmap);

  // Create the region from the bitmap and assign it to the window
  wxRegion region(maskBitmap, MASK_COLOR);
  SetShape(region);
}


void OptionFrame::OnPaint(wxPaintEvent& evt) {
  wxPaintDC dc(this);

  if (layoutInvalidated_) UpdateLayout();
  if (maskInvalidated_) UpdateMask();
}


void OptionFrame::OnSize(wxSizeEvent& evt) {
  InvalidateLayout();
  InvalidateMask();
}


void OptionFrame::OnMove(wxMoveEvent& evt) {

}


void OptionFrame::OnEraseBackground(wxEraseEvent &evt) {

}