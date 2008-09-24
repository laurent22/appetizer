#include "BitmapControl.h"
#include "wx/dcbuffer.h"


BEGIN_EVENT_TABLE(BitmapControl, wxPanel)
  EVT_PAINT(BitmapControl::OnPaint)
  EVT_ERASE_BACKGROUND(BitmapControl::OnEraseBackground)
  EVT_SIZE(BitmapControl::OnSize)
  EVT_MOVE(BitmapControl::OnMove)
END_EVENT_TABLE()


BitmapControl::BitmapControl(wxWindow *owner, int id, wxPoint point, wxSize size):
wxPanel(owner, id, point, size) {
  //, 0 | wxFRAME_SHAPED | wxSIMPLE_BORDER | wxTRANSPARENT_WINDOW
  //SetBackgroundStyle(wxBG_STYLE_CUSTOM);
  controlBitmap_ = NULL;
}


wxBitmap* BitmapControl::GetControlBitmap() { return controlBitmap_; }

void BitmapControl::InvalidateControlBitmap() {
  // TODO? Whenever the bitmap is invalidated, it should probably
  // be destroyed and recreated, so that parts of the old bitmap
  // don't appear in the new one.
  controlBitmapInvalidated_ = true;
  Refresh();
}

void BitmapControl::UpdateControlBitmap() {
  if (!controlBitmap_) {
    controlBitmap_ = new wxBitmap(GetRect().GetWidth(), GetRect().GetHeight(), 32);
    // TODO: We need UseAlpha() so that the alpha channel is used when drawing
    // the bitmap. However UseAlpha() is an undocumented method so it would
    // be good to find an alternative.
    controlBitmap_->UseAlpha();
  }

  // Mark the bitmap as invalidated. It will be updated in the next PAINT event
  controlBitmapInvalidated_ = false;
}


void BitmapControl::OnSize(wxSizeEvent& evt) {
  wxDELETE(controlBitmap_);
	InvalidateControlBitmap();
}


void BitmapControl::OnMove(wxMoveEvent& evt) {
  // Refresh the control since its background must
  // be repainted
  Refresh();
}


void BitmapControl::OnEraseBackground(wxEraseEvent &evt) {
  // Cancel the default behaviour to avoid flickering.
}


void BitmapControl::OnPaint(wxPaintEvent& evt) {

  // Update the control bitmap if it has been invalidated
  if (controlBitmapInvalidated_) {
    UpdateControlBitmap();
    controlBitmapInvalidated_ = false;
  }

  wxBufferedPaintDC dc(this);

  // Get the parent and check if it's an BitmapControl. If it's not, the dynamic cast
  // returns NULL.
  BitmapControl* parent = dynamic_cast<BitmapControl*>(GetParent());

  if (parent) {
    wxBitmap* parentBitmap = parent->GetControlBitmap();
    // Copy the parent sub bitmap as a background of this control

    // TODO: Check that the rectangle is not off-bounds
    wxBitmap subBitmap = parentBitmap->GetSubBitmap(this->GetRect());
    dc.DrawBitmap(subBitmap, 0, 0);
  }

  // Finally, blit the control bitmap
  if (controlBitmap_) dc.DrawBitmap(*controlBitmap_, 0, 0);
}