/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

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
  controlBitmap_ = NULL;
  horizontalFlip_ = false;
  bitmapRotation_ = 0;
}


void BitmapControl::SetHorizontalFlip(bool horizontalFlip) {
  if (horizontalFlip_ == horizontalFlip) return;
  horizontalFlip_ = horizontalFlip;
  InvalidateControlBitmap();
}


BitmapControl::~BitmapControl() {
  wxDELETE(controlBitmap_);
}


wxBitmap* BitmapControl::GetControlBitmap() { return controlBitmap_; }


/**
 * By calling this function, the control bitmap is going
 * to be redrawn on the next call to UpdateControlBitmap()
 */
void BitmapControl::InvalidateControlBitmap() {
  wxDELETE(controlBitmap_);
  controlBitmapInvalidated_ = true;
  Refresh();
}


/**
 * This is where the control bitmap should be drawn. Classes
 * which inherit from this class, should override this method.
 */
void BitmapControl::UpdateControlBitmap() {
  if (!controlBitmap_ && GetRect().GetWidth() > 0 && GetRect().GetHeight() > 0) {
    controlBitmap_ = new wxBitmap(GetRect().GetWidth(), GetRect().GetHeight(), 32);
    // We need UseAlpha() so that the alpha channel is used when drawing
    // the bitmap. However UseAlpha() is an undocumented method so it would
    // be good to find an alternative.
    // It seems like one of the side effect of using UseAlpha() is that
    // all the bitmaps drawn on controlBitmap_ MUST have an alpha channel,
    // otherwise we get an assertion error. See IconPanel::LoadImage for
    // an example on how to force a bitmap to have an alpha channel.
    controlBitmap_->UseAlpha();
    hasBeenFlipped_ = false;
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


int BitmapControl::GetBitmapRotation() {
  return bitmapRotation_;
}


void BitmapControl::SetBitmapRotation(int rotation) {
  bitmapRotation_ = rotation;
  InvalidateControlBitmap();
}


void BitmapControl::OnEraseBackground(wxEraseEvent &evt) {
  // Cancel the default behaviour to avoid flickering.
}


void BitmapControl::OnPaint(wxPaintEvent& evt) {
  wxBufferedPaintDC dc(this);

  // Update the control bitmap if it has been invalidated
  if (controlBitmapInvalidated_) {
    UpdateControlBitmap();
    controlBitmapInvalidated_ = false;
  }

  if (!controlBitmap_) return;

  if (horizontalFlip_ && !hasBeenFlipped_) {
    if (controlBitmap_->IsOk()) {
      wxImage tempImage = controlBitmap_->ConvertToImage();
      tempImage = tempImage.Mirror();
      wxDELETE(controlBitmap_);
      controlBitmap_ = new wxBitmap(tempImage);
      controlBitmap_->UseAlpha();
      hasBeenFlipped_ = true;
    }
  }

  if (controlBitmap_->GetWidth() <= 0 || controlBitmap_->GetHeight() <= 0) return; 

  // Get the parent and check if it's an BitmapControl. If it's not, the dynamic cast
  // returns NULL.
  BitmapControl* parent = dynamic_cast<BitmapControl*>(GetParent());

  if (parent) {
    wxBitmap* parentBitmap = parent->GetControlBitmap();    

    if (parentBitmap) {
      // Check that the rectangle is not off-bounds
      wxRect rect = this->GetRect();
      if (rect.GetLeft() < 0) rect.SetLeft(0);
      if (rect.GetTop() < 0) rect.SetTop(0);
      if (rect.GetRight() >= parentBitmap->GetWidth()) rect.SetRight(parentBitmap->GetWidth() - 1);
      if (rect.GetBottom() >= parentBitmap->GetHeight()) rect.SetBottom(parentBitmap->GetHeight() - 1);

      if (rect.GetWidth() > 0 && rect.GetHeight() > 0) {
        // Copy the parent sub bitmap as a background of this control
        wxBitmap subBitmap = parentBitmap->GetSubBitmap(rect);
        dc.DrawBitmap(subBitmap, 0, 0);
      }
    }
  }

  // Finally, blit the control bitmap
  if (controlBitmap_->GetWidth() > 0 && controlBitmap_->GetHeight() > 0) {
    if (controlBitmap_) {
      dc.DrawBitmap(*controlBitmap_, 0, 0);
    }
  }
}